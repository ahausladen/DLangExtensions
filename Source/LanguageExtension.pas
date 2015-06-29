unit LanguageExtension;

{$STRINGCHECKS OFF}

interface

uses
  Windows, SysUtils, Classes, Contnrs, DelphiLexer, LangExtIntf, InterceptIntf,
  Utils, Generics.Collections;

const
  sVersionStr = '2.0';

type
  TLoadFileContentProc = function(const Filename: string): UTF8String;

  TProc = procedure;

  TLanguageExtensionManager = class;

  TLanguageExtension = class(TInterfaceEnabledObject)
  private
    FFilename: string;
    FManager: TLanguageExtensionManager;
    FNamespace: string;
  protected
    procedure RegisterVirtualDiskFile(const Filename: string); inline;
    procedure UnregisterVirtualDiskFile(const Filename: string); inline;

    function MakeDLangExtFilename(const Filename: string): string;
    function ExtractDLangExtFilename(const Filename: string): string;
    procedure ForceDLangExtDirectory(const Directory: string);
  public
    constructor Create(const AFilename: string; AManager: TLanguageExtensionManager); virtual;
    procedure Initialize; virtual;
  protected
    class procedure CompileProject(const AFilename: string; AIsCodeInsight: Boolean); virtual;

    { In AlterContent() the plain content can be changed. It is the first method that is called. }
    function AlterContent(var Content: UTF8String): Boolean; virtual;

    { In AlterLexer() the content represented by the Lexer can be changed. This method is called
      after the AlterContent() method of all language extensions were called. }
    procedure AlterLexer(Lexer: TDelphiLexer); virtual; //
  public
    { The full qualified file name of the current file }
    property Filename: string read FFilename;

    { The unit namespace (filename without path and without suffix }
    property Namespace: string read FNamespace;

    property Manager: TLanguageExtensionManager read FManager;
  end;

  TLanguageExtensionClass = class of TLanguageExtension;

  TLanguageExtensionManager = class(TObject)
  private
    FExtensions: TClassList; // LanguageExtensions modules
    FVirtualDiskFiles: TStringList; // files who's content shouldn't be read form the IDE editor

    FSourcePaths: string;
    FDcuOutputDir: string;
    FUnitPaths: string;
    FLoadFileContent: TLoadFileContentProc;
    FFirstFile: Boolean;
    FIsCodeInsight: Boolean;

    function GetExtensionCount: Integer;
    function GetExtension(Index: Integer): TLanguageExtensionClass;
    function InternAlterContent(const Filename: string; var Content: UTF8String): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Reset(const AFilename: string; AIsCodeInsight: Boolean);

    procedure RegisterExtension(Item: TLanguageExtensionClass);
    procedure RegisterVirtualDiskFile(const Filename: string);
    procedure UnregisterVirtualDiskFile(const Filename: string);
    function HasVirtualDiskFile(const Filename: string): Boolean;

    function AlterContent(const AFilename: string; var Content: UTF8String): Boolean;

    property ExtensionCount: Integer read GetExtensionCount;
    property Extensions[Index: Integer]: TLanguageExtensionClass read GetExtension; default;

    property UnitPaths: string read FUnitPaths write FUnitPaths;
    property SourcePaths: string read FSourcePaths write FSourcePaths;
    property DcuOutputDir: string read FDcuOutputDir write FDcuOutputDir;
    property IsCodeInsight: Boolean read FIsCodeInsight write FIsCodeInsight;

    property LoadFileContent: TLoadFileContentProc read FLoadFileContent write FLoadFileContent;
  end;

var
  LanguageExtensionManager: TLanguageExtensionManager;

type
  IString = interface // UTF8
    ['{264156F0-06C4-4CD7-9364-63CF1A5BF683}']
    procedure SetString(Buffer: PUtf8Char; Len: Cardinal); stdcall;
    function GetString(out Len: Cardinal): PUtf8Char; stdcall;
  end;

procedure AddCleanupProc(Proc: TProc);


function DLangExt_GetVersion(const VersionStr: IString): LongBool; stdcall;
procedure DLangExt_InitPaths(UnitPaths, SourcePaths, DcuOutputDir: PWideChar); stdcall;
function DLangExt_AlterContent(const Filename: PWideChar; Content: PByte; Size: Cardinal;
  const ErrorMsg: IString): IVirtualStream; stdcall;
procedure DLangExt_Cleanup; stdcall;

exports
  DLangExt_GetVersion,
  DLangExt_InitPaths,
  DLangExt_AlterContent,
  DLangExt_Cleanup;

implementation

uses
  LangExtRegister, Utf8StringUtils;

var
  CleanupList: TList;


{ TLanguageExtensionManager }

constructor TLanguageExtensionManager.Create;
begin
  inherited Create;
  @FLoadFileContent := @LoadTextFileToUtf8String;

  FExtensions := TClassList.Create;
  FVirtualDiskFiles := TStringList.Create;
  FVirtualDiskFiles.Sorted := True;
  FFirstFile := True;
end;

destructor TLanguageExtensionManager.Destroy;
begin
  FVirtualDiskFiles.Free;
  FExtensions.Free;
  inherited Destroy;
end;

procedure TLanguageExtensionManager.RegisterExtension(Item: TLanguageExtensionClass);
begin
  FExtensions.Add(Item);
end;

function TLanguageExtensionManager.GetExtensionCount: Integer;
begin
  Result := FExtensions.Count;
end;

function TLanguageExtensionManager.GetExtension(Index: Integer): TLanguageExtensionClass;
begin
  Result := TLanguageExtensionClass(FExtensions[Index]);
end;

procedure TLanguageExtensionManager.Reset(const AFilename: string; AIsCodeInsight: Boolean);
var
  I: Integer;
begin
  FIsCodeInsight := AIsCodeInsight;
  FVirtualDiskFiles.Clear;
  for I := 0 to ExtensionCount - 1 do
    Extensions[I].CompileProject(AFilename, AIsCodeInsight);
  FFirstFile := False;
end;

function TLanguageExtensionManager.HasVirtualDiskFile(const Filename: string): Boolean;
begin
  Result := FVirtualDiskFiles.IndexOf(Filename) <> -1;
end;

procedure TLanguageExtensionManager.RegisterVirtualDiskFile(const Filename: string);
begin
  if FVirtualDiskFiles.IndexOf(Filename) = -1 then
    FVirtualDiskFiles.Add(Filename);
end;

procedure TLanguageExtensionManager.UnregisterVirtualDiskFile(const Filename: string);
var
  Index: Integer;
begin
  if FVirtualDiskFiles.Find(Filename, Index) then
    FVirtualDiskFiles.Delete(Index);
end;

function TLanguageExtensionManager.AlterContent(const AFilename: string; var Content: UTF8String): Boolean;
var
  FileName, Name: string;
  Ext: string;
begin
  FileName := ExpandFileName(AFilename);
  Name := ExtractFileName(AFileName);

  { It is a new project or a new compilation }
  Ext := LowerCase(ExtractFileExt(Name));
  if FFirstFile then
  begin
    if (Ext = '.dpr') or (Ext = '.dpk') then
    begin
      FFirstFile := False;
      Reset(AFilename, IsCodeInsight);
    end
    else
    if Ext = '.pas' then
    begin
      FFirstFile := False;
      Reset('', IsCodeInsight); // compiling single unit
    end;
  end;

  { Preprocess the file }
  Result := InternAlterContent(Filename, Content);
end;

function TLanguageExtensionManager.InternAlterContent(const Filename: string; var Content: UTF8String): Boolean;
var
  I: Integer;
  Lexer: TDelphiLexer;
  Extensions: TObjectList;
  Item: TLanguageExtension;
begin
  Result := False;
  Extensions := TObjectList.Create;
  try
    { Create language extension instances for this module and do the plain content modifications }
    for I := 0 to LanguageExtensionManager.ExtensionCount - 1 do
    begin
      Item := LanguageExtensionManager[I].Create(Filename, Self);
      Extensions.Add(Item);

      if Item.AlterContent(Content) then
        Result := True;
    end;

    { Apply the language extensions that use the lexer }
    Lexer := TDelphiLexer.Create(Filename, Content);
    try
      Lexer.SupportMacroTokens := True;
      for I := 0 to Extensions.Count - 1 do
      begin
        Item := TLanguageExtension(Extensions[I]);
        Lexer.RestartLexer;
        Item.AlterLexer(Lexer);
      end;

      if Lexer.Modified then
      begin
        Content := Lexer.Text;
        Result := True;
      end;
    finally
      Lexer.Free;
    end;
  finally
    Extensions.Free;
  end;
end;

{ TLanguageExtension }

constructor TLanguageExtension.Create(const AFilename: string; AManager: TLanguageExtensionManager);
begin
  inherited Create;
  FFilename := AFilename;
  FManager := AManager;
  FNamespace := ChangeFileExt(ExtractFileName(Filename), '');

  Initialize;
end;

procedure TLanguageExtension.Initialize;
begin
end;

class procedure TLanguageExtension.CompileProject(const AFilename: string; AIsCodeInsight: Boolean);
begin
end;

procedure TLanguageExtension.AlterLexer(Lexer: TDelphiLexer);
begin
end;

function TLanguageExtension.AlterContent(var Content: UTF8String): Boolean;
begin
  Result := False;
end;

function TLanguageExtension.ExtractDLangExtFilename(const Filename: string): string;
begin
  Result := '__DLangExt' + PathDelim + ExtractFileName(Filename);
end;

procedure TLanguageExtension.ForceDLangExtDirectory(const Directory: string);
begin
  if Directory <> '' then
    ForceDirectories(Directory);
end;

function TLanguageExtension.MakeDLangExtFilename(const Filename: string): string;
begin
  Result := ExtractFilePath(Filename) + '__DLangExt' + PathDelim + ExtractFileName(Filename);
end;

procedure TLanguageExtension.RegisterVirtualDiskFile(const Filename: string);
begin
  LanguageExtensionManager.RegisterVirtualDiskFile(Filename);
end;

procedure TLanguageExtension.UnregisterVirtualDiskFile(const Filename: string);
begin
  LanguageExtensionManager.UnregisterVirtualDiskFile(Filename);
end;

{--------------------------------------------------------------------------------------------------}

function DLangExt_GetVersion(const VersionStr: IString): LongBool; stdcall;
begin
  Result := VersionStr <> nil;
  if Result then
    VersionStr.SetString(PAnsiChar(sVersionStr), Length(sVersionStr));
end;

procedure DLangExt_InitPaths(UnitPaths, SourcePaths, DcuOutputDir: PWideChar); stdcall;
begin
  LanguageExtensionManager.UnitPaths := RemoveQuotes(UnitPaths);
  LanguageExtensionManager.SourcePaths := RemoveQuotes(SourcePaths);
  LanguageExtensionManager.DcuOutputDir := RemoveQuotes(DcuOutputDir);
end;

function DLangExt_AlterContent(const Filename: PWideChar; Content: PByte; Size: Cardinal;
  const ErrorMsg: IString): IVirtualStream; stdcall;
var
  S: UTF8String;
  Intf: LangExtIntf.IVirtualStream;
begin
  Result := nil;

  if ErrorMsg <> nil then
    ErrorMsg.SetString(nil, 0);
  try
    if Content <> nil then
    begin
      S := LoadBufferToUTF8String(Content, Size);
      if LanguageExtensionManager.AlterContent(Filename, S) then
      begin
        Intf := TVirtualStream.Create(CreateMemoryStreamFromUTF8String(S), 0);
        Result := IVirtualStream(Intf); // both interfaces are equal, same GUID
      end;
    end;
  except
    on EAbort do
      Result := nil;
    on E: Exception do
    begin
      if ErrorMsg <> nil then
      begin
        S := UTF8Encode(E.Message);
        ErrorMsg.SetString(PUtf8Char(S), Length(S));
      end;
      Result := nil
    end;
  end;
end;

procedure DLangExt_Cleanup; stdcall;
var
  I: Integer;
  List: TList;
begin
  List := CleanupList;
  if CleanupList <> nil then
  begin
    CleanupList := nil;
    try
      for I := List.Count - 1 downto 0 do
        TProc(List[i])();
    finally
      List.Free;
    end;
  end;
end;

procedure AddCleanupProc(Proc: TProc);
begin
  if CleanupList = nil then
    CleanupList := TList.Create;
  CleanupList.Add(@Proc);
end;

initialization
  LanguageExtensionManager := TLanguageExtensionManager.Create;
  RegisterLanguageExtensions;

finalization
  FreeAndNil(LanguageExtensionManager);
  DLangExt_Cleanup;

end.
