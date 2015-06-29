unit DLangExtCompiler;

interface

uses
  Windows, SysUtils, Classes, InterceptIntf;

type
  TDLangExtCompiler = class(TComponent, ICompileInterceptor)
  protected
    class procedure HandleException(Sender: TObject);
  public
    function GetOptions: TCompileInterceptOptions; stdcall;

    function AlterFile(Filename: PWideChar; Content: PByte; FileDate: Integer;
      FileSize: Integer): IVirtualStream; stdcall;
    function AlterMessage(IsCompilerMessage: Boolean; var MsgKind: TMsgKind; var Code: Integer;
      const Filename: IWideString; var Line, Column: Integer; const Msg: IWideString): Boolean; stdcall;
    function GetVirtualFile(Filename: PWideChar): IVirtualStream; stdcall;
    procedure InspectFilename(Filename: PWideChar; FileMode: TInspectFileMode); stdcall;

    procedure CompileProject(ProjectFilename, UnitPaths, SourcePaths, DcuOutputDir: PWideChar;
      IsCodeInsight: Boolean; var Cancel: Boolean); stdcall;
  end;

  TVirtualStreamAdapter = class(TInterfacedObject, IVirtualStream)
  private
    FStream: TStream;
    FAutoDelete: Boolean;
    FFileDate: Integer;
  public
    constructor Create(AStream: TStream; AFileDate: Integer; AAutoDelete: Boolean = True);
    destructor Destroy; override;

    function Seek(Offset: Integer; Origin: Integer): Integer; stdcall;
    function Read(var Buffer; Size: Integer): Integer; stdcall;
    procedure FileStatus(out FileDate: Integer; out FileSize: Integer); stdcall;
  end;

implementation

uses
  LanguageExtension, Utf8StringUtils, Messages, IDEHooks;

type
  TVirtualDiskStream = class(TInterfacedObject, IVirtualStream)
  private
    FStream: TFileStream;
  public
    constructor Create(const Filename: string);
    destructor Destroy; override;
    procedure FileStatus(out FileDate: Integer; out FileSize: Integer); stdcall;
    function Read(var Buffer; Size: Integer): Integer; stdcall;
    function Seek(Offset: Integer; Origin: Integer): Integer; stdcall;
  end;

function StrFileExt(P: PWideChar): PWideChar;
begin
  Result := nil;
  if P <> nil then
    while (P^ <> #0) do
    begin
      if (P^ = '.') then
        Result := P
      else if (P^ = '\') or (P^ = '/') then
        Result := nil;
      Inc(P);
    end;
  if Result = nil then
    Result := P; // points to #0
end;

{ TDLangExtCompiler }

function TDLangExtCompiler.GetOptions: TCompileInterceptOptions;
begin
  Result := CIO_ALTERFILES or CIO_VIRTUALFILES or CIO_COMPILEPROJECTS;
end;

function TDLangExtCompiler.AlterFile(Filename: PWideChar; Content: PByte; FileDate, FileSize: Integer): IVirtualStream;
var
  S: UTF8String;
  Ext: PWideChar;
begin
  Result := nil;
  Ext := StrFileExt(Filename);
  if (StrIComp(Ext, '.pas') = 0) or (StrIComp(Ext, '.inc') = 0) or (StrIComp(Ext, '.dpr') = 0) then
  begin
    S := LoadBufferToUTF8String(Content, FileSize);
    try
      if LanguageExtensionManager.AlterContent(Filename, S) then
        Result := TVirtualStreamAdapter.Create(CreateMemoryStreamFromUTF8String(S), FileDate);
    except
      if Assigned(ApplicationHandleException) then
        ApplicationHandleException(Self);
    end;
  end;
end;

function TDLangExtCompiler.AlterMessage(IsCompilerMessage: Boolean; var MsgKind: TMsgKind;
  var Code: Integer; const Filename: IWideString; var Line, Column: Integer; const Msg: IWideString): Boolean;
begin
  Result := False;
end;

procedure TDLangExtCompiler.CompileProject(ProjectFilename, UnitPaths, SourcePaths, DcuOutputDir: PWideChar;
  IsCodeInsight: Boolean; var Cancel: Boolean);
begin
  DLangExt_InitPaths(UnitPaths, SourcePaths, DcuOutputDir);
  LanguageExtensionManager.Reset(ProjectFilename, IsCodeInsight);
end;

function TDLangExtCompiler.GetVirtualFile(Filename: PWideChar): IVirtualStream;
var
  ExpFilename: string;
begin
  try
    ExpFilename := ExpandFileName(Filename);
    if LanguageExtensionManager.HasVirtualDiskFile(ExpFilename) then
      Result := TVirtualDiskStream.Create(ExpFilename)
    else
      Result := nil;
  except
    if Assigned(ApplicationHandleException) then
      ApplicationHandleException(Self);
    Result := nil;
  end;
end;

class procedure TDLangExtCompiler.HandleException(Sender: TObject);
begin
  if GetCapture <> 0 then
    SendMessage(GetCapture, WM_CANCELMODE, 0, 0);
  SysUtils.ShowException(ExceptObject, ExceptAddr);
end;

procedure TDLangExtCompiler.InspectFilename(Filename: PWideChar; FileMode: TInspectFileMode);
begin
end;

{ TVirtualStreamAdapter }

constructor TVirtualStreamAdapter.Create(AStream: TStream; AFileDate: Integer; AAutoDelete: Boolean);
begin
  inherited Create;
  FStream := AStream;
  FAutoDelete := AAutoDelete;
  FFileDate := AFileDate;
end;

destructor TVirtualStreamAdapter.Destroy;
begin
  if FAutoDelete then
    FStream.Free;
  inherited Destroy;
end;

procedure TVirtualStreamAdapter.FileStatus(out FileDate, FileSize: Integer);
begin
  FileSize := FStream.Size;
  FileDate := FFileDate;
end;

function TVirtualStreamAdapter.Read(var Buffer; Size: Integer): Integer;
begin
  Result := FStream.Read(Buffer, Size);
end;

function TVirtualStreamAdapter.Seek(Offset, Origin: Integer): Integer;
begin
  Result := FStream.Seek(Offset, Origin);
end;

{ TVirtualDiskStream }

constructor TVirtualDiskStream.Create(const Filename: string);
begin
  inherited Create;
  FStream := TFileStream.Create(Filename, fmOpenRead or fmShareDenyNone);
end;

destructor TVirtualDiskStream.Destroy;
begin
  FStream.Free;
  inherited Destroy;
end;

procedure TVirtualDiskStream.FileStatus(out FileDate, FileSize: Integer);
begin
  FileDate := FileGetDate(FStream.Handle);
  FileSize := FStream.Size;
end;

function TVirtualDiskStream.Read(var Buffer; Size: Integer): Integer;
begin
  Result := FStream.Read(Buffer, Size);
end;

function TVirtualDiskStream.Seek(Offset, Origin: Integer): Integer;
begin
  Result := FStream.Seek(Offset, Origin);
end;

initialization
  if not Assigned(ApplicationHandleException) then
    ApplicationHandleException := TDLangExtCompiler.HandleException;

end.
