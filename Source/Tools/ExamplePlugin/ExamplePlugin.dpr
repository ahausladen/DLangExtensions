library ExamplePlugin;

uses
  SysUtils,
  LangExtIntf in 'LangExtIntf.pas';

{$R *.res}

const
  sLogFilename = 'CompileLog.txt';

type
  TExampleExtension = class(TInterfacedObject, ILanguageExtension)
  private
    FLogFile: TextFile;
  public
    constructor Create;
    destructor Destroy; override;

    function AlterContent(Services: ILanguageExtensionServices; Content: IString): LongBool; stdcall;
    function AlterContentSecondPass(Services: ILanguageExtensionServices; Content: IString): LongBool; stdcall;
    procedure CompileProject(ProjectFilename: PAnsiChar); stdcall;
    procedure GenerateExportStreams(Services: ILanguageExtensionServices); stdcall;
  end;

{ TExampleExtension }

constructor TExampleExtension.Create;
begin
  inherited Create;
  AssignFile(FLogFile, ExtractFilePath(GetModuleName(HInstance)) + sLogFilename);
  Rewrite(FLogFile);
end;

destructor TExampleExtension.Destroy;
begin
  CloseFile(FLogFile);
  inherited Destroy;
end;

procedure TExampleExtension.CompileProject(ProjectFilename: PAnsiChar);
begin
  WriteLn(FLogFile, '===================================================');
  WriteLn(FLogFile, 'Project: ', ProjectFilename);
end;

function TExampleExtension.AlterContent(Services: ILanguageExtensionServices; Content: IString): LongBool;
var
  S: AnsiString;
  Len: Cardinal;
begin
  SetString(S, Content.GetString(Len), Len);
  { BDS 2006 or newer: Unicode BOM is included in the Content }

  { Some poking and logging }
  WriteLn(FLogFile, 'Preprocessing: ', Services.Filename);
  if Pos('program ', S) = 0 then
    WriteLn(FLogFile, '  Filetype: Program')
  else if Pos('library ', S) = 0 then
    WriteLn(FLogFile, '  Filetype: Library')
  else if Pos('package ', S) = 0 then
    WriteLn(FLogFile, '  Filetype: Package')
  else if Pos('unit ', S) = 0 then
    WriteLn(FLogFile, '  Filetype: Unit');
  WriteLn(FLogFile, '  Filesize: ', Len);

  Result := False; // we haven't changed the Content
end;

function TExampleExtension.AlterContentSecondPass(Services: ILanguageExtensionServices; Content: IString): LongBool;
begin
  Result := False; // we haven't changed the Content
end;

procedure TExampleExtension.GenerateExportStreams(Services: ILanguageExtensionServices);
begin
end;

{--------------------------------------------------------------------------------------------------}

procedure RegisterLanguageExtensions;
begin
  DLangExt_RegisterExtension(TExampleExtension.Create);
end;

{ DLLMain }
begin
  RegisterLanguageExtensionProc := RegisterLanguageExtensions;
end.
