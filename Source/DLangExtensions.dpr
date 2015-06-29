library DLangExtensions;

uses
  Windows,
  SysUtils,
  Classes,
  LangExtIntf in 'LangExtIntf.pas',
  InterceptIntf in 'Shared\CompileInterceptor\InterceptIntf.pas',
  InterceptLoader in 'Shared\CompileInterceptor\InterceptLoader.pas',
  DelphiExpr in 'Shared\DelphiExpr.pas',
  DelphiLexer in 'Shared\DelphiLexer.pas',
  DelphiParserContainers in 'Shared\DelphiParserContainers.pas',
  DelphiPreproc in 'Shared\DelphiPreproc.pas',
  Utf8StringUtils in 'Shared\Utf8StringUtils.pas',
  IDEHooks in 'Shared\IDEHooks.pas',
  Utils in 'Utils.pas',
  Macros in 'Macros\Macros.pas',
  LanguageExtension in 'LanguageExtension.pas',
  LangExtRegister in 'LangExtRegister.pas',
  DLangExtCompiler in 'DLangExtCompiler.pas',
  DelphiLexerUtils in 'DelphiLexerUtils.pas',
  LexerImplInclude in 'LexerImplInclude.pas',
  MultilineStrings in 'MultilineStrings\MultilineStrings.pas',
  ForInExtension in 'ForIn\ForInExtension.pas',
  CaseStringOfExtension in 'CaseStringOf\CaseStringOfExtension.pas',
  PluginExtensions in 'PluginExtensions\PluginExtensions.pas',
  UniSwitchExtension in 'UniSwitch\UniSwitchExtension.pas';

{$R *.res}

const
  WizardEntryPoint = 'INITWIZARD0001';

type
  TWizardRegisterProc = function(const Wizard: IInterface): Boolean;
  TWizardTerminateProc = procedure;

var
  DLangExtCompiler: TDLangExtCompiler;
  DLangExtCompilerId: Integer;
  Services: ICompileInterceptorServices;

function LoadFileFromIDE(const Filename: string): UTF8String;
var
  Stream: IVirtualStream;
  Date, Size: Integer;
begin
  Result := '';
  Stream := Services.GetFileContent(PWideChar(Filename));
  if Stream <> nil then
  begin
    Stream.FileStatus(Date, Size);
    SetLength(Result, Size);
    if Size > 0 then
      Stream.Read(Result[1], Size);
  end;
end;

procedure DoneWizard;
begin
  if (DLangExtCompilerId <> -1) and (Services <> nil) then
    Services.UnregisterInterceptor(DLangExtCompilerId);
  DLangExtCompilerId := -1;
  FreeAndNil(DLangExtCompiler);
  Services := nil;
  UnloadCompilerInterceptorServices;
end;

function InitWizard(const BorlandIDEServices: IInterface;
  RegisterProc: TWizardRegisterProc; var Terminate: TWizardTerminateProc): Boolean; stdcall;
begin
//  Terminate := DoneWizard;   moved to the bottom because Delphi 2009's stupid Local Variables View calls Terminate after every refresh
  Result := True;
  DLangExtCompilerId := -1;

  if Services <> nil then
    DoneWizard;

  try
    try
      Services := GetCompileInterceptorServices();
    except
      on E: Exception do
        raise Exception.CreateFmt('DLangExtensions IDE intergration initialization failed: %s', [E.Message]);
    end;
    if Services = nil then
      raise Exception.Create('DLangExtensions IDE intergration initialization failed: No CompileInterceptor services available.');

    DLangExtCompiler := TDLangExtCompiler.Create(nil);
    DLangExtCompilerId := Services.RegisterInterceptor(DLangExtCompiler);
    @LanguageExtensionManager.LoadFileContent := @LoadFileFromIDE;
  except
    on E: Exception do
      MessageBox(GetActiveWindow, PChar(E.Message), 'DLangExtensions IDE integration', MB_OK or MB_ICONERROR);
  end;
  Terminate := DoneWizard;
end;

exports
  InitWizard name WizardEntryPoint;

begin
end.
