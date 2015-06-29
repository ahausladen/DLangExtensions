unit LangExtRegister;

interface

uses
  SysUtils, LanguageExtension;

procedure RegisterLanguageExtensions;

implementation

uses
  MultilineStrings, Macros, ForInExtension, CaseStringOfExtension,
  PluginExtensions, UniSwitchExtension;

procedure RegisterLanguageExtensions;
begin
  { plain content }
  LanguageExtensionManager.RegisterExtension(TMultiLineString);

  { Lexer }
  LanguageExtensionManager.RegisterExtension(TMacros);
  LanguageExtensionManager.RegisterExtension(TUniSwitchExtension);
  LanguageExtensionManager.RegisterExtension(TForInExtension);
  LanguageExtensionManager.RegisterExtension(TCaseStringOfExtension);

  { Plugins }
  LanguageExtensionManager.RegisterExtension(TPluginExtensions);
end;

initialization
  { force US format for floats }
  DecimalSeparator := '.';
  ThousandSeparator := ',';

end.
