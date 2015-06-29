program Dcc32le;

{$APPTYPE CONSOLE}

uses
  Windows,
  SysUtils,
  DLLLoader in 'DLLLoader.pas',
  DefaultAPIHooks in 'DefaultAPIHooks.pas',
  FileAPIHooks in 'FileAPIHooks.pas',
  DebugInfos in 'DebugInfos.pas',
  CompilerHooks in 'CompilerHooks.pas',
  DLangExtImport in '..\DLangExtImport.pas';

{--------------------------------------------------------------------------------------------------}

var
  ExecutableName: string;
  VersionStr: TStringValue;
  ModuleName: string;
begin
  VersionStr := TStringValue.Create('1.0');
  try
    DLangExt_GetVersion(VersionStr);
    WriteLn('DLangExtensions Preprocessor - Version ', VersionStr.Value);
    WriteLn('Copyright (c) 2007 Andreas Hausladen.');
    WriteLn;
  finally
    VersionStr.Free;
  end;

  ModuleName := GetModuleName(0);
  if SameText(ExtractFileName(ModuleName), 'dcc32.exe') then
    ExecutableName := ExpandFileName(ExtractFilePath(ModuleName) + 'dcc32compiler.exe')
  else if SameText(ExtractFileName(ModuleName), 'dccil.exe') then
    ExecutableName := ExpandFileName(ExtractFilePath(ModuleName) + 'dccilcompiler.exe')
  else
    ExecutableName := FileSearch('dcc32.exe', ExtractFileDir(ModuleName) + ';.;' + GetEnvironmentVariable('PATH'));

  CommandLineA := '"' + ExecutableName + '"' + SkipString(GetCommandLineA);
  CommandLineW := '"' + ExecutableName + '"' + SkipStringW(GetCommandLineW);

  InitCommandLinePaths;

  hApp := TDefaultExeLoader(LoadExeMem(PChar(ExecutableName)));
  if hApp <> nil then
  begin
    try
      TExeEntryPoint(hApp.EntryPoint)();
      { ---------------------------------------- }
      { -- The process ends in the EntryPoint -- }
      { ---------------------------------------- }
    except
      on E: Exception do
        WriteLn(ErrOutput, 'Error: ', E.Message);
    end;
  end
  else
  begin
    WriteLn(ErrOutput, 'Error: Cannot load "', ExecutableName, '"');
    Halt(1);
  end;
end.
