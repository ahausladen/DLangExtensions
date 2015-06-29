unit CompilerHooks;

interface

uses
  Windows, SysUtils, Classes;

function CompilerOpenFile(Filename: PChar; Handle: THandle): TStream;
procedure InspectFilename(Filename: PChar; WriteMode: Boolean);
procedure PathsFromConfigFile(ConfigContent: PAnsiChar; out UnitPaths: string; var DcuOutputDir: AnsiString);
procedure InitCommandLinePaths;

var
  UnitPaths: AnsiString;
  DcuOutputDir: AnsiString;

implementation

uses
  DefaultAPIHooks, DLangExtImport;

var
  ErrorValue: TStringValue;
  PathsInitialized: Boolean = False;
  ConfigUnitPaths: AnsiString;

{--------------------------------------------------------------------------------------------------}

procedure PathsFromConfigFile(ConfigContent: PAnsiChar; out UnitPaths: string; var DcuOutputDir: AnsiString);
var
  P: PAnsiChar;
  Param: AnsiString;
begin
  UnitPaths := '';
  P := GetNextParameter(ConfigContent, Param);
  while P <> nil do
  begin
    while Param[1] = '"' do
      Delete(Param, 1, 1);

    if (Param[1] = '-') or (Param[1] = '/') then
    begin
      if (Length(Param) > 2) and (Param[2] = 'U') or (Param[2] = 'u') then
        UnitPaths := UnitPaths + RemoveQuotes(Copy(Param, 3, MaxInt)) + ';'
      else
      if (Length(Param) > 2) and (Param[2] = 'N') or (Param[2] = 'n') then
      begin
        if Length(Param) > 3 then
        begin
          if (Param[3] = '0') or not (Param[3] in ['H', 'O', 'B', 'S']) then
          begin
            if Param[3] = '0' then
              DcuOutputDir := RemoveQuotes(Copy(Param, 4, MaxInt))
            else
              DcuOutputDir := RemoveQuotes(Copy(Param, 3, MaxInt));
          end;
        end;
      end;
    end;

    P := GetNextParameter(P, Param);
  end;
  if (UnitPaths <> '') {and (UnitPaths[Length(UnitPaths)] = ';')} then
    Delete(UnitPaths, Length(UnitPaths), 1);
end;

procedure InitCommandLinePaths;
var
  Param: AnsiString;
begin
  PathsFromConfigFile(GetNextParameter(PAnsiChar(CommandLineA), Param), UnitPaths, DcuOutputDir);
end;

procedure ParseConfigFile(P: PAnsiChar);
var
  LocalUnitPaths: AnsiString;
begin
  PathsFromConfigFile(P, LocalUnitPaths, DcuOutputDir);
  if LocalUnitPaths <> '' then
  begin
    if ConfigUnitPaths <> '' then
      ConfigUnitPaths := ConfigUnitPaths + ';' + LocalUnitPaths
    else
      ConfigUnitPaths := LocalUnitPaths;
  end;
end;

{--------------------------------------------------------------------------------------------------}

function CompilerOpenFile(Filename: PChar; Handle: THandle): TStream;
var
  Ext: PChar;
  Data: AnsiString;
  StringValue: TStringValue;
  IsCfg: Boolean;
begin
  Result := nil;
  Ext := StrFileExt(Filename);
  IsCfg := not PathsInitialized and (StrLIComp(Ext, '.cfg', 4) = 0);
  if IsCfg or (StrLIComp(Ext, '.pas', 4) = 0) or (StrLIComp(Ext, '.inc', 4) = 0) or
     (StrLIComp(Ext, '.dpr', 4) = 0) then
  begin
    SetLength(Data, GetFileSize(Handle, nil));
    if Data <> '' then
      FileRead(Handle, PAnsiChar(Data)^, Length(Data));
    if not IsCfg then
    begin
      if not PathsInitialized then
      begin
        PathsInitialized := True;
        InitCommandLinePaths; // overwrite DcuOutputDir if set by command line
        if UnitPaths = '' then
          UnitPaths := ConfigUnitPaths
        else if ConfigUnitPaths <> '' then
          UnitPaths := UnitPaths + ';' + ConfigUnitPaths;
        DLangExt_InitPaths(PAnsiChar(UnitPaths), PAnsiChar(UnitPaths), PAnsiChar(DcuOutputDir));
      end;
      StringValue := TStringValue.Create(Data);
      try
        ErrorValue.Value := '';
        DLangExt_AlterContent(Filename, StringValue, ErrorValue);
        Result := TStringStream.Create(StringValue.Value);
        if ErrorValue.Value <> '' then
          WriteLn(ErrOutput, 'Fatal: ', ErrorValue.Value);
      finally
        StringValue.Free;
      end;
    end
    else
      { Parse config file }
      if Data <> '' then
        ParseConfigFile(PAnsiChar(Data));
  end;
end;

procedure InspectFilename(Filename: PChar; WriteMode: Boolean);
const
  FileMode: array[Boolean] of TInspectFileMode = (ifmOpen, ifmCreate);
begin
  if StrLIComp(StrFileExt(Filename), '.dcu', 4) = 0 then
    DLangExt_InspectFilename(Filename, FileMode[WriteMode]);
end;

initialization
  ErrorValue := TStringValue.Create('');

finalization
  ErrorValue.Free;
  DLangExt_Cleanup();

end.
