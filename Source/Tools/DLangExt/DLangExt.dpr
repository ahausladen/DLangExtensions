program DLangExt;

{$APPTYPE CONSOLE}

uses
  Windows,
  SysUtils,
  DLangExtImport in '..\DLangExtImport.pas';

{--------------------------------------------------------------------------------------------------}
var
  TestMode: Boolean;
  Recursive: Boolean;
  OverwriteOriginalFile: Boolean;

  ErrorValue: TStringValue;
  ResolveCount: Integer;

function PreprocessFile(const Filename: string): Boolean;
var
  hFile: THandle;
  Size: DWORD;
  StringValue: TStringValue;
  FullFilename: string;
  CurDir: string;
begin
  FullFilename := ExpandFileName(Filename);
  StringValue := TStringValue.Create('');
  try
    hFile := FileOpen(FullFilename, fmOpenRead or fmShareDenyNone);
    try
      Size := GetFileSize(hFile, nil);
      if Size > 0 then
      begin
        StringValue.SetLength(Size);
        FileRead(hFile, StringValue.GetString(Size)^, Size);
      end;
    finally
      FileClose(hFile);
    end;

    CurDir := GetCurrentDir;
    Result := DLangExt_AlterContent(PAnsiChar(FullFilename), StringValue, ErrorValue);
    if Result then
    begin
      if not TestMode then
      begin
        if OverwriteOriginalFile then
          hFile := FileOpen(FullFilename, fmOpenWrite or fmShareDenyWrite)
        else
          hFile := FileCreate(FullFilename + '.i');
        try
          FileWrite(hFile, StringValue.GetString(Size)^, Size);
          SetEndOfFile(hFile)
        finally
          FileClose(hFile);
        end;
        WriteLn('Preprocessed: ', Filename);
      end
      else
        WriteLn('Preprocessed: ', Filename, '  (test)');
    end;
    if GetCurrentDir <> CurDir then
      SetCurrentDir(CurDir);
  finally
    StringValue.Free;
  end;

  if not Result and (ErrorValue.Value <> '') then
    raise Exception.Create(ErrorValue.Value);
end;

function PreprocessFiles(const PathMask: string): Boolean;
var
  Path, Ext: string;
  sr: TSearchRec;
begin
  Result := False;
  Path := ExtractFilePath(PathMask);
  if FindFirst(PathMask, faAnyFile or faDirectory, sr) = 0 then
  begin
    try
      repeat
        if sr.Attr and faDirectory <> 0 then
        begin
          if Recursive and (sr.Name <> '.') and (sr.Name <> '..') then
          begin
            Result := PreprocessFiles(Path + sr.Name + PathDelim + ExtractFileName(PathMask));
            if ErrorValue.Value <> '' then
              Exit;
          end;
        end
        else
        begin
          Ext := ExtractFileExt(sr.Name);
          if SameText(Ext, '.pas') or SameText(Ext, '.inc') or SameText(Ext, '.dpr') then
          begin
            if PreprocessFile(Path + sr.Name) then
              Result := True
            else
            if ErrorValue.Value <> '' then
              Exit;
          end;
        end;
      until FindNext(sr) <> 0;
    finally
      FindClose(sr);
    end;
  end;
end;

var
  Filename: string;
  i: Integer;
  VersionStr: TStringValue;
  ElapsedTime: TDateTime;
  UnitPaths: string;
begin
  VersionStr := TStringValue.Create('1.0');
  try
    DLangExt_GetVersion(VersionStr);
    WriteLn('DLangExtensions command line tool [Version ', VersionStr.Value, ']');
    WriteLn('(C) Copyright 2007 Andreas Hausladen.');
    WriteLn;
  finally
    VersionStr.Free;
  end;

  try
    TestMode := FindCmdLineSwitch('t', False);
    Recursive := FindCmdLineSwitch('r', False);
    OverwriteOriginalFile := FindCmdLineSwitch('o', False);

    { Read parameters }
    UnitPaths := '';
    for i := 1 to ParamCount do
    begin
      Filename := ParamStr(i);
      if (Filename = '-t') or (Filename = '/t') or (Filename = '-r') or (Filename = '/r') then
        Continue;
      if StrLIComp(PChar(Filename), '-U', 2) = 0 then
      begin
        if UnitPaths <> '' then
          UnitPaths := UnitPaths + ';' + Copy(Filename, 3, MaxInt)
        else
          UnitPaths := Copy(Filename, 3, MaxInt);
      end;
    end;
    if UnitPaths <> '' then
      DLangExt_InitPaths(PAnsiChar(UnitPaths), PAnsiChar(UnitPaths), nil);

    ResolveCount := 0;
    ErrorValue := TStringValue.Create('');
    try
      ElapsedTime := Now;
      { Preprocess filemask / file / directory }
      for i := 1 to ParamCount do
      begin
        if (Filename = '-t') or (Filename = '/t') or (Filename = '-r') or (Filename = '/r') then
          Continue;
        if StrLIComp(PChar(Filename), '-U', 2) = 0 then
          Continue;

        if FileExists(Filename) then
          PreprocessFile(Filename)
        else
        if DirectoryExists(Filename) then
          PreprocessFiles(Filename + '\*.*')
        else
          PreprocessFiles(Filename);
      end;
      ElapsedTime := (Now - ElapsedTime) * SecsPerDay;

      if ResolveCount > 0 then
      begin
        WriteLn;
        if ResolveCount = 1 then
          WriteLn(Format('Processed %d file in %.1f seconds', [ResolveCount, ElapsedTime]))
        else
          WriteLn(Format('Processed %d files in %.1f seconds', [ResolveCount, ElapsedTime]));
      end;
      ExitCode := 0;
    finally
      ErrorValue.Free;
      DLangExt_Cleanup();
    end;
    if ParamCount = 0 then
    begin
      WriteLn('No file/directory/mask specified.');
      WriteLn;
      WriteLn('Usage:');
      WriteLn('  DLangExt.exe [options] [ filemask | file | directory ] ...');
      WriteLn;
      WriteLn('Options:');
      WriteLn('  -t         Test, do not modify the files');
      WriteLn('  -r         Recursive, modify files in sub directories');
      WriteLn('  -o         Overwrite the original file instead of creating .i files');
      WriteLn('  -U<paths>  Unit directories'); 
      WriteLn;
      ExitCode := 1;
    end;
  except
    on E: Exception do
    begin
      Writeln(E.Classname, ': ', E.Message);
      ExitCode := 1;
    end;
  end;
end.

