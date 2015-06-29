unit DebugInfos;

interface

{$IFDEF DEBUG_INFO}
uses
  Windows, SysUtils, Classes, MMSystem, dotNetStrings;

procedure WriteFuncCall(const FuncName: string; const Args: array of const);
procedure WriteReturn(Value: Variant);
procedure Log(const Text: string);
procedure LogFilename(FileHandle: THandle);

function GetAccessRightsStr(dwDesiredAccess: DWORD): string;
{$ENDIF DEBUG_INFO}

implementation

{$IFDEF DEBUG_INFO}
var
  LogFile: TextFile;
  LogFileCS: TRTLCriticalSection;
  StartTime: Cardinal;
  OpenFiles: TStrings;
  LockWriting: Integer = 0;
  HasReturn: Boolean = False;

procedure WriteFuncCall(const FuncName: string; const Args: array of const);
var
  i: Integer;
  S: string;
begin
  if LockWriting <> 0 then
    Exit;
  EnterCriticalSection(LogFileCS);
  Inc(LockWriting);
  WriteLn(LogFile);
  Write(LogFile, {FormatDateTime('hh:nn:ss.zzz', Time - StartTime)}timeGetTime - StartTime, ': ', FuncName);
  S := '';
  for i := 0 to High(Args) do
  begin
    if i > 0 then
      S := S + ', {' + IntToStr(i) + '}'
    else
      S := S + '{' + IntToStr(i) + '}';
  end;
  if S <> '' then
  begin
    try
      Write(LogFile, '(', DotNetFormat(S, Args), ')')
    except
      on E: Exception do
      begin
        WriteLn(LogFile);
        Write(LogFile, 'ERROR: ', E.ClassName, ': ', E.Message);
      end;
    end;
  end
  else
    Write('()');
  Flush(LogFile);
  HasReturn := False;
  Dec(LockWriting);
  LeaveCriticalSection(LogFileCS);
end;

procedure WriteReturn(Value: Variant);
begin
  if LockWriting <> 0 then
    Exit;

  EnterCriticalSection(LogFileCS);
  Inc(LockWriting);
  try
    Write(LogFile, ' = return ', DotNetFormat('{0}', [Value]));
  except
    on E: Exception do
    begin
      WriteLn(LogFile);
      WriteLn(LogFile, 'ERROR: ', E.ClassName, ': ', E.Message);
    end;
  end;
  Flush(LogFile);
  HasReturn := True;
  Dec(LockWriting);
  LeaveCriticalSection(LogFileCS);
end;

procedure Log(const Text: string);
begin
  if LockWriting <> 0 then
    Exit;
  EnterCriticalSection(LogFileCS);
  Inc(LockWriting);
  if not HasReturn then
    WriteLn(LogFile);
  HasReturn := True;
  WriteLn(LogFile, Text);
  Flush(LogFile);
  Dec(LockWriting);
  LeaveCriticalSection(LogFileCS);
end;

procedure LogFilename(FileHandle: THandle);
var
  Idx: Integer;
begin
  for Idx := OpenFiles.Count - 1 downto 0 do
    if OpenFiles.Objects[Idx] = Pointer(FileHandle) then
    begin
      Log('  |- ' + OpenFiles[Idx]);
      Exit;
    end;
end;

function GetAccessRightsStr(dwDesiredAccess: DWORD): string;
begin
  if dwDesiredAccess and (GENERIC_WRITE or GENERIC_READ) = GENERIC_WRITE or GENERIC_READ then
    Result := 'ReadWrite'
  else if dwDesiredAccess and GENERIC_READ <> 0 then
    Result := 'Read'
  else if dwDesiredAccess and GENERIC_WRITE <> 0 then
    Result := 'Write'
  else
    Result := 'unknown';
end;

initialization
  InitializeCriticalSection(LogFileCS);
  OpenFiles := TStringList.Create;

  AssignFile(LogFile, 'C:\Log_' + ExtractFileName(ParamStr(0)) + '.txt');
  Rewrite(LogFile);
  StartTime := timeGetTime;

finalization
  CloseFile(LogFile);

  OpenFiles.SaveToFile('C:\Log_' + ExtractFileName(ParamStr(0)) + '_files.txt');
  OpenFiles.Free;

  DeleteCriticalSection(LogFileCS);
{$ENDIF DEBUG_INFO}

end.
