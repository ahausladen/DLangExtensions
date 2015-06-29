unit FileAPIHooks;

interface

uses
  {$IFDEF DEBUG_INFO}
  Variants,
  {$ENDIF}
  Windows, SysUtils, DLLLoader, DefaultAPIHooks, Classes, Contnrs;

{ TFileExeLoader }

type
  TFileHookExeLoader = class(TDefaultExeLoader)
  protected
    function DoGetProcAddress(hModule: HMODULE; ProcName: PAnsiChar): Pointer; override;
  end;

implementation

uses
  DebugInfos, CompilerHooks;

var
  FileDataHandles: TBucketList;

function CompilerOpenFileW(Filename: PWideChar; Handle: THandle): TStream;
var
  S: string;
begin
  S := Filename;
  Result := CompilerOpenFile(PChar(S), Handle);
end;

procedure InspectFilenameW(Filename: PWideChar; WriteMode: Boolean);
var
  S: string;
begin
  S := Filename;
  InspectFilename(PChar(S), WriteMode);
end;

{------------------------------------------------------------------------------}
function MyCreateFileA(lpFileName: PAnsiChar; dwDesiredAccess, dwShareMode: DWORD;
  lpSecurityAttributes: PSecurityAttributes; dwCreationDisposition, dwFlagsAndAttributes: DWORD;
  hTemplateFile: THandle): THandle; stdcall;
var
  Ext: PAnsiChar;
  Stream: TStream;
begin
  {$IFDEF DEBUG_INFO}
  WriteFuncCall('CreateFileA', [GetAccessRightsStr(dwDesiredAccess),
    string(lpFileName), dwDesiredAccess, dwShareMode,
    lpSecurityAttributes, dwCreationDisposition, dwFlagsAndAttributes,
    hTemplateFile]);
  {$ENDIF DEBUG_INFO}

  Result := CreateFileA(lpFilename, dwDesiredAccess, dwShareMode,
    lpSecurityAttributes, dwCreationDisposition, dwFlagsAndAttributes,
    hTemplateFile);

  if Result <> INVALID_HANDLE_VALUE then
  begin
    if dwDesiredAccess and (GENERIC_READ or GENERIC_WRITE) = GENERIC_READ then
    begin
      { Open file for read }
      Ext := StrFileExt(lpFileName);
      if (StrIComp(Ext, '.dcu') <> 0) and (StrIComp(Ext, '.dcp') <> 0) and
         (StrIComp(Ext, '.dcuil') <> 0) and (StrIComp(Ext, '.dcpil') <> 0) and
         (StrIComp(Ext, '.exe') <> 0) then
      begin
        Stream := CompilerOpenFile(lpFileName, Result);
        if Stream <> nil then
          FileDataHandles.Add(Pointer(Result), Stream)
        {else
          SetFilePointer(Result, 0, 0)};
      end;
    end;
    if Result <> INVALID_HANDLE_VALUE then
      InspectFilename(lpFileName, dwDesiredAccess and GENERIC_WRITE <> 0);
  end;
  {$IFDEF DEBUG_INFO}
  WriteReturn(IntToHex(Result, 8));
  {$ENDIF DEBUG_INFO}
end;

function MyCreateFileW(lpFileName: PWideChar; dwDesiredAccess, dwShareMode: DWORD;
  lpSecurityAttributes: PSecurityAttributes; dwCreationDisposition, dwFlagsAndAttributes: DWORD;
  hTemplateFile: THandle): THandle; stdcall;
var
  Ext: PWideChar;
  Stream: TStream;
begin
  {$IFDEF DEBUG_INFO}
  WriteFuncCall('CreateFileW', [GetAccessRightsStr(dwDesiredAccess),
    lpFileName, dwDesiredAccess, dwShareMode,
    lpSecurityAttributes, dwCreationDisposition, dwFlagsAndAttributes,
    hTemplateFile]);
  {$ENDIF DEBUG_INFO}

  Result := CreateFileW(lpFilename, dwDesiredAccess, dwShareMode,
    lpSecurityAttributes, dwCreationDisposition, dwFlagsAndAttributes,
    hTemplateFile);

  if Result <> INVALID_HANDLE_VALUE then
  begin
    if dwDesiredAccess and (GENERIC_READ or GENERIC_WRITE) = GENERIC_READ then
    begin
      { Open file for read }
      Ext := StrFileExtW(lpFileName);
      if (StrICompW(Ext, '.dcu') <> 0) and (StrICompW(Ext, '.dcp') <> 0) and
         (StrICompW(Ext, '.dcuil') <> 0) and (StrICompW(Ext, '.dcpil') <> 0) and
         (StrICompW(Ext, '.exe') <> 0) then
      begin
        Stream := CompilerOpenFileW(lpFileName, Result);
        if Stream <> nil then
          FileDataHandles.Add(Pointer(Result), Stream)
        {else
          SetFilePointer(Result, 0, 0)};
      end;
    end;
    if Result <> INVALID_HANDLE_VALUE then
      InspectFilenameW(lpFileName, dwDesiredAccess and GENERIC_WRITE <> 0);
  end;
  {$IFDEF DEBUG_INFO}
  WriteReturn(IntToHex(Result, 8));
  {$ENDIF DEBUG_INFO}
end;

{------------------------------------------------------------------------------}
function MyCloseHandle(hObject: THandle): BOOL; stdcall;
var
  Stream: TStream;
begin
  {$IFDEF DEBUG_INFO}
  WriteFuncCall('CloseHandle', [IntToHex(hObject, 8)]);
  {$ENDIF DEBUG_INFO}
  if (hObject <> INVALID_HANDLE_VALUE) then
  begin
    Stream := TStream(FileDataHandles.Remove(Pointer(hObject)));
    if Stream <> nil then
      Stream.Free;
  end;

  Result := CloseHandle(hObject);
end;

{------------------------------------------------------------------------------}
function MyReadFile(hFile: THandle; var Buffer; nNumberOfBytesToRead: DWORD;
  var lpNumberOfBytesRead: DWORD; lpOverlapped: POverlapped): BOOL; stdcall;
var
  BytesRead: DWORD;
  Stream: TStream;
begin
  {$IFDEF DEBUG_INFO}
  WriteFuncCall('ReadFile', [IntToHex(hFile, 8), Pointer(@Buffer), nNumberOfBytesToRead]);
  {$ENDIF DEBUG_INFO}

  if FileDataHandles.Find(Pointer(hFile), Pointer(Stream)) then
  begin
    BytesRead := 0;
    if @Buffer <> nil then
    begin
      BytesRead := Stream.Read(Buffer, nNumberOfBytesToRead);
      Result := True;
    end
    else
      Result := False;
  end
  else
    Result := ReadFile(hFile, Buffer, nNumberOfBytesToRead, BytesRead, lpOverlapped);

  if @lpNumberOfBytesRead <> nil then
    lpNumberOfBytesRead := BytesRead;

  {$IFDEF DEBUG_INFO}
  WriteReturn(VarToStr(Result) + ' | ' + IntToStr(BytesRead));
  {$ENDIF DEBUG_INFO}
end;

{------------------------------------------------------------------------------}
function MyWriteFile(hFile: THandle; const Buffer; nNumberOfBytesToWrite: DWORD;
  var lpNumberOfBytesWritten: DWORD; lpOverlapped: POverlapped): BOOL; stdcall;
var
  BytesWritten: DWORD;
begin
  {$IFDEF DEBUG_INFO}
  WriteFuncCall('WriteFile', [IntToHex(hFile, 8), Pointer(@Buffer), nNumberOfBytesToWrite]);
  {$ENDIF DEBUG_INFO}

  Result := WriteFile(hFile, Buffer, nNumberOfBytesToWrite, BytesWritten, lpOverlapped);
  if @lpNumberOfBytesWritten <> nil then
    lpNumberOfBytesWritten := BytesWritten;
  {$IFDEF DEBUG_INFO}
  WriteReturn(Result);
  {$ENDIF DEBUG_INFO}
end;

{------------------------------------------------------------------------------}
function MySetFilePointer(hFile: THandle; lDistanceToMove: Longint;
  lpDistanceToMoveHigh: Pointer; dwMoveMethod: DWORD): DWORD; stdcall;
var
  Stream: TStream;
begin
  {$IFDEF DEBUG_INFO}
  WriteFuncCall('SetFilePointer', [IntToHex(hFile, 8), lDistanceToMove, dwMoveMethod]);
  LogFilename(hFile);
  {$ENDIF DEBUG_INFO}
  if FileDataHandles.Find(Pointer(hFile), Pointer(Stream)) then
  begin
    Result := Stream.Seek(lDistanceToMove, dwMoveMethod);
    if lpDistanceToMoveHigh <> nil then
      DWORD(lpDistanceToMoveHigh^) := 0;
  end
  else
    Result := SetFilePointer(hFile, lDistanceToMove, lpDistanceToMoveHigh, dwMoveMethod);
  {$IFDEF DEBUG_INFO}
  WriteReturn(Result);
  {$ENDIF DEBUG_INFO}
end;

{------------------------------------------------------------------------------}
function MySetEndOfFile(hFile: THandle): BOOL; stdcall;
var
  Stream: TStream;
begin
  {$IFDEF DEBUG_INFO}
  WriteFuncCall('SetEndOfFile', [IntToHex(hFile, 8)]);
  LogFilename(hFile);
  {$ENDIF DEBUG_INFO}
  if FileDataHandles.Find(Pointer(hFile), Pointer(Stream)) then
  begin
    try
      Stream.Size := Stream.Position;
      Result := True;
    except
      Result := False;
    end;
  end
  else
    Result := SetEndOfFile(hFile);
  {$IFDEF DEBUG_INFO}
  WriteReturn(Result);
  {$ENDIF DEBUG_INFO}
end;

{------------------------------------------------------------------------------}
function MyGetFileInformationByHandle(hFile: THandle; var lpFileInformation: TByHandleFileInformation): BOOL; stdcall;
var
  Stream: TStream;
begin
  {$IFDEF DEBUG_INFO}
  WriteFuncCall('GetFileInformationByHandle', [IntToHex(hFile, 8)]);
  LogFilename(hFile);
  {$ENDIF DEBUG_INFO}
  Result := GetFileInformationByHandle(hFile, lpFileInformation);
  if FileDataHandles.Find(Pointer(hFile), Pointer(Stream)) then
  begin
    lpFileInformation.nFileSizeHigh := 0;
    lpFileInformation.nFileSizeLow := Stream.Size;
  end
  else
  {$IFDEF DEBUG_INFO}
  WriteReturn(Result);
  {$ENDIF DEBUG_INFO}
end;

{------------------------------------------------------------------------------}
function MyGetFileSize(hFile: THandle; lpFileSizeHigh: Pointer): DWORD; stdcall;
var
  Stream: TStream;
begin
  {$IFDEF DEBUG_INFO}
  WriteFuncCall('GetFileSize', [IntToHex(hFile, 8)]);
  LogFilename(hFile);
  {$ENDIF DEBUG_INFO}
  if FileDataHandles.Find(Pointer(hFile), Pointer(Stream)) then
  begin
    Result := Stream.Size;
    if lpFileSizeHigh <> nil then
      PInteger(lpFileSizeHigh)^ := 0;
  end
  else
    Result := GetFileSize(hFile, lpFileSizeHigh);
  {$IFDEF DEBUG_INFO}
  WriteReturn(Result);
  {$ENDIF DEBUG_INFO}
end;

(*function MyOpenFile(const lpFileName: LPCSTR; var lpReOpenBuff: TOFStruct; uStyle: UINT): HFILE; stdcall;
begin
  {$IFDEF DEBUG_INFO}
  WriteFuncCall('OpenFile', [string(lpFileName)]);
  {$ENDIF DEBUG_INFO}
  Result := OpenFile(lpFileName, lpReOpenBuff, uStyle);
  {$IFDEF DEBUG_INFO}
  WriteReturn(Result);
  {$ENDIF DEBUG_INFO}
end;

function My_lopen(const lpPathName: LPCSTR; iReadWrite: Integer): HFILE; stdcall;
begin
  {$IFDEF DEBUG_INFO}
  WriteFuncCall('_lopen', [string(lpPathName)]);
  {$ENDIF DEBUG_INFO}
  Result := _lopen(lpPathName, iReadWrite);
  {$IFDEF DEBUG_INFO}
  WriteReturn(Result);
  {$ENDIF DEBUG_INFO}
end;

function My_lcreat(const lpPathName: LPCSTR; iAttribute: Integer): HFILE; stdcall;
begin
  {$IFDEF DEBUG_INFO}
  WriteFuncCall('_lcreat', [string(lpPathName)]);
  {$ENDIF DEBUG_INFO}
  Result := _lcreat(lpPathName, iAttribute);
  {$IFDEF DEBUG_INFO}
  WriteReturn(Result);
  {$ENDIF DEBUG_INFO}
end;*)

{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}

function TFileHookExeLoader.DoGetProcAddress(hModule: HMODULE; ProcName: PAnsiChar): Pointer;
begin
  if hModule = hKernel then
  begin
    if StrIComp(ProcName, 'CreateFileA') = 0 then
      Result := @MyCreateFileA
    else if StrIComp(ProcName, 'CreateFileW') = 0 then
      Result := @MyCreateFileW
    else if StrIComp(ProcName, 'CloseHandle') = 0 then
      Result := @MyCloseHandle
    else if StrIComp(ProcName, 'ReadFile') = 0 then
      Result := @MyReadFile
    else if StrIComp(ProcName, 'WriteFile') = 0 then
      Result := @MyWriteFile
    else if StrIComp(ProcName, 'SetFilePointer') = 0 then
      Result := @MySetFilePointer
    else if StrIComp(ProcName, 'SetEndOfFile') = 0 then
      Result := @MySetEndOfFile
    else if StrIComp(ProcName, 'GetFileInformationByHandle') = 0 then
      Result := @MyGetFileInformationByHandle
    else if StrIComp(ProcName, 'GetFileSize') = 0 then
      Result := @MyGetFileSize

    {else if (StrIComp(ProcName, 'OpenFile') = 0) then
      Result := @MyOpenFile
    else if (StrIComp(ProcName, '_lopen') = 0) then
      Result := @My_lopen
    else if (StrIComp(ProcName, '_lcreat') = 0) then
      Result := @My_lcreat}
    else
      Result := inherited DoGetProcAddress(hModule, ProcName);
  end
  else
    Result := inherited DoGetProcAddress(hModule, ProcName);
end;

initialization
  FileDataHandles := TBucketList.Create(bl128);
  DllLoaderClass := TFileHookExeLoader;

finalization
  FileDataHandles.Free;

end.
