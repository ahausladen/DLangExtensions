unit DefaultAPIHooks;

interface

uses
  Windows, SysUtils, DLLLoader;

type
  TDefaultExeLoader = class(TDLLLoader)
  protected
    function DoGetProcAddress(hModule: HMODULE; ProcName: PAnsiChar): Pointer; override;
  end;

var
  hKernel: HMODULE;
  hApp: TDefaultExeLoader;
  CommandLineA: AnsiString;
  CommandLineW: WideString;

function StrICompW(const Str1, Str2: PWideChar): Integer;
function StrFileExt(P: PAnsiChar): PAnsiChar;
function StrFileExtW(P: PWideChar): PWideChar;
function SkipString(P: PAnsiChar): PAnsiChar;
function SkipStringW(P: PWideChar): PWideChar;
function SkipWhiteChars(P: PAnsiChar): PAnsiChar; inline;
function SkipWhiteCharsW(P: PWideChar): PWideChar; inline;
function GetNextParameter(CmdLine: PAnsiChar; var Param: AnsiString): PAnsiChar;
function GetNextParameterW(CmdLine: PWideChar; var Param: WideString): PWideChar;
function RemoveQuotes(const Paths: AnsiString): AnsiString;
function RemoveQuotesW(const Paths: WideString): WideString;

implementation

function StrICompW(const Str1, Str2: PWideChar): Integer;
var
  S1, S2: WideString;
begin
  S1 := Str1;
  S2 := Str2;
  Result := WideCompareText(Str1, Str2);
end;

function StrFileExt(P: PAnsiChar): PAnsiChar;
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

function StrFileExtW(P: PWideChar): PWideChar;
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

function SkipString(P: PAnsiChar): PAnsiChar;
begin
  if P^ = '"' then
  begin
    Inc(P);
    while not (P^ in [#0, '"']) do
      Inc(P);
    if P^ <> #0 then
      Inc(P);
  end
  else
    while P^ > ' ' do
    begin
      if P^ = '"' then
      begin
        Inc(P);
        while not (P^ in [#0, '"']) do
          Inc(P);
        if P^ = #0 then
          Break;
      end;
      Inc(P);
    end;
  Result := P;
end;

function SkipStringW(P: PWideChar): PWideChar;
begin
  if P^ = '"' then
  begin
    Inc(P);
    while (P^ <> #0) and (P^ <> '"') do
      Inc(P);
    if P^ <> #0 then
      Inc(P);
  end
  else
    while P^ > ' ' do
    begin
      if P^ = '"' then
      begin
        Inc(P);
        while (P^ <> #0) and (P^ <> '"') do
          Inc(P);
        if P^ = #0 then
          Break;
      end;
      Inc(P);
    end;
  Result := P;
end;

function SkipWhiteChars(P: PAnsiChar): PAnsiChar;
begin
  Result := P;
  while (Result^ <> #0) and (Result^ <= ' ') do
    Inc(Result);
end;

function SkipWhiteCharsW(P: PWideChar): PWideChar;
begin
  Result := P;
  while (Result^ <> #0) and (Result^ <= ' ') do
    Inc(Result);
end;

function GetNextParameter(CmdLine: PAnsiChar; var Param: AnsiString): PAnsiChar;
var
  P, F: PAnsiChar;
begin
  Result := nil;
  Param := '';
  if CmdLine <> nil then
  begin
    P := SkipWhiteChars(CmdLine);
    if P^ <> #0 then
    begin
      F := P;
      P := SkipString(P);
      SetString(Param, F, P - F);
      Result := P;
    end;
  end;
end;

function GetNextParameterW(CmdLine: PWideChar; var Param: WideString): PWideChar;
var
  P, F: PWideChar;
begin
  Result := nil;
  Param := '';
  if CmdLine <> nil then
  begin
    P := SkipWhiteCharsW(CmdLine);
    if P^ <> #0 then
    begin
      F := P;
      P := SkipStringW(P);
      SetString(Param, F, P - F);
      Result := P;
    end;
  end;
end;

function RemoveQuotes(const Paths: AnsiString): AnsiString;
var
  ps: Integer;
begin
  Result := Paths;
  ps := Pos('"', Paths);
  while ps > 0 do
  begin
    Delete(Result, ps, 1);
    ps := Pos('"', Result);
  end;
end;

function RemoveQuotesW(const Paths: WideString): WideString;
var
  ps: Integer;
begin
  Result := Paths;
  ps := Pos('"', Paths);
  while ps > 0 do
  begin
    Delete(Result, ps, 1);
    ps := Pos('"', Result);
  end;
end;

function MyGetModuleHandleA(AName: PAnsiChar): HMODULE; stdcall;
begin
  if (AName = nil) or (StrIComp(AName, PChar(hApp.Name)) = 0) or (StrIComp(AName, PChar(hApp.Filename)) = 0) then
    Result := HMODULE(TDLLLoader(hApp).ImageBase)
  else
    Result := GetModuleHandleA(AName);
end;

function MyGetModuleHandleW(AName: PWideChar): HMODULE; stdcall;
begin
  if (AName = nil) or (StrICompW(AName, PWideChar(hApp.NameW)) = 0) or (StrICompW(AName, PWideChar(hApp.FilenameW)) = 0) then
    Result := HMODULE(TDLLLoader(hApp).ImageBase)
  else
    Result := GetModuleHandleW(AName);
end;

function MyGetModuleFileNameA(hModule: HINST; lpFilename: PAnsiChar; nSize: DWORD): DWORD; stdcall;

  function GetModuleFileNameMemA(hModule: HINST; lpFilename: PAnsiChar; nSize: DWORD): DWORD;
  var
    S: AnsiString;
  begin
    SetLastError(NOERROR);
    S := hApp.Filename;
    Result := Length(S);
    if (nSize > Result) then
      nSize := Result;
    if (nSize > Result) or (lpFilename = nil) then
      SetLastError(ERROR_INSUFFICIENT_BUFFER);
    if (nSize > 0) and (lpFilename <> nil) then
      Move(PAnsiChar(S)^, lpFilename^, SizeOf(AnsiChar) * nSize);
  end;

begin
  if (hModule = 0) or (hModule = HINST(hApp.ImageBase)) then
    Result := GetModuleFileNameMemA(hModule, lpFilename, nSize)
  else
    Result := GetModuleFileNameA(hModule, lpFilename, nSize);
end;

function MyGetModuleFileNameW(hModule: HINST; lpFilename: PWideChar; nSize: DWORD): DWORD; stdcall;

  function GetModuleFileNameMemW(hModule: HINST; lpFilename: PWideChar; nSize: DWORD): DWORD;
  var
    S: WideString;
  begin
    SetLastError(NOERROR);
    S := hApp.FilenameW;
    Result := Length(S);
    if (nSize > Result) then
      nSize := Result;
    if (nSize > Result) or (lpFilename = nil) then
      SetLastError(ERROR_INSUFFICIENT_BUFFER);
    if (nSize > 0) and (lpFilename <> nil) then
      Move(PWideChar(S)^, lpFilename^, SizeOf(WideChar) * nSize);
  end;

begin
  if (hModule = 0) or (hModule = HINST(hApp.ImageBase)) then
    Result := GetModuleFileNameMemW(hModule, lpFilename, nSize)
  else
    Result := GetModuleFileNameW(hModule, lpFilename, nSize);
end;

function MyGetCommandLineA: PAnsiChar; stdcall;
begin
  Result := PAnsiChar(CommandLineA);
end;

function MyGetCommandLineW: PWideChar; stdcall;
begin
  Result := PWideChar(CommandLineW);
end;

procedure MyExitProcess(uExitCode: UINT); stdcall;
begin
  IOResult; // something must fail, but I don't know what. Maybe the compiler writes into my TLS
  //FreeLibraryMem(HMODULE(hApp));
  //ReadLn(Input);
  {$IFDEF DEBUG_INFO}
  {if DebugHook <> 0 then
    ReadLn;}
  {$ENDIF DEBUG_INFO}
  Halt(uExitCode);
  //ExitProcess(uExitCode);
end;

type
  HLOCAL = Pointer;

function MyLocalAlloc(uFlags, uBytes: UINT): HLOCAL; stdcall;
begin
  if uFlags and LMEM_ZEROINIT <> 0 then
    Result := AllocMem(uBytes)
  else
    Result := GetMemory(uBytes);
end;

function MyLocalReAlloc(hMem: HLOCAL; uBytes, uFlags: UINT): HLOCAL; stdcall;
begin
  Result := ReallocMemory(hMem, uBytes);
end;

function MyLocalFree(hMem: HLOCAL): HLOCAL; stdcall;
begin
  FreeMemory(hMem);
  Result := nil;
end;

function MyHeapAlloc(hHeap: THandle; dwFlags, dwBytes: DWORD): Pointer; stdcall;
begin
  if dwFlags and HEAP_ZERO_MEMORY <> 0 then
    Result := AllocMem(dwBytes)
  else
    Result := GetMemory(dwBytes);
end;

function MyHeapFree(hHeap: THandle; dwFlags: DWORD; lpMem: Pointer): BOOL; stdcall;
begin
  FreeMem(lpMem);
  Result := True;
end;

function MyGetProcAddress(hModule: HMODULE; lpProcName: LPCSTR): FARPROC; stdcall;

  function InternGetProcAddress(Module: THandle; lpProcName: LPCSTR): FARPROC;
  begin { requires string temporary }
    if hModule = HINST(hApp.ImageBase) then
      Result := hApp.FindExport(lpProcName)
    else
      Result := hApp.DoGetProcAddress(Module, lpProcName);
  end;

begin
  if (hModule = hKernel) or (hModule = HINST(hApp.ImageBase)) then
    Result := InternGetProcAddress(hModule, lpProcName)
  else
    Result := GetProcAddress(hModule, lpProcName);
end;

function TDefaultExeLoader.DoGetProcAddress(hModule: HMODULE; ProcName: PAnsiChar): Pointer;
begin
  if hModule = hKernel then
  begin
    if (StrIComp(ProcName, 'GetProcAddress') = 0) then
      Result := @MyGetProcAddress

    else if (StrIComp(ProcName, 'GetModuleHandleA') = 0) then
      Result := @MyGetModuleHandleA
    else if (StrIComp(ProcName, 'GetModuleHandleW') = 0) then
      Result := @MyGetModuleHandleW

    else if (StrIComp(ProcName, 'GetModuleFileNameA') = 0) then
      Result := @MyGetModuleFileNameA
    else if (StrIComp(ProcName, 'GetModuleFileNameW') = 0) then
      Result := @MyGetModuleFileNameW

    else if (StrIComp(ProcName, 'GetCommandLineA') = 0) then
      Result := @MyGetCommandLineA
    else if (StrIComp(ProcName, 'GetCommandLineW') = 0) then
      Result := @MyGetCommandLineW

    else if (StrIComp(ProcName, 'ExitProcess') = 0) then
      Result := @MyExitProcess

    else if (StrIComp(ProcName, 'LocalAlloc') = 0) then
      Result := @MyLocalAlloc
    else if (StrIComp(ProcName, 'LocalReAlloc') = 0) then
      Result := @MyLocalReAlloc
    else if (StrIComp(ProcName, 'LocalFree') = 0) then
      Result := @MyLocalFree

    else if (StrIComp(ProcName, 'HeapAlloc') = 0) then
      Result := @MyHeapAlloc
    else if (StrIComp(ProcName, 'HeapFree') = 0) then
      Result := @MyHeapFree

    else
      Result := inherited DoGetProcAddress(hModule, ProcName);
  end
  else
    Result := inherited DoGetProcAddress(hModule, ProcName);
end;

initialization
  hKernel := GetModuleHandle(kernel32);

end.
