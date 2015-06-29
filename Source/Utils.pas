unit Utils;

interface

uses
  Windows, SysUtils, Classes, StrUtils;

function CompareFileAge(const FileName1, FileName2: string): Integer;

function ExtractNamespace(const S: string): string;
function ExtractNamespaceName(const S: string): string;

function RemoveQuotes(const Paths: string): string;
function RemoveLineBreaks(const S: string): string;

type
  TInterfaceEnabledObject = class(TObject, IInterface)
  public
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  end;

implementation

function TInterfaceEnabledObject.QueryInterface(const IID: TGUID; out Obj): HRESULT;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TInterfaceEnabledObject._AddRef: Integer;
begin
  Result := -1;
end;

function TInterfaceEnabledObject._Release: Integer;
begin
  Result := -1;
end;

procedure WriteStringList(Writer: TWriter; List: TStrings);
var
  I: Integer;
begin
  Writer.WriteListBegin;
  for I := 0 to List.Count - 1 do
    Writer.WriteString(List[I]);
  Writer.WriteListEnd;
end;

procedure ReadStringList(Reader: TReader; List: TStrings);
begin
  Reader.ReadListBegin;
  while not Reader.EndOfList do
    List.Add(Reader.ReadString);
  Reader.ReadListEnd;
end;

function CompareFileAge(const FileName1, FileName2: string): Integer;
var
  FindData1, FindData2: TWin32FileAttributeData;
begin
  if not GetFileAttributesEx(Pointer(FileName1), GetFileExInfoStandard, @FindData1) then
    Int64(FindData1.ftLastWriteTime) := 0;
  if not GetFileAttributesEx(Pointer(FileName2), GetFileExInfoStandard, @FindData2) then
    Int64(FindData2.ftLastWriteTime) := 0;

  if Int64(FindData1.ftLastWriteTime) < Int64(FindData2.ftLastWriteTime) then
    Result := -1
  else
  if Int64(FindData1.ftLastWriteTime) = Int64(FindData2.ftLastWriteTime) then
    Result := 0
  else
    Result := 1;
end;

function ExtractNamespace(const S: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := Length(S) downto 0 do
    if S[I] = '.' then
    begin
      Result := Copy(S, 1, I - 1);
      Break;
    end;
end;

function ExtractNamespaceName(const S: string): string;
var
  I: Integer;
begin
  for I := Length(S) downto 0 do
    if S[I] = '.' then
    begin
      Result := Copy(S, I + 1, MaxInt);
      Exit;
    end;
  Result := S;
end;

function RemoveQuotes(const Paths: string): string;
var
  ps: Integer;
begin
  Result := Paths;
  ps := Pos('"', Paths);
  while ps > 0 do
  begin
    Delete(Result, ps, 1);
    ps := PosEx('"', Result, ps);
  end;
end;

function RemoveLineBreaks(const S: string): string;
var
  I: Integer;
begin
  Result := S;
  for I := Length(Result) downto 0 do
    if (Result <= #255) and (AnsiChar(Result[I]) in [#10, #13]) then
      Delete(Result, I, 1);
end;

end.
