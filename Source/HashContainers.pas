unit HashContainers;

{ WARNING: This unit is broken by Compiler Generic bugs. }

interface

uses
  SysUtils, Classes, Generics.Collections;

const
  MaxCacheItems = $100;

type
  THashValue = type Cardinal;
  THashFunction<T> = function(const Value: T): THashValue;
  TCompareFunction<T> = function(const Str1, Str2: T): Integer;

  TStringHash<TString, TValue> = class(TObject)
  private
    type
      PItem = ^TItem;
      TItem = record
        Key: TString;
        Value: TValue;
        Next: PItem;
      end;
  private
    FItems: array[0..$1000 - 1] of PItem;
    FCount: Integer;
    FCacheItems: array[0..MaxCacheItems] of TItem;
    FCacheIndex: Integer;
    FHashFunc: THashFunction<TString>;
    FCompareFunc: TCompareFunction<TString>;
  public
    constructor Create(AHashFunc: THashFunction<TString>; ACompareFunc: TCompareFunction<TString>);
    destructor Destroy; override;
    procedure Clear; virtual;

    property HashString: THashFunction<TString> read FHashFunc;

    function Find(const AItem: TString; out Value: TValue): Boolean; overload;
    function HasKey(const AItem: TString): Boolean; overload;

    function Find(AHash: THashValue; const AItem: TString; out Value: TValue): Boolean; overload;
    function HasKey(AHash: THashValue; const AItem: TString): Boolean; overload;

    function Add(const AItem: TString; AData: TValue): TValue; overload;
    function Add(AHash: THashValue; const AItem: TString; AData: TValue): TValue; overload;
    function Remove(const AItem: TString): TValue; overload;
    function Remove(AHash: THashValue; const AItem: TString): TValue; overload;
    procedure SetValue(const AItem: TString; AData: TValue); overload;
    procedure SetValue(AHash: THashValue; const AItem: TString; AData: TValue); overload;

    property Count: Integer read FCount;
  end;

  TAnsiStringValueHash<TValue> = class(TStringHash<AnsiString, TValue>)
  public
    constructor Create(ACaseSensitive: Boolean = True);
  end;

  TStringValueHash<TValue> = class(TStringHash<string, TValue>)
  public
    constructor Create(ACaseSensitive: Boolean = True);
  end;

  TUTF8StringValueHash<TValue> = class(TStringHash<UTF8String, TValue>)
  public
    constructor Create(ACaseSensitive: Boolean = True);
  end;

  TAnsiStringIntegerHash = class(TAnsiStringValueHash<Integer>);
  TAnsiStringStringHash = class(TAnsiStringValueHash<string>);
  TStringIntegerHash = class(TStringValueHash<Integer>);
  TStringStringHash = class(TStringValueHash<string>);
  TUTF8StringIntegerHash = class(TUTF8StringValueHash<Integer>);
  TUTF8StringStringHash = class(TUTF8StringValueHash<string>);

function HashString(const AItem: string): THashValue;
function HashUpperString(const AItem: string): THashValue;
function HashAnsiString(const AItem: AnsiString): THashValue;
function HashAnsiUpperString(const AItem: AnsiString): THashValue;
function HashUtf8String(const AItem: UTF8String): THashValue;
function HashUtf8UpperString(const AItem: UTF8String): THashValue;
function Utf8CompareStr(const S1, S2: UTF8String): Integer;
function Utf8CompareText(const S1, S2: UTF8String): Integer;

implementation

uses
  AnsiStrings;

function HashAnsiString(const AItem: AnsiString): THashValue;
asm
  or eax, eax
  jz @@Leave

  xchg eax, edx
  mov eax, [edx-$04] // Length(AItem)
  xor ecx, ecx

@@HasStringNextChar:
  mov cl, [edx]
  ror cl, 4
  shl cx, 1
  add eax, ecx
  xor ch, ch
  inc edx
  or ecx, ecx
  jnz @@HasStringNextChar

  and eax, $00000fff
@@Leave:
end;

function HashAnsiUpperString(const AItem: AnsiString): THashValue;
begin
  Result := HashAnsiString(AnsiStrings.AnsiUpperCase(AItem));
end;

function HashString(const AItem: string): THashValue;
asm
  or eax, eax
  jz @@Leave

  xchg eax, edx
  mov eax, [edx-$04] // Length(AItem)
  xor ecx, ecx

@@HasStringNextChar:
  mov cx, [edx]
{  ror cl, 4
  shl cx, 1}
  add eax, ecx
  and ecx, $0000ffff
  inc edx
  or ecx, ecx
  jnz @@HasStringNextChar

  and eax, $00000fff
@@Leave:
end;

function HashUpperString(const AItem: string): THashValue;
begin
  Result := HashString(SysUtils.AnsiUpperCase(AItem));
end;

function HashUtf8String(const AItem: UTF8String): THashValue;
asm
  jmp HashAnsiString
end;

function HashUtf8UpperString(const AItem: UTF8String): THashValue;
begin
  Result := HashAnsiString(UTF8Encode(AnsiUpperCase(UTF8ToString(AItem))));
end;

function Utf8CompareStr(const S1, S2: UTF8String): Integer;
asm
  jmp AnsiStrings.CompareStr
end;

function Utf8CompareText(const S1, S2: UTF8String): Integer;
asm
  jmp AnsiStrings.CompareText
end;

{ TStringHash<TString, TValue> }

constructor TStringHash<TString, TValue>.Create(AHashFunc: THashFunction<TString>; ACompareFunc: TCompareFunction<TString>);
begin
  inherited Create;
  FHashFunc := AHashFunc;
  FCompareFunc := ACompareFunc;
end;

destructor TStringHash<TString, TValue>.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TStringHash<TString, TValue>.Clear;
var
  P, N: PItem;
  i: Integer;
begin
  if FCount > 0 then
  begin
    for i := 0 to High(FItems) do
    begin
      P := FItems[i];
      while P <> nil do
      begin
        N := P.Next;
        if (Cardinal(P) < Cardinal(@FCacheItems[0])) or (Cardinal(P) > Cardinal(@FCacheItems[High(FCacheItems)])) then
          Dispose(P)
        else
          P.Key := Default(TString);
        P := N;
        Dec(FCount);
      end;
      FItems[i] := nil;
      if FCount = 0 then
        Break;
    end;
  end;
  FCount := 0;
  FCacheIndex := 0;
end;

function TStringHash<TString, TValue>.Add(const AItem: TString; AData: TValue): TValue;
begin
  Result := Add(FHashFunc(AItem), AItem, AData);
end;

function TStringHash<TString, TValue>.Add(AHash: THashValue; const AItem: TString; AData: TValue): TValue;
var
  N: PItem;
begin
  if FCacheIndex < Length(FCacheItems) then
  begin
    N := @FCacheItems[FCacheIndex];
    Inc(FCacheIndex);
  end
  else
    New(N);
  N.Next := FItems[AHash];
  FItems[AHash] := N;
  Inc(FCount);
  N.Key := AItem;
  N.Value := AData;
  Result := AData;
end;

function TStringHash<TString, TValue>.Remove(const AItem: TString): TValue;
begin
  Result := Remove(FHashFunc(AItem), AItem);
end;

function TStringHash<TString, TValue>.Remove(AHash: THashValue; const AItem: TString): TValue;
var
  Index: Integer;
  P, N: PItem;
begin
  Index := AHash;
  N := FItems[Index];
  if N <> nil then
  begin
    if FCompareFunc(N.Key, AItem) = 0 then
    begin
      Result := N.Value;
      P := N.Next;
      if (Cardinal(N) < Cardinal(@FCacheItems[0])) or (Cardinal(N) > Cardinal(@FCacheItems[High(FCacheItems)])) then
        Dispose(N)
      else
        N.Key := Default(TString);
      FItems[Index] := P;
      Dec(FCount);
      Exit;
    end
    else
    begin
      P := N;
      N := N.Next;
      while N <> nil do
      begin
        if FCompareFunc(N.Key, AItem) = 0 then
        begin
          Result := N.Value;
          P.Next := N.Next;
          if (Cardinal(N) < Cardinal(@FCacheItems[0])) or (Cardinal(N) > Cardinal(@FCacheItems[High(FCacheItems)])) then
            Dispose(N)
          else
            N.Key := Default(TString);
          Dec(FCount);
          Exit;
        end;
        P := N;
        N := N.Next;
      end;
    end;
  end;
  Result := Default(TValue);
end;

function TStringHash<TString, TValue>.Find(const AItem: TString; out Value: TValue): Boolean;
begin
  Result := Find(FHashFunc(AItem), AItem, Value);
end;

function TStringHash<TString, TValue>.Find(AHash: THashValue; const AItem: TString; out Value: TValue): Boolean;
var
  N: PItem;
begin
  Value := Default(TValue);
  N := FItems[AHash];
  while N <> nil do
  begin
    if FCompareFunc(N.Key, AItem) = 0 then
    begin
      Value := N.Value;
      Result := True;
      Exit;
    end;
    N := N.Next;
  end;
  Result := False;
end;

function TStringHash<TString, TValue>.HasKey(const AItem: TString): Boolean;
var
  Value: TValue;
begin
  Result := Find(FHashFunc(AItem), AItem, Value);
end;

function TStringHash<TString, TValue>.HasKey(AHash: THashValue; const AItem: TString): Boolean;
var
  N: PItem;
begin
  N := FItems[AHash];
  while N <> nil do
  begin
    if FCompareFunc(N.Key, AItem) = 0 then
    begin
      Result := True;
      Exit;
    end;
    N := N.Next;
  end;
  Result := False;
end;

procedure TStringHash<TString, TValue>.SetValue(const AItem: TString; AData: TValue);
begin
  SetValue(FHashFunc(AItem), AItem, AData);
end;

procedure TStringHash<TString, TValue>.SetValue(AHash: THashValue; const AItem: TString; AData: TValue);
var
  N: PItem;
begin
  N := FItems[AHash];
  while N <> nil do
  begin
    if FCompareFunc(N.Key, AItem) = 0 then
    begin
      N.Value := AData;
      Exit;
    end;
    N := N.Next;
  end;
  Add(AHash, AItem, AData);
end;

{ TAnsiStringValueHash<TValue> }

constructor TAnsiStringValueHash<TValue>.Create(ACaseSensitive: Boolean);
begin
  if ACaseSensitive then
    inherited Create(HashAnsiString, AnsiStrings.CompareStr)
  else
    inherited Create(HashAnsiUpperString, AnsiStrings.CompareText);
end;

{ TStringValueHash<TValue> }

constructor TStringValueHash<TValue>.Create(ACaseSensitive: Boolean);
begin
  if ACaseSensitive then
    inherited Create(HashString, SysUtils.AnsiCompareStr)
  else
    inherited Create(HashUpperString, SysUtils.AnsiCompareText);
end;

{ TUTF8StringValueHash<TValue> }

constructor TUTF8StringValueHash<TValue>.Create(ACaseSensitive: Boolean);
begin
  if ACaseSensitive then
    inherited Create(HashUtf8String, Utf8CompareStr)
  else
    inherited Create(HashUtf8UpperString, Utf8CompareText);
end;

end.
