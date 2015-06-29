unit Utf8StringUtils;

{$IFDEF UNICODE}
  {$STRINGCHECKS OFF}
{$ENDIF UNICODE}

interface

uses
  Windows, SysUtils, Classes;

type
  Utf8Char = AnsiChar;
  PUtf8Char = PAnsiChar;
  {$IFNDEF UNICODE}
  RawByteString = AnsiString;
  TStringsA = TStrings;
  TStringListA = TStringList;

  {$ELSE}

  TStringsA = class;

{ TStringsA class }

  TStringsEnumeratorA = class
  private
    FIndex: Integer;
    FStrings: TStringsA;
  public
    constructor Create(AStrings: TStringsA);
    function GetCurrent: RawByteString;
    function MoveNext: Boolean;
    property Current: RawByteString read GetCurrent;
  end;

  TStringsA = class(TPersistent)
  private
    FDefined: TStringsDefined;
    FDelimiter: AnsiChar;
    FLineBreak: RawByteString;
    FQuoteChar: AnsiChar;
    FNameValueSeparator: AnsiChar;
    FStrictDelimiter: Boolean;
    FUpdateCount: Integer;
    function GetCommaText: RawByteString;
    function GetDelimitedText: RawByteString;
    function GetName(Index: Integer): RawByteString;
    function GetValue(const Name: RawByteString): RawByteString;
    procedure ReadData(Reader: TReader);
    procedure SetCommaText(const Value: RawByteString);
    procedure SetDelimitedText(const Value: RawByteString);
    procedure SetValue(const Name, Value: RawByteString);
    procedure WriteData(Writer: TWriter);
    function GetDelimiter: AnsiChar;
    procedure SetDelimiter(const Value: AnsiChar);
    function GetLineBreak: RawByteString;
    procedure SetLineBreak(const Value: RawByteString);
    function GetQuoteChar: AnsiChar;
    procedure SetQuoteChar(const Value: AnsiChar);
    function GetNameValueSeparator: AnsiChar;
    procedure SetNameValueSeparator(const Value: AnsiChar);
    function GetStrictDelimiter: Boolean;
    procedure SetStrictDelimiter(const Value: Boolean);
    function GetValueFromIndex(Index: Integer): RawByteString;
    procedure SetValueFromIndex(Index: Integer; const Value: RawByteString);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure Error(const Msg: string; Data: Integer); overload;
    procedure Error(Msg: PResStringRec; Data: Integer); overload;
    function ExtractName(const S: RawByteString): RawByteString;
    function Get(Index: Integer): RawByteString; virtual; abstract;
    function GetCapacity: Integer; virtual;
    function GetCount: Integer; virtual; abstract;
    function GetObject(Index: Integer): TObject; virtual;
    function GetTextStr: RawByteString; virtual;
    procedure Put(Index: Integer; const S: RawByteString); virtual;
    procedure PutObject(Index: Integer; AObject: TObject); virtual;
    procedure SetCapacity(NewCapacity: Integer); virtual;
    procedure SetTextStr(const Value: RawByteString); virtual;
    procedure SetUpdateState(Updating: Boolean); virtual;
    property UpdateCount: Integer read FUpdateCount;
    function CompareStrings(const S1, S2: RawByteString): Integer; virtual;
  public
    function Add(const S: RawByteString): Integer; virtual;
    function AddObject(const S: RawByteString; AObject: TObject): Integer; virtual;
    procedure Append(const S: RawByteString);
    procedure AddStrings(Strings: TStringsA); virtual;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate;
    procedure Clear; virtual; abstract;
    procedure Delete(Index: Integer); virtual; abstract;
    procedure EndUpdate;
    function Equals(Strings: TStringsA): Boolean; reintroduce;
    procedure Exchange(Index1, Index2: Integer); virtual;
    function GetEnumerator: TStringsEnumeratorA;
    function GetText: PAnsiChar; virtual;
    function IndexOf(const S: RawByteString): Integer; virtual;
    function IndexOfName(const Name: RawByteString): Integer; virtual;
    function IndexOfObject(AObject: TObject): Integer; virtual;
    procedure Insert(Index: Integer; const S: RawByteString); virtual; abstract;
    procedure InsertObject(Index: Integer; const S: RawByteString;
      AObject: TObject); virtual;
    procedure LoadFromFile(const FileName: string); overload; virtual;
    procedure LoadFromFile(const FileName: string; Encoding: TEncoding); overload; virtual;
    procedure LoadFromStream(Stream: TStream); overload; virtual;
    procedure LoadFromStream(Stream: TStream; Encoding: TEncoding); overload; virtual;
    procedure Move(CurIndex, NewIndex: Integer); virtual;
    procedure SaveToFile(const FileName: string); overload; virtual;
    procedure SaveToFile(const FileName: string; Encoding: TEncoding); overload; virtual;
    procedure SaveToStream(Stream: TStream); overload; virtual;
    procedure SaveToStream(Stream: TStream; Encoding: TEncoding); overload; virtual;
    procedure SetText(Text: PAnsiChar); virtual;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property CommaText: RawByteString read GetCommaText write SetCommaText;
    property Count: Integer read GetCount;
    property Delimiter: AnsiChar read GetDelimiter write SetDelimiter;
    property DelimitedText: RawByteString read GetDelimitedText write SetDelimitedText;
    property LineBreak: RawByteString read GetLineBreak write SetLineBreak;
    property Names[Index: Integer]: RawByteString read GetName;
    property Objects[Index: Integer]: TObject read GetObject write PutObject;
    property QuoteChar: AnsiChar read GetQuoteChar write SetQuoteChar;
    property Values[const Name: RawByteString]: RawByteString read GetValue write SetValue;
    property ValueFromIndex[Index: Integer]: RawByteString read GetValueFromIndex write SetValueFromIndex;
    property NameValueSeparator: AnsiChar read GetNameValueSeparator write SetNameValueSeparator;
    property StrictDelimiter: Boolean read GetStrictDelimiter write SetStrictDelimiter;
    property Strings[Index: Integer]: RawByteString read Get write Put; default;
    property Text: RawByteString read GetTextStr write SetTextStr;
  end;

{ TStringListA class }

  TStringListA = class;

  PStringItemA = ^TStringItemA;
  TStringItemA = record
    FString: RawByteString;
    FObject: TObject;
  end;

  PStringItemListA = ^TStringItemListA;
  TStringItemListA = array[0..MaxListSize] of TStringItemA;
  TStringListSortCompareA = function(List: TStringListA; Index1, Index2: Integer): Integer;

  TStringListA = class(TStringsA)
  private
    FList: PStringItemListA;
    FCount: Integer;
    FCapacity: Integer;
    FSorted: Boolean;
    FDuplicates: TDuplicates;
    FCaseSensitive: Boolean;
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;
    FOwnsObject: Boolean;
    procedure ExchangeItems(Index1, Index2: Integer);
    procedure Grow;
    procedure QuickSort(L, R: Integer; SCompare: TStringListSortCompareA);
    procedure SetSorted(Value: Boolean);
    procedure SetCaseSensitive(const Value: Boolean);
  protected
    procedure Changed; virtual;
    procedure Changing; virtual;
    function Get(Index: Integer): RawByteString; override;
    function GetCapacity: Integer; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const S: RawByteString); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetCapacity(NewCapacity: Integer); override;
    procedure SetUpdateState(Updating: Boolean); override;
    function CompareStrings(const S1, S2: RawByteString): Integer; override;
    procedure InsertItem(Index: Integer; const S: RawByteString; AObject: TObject); virtual;
  public
    constructor Create; overload;
    constructor Create(OwnsObjects: Boolean); overload;
    destructor Destroy; override;
    function Add(const S: RawByteString): Integer; override;
    function AddObject(const S: RawByteString; AObject: TObject): Integer; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Exchange(Index1, Index2: Integer); override;
    function Find(const S: RawByteString; var Index: Integer): Boolean; virtual;
    function IndexOf(const S: RawByteString): Integer; override;
    procedure Insert(Index: Integer; const S: RawByteString); override;
    procedure InsertObject(Index: Integer; const S: RawByteString;
      AObject: TObject); override;
    procedure Sort; virtual;
    procedure CustomSort(Compare: TStringListSortCompareA); virtual;
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
    property Sorted: Boolean read FSorted write SetSorted;
    property CaseSensitive: Boolean read FCaseSensitive write SetCaseSensitive;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
    property OwnsObjects: Boolean read FOwnsObject write FOwnsObject;
  end;
  {$ENDIF ~UNICODE}


function TrimRightA(const S: RawByteString): RawByteString;

function LoadBufferToUTF8String(const Buffer: TBytes): UTF8String; overload;
function LoadBufferToUTF8String(Buffer: PByte; Size: Cardinal): UTF8String; overload;
function LoadTextFileToUTF8String(const Filename: string): UTF8String;
function CreateMemoryStreamFromUTF8String(const S: UTF8String): TMemoryStream;

implementation

uses
  {$IFDEF UNICODE}
  AnsiStrings,
  {$ENDIF UNICODE}
  Consts, RtlConsts;

{$IFDEF UNICODE}

{ TStringsEnumeratorA }

constructor TStringsEnumeratorA.Create(AStrings: TStringsA);
begin
  inherited Create;
  FIndex := -1;
  FStrings := AStrings;
end;

function TStringsEnumeratorA.GetCurrent: RawByteString;
begin
  Result := FStrings[FIndex];
end;

function TStringsEnumeratorA.MoveNext: Boolean;
begin
  Result := FIndex < FStrings.Count - 1;
  if Result then
    Inc(FIndex);
end;

{ TStringsA }

function TStringsA.Add(const S: RawByteString): Integer;
begin
  Result := GetCount;
  Insert(Result, S);
end;

function TStringsA.AddObject(const S: RawByteString; AObject: TObject): Integer;
begin
  Result := Add(S);
  PutObject(Result, AObject);
end;

procedure TStringsA.Append(const S: RawByteString);
begin
  Add(S);
end;

procedure TStringsA.AddStrings(Strings: TStringsA);
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to Strings.Count - 1 do
      AddObject(Strings[I], Strings.Objects[I]);
  finally
    EndUpdate;
  end;
end;

procedure TStringsA.Assign(Source: TPersistent);
begin
  if Source is TStringsA then
  begin
    BeginUpdate;
    try
      Clear;
      FDefined := TStringsA(Source).FDefined;
      FNameValueSeparator := TStringsA(Source).FNameValueSeparator;
      FQuoteChar := TStringsA(Source).FQuoteChar;
      FDelimiter := TStringsA(Source).FDelimiter;
      FLineBreak := TStringsA(Source).FLineBreak;
      FStrictDelimiter := TStringsA(Source).FStrictDelimiter;
      AddStrings(TStringsA(Source));
    finally
      EndUpdate;
    end;
    Exit;
  end;
  inherited Assign(Source);
end;

procedure TStringsA.BeginUpdate;
begin
  if FUpdateCount = 0 then SetUpdateState(True);
  Inc(FUpdateCount);
end;

procedure TStringsA.DefineProperties(Filer: TFiler);

  function DoWrite: Boolean;
  begin
    if Filer.Ancestor <> nil then
    begin
      Result := True;
      if Filer.Ancestor is TStringsA then
        Result := not Equals(TStringsA(Filer.Ancestor))
    end
    else Result := Count > 0;
  end;

begin
  Filer.DefineProperty('Strings', ReadData, WriteData, DoWrite);
end;

procedure TStringsA.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then SetUpdateState(False);
end;

function TStringsA.Equals(Strings: TStringsA): Boolean;
var
  I, Count: Integer;
begin
  Result := False;
  Count := GetCount;
  if Count <> Strings.GetCount then Exit;
  for I := 0 to Count - 1 do if Get(I) <> Strings.Get(I) then Exit;
  Result := True;
end;

procedure TStringsA.Error(const Msg: string; Data: Integer);

  function ReturnAddr: Pointer;
  asm
          MOV     EAX,[EBP+4]
  end;

begin
  raise EStringListError.CreateFmt(Msg, [Data]) at ReturnAddr;
end;

procedure TStringsA.Error(Msg: PResStringRec; Data: Integer);
begin
  Error(LoadResString(Msg), Data);
end;

procedure TStringsA.Exchange(Index1, Index2: Integer);
var
  TempObject: TObject;
  TempString: RawByteString;
begin
  BeginUpdate;
  try
    TempString := Strings[Index1];
    TempObject := Objects[Index1];
    Strings[Index1] := Strings[Index2];
    Objects[Index1] := Objects[Index2];
    Strings[Index2] := TempString;
    Objects[Index2] := TempObject;
  finally
    EndUpdate;
  end;
end;

function TStringsA.ExtractName(const S: RawByteString): RawByteString;
var
  P: Integer;
begin
  Result := S;
  P := AnsiPos(NameValueSeparator, Result);
  if P <> 0 then
    SetLength(Result, P-1) else
    SetLength(Result, 0);
end;

function TStringsA.GetCapacity: Integer;
begin  // descendents may optionally override/replace this default implementation
  Result := Count;
end;

function TStringsA.GetCommaText: RawByteString;
var
  LOldDefined: TStringsDefined;
  LOldDelimiter: AnsiChar;
  LOldQuoteChar: AnsiChar;
begin
  LOldDefined := FDefined;
  LOldDelimiter := FDelimiter;
  LOldQuoteChar := FQuoteChar;
  Delimiter := ',';
  QuoteChar := '"';
  try
    Result := GetDelimitedText;
  finally
    FDelimiter := LOldDelimiter;
    FQuoteChar := LOldQuoteChar;
    FDefined := LOldDefined;
  end;
end;

function TStringsA.GetDelimitedText: RawByteString;
var
  S: RawByteString;
  P: PAnsiChar;
  I, Count: Integer;
  LDelimiters: TSysCharSet;
begin
  Count := GetCount;
  if (Count = 1) and (Get(0) = '') then
    Result := QuoteChar + QuoteChar
  else
  begin
    Result := '';
    LDelimiters := [AnsiChar(#0), AnsiChar(QuoteChar), AnsiChar(Delimiter)];
    if not StrictDelimiter then
      LDelimiters := LDelimiters + [AnsiChar(#1)..AnsiChar(' ')];
    for I := 0 to Count - 1 do
    begin
      S := Get(I);
      P := PAnsiChar(S);
      while not (P^ in LDelimiters) do
      {$IFDEF MSWINDOWS}
        P := CharNextA(P);
      {$ELSE}
        Inc(P);
      {$ENDIF}
      if (P^ <> #0) then S := AnsiQuotedStr(S, QuoteChar);
      Result := Result + S + Delimiter;
    end;
    System.Delete(Result, Length(Result), 1);
  end;
end;

function TStringsA.GetEnumerator: TStringsEnumeratorA;
begin
  Result := TStringsEnumeratorA.Create(Self);
end;

function TStringsA.GetName(Index: Integer): RawByteString;
begin
  Result := ExtractName(Get(Index));
end;

function TStringsA.GetObject(Index: Integer): TObject;
begin
  Result := nil;
end;

function TStringsA.GetText: PAnsiChar;
begin
  Result := StrNew(PAnsiChar(GetTextStr));
end;

function TStringsA.GetTextStr: RawByteString;
var
  I, L, Size, Count: Integer;
  P: PAnsiChar;
  S, LB: RawByteString;
begin
  Count := GetCount;
  Size := 0;
  LB := LineBreak;
  for I := 0 to Count - 1 do Inc(Size, Length(Get(I)) + Length(LB));
  SetString(Result, nil, Size);
  P := Pointer(Result);
  for I := 0 to Count - 1 do
  begin
    S := Get(I);
    L := Length(S);
    if L <> 0 then
    begin
      System.Move(Pointer(S)^, P^, L * SizeOf(AnsiChar));
      Inc(P, L);
    end;
    L := Length(LB);
    if L <> 0 then
    begin
      System.Move(Pointer(LB)^, P^, L * SizeOf(AnsiChar));
      Inc(P, L);
    end;
  end;
end;

function TStringsA.GetValue(const Name: RawByteString): RawByteString;
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if I >= 0 then
    Result := Copy(Get(I), Length(Name) + 2, MaxInt) else
    Result := '';
end;

function TStringsA.IndexOf(const S: RawByteString): Integer;
begin
  for Result := 0 to GetCount - 1 do
    if CompareStrings(Get(Result), S) = 0 then Exit;
  Result := -1;
end;

function TStringsA.IndexOfName(const Name: RawByteString): Integer;
var
  P: Integer;
  S: RawByteString;
begin
  for Result := 0 to GetCount - 1 do
  begin
    S := Get(Result);
    P := AnsiPos(NameValueSeparator, S);
    if (P <> 0) and (CompareStrings(Copy(S, 1, P - 1), Name) = 0) then Exit;
  end;
  Result := -1;
end;

function TStringsA.IndexOfObject(AObject: TObject): Integer;
begin
  for Result := 0 to GetCount - 1 do
    if GetObject(Result) = AObject then Exit;
  Result := -1;
end;

procedure TStringsA.InsertObject(Index: Integer; const S: RawByteString; AObject: TObject);
begin
  Insert(Index, S);
  PutObject(Index, AObject);
end;

procedure TStringsA.LoadFromFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TStringsA.LoadFromFile(const FileName: string; Encoding: TEncoding);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream, Encoding);
  finally
    Stream.Free;
  end;
end;

procedure TStringsA.LoadFromStream(Stream: TStream);
begin
  LoadFromStream(Stream, nil);
end;

procedure TStringsA.LoadFromStream(Stream: TStream; Encoding: TEncoding);
var
  Size: Integer;
  Buffer: TBytes;
  S: RawByteString;
begin
  BeginUpdate;
  try
    Size := Stream.Size - Stream.Position;
    SetLength(Buffer, Size);
    Stream.Read(Buffer[0], Size);

    Size := TEncoding.GetBufferEncoding(Buffer, Encoding);
    if Encoding.IsSingleByte then
    begin
      SetString(S, PAnsiChar(@Buffer[Size]), Length(Buffer) - Size);
      if Encoding = Encoding.UTF7 then
        SetCodePage(S, CP_UTF7, False)
      else
      if Encoding = Encoding.UTF8 then
        SetCodePage(S, CP_UTF8, False)
      else
        SetCodePage(S, CP_ACP, False);
    end
    else
      SetTextStr(UTF8Encode(Encoding.GetString(Buffer, Size, Length(Buffer) - Size)));
  finally
    EndUpdate;
  end;
end;

procedure TStringsA.Move(CurIndex, NewIndex: Integer);
var
  TempObject: TObject;
  TempString: RawByteString;
begin
  if CurIndex <> NewIndex then
  begin
    BeginUpdate;
    try
      TempString := Get(CurIndex);
      TempObject := GetObject(CurIndex);
      Delete(CurIndex);
      InsertObject(NewIndex, TempString, TempObject);
    finally
      EndUpdate;
    end;
  end;
end;

procedure TStringsA.Put(Index: Integer; const S: RawByteString);
var
  TempObject: TObject;
begin
  TempObject := GetObject(Index);
  Delete(Index);
  InsertObject(Index, S, TempObject);
end;

procedure TStringsA.PutObject(Index: Integer; AObject: TObject);
begin
end;

procedure TStringsA.ReadData(Reader: TReader);
begin
  Reader.ReadListBegin;
  BeginUpdate;
  try
    Clear;
    while not Reader.EndOfList do Add(UTF8Encode(Reader.ReadString));
  finally
    EndUpdate;
  end;
  Reader.ReadListEnd;
end;

procedure TStringsA.SaveToFile(const FileName: string);
begin
  SaveToFile(FileName, nil);
end;

procedure TStringsA.SaveToFile(const FileName: string; Encoding: TEncoding);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream, Encoding);
  finally
    Stream.Free;
  end;
end;

procedure TStringsA.SaveToStream(Stream: TStream);
begin
  SaveToStream(Stream, nil);
end;

procedure TStringsA.SaveToStream(Stream: TStream; Encoding: TEncoding);
var
  Buffer, Preamble: TBytes;
begin
  if Encoding = nil then
    Encoding := TEncoding.Default;
  Buffer := Encoding.GetBytes(UnicodeString(GetTextStr));
  Preamble := Encoding.GetPreamble;
  if Length(Preamble) > 0 then
    Stream.WriteBuffer(Preamble[0], Length(Preamble));
  Stream.WriteBuffer(Buffer[0], Length(Buffer));
end;

procedure TStringsA.SetCapacity(NewCapacity: Integer);
begin
  // do nothing - descendents may optionally implement this method
end;

procedure TStringsA.SetCommaText(const Value: RawByteString);
begin
  Delimiter := ',';
  QuoteChar := '"';
  SetDelimitedText(Value);
end;

procedure TStringsA.SetText(Text: PAnsiChar);
begin
  SetTextStr(Text);
end;

procedure TStringsA.SetTextStr(const Value: RawByteString);
var
  P, Start, LB: PAnsiChar;
  S: RawByteString;
  LineBreakLen: Integer;
begin
  BeginUpdate;
  try
    Clear;
    P := Pointer(Value);
    if P <> nil then
      if CompareStr(LineBreak, sLineBreak) = 0 then
      begin
        // This is a lot faster than using StrPos/AnsiStrPos when
        // LineBreak is the default (#13#10)
        while P^ <> #0 do
        begin
          Start := P;
          while not (P^ in [#0, #10, #13]) do Inc(P);
          SetString(S, Start, P - Start);
          Add(S);
          if P^ = #13 then Inc(P);
          if P^ = #10 then Inc(P);
        end;
      end
      else
      begin
        LineBreakLen := Length(LineBreak);
        while P^ <> #0 do
        begin
          Start := P;
          LB := AnsiStrPos(P, PAnsiChar(LineBreak));
          while (P^ <> #0) and (P <> LB) do Inc(P);
          SetString(S, Start, P - Start);
          Add(S);
          if P = LB then
            Inc(P, LineBreakLen);
        end;
      end;
  finally
    EndUpdate;
  end;
end;

procedure TStringsA.SetUpdateState(Updating: Boolean);
begin
end;

procedure TStringsA.SetValue(const Name, Value: RawByteString);
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if Value <> '' then
  begin
    if I < 0 then I := Add('');
    Put(I, Name + NameValueSeparator + Value);
  end else
  begin
    if I >= 0 then Delete(I);
  end;
end;

procedure TStringsA.WriteData(Writer: TWriter);
var
  I: Integer;
begin
  Writer.WriteListBegin;
  for I := 0 to Count - 1 do Writer.WriteString(UnicodeString(Get(I)));
  Writer.WriteListEnd;
end;

procedure TStringsA.SetDelimitedText(const Value: RawByteString);
var
  P, P1: PAnsiChar;
  S: RawByteString;
begin
  BeginUpdate;
  try
    Clear;
    P := PAnsiChar(Value);
    if not StrictDelimiter then
      while (P^ in [#1..' ']) do
      {$IFDEF MSWINDOWS}
        P := CharNextA(P);
      {$ELSE}
        Inc(P);
      {$ENDIF}
    while P^ <> #0 do
    begin
      if P^ = QuoteChar then
        S := AnsiExtractQuotedStr(P, QuoteChar)
      else
      begin
        P1 := P;
        while ((not FStrictDelimiter and (P^ > ' ')) or
              (FStrictDelimiter and (P^ <> #0))) and (P^ <> Delimiter) do
        {$IFDEF MSWINDOWS}
          P := CharNextA(P);
        {$ELSE}
          Inc(P);
        {$ENDIF}
        SetString(S, P1, P - P1);
      end;
      Add(S);
      if not FStrictDelimiter then
        while (P^ in [#1..' ']) do
        {$IFDEF MSWINDOWS}
          P := CharNextA(P);
        {$ELSE}
          Inc(P);
        {$ENDIF}

      if P^ = Delimiter then
      begin
        P1 := P;
        {$IFDEF MSWINDOWS}
        if CharNextA(P1)^ = #0 then
        {$ELSE}
        Inc(P1);
        if P1^ = #0 then
        {$ENDIF}
          Add('');
        repeat
          {$IFDEF MSWINDOWS}
          P := CharNextA(P);
          {$ELSE}
          Inc(P);
          {$ENDIF}
        until not (not FStrictDelimiter and (P^ in [#1..' ']));
      end;
    end;
  finally
    EndUpdate;
  end;
end;

function TStringsA.GetDelimiter: AnsiChar;
begin
  if not (sdDelimiter in FDefined) then
    Delimiter := ',';
  Result := FDelimiter;
end;

function TStringsA.GetLineBreak: RawByteString;
begin
  if not (sdLineBreak in FDefined) then
    LineBreak := sLineBreak;
  Result := FLineBreak;
end;

function TStringsA.GetQuoteChar: AnsiChar;
begin
  if not (sdQuoteChar in FDefined) then
    QuoteChar := '"';
  Result := FQuoteChar;
end;

function TStringsA.GetStrictDelimiter: Boolean;
begin
  if not (sdStrictDelimiter in FDefined) then
    StrictDelimiter := False;
  Result := FStrictDelimiter;
end;

procedure TStringsA.SetDelimiter(const Value: AnsiChar);
begin
  if (FDelimiter <> Value) or not (sdDelimiter in FDefined) then
  begin
    Include(FDefined, sdDelimiter);
    FDelimiter := Value;
  end
end;

procedure TStringsA.SetLineBreak(const Value: RawByteString);
begin
  if (FLineBreak <> Value) or not (sdLineBreak in FDefined) then
  begin
    Include(FDefined, sdLineBreak);
    FLineBreak := Value;
  end
end;

procedure TStringsA.SetQuoteChar(const Value: AnsiChar);
begin
  if (FQuoteChar <> Value) or not (sdQuoteChar in FDefined) then
  begin
    Include(FDefined, sdQuoteChar);
    FQuoteChar := Value;
  end
end;

procedure TStringsA.SetStrictDelimiter(const Value: Boolean);
begin
  if (FStrictDelimiter <> Value) or not (sdStrictDelimiter in FDefined) then
  begin
    Include(FDefined, sdStrictDelimiter);
    FStrictDelimiter := Value;
  end
end;

function TStringsA.CompareStrings(const S1, S2: RawByteString): Integer;
begin
  Result := AnsiCompareText(S1, S2);
end;

function TStringsA.GetNameValueSeparator: AnsiChar;
begin
  if not (sdNameValueSeparator in FDefined) then
    NameValueSeparator := '=';
  Result := FNameValueSeparator;
end;

procedure TStringsA.SetNameValueSeparator(const Value: AnsiChar);
begin
  if (FNameValueSeparator <> Value) or not (sdNameValueSeparator in FDefined) then
  begin
    Include(FDefined, sdNameValueSeparator);
    FNameValueSeparator := Value;
  end
end;

function TStringsA.GetValueFromIndex(Index: Integer): RawByteString;
begin
  if Index >= 0 then
    Result := Copy(Get(Index), Length(Names[Index]) + 2, MaxInt) else
    Result := '';
end;

procedure TStringsA.SetValueFromIndex(Index: Integer; const Value: RawByteString);
begin
  if Value <> '' then
  begin
    if Index < 0 then Index := Add('');
    Put(Index, Names[Index] + NameValueSeparator + Value);
  end
  else
    if Index >= 0 then Delete(Index);
end;

{ TStringListA }

destructor TStringListA.Destroy;
var
  I: Integer;
begin
  FOnChange := nil;
  FOnChanging := nil;

  // In the event that we own the Objects make sure to free them all when we
  // destroy the stringlist.
  if OwnsObjects then
  begin
    for I := 0 to FCount - 1 do
      GetObject(I).Free;
  end;

  inherited Destroy;
  if FCount <> 0 then Finalize(FList^[0], FCount);
  FCount := 0;
  SetCapacity(0);
end;

function TStringListA.Add(const S: RawByteString): Integer;
begin
  Result := AddObject(S, nil);
end;

function TStringListA.AddObject(const S: RawByteString; AObject: TObject): Integer;
begin
  if not Sorted then
    Result := FCount
  else
    if Find(S, Result) then
      case Duplicates of
        dupIgnore: Exit;
        dupError: Error(@SDuplicateString, 0);
      end;
  InsertItem(Result, S, AObject);
end;

procedure TStringListA.Changed;
begin
  if (FUpdateCount = 0) and Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TStringListA.Changing;
begin
  if (FUpdateCount = 0) and Assigned(FOnChanging) then
    FOnChanging(Self);
end;

procedure TStringListA.Clear;
var
  I: Integer;
  Obj: TObject;
begin
  if FCount <> 0 then
  begin
    Changing;

    //Free all objects in the event that this list owns its objects
    if OwnsObjects then
    begin
      for I := 0 to FCount - 1 do
      begin
        Obj := GetObject(I);
        Obj.Free;
      end;
    end;

    Finalize(FList^[0], FCount);
    FCount := 0;
    SetCapacity(0);
    Changed;
  end;
end;

procedure TStringListA.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then Error(@SListIndexError, Index);
  Changing;
  // If this list owns its objects then free the associated TObject with this index
  if OwnsObjects then
    GetObject(Index).Free;
  Finalize(FList^[Index]);
  Dec(FCount);
  if Index < FCount then
    System.Move(FList^[Index + 1], FList^[Index],
      (FCount - Index) * SizeOf(TStringItemA));
  Changed;
end;

procedure TStringListA.Exchange(Index1, Index2: Integer);
begin
  if (Index1 < 0) or (Index1 >= FCount) then Error(@SListIndexError, Index1);
  if (Index2 < 0) or (Index2 >= FCount) then Error(@SListIndexError, Index2);
  Changing;
  ExchangeItems(Index1, Index2);
  Changed;
end;

procedure TStringListA.ExchangeItems(Index1, Index2: Integer);
var
  Temp: Integer;
  Item1, Item2: PStringItem;
begin
  Item1 := @FList^[Index1];
  Item2 := @FList^[Index2];
  Temp := Integer(Item1^.FString);
  Integer(Item1^.FString) := Integer(Item2^.FString);
  Integer(Item2^.FString) := Temp;
  Temp := Integer(Item1^.FObject);
  Integer(Item1^.FObject) := Integer(Item2^.FObject);
  Integer(Item2^.FObject) := Temp;
end;

function TStringListA.Find(const S: RawByteString; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareStrings(FList^[I].FString, S);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        if Duplicates <> dupAccept then L := I;
      end;
    end;
  end;
  Index := L;
end;

function TStringListA.Get(Index: Integer): RawByteString;
begin
  if (Index < 0) or (Index >= FCount) then Error(@SListIndexError, Index);
  Result := FList^[Index].FString;
end;

function TStringListA.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

function TStringListA.GetCount: Integer;
begin
  Result := FCount;
end;

function TStringListA.GetObject(Index: Integer): TObject;
begin
  if (Index < 0) or (Index >= FCount) then Error(@SListIndexError, Index);
  Result := FList^[Index].FObject;
end;

procedure TStringListA.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then Delta := FCapacity div 4 else
    if FCapacity > 8 then Delta := 16 else
      Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

function TStringListA.IndexOf(const S: RawByteString): Integer;
begin
  if not Sorted then Result := inherited IndexOf(S) else
    if not Find(S, Result) then Result := -1;
end;

procedure TStringListA.Insert(Index: Integer; const S: RawByteString);
begin
  InsertObject(Index, S, nil);
end;

procedure TStringListA.InsertObject(Index: Integer; const S: RawByteString;
  AObject: TObject);
begin
  if Sorted then Error(@SSortedListError, 0);
  if (Index < 0) or (Index > FCount) then Error(@SListIndexError, Index);
  InsertItem(Index, S, AObject);
end;

procedure TStringListA.InsertItem(Index: Integer; const S: RawByteString; AObject: TObject);
begin
  Changing;
  if FCount = FCapacity then Grow;
  if Index < FCount then
    System.Move(FList^[Index], FList^[Index + 1],
      (FCount - Index) * SizeOf(TStringItemA));
  with FList^[Index] do
  begin
    Pointer(FString) := nil;
    FObject := AObject;
    FString := S;
  end;
  Inc(FCount);
  Changed;
end;

procedure TStringListA.Put(Index: Integer; const S: RawByteString);
begin
  if Sorted then Error(@SSortedListError, 0);
  if (Index < 0) or (Index >= FCount) then Error(@SListIndexError, Index);
  Changing;
  FList^[Index].FString := S;
  Changed;
end;

procedure TStringListA.PutObject(Index: Integer; AObject: TObject);
begin
  if (Index < 0) or (Index >= FCount) then Error(@SListIndexError, Index);
  Changing;
  FList^[Index].FObject := AObject;
  Changed;
end;

procedure TStringListA.QuickSort(L, R: Integer; SCompare: TStringListSortCompareA);
var
  I, J, P: Integer;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while SCompare(Self, I, P) < 0 do Inc(I);
      while SCompare(Self, J, P) > 0 do Dec(J);
      if I <= J then
      begin
        if I <> J then
          ExchangeItems(I, J);
        if P = I then
          P := J
        else if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then QuickSort(L, J, SCompare);
    L := I;
  until I >= R;
end;

procedure TStringListA.SetCapacity(NewCapacity: Integer);
begin
  if (NewCapacity < FCount) or (NewCapacity > MaxListSize) then
    Error(@SListCapacityError, NewCapacity);
  if NewCapacity <> FCapacity then
  begin
    ReallocMem(FList, NewCapacity * SizeOf(TStringItemA));
    FCapacity := NewCapacity;
  end;
end;

procedure TStringListA.SetSorted(Value: Boolean);
begin
  if FSorted <> Value then
  begin
    if Value then Sort;
    FSorted := Value;
  end;
end;

procedure TStringListA.SetUpdateState(Updating: Boolean);
begin
  if Updating then Changing else Changed;
end;

function StringListCompareStrings(List: TStringListA; Index1, Index2: Integer): Integer;
begin
  Result := List.CompareStrings(List.FList^[Index1].FString,
                                List.FList^[Index2].FString);
end;

procedure TStringListA.Sort;
begin
  CustomSort(StringListCompareStrings);
end;

procedure TStringListA.CustomSort(Compare: TStringListSortCompareA);
begin
  if not Sorted and (FCount > 1) then
  begin
    Changing;
    QuickSort(0, FCount - 1, Compare);
    Changed;
  end;
end;

function TStringListA.CompareStrings(const S1, S2: RawByteString): Integer;
begin
  if CaseSensitive then
    Result := AnsiCompareStr(S1, S2)
  else
    Result := AnsiCompareText(S1, S2);
end;

constructor TStringListA.Create;
begin
  inherited;
end;

constructor TStringListA.Create(OwnsObjects: Boolean);
begin
  inherited Create;
  FOwnsObject := OwnsObjects;
end;

procedure TStringListA.SetCaseSensitive(const Value: Boolean);
begin
  if Value <> FCaseSensitive then
  begin
    FCaseSensitive := Value;
    if Sorted then
    begin
      // Calling Sort won't sort the list because CustomSort will
      // only sort the list if it's not already sorted
      Sorted := False;
      Sorted := True;
    end;
  end;
end;
{$ENDIF UNICODE}

function TrimRightA(const S: RawByteString): RawByteString;
var
  I: Integer;
begin
  I := Length(S);
  while (I > 0) and (S[I] <= ' ') do
    Dec(I);
  Result := Copy(S, 1, I);
end;

type
  TBOMType = (bomAnsi, bomUtf8, bomUcs2BE, bomUcs2LE, bomUcs4BE, bomUcs4LE);

procedure SwapBytes(Data: PByte; Size: Cardinal; StartIndex: Cardinal);
var
  I, Idx: Cardinal;
  b: Byte;
begin
  for I := 0 to (Size - StartIndex) div 2 - 1 do
  begin
    Idx := StartIndex + I * 2;
    b := Data[Idx];
    Data[Idx] := Data[Idx + 1];
    Data[Idx + 1] := b;
  end;
end;

function ReadBOM(Buffer: PByte; Size: Cardinal; var BufStart: Integer): TBOMType;
begin
  BufStart := 0;

  Result := bomAnsi;
  if Size >= 4 then
  begin
    if (Buffer[0] = $EF) and (Buffer[1] = $BB) then
    begin
      if Buffer[2] = $BF then
      begin
        Result := bomUtf8;
        BufStart := 3;
      end;
    end
    else if (Buffer[0] = $FF) and (Buffer[1] = $FE) then
    begin
      if (Buffer[2] = 0) and (Buffer[3] = 0) then
      begin
        Result := bomUcs4LE;
        BufStart := 4;
      end
      else
      begin
        Result := bomUcs2LE;
        BufStart := 2;
      end;
    end
    else if (Buffer[0] = $FE) and (Buffer[1] = $FF) then
    begin
      Result := bomUcs2BE;
      BufStart := 2;
    end
    else if (Buffer[0] = 0) and (Buffer[1] = 0) then
    begin
      if (Buffer[2] = $FE) and (Buffer[3] = $FF) then
      begin
        Result := bomUcs4BE;
        BufStart := 4;
      end;
    end;
  end;
end;

function LoadBufferToUTF8String(const Buffer: TBytes): UTF8String;
begin
  if Length(Buffer) > 0 then
    Result := LoadBufferToUTF8String(@Buffer[0], Length(Buffer))
  else
    Result := '';
end;

function LoadBufferToUTF8String(Buffer: PByte; Size: Cardinal): UTF8String; overload;
var
  StrAnsi: AnsiString;
  StrWide: UnicodeString;
  StrUCS4: UCS4String;

  BOMType: TBOMType;
  BufStart: Integer;
begin
  Result := '';

  if Size > 0 then
  begin
    BOMType := ReadBOM(Buffer, Size, BufStart);
    Dec(Size, BufStart);
    case BOMType of
      bomAnsi:
        begin
          SetString(StrAnsi, PAnsiChar(@Buffer[BufStart]), Size);
          Result := UTF8Encode(StrAnsi);
        end;
      bomUtf8:
        SetString(Result, PAnsiChar(@Buffer[BufStart]), Size);
      bomUcs2BE:
        begin
          SwapBytes(Buffer, Size, BufStart);
          SetString(StrWide, PWideChar(@Buffer[BufStart]), Size div SizeOf(WideChar));
        end;
      bomUcs2LE:
        SetString(StrWide, PWideChar(@Buffer[BufStart]), Size div SizeOf(WideChar));
      bomUcs4BE, bomUcs4LE:
        begin
          SetLength(StrUCS4, Size div SizeOf(UCS4Char));
          if Length(StrUCS4) > 0 then
          begin
            if BOMType = bomUcs4BE then
              SwapBytes(Buffer, Size, BufStart);
            Move(Buffer[BufStart], StrUCS4[0], Size);
            Result := AnsiToUtf8(UCS4StringToUnicodeString(StrUCS4));
          end;
        end;
    end;
  end;
end;

function LoadTextFileToUTF8String(const Filename: string): UTF8String;
var
  Data: TBytes;
  hFile: THandle;
  Size: Cardinal;
begin
  hFile := FileOpen(Filename, fmOpenRead or fmShareDenyWrite);
  try
    Size := GetFileSize(hFile, nil);
    SetLength(Data, Size);
    if Size > 0 then
      FileRead(hFile, Data[0], Size);
  finally
    FileClose(hFile);
  end;

  Result := LoadBufferToUTF8String(Data);
end;

function CreateMemoryStreamFromUTF8String(const S: UTF8String): TMemoryStream;
var
  Preamble: TBytes;
begin
  Result := TMemoryStream.Create;
  try
    if S <> '' then
    begin
      Preamble := TEncoding.UTF8.GetPreamble;
      if Length(Preamble) > 0 then
        Result.Write(Preamble[0], Length(Preamble));
      Result.Write(S[1], Length(S));
      Result.Position := 0;
    end;
  except
    Result.Free;
    raise;
  end;
end;

end.
