unit ExportStreams;

interface

uses
  Windows, SysUtils, Classes, Contnrs, HashContainers, Utils, LangExtIntf;

type
  TExportStreamItem = class(TInterfaceEnabledObject, IExportStreamItem)
  private
    FName: UTF8String;
    FStreams: TObjectList;
    function GetCount: Integer; 
    function GetStream(Index: Integer): TMemoryStream; inline;
  protected
    { IExportStreamItem }
    function Get_Count: Integer; stdcall;
    function Get_Stream(Index: Integer): IVirtualStream; stdcall;
    function Get_Name: PAnsiChar; stdcall;
  public
    constructor Create(const AName: UTF8String); overload;
    constructor Create(Stream: TStream); overload;
    destructor Destroy; override;

    function Add: TMemoryStream;
    procedure WriteToStream(Stream: TStream);

    property Count: Integer read GetCount;
    property Streams[Index: Integer]: TMemoryStream read GetStream; default;

    property Name: UTF8String read FName;
  end;

  TExportStream = class(TInterfaceEnabledObject, IExportStream)
  private
    FFilename: UTF8String;
    FStreams: TObjectList;
    FStreamNames: TUTF8StringIntegerHash;
    function GetCount: Integer;
    function GetItem(Index: Integer): TExportStreamItem; inline;
  protected
    function Find(Name: PAnsiChar): IExportStreamItem; overload; stdcall;
    function Get_Count: Integer; stdcall;
    function Get_Item(Index: Integer): IExportStreamItem; stdcall;
    function Get_Filename: PAnsiChar; stdcall;
  public
    constructor Create(const AFilename: UTF8String);
    destructor Destroy; override;

    procedure Clear;

    function AddStream(const Name: UTF8String): TStream; // case-sensitive
    function Find(const Name: UTF8String): TExportStreamItem; overload; // case-sensitive

    procedure SaveToFile(const Filename: string);
    function LoadFromFile(const Filename: string): Boolean;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TExportStreamItem read GetItem; default;

    property Filename: UTF8String read FFilename;
  end;

  TExportStreamList = class(TInterfacedObject, IExportStreamList) // ref counted
  private
    FItems: TObjectList;
    FHash: TUTF8StringIntegerHash;
    function GetCount: Integer; inline;
    function GetItem(Index: Integer): TExportStream; inline;
  protected
    { IExportStreamList }
    function Find(Filename: PAnsiChar): IExportStream; overload; stdcall;
    function Get_Count: Integer; stdcall;
    function Get_Item(Index: Integer): IExportStream; stdcall;
  public
    constructor Create(AOwnsExportStreams: Boolean = True);
    destructor Destroy; override;
    procedure Clear;
    procedure Remove(const Filename: UTF8String);

    function Append(Item: TExportStream): Integer;
    function CreateExportStreamListOf(const Name: UTF8String): TExportStreamList; // case sensitive

    function Find(const Filename: UTF8String): TExportStream; overload;
    function Add(const Filename: UTF8String): TExportStream;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TExportStream read GetItem; default;
  end;

implementation

const
  ExportStreamHeader: AnsiString = 'DLANGEXT 1.0';

resourcestring
  RsInvalidExportStream = 'Invalid ExportStream file';

{ TExportStreamList }

constructor TExportStreamList.Create(AOwnsExportStreams: Boolean);
begin
  inherited Create;
  FItems := TObjectList.Create(AOwnsExportStreams);
  FHash := TUTF8StringIntegerHash.Create(False);
end;

destructor TExportStreamList.Destroy;
begin
  FHash.Free;
  FItems.Free;
  inherited Destroy;
end;

function TExportStreamList.CreateExportStreamListOf(const Name: UTF8String): TExportStreamList;
var
  I: Integer;
begin
  Result := TExportStreamList.Create(False);
  for I := 0 to Count - 1 do
    if Items[I].Find(Name) <> nil then
      Result.Append(Items[I]);
end;

function TExportStreamList.Append(Item: TExportStream): Integer;
begin
  Result := FItems.Add(Item);
  FHash.Add(Item.Filename, Integer(Item));
end;

procedure TExportStreamList.Clear;
begin
  FHash.Clear;
  FItems.Clear;
end;

function TExportStreamList.Find(const Filename: UTF8String): TExportStream;
begin
  if not FHash.Find(Filename, Integer(Result)) then
    Result := nil;
end;

function TExportStreamList.Add(const Filename: UTF8String): TExportStream;
begin
  Assert( Find(Filename) = nil );
  Result := TExportStream.Create(Filename);
  Append(Result);
end;

function TExportStreamList.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TExportStreamList.GetItem(Index: Integer): TExportStream;
begin
  Result := TExportStream(FItems[Index]);
end;

procedure TExportStreamList.Remove(const Filename: UTF8String);
var
  Item: TExportStream;
begin
  if FHash.Find(Filename, Integer(Item)) then
  begin
    FHash.Remove(Filename);
    FItems.Remove(Item);
  end;
end;

function TExportStreamList.Get_Count: Integer;
begin
  Result := Count;
end;

function TExportStreamList.Get_Item(Index: Integer): IExportStream;
begin
  Result := Items[Index];
end;

function TExportStreamList.Find(Filename: PAnsiChar): IExportStream;
var
  S: UTF8String;
begin
  S := UTF8String(Filename);
  Result := Find(S);
end;

{ TExportStream }

constructor TExportStream.Create(const AFilename: UTF8String);
begin
  inherited Create;
  FFilename := AFilename;
end;

destructor TExportStream.Destroy;
begin
  FStreams.Free;
  FStreamNames.Free;
  inherited Destroy;
end;

procedure TExportStream.Clear;
begin
  if FStreams <> nil then
  begin
    FStreams.Clear;
    FStreamNames.Clear;
  end;
end;

function TExportStream.Find(const Name: UTF8String): TExportStreamItem;
var
  Index: Integer;
begin
  Result := nil;
  if (FStreamNames <> nil) and FStreamNames.Find(Name, Index) then
    Result := TExportStreamItem(FStreams[Index]);
end;

function TExportStream.GetCount: Integer;
begin
  Result := 0;
  if FStreams <> nil then
    Result := FStreams.Count;
end;

function TExportStream.GetItem(Index: Integer): TExportStreamItem;
begin
  Result := TExportStreamItem(FStreams[Index]);
end;

function TExportStream.LoadFromFile(const Filename: string): Boolean;
var
  Cnt: Integer;
  Stream: TFileStream;
  Item: TExportStreamItem;
begin
  Result := False;
  if FStreams <> nil then
  begin
    FStreams.Clear;
    FStreamNames.Clear;
  end;
  Stream := TFileStream.Create(Filename, fmOpenRead);
  try
    Stream.Seek(Length(ExportStreamHeader) * SizeOf(AnsiChar), soCurrent);
    if Stream.Read(Cnt, SizeOf(Cnt)) <> SizeOf(Cnt) then
      Exit;
    Result := True;
    try
      while Cnt > 0 do
      begin
        Item := TExportStreamItem.Create(Stream);
        if FStreams = nil then
        begin
          FStreams := TObjectList.Create;
          FStreamNames := TUTF8StringIntegerHash.Create(True); // case sensitive
        end;
        FStreamNames.Add(Item.Name, FStreams.Add(Item));
        Dec(Cnt);
      end;
    except
      FreeAndNil(FStreams);
      FreeAndNil(FStreamNames);
      Result := False;
    end;
  finally
    Stream.Free;
  end;
end;

procedure TExportStream.SaveToFile(const Filename: string);
var
  I, Cnt: Integer;
  HasData: Boolean;
  Stream: TFileStream;
begin
  HasData := False;
  for I := 0 to Count - 1 do
  begin
    if Items[I].Count > 0 then
    begin
      HasData := True;
      Break;
    end;
  end;

  if HasData then
  begin
    Stream := TFileStream.Create(Filename, fmCreate);
    try
      Stream.Write(ExportStreamHeader[1], Length(ExportStreamHeader) * SizeOf(AnsiChar));
      Cnt := Count;
      Stream.Write(Cnt, SizeOf(Cnt));
      for I := 0 to Count - 1 do
        Items[I].WriteToStream(Stream);
    finally
      Stream.Free;
    end;
  end
  else
    DeleteFile(Filename);
end;

function TExportStream.AddStream(const Name: UTF8String): TStream;
var
  Hash: Integer;
  Index: Integer;
  Item: TExportStreamItem;
begin
  Hash := FStreamNames.HashString(Name);
  if FStreamNames = nil then
  begin
    FStreams := TObjectList.Create;
    FStreamNames := TUTF8StringIntegerHash.Create(True); // case-sensitive
  end;
  if FStreamNames.Find(Hash, Name, Index) then
    Item := Items[Index]
  else
  begin
    Item := TExportStreamItem.Create(Name);
    FStreamNames.Add(Hash, Name, FStreams.Add(Item));
  end;
  Result := Item.Add;
end;

function TExportStream.Find(Name: PAnsiChar): IExportStreamItem;
var
  S: UTF8String;
begin
  S := Name;
  Result := Find(S);
end;

function TExportStream.Get_Count: Integer;
begin
  Result := Count;
end;

function TExportStream.Get_Filename: PAnsiChar;
begin
  Result := PAnsiChar(Filename);
end;

function TExportStream.Get_Item(Index: Integer): IExportStreamItem;
begin
  Result := Items[Index];
end;

{ TExportStreamItem }

constructor TExportStreamItem.Create(const AName: UTF8String);
begin
  inherited Create;
  FName := AName;
  FStreams := TObjectList.Create;
end;

destructor TExportStreamItem.Destroy;
begin
  FStreams.Free;
  inherited Destroy;
end;

constructor TExportStreamItem.Create(Stream: TStream);
var
  Len, Cnt: Integer;
  MemStream: TMemoryStream;
begin
  Create('');

  if Stream.Read(Len, SizeOf(Len)) <> SizeOf(Len) then
    raise EReadError.CreateRes(@RsInvalidExportStream);
  SetLength(FName, Len);
  if Len > 0 then
    if Stream.Read(FName[1], Len) <> Len then
      raise EReadError.CreateRes(@RsInvalidExportStream);

  if Stream.Read(Cnt, SizeOf(Cnt)) <> SizeOf(Cnt) then
    raise EReadError.CreateRes(@RsInvalidExportStream);
  while Cnt > 0 do
  begin
    if Stream.Read(Len, SizeOf(Len)) <> SizeOf(Len) then
      raise EReadError.CreateRes(@RsInvalidExportStream);
    MemStream := Add();
    MemStream.Size := Len;
    if Stream.Read(MemStream.Memory^, Len) <> Len then
      raise EReadError.CreateRes(@RsInvalidExportStream);
    MemStream.Position := 0;
    Dec(Cnt);
  end;
end;

procedure TExportStreamItem.WriteToStream(Stream: TStream);
var
  Len, Cnt: Integer;
  I: Integer;
  MemStream: TMemoryStream;
begin
  Len := Length(Name);
  Stream.Write(Len, SizeOf(Len));
  if Len > 0 then
    Stream.Write(FName[1], Len);
  Cnt := Count;
  Stream.Write(Cnt, SizeOf(Cnt));
  for I := 0 to Cnt - 1 do
  begin
    MemStream := Streams[I];
    MemStream.Position := 0;
    Len := MemStream.Size;
    Stream.Write(Len, SizeOf(Len));
    if Len > 0 then
      Stream.Write(MemStream.Memory^, Len);
  end;
end;

function TExportStreamItem.Add: TMemoryStream;
begin
  Result := TMemoryStream.Create;
  FStreams.Add(Result);
end;

function TExportStreamItem.GetCount: Integer;
begin
  Result := FStreams.Count;
end;

function TExportStreamItem.GetStream(Index: Integer): TMemoryStream;
begin
  Result := TMemoryStream(FStreams[Index]);
end;

function TExportStreamItem.Get_Count: Integer;
begin
  Result := Count;
end;

function TExportStreamItem.Get_Name: PAnsiChar;
begin
  Result := PAnsiChar(Name);
end;

function TExportStreamItem.Get_Stream(Index: Integer): IVirtualStream;
begin
  Result := TVirtualStreamAdapter.Create(GetStream(Index), 0, False);
end;

end.
