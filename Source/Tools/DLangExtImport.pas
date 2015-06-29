unit DLangExtImport;

interface

const
  DLangExtensions = 'DLangExtensions.dll';

type
  TInspectFileMode = (ifmOpen, ifmCreate);

  IString = interface
    ['{264156F0-06C4-4CD7-9364-63CF1A5BF683}']
    procedure SetString(Buffer: PAnsiChar; Len: Cardinal); stdcall;
    function GetString(out Len: Cardinal): PAnsiChar; stdcall;
  end;

  TStringValue = class(TObject, IString)
  private
    FValue: string;
  public
    constructor Create(const AValue: string);
    function GetString(out Len: Cardinal): PAnsiChar; stdcall;
    procedure SetString(Buffer: PAnsiChar; Len: Cardinal); stdcall;

    procedure SetLength(NewLen: Cardinal);

    function QueryInterface(const IID: TGUID; out Obj): HRESULT; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

    property Value: string read FValue write FValue;
  end;

function DLangExt_GetVersion(const VersionStr: IString): LongBool; stdcall;
  external DLangExtensions name 'DLangExt_GetVersion';

procedure DLangExt_InitPaths(UnitPaths, SourcePaths, DcuOutputDir: PAnsiChar); stdcall;
  external DLangExtensions name 'DLangExt_InitPaths';

function DLangExt_AlterContent(const Filename: PAnsiChar; const Content, ErrorMsg: IString): LongBool; stdcall;
  external DLangExtensions name 'DLangExt_AlterContent';

procedure DLangExt_InspectFilename(const Filename: PAnsiChar; FileMode: TInspectFileMode); stdcall;
  external DLangExtensions name 'DLangExt_InspectFilename';

procedure DLangExt_Cleanup; stdcall;
  external DLangExtensions name 'DLangExt_Cleanup';

implementation

{ TStringValue }

constructor TStringValue.Create(const AValue: string);
begin
  inherited Create;
  FValue := AValue;
end;

function TStringValue.GetString(out Len: Cardinal): PAnsiChar;
begin
  Result := PAnsiChar(FValue);
  Len := Length(Value);
end;

procedure TStringValue.SetLength(NewLen: Cardinal);
begin
  System.SetLength(FValue, NewLen);
end;

procedure TStringValue.SetString(Buffer: PAnsiChar; Len: Cardinal);
begin
  System.SetString(FValue, Buffer, Len);
end;

function TStringValue.QueryInterface(const IID: TGUID; out Obj): HRESULT;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := S_FALSE;
end;

function TStringValue._AddRef: Integer;
begin
  Result := 1;
end;

function TStringValue._Release: Integer;
begin
  Result := 1;
end;

end.
