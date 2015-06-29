{******************************************************************************}
{*                                                                            *}
{* Delphi Lexer                                                               *}
{*                                                                            *}
{* (C) 2005 Andreas Hausladen                                                 *}
{*                                                                            *}
{******************************************************************************}

unit DelphiLexer;

interface

uses
  SysUtils, Classes, Contnrs, TypInfo, StrUtils;

const
  WhiteChars = [#1..#32];
  NumberChars = ['0'..'9'];
  HexNumberChars = NumberChars + ['A'..'F', 'a'..'f'];
  IdentFirstChars = ['a'..'z', 'A'..'Z', '_'];
  IdentChars = IdentFirstChars + NumberChars;

type
  TTokenKind = (
    tkNone,

    tkSymbol,
      tkEqual,
      tkGreaterThan,
      tkLessThan,
      tkMinus,
      tkPlus,
      tkMultiply,
      tkDivide,

      tkLParan,
      tkRParan,
      tkLBracket,
      tkRBracket,
      tkColon,
      tkSemicolon,
      tkComma,
      tkPointer,     // '^'
      tkAddr,        // '@'
      tkAmp,         // '&' // D8/9
      tkQualifier,   // '.'

    // tkSymbolLevel2 >= tkGreaterEqualThan
      tkGreaterEqualThan, // '>='
      tkLessEqualThan,    // '<='
      tkNotEqual,         // '<>'
      tkRange,            // '..'
      tkAssign,           // ':='

    tkComment,
    tkDirective,

    tkString,
    tkInt,
    tkFloat,

    tkMacro, // macro support
    tkIdent,
      tkI_read,
      tkI_write,
      tkI_add,
      tkI_remove,
      tkI_default,
      tkI_name,
      tkI_index,
      tkI_message,
      tkI_static, // D9
      tkI_forward,
      tkI_requires,
      tkI_contains,
      tkI_platform,
      tkI_deprecated,
      tkI_experimental, // D9
      tkI_out,
      tkI_helper,

      tkI_overload,
      tkI_virtual,
      tkI_override,
      tkI_dynamic,
      tkI_abstract,
      tkI_reintroduce,

      tkI_assembler,
      tkI_register,
      tkI_stdcall,
      tkI_cdecl,
      tkI_safecall,
      tkI_pascal,
      tkI_external,

      tkI_automated,
      tkI_strict, // D9
      tkI_private,
      tkI_protected,
      tkI_public,
      tkI_published,
      tkI_local,  // D9
      tkI_export,
      tkI_resident, // D9
      tkI_far,
      tkI_near,
      tkI_readonly,
      tkI_writeonly,
      tkI_nodefault,
      tki_stored,
      tkI_implements,
      tkI_varargs,
      tkI_dispid,

      tkI_absolute,
      tkI_final, // D9
      tkI_unsafe, // D9
      tkI_sealed, // D9

      tkI_package,

      tkI_on,
      tkI_at,

    //tkIdentStrictReserved >= tkI_if
      tkI_if,
      tkI_else,
      tkI_then,
      tkI_while,
      tkI_do,
      tkI_for,
      tkI_to,
      tkI_downto,
      tkI_repeat,
      tkI_until,
      tkI_case,
      tkI_with,
      tkI_raise,
      tkI_in,
      tkI_is,
      tkI_as,
      tkI_of,
      tkI_file,
      tkI_goto,
      tkI_except,
      tkI_finally,
      tkI_try,
      tkI_nil,

      tkI_shl,
      tkI_shr,
      tkI_mod,
      tkI_div,
      tkI_and,
      tkI_or,
      tkI_not,
      tkI_xor,

      tkI_class,
      tkI_object,
      tkI_record,
      tkI_var,
      tkI_const,
      tkI_type,
      tkI_threadvar,
      tkI_label,
      tkI_exports,
      tkI_resourcestring,
      tkI_unit,
      tkI_program,
      tkI_uses,
      tkI_interface,
      tkI_implementation,
      tkI_begin,
      tkI_end,
      tkI_procedure,
      tkI_function,
      tkI_constructor,
      tkI_destructor,
      tkI_property,
      tkI_packed,
      tkI_dispinterface,
      tkI_string,

      tkI_array,
      tkI_set,

      tkI_asm,

      tkI_inherited,
      tkI_inline,

      tkI_library,
      tkI_initialization,
      tkI_finalization
  );

  TTokenKindSet = set of TTokenKind;

const
  tkSymbolLevel2 = tkGreaterEqualThan;
  tkIdentStrictReserved = tkI_if;
  tkLast = High(TTokenKind);
  tkDot = tkQualifier;

type
  TDelphiLexer = class;

  TToken = class(TObject)
  private
    FTokenIndex: Integer;
    FKind: TTokenKind;
    FValue: string;
    FColumn: Integer;
    FLine: Integer;
    FIndex: Integer;
    FFilename: string;
  protected
    procedure OffsetToken(OffsetTokenIndex, OffsetIndex, OffsetColumn, OffsetLine: Integer);
  public
    constructor Create(ATokenIndex: Integer; AKind: TTokenKind; const AValue, AFilename: string;
      ALine, AColumn, AIndex: Integer);

    function IsIdent: Boolean; inline;
    function IsFreeUsableIdent: Boolean; inline;
    function IsTypeIdent: Boolean; inline;

    property Kind: TTokenKind read FKind;
    property Value: string read FValue;

    property Line: Integer read FLine; // 0..n-1
    property Column: Integer read FColumn; // 1..m
    property Index: Integer read FIndex; // 1..k
    property Filename: string read FFilename;
    property TokenIndex: Integer read FTokenIndex; // 0..j-1
  end;

  TDelphiLexer = class(TObject)
  private
    FFilename: string;
    FText: AnsiString;
    FLine: Integer;
    FColumn: Integer;
    FIndex: Integer;

    FTokens: TObjectList;
    FCurTokenIndex: Integer;
    FModified: Boolean;

    function GetCount: Integer;
    function GetTokenProp(Index: Integer): TToken;
    procedure DiscardTokens(StartIndex: Integer);
    function GetCurrentToken: TToken;
    function GetPreviousToken: TToken;
  protected
    function GetToken: TToken; overload;
    function GetToken(out Token: TToken): Boolean; overload;
  public
    constructor Create(const AFilename: string; const AText: AnsiString;
      OwnTokens: Boolean = True; StartLine: Integer = 1; StartColumn: Integer = 1);
    destructor Destroy; override;

    procedure RestartLexer;
    procedure RewindLastToken;
    function NextToken: TToken; overload;
    function NextToken(out Token: TToken): Boolean; overload;
    function NextTokenNoComment: TToken; overload;
    function NextTokenNoComment(out Token: TToken): Boolean; overload;
    function PreTokenOf(Token: TToken): TToken;
    function PreTokenNoCommentOf(Token: TToken): TToken;
    function NextTokenOf(Token: TToken): TToken;
    function NextTokenNoCommentOf(Token: TToken): TToken;
    function GetFullStringNext: string; // concates the current token string and all following string concatinations. Leave: next token after last string token
    function LookAhead: TToken;  
    function LookAheadNoComment: TToken;  

    function ParseIdentifierName: string;

    property Index: Integer read FIndex;
    property Line: Integer read FLine;
    property Column: Integer read FColumn;
    property Text: AnsiString read FText;
    property Filename: string read FFilename;

    procedure DeleteToken(Token: TToken);
    procedure DeleteTokens(StartToken, EndToken: TToken);
    procedure ReplaceToken(Token: TToken; const Text: string);
    { Index is 1-based }
    procedure DeleteText(StartIndex, Len: Integer);
    procedure InsertText(StartIndex: Integer; const Text: string);
    procedure ReplaceText(StartIndex, Len: Integer; const Text: string);

      // already parsed tokens
    property Count: Integer read GetCount;
    property Tokens[Index: Integer]: TToken read GetTokenProp;
    property Modified: Boolean read FModified;
    property PreviousToken: TToken read GetPreviousToken;
    property CurrentToken: TToken read GetCurrentToken;
  end;

function TokenKindToString(Kind: TTokenKind): string;
function LoadFileToString(const Filename: string): AnsiString;
function Mangle(const Name: string): string;
function IsEndIfToken(Token: TToken): Boolean; inline;

implementation

uses
  HashContainers;

resourcestring
  RsIdentifier = 'Identifier';
  RsString = 'String';
  RsInteger = 'Integer';
  RsFloat = 'Float number';

var
  TokenStrings: TStringIntegerHash;

function Mangle(const Name: string): string;
var
  P: PChar;
begin
  Result := IntToStr(Length(Name)) + Name;
  P := PChar(Result);
  while P[0] <> #0 do
  begin
    if P[0] = '.' then
      P[0] := '_';
    Inc(P);
  end;
end;

function IsEndIfToken(Token: TToken): Boolean; inline;
begin
  Result := (Token.Kind = tkDirective) and
            (StartsText('{$ENDIF', Token.Value) or StartsText('{$IFEND', Token.Value) or
             StartsText('(*$ENDIF', Token.Value) or StartsText('(*$IFEND', Token.Value));
end;

function FindLineStart(const S: string; Index: Integer): Integer;
begin
  Result := Index;
  while (Result > 0) do
  begin
    case S[Result] of
      #10, #13:
        Break;
    end;
    Dec(Result);
  end;
  Inc(Result);
end;

procedure InitTokenStrings;
var
  Kind: TTokenKind;
  Info: PTypeInfo;
begin
  TokenStrings := TStringIntegerHash.Create({CaseSensitive:=}False);

  Info := TypeInfo(TTokenKind);
  for Kind := Succ(tkIdent) to High(TTokenKind) do
    TokenStrings.Add(
      Copy(GetEnumName(Info, Integer(Kind)), 5, MaxInt),
      Integer(Kind)
    );
end;

function FindTokenIdentKind(const Value: string): TTokenKind;
var
  TokenKindValue: Integer;
begin
  if TokenStrings.Find(Value, TokenKindValue) then
    Result := TTokenKind(TokenKindValue)
  else
    Result := tkIdent;
end;

function TokenKindToString(Kind: TTokenKind): string;
begin
  if Kind > tkIdent then
    Result := AnsiUpperCase(Copy(GetEnumName(TypeInfo(TTokenKind), Integer(Kind)), 5, MaxInt))
  else
  begin
    case Kind of
      tkEqual: Result := '=';
      tkGreaterThan: Result := '>';
      tkLessThan: Result := '<';
      tkMinus: Result := '-';
      tkPlus: Result := '+';
      tkMultiply: Result := '*';
      tkDivide: Result := '/';

      tkLParan: Result := '(';
      tkRParan: Result := ')';
      tkLBracket: Result := '[';
      tkRBracket: Result := ']';
      tkColon: Result := ':';
      tkSemicolon: Result := ';';
      tkComma: Result := ',';
      tkPointer: Result := '^';
      tkAddr: Result := '@';
      tkAmp: Result := '&';
      tkQualifier: Result := '.';

      tkGreaterEqualThan: Result := '>=';
      tkLessEqualThan: Result := '<=';
      tkNotEqual: Result := '<>';
      tkRange: Result := '..';
      tkAssign: Result := ':=';

    {tkComment: Result := RsComment;
    tkDirective: Result := RsCompilerDirective}

      tkIdent: Result := RsIdentifier;
      tkString: Result := RsString;
      tkInt: Result := RsInteger;
      tkFloat: Result := RsFloat;
    end;
    Result := '' + Result + '';
  end;
end;

function LoadFileToString(const Filename: string): AnsiString;
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(Filename, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(Result, Stream.Size);
    if Result <> '' then
      Stream.Read(Result[1], Length(Result));
  finally
    Stream.Free;
  end;
end;

{ TToken }

constructor TToken.Create(ATokenIndex: Integer; AKind: TTokenKind;
  const AValue, AFilename: string; ALine, AColumn, AIndex: Integer);
begin
  inherited Create;
  FTokenIndex := ATokenIndex;
  FKind := AKind;
  FValue := AValue;
  FLine := ALine;
  FColumn := AColumn;
  FIndex := AIndex;
  FFilename := AFilename;
end;

function TToken.IsIdent: Boolean;
begin
  Result := Kind >= tkIdent;
end;

function TToken.IsTypeIdent: Boolean;
begin
  Result := (Kind >= tkIdent) and ((Kind < tkIdentStrictReserved) or (Kind = tkI_string));
end;

function TToken.IsFreeUsableIdent: Boolean;
begin
  Result := (Kind >= tkIdent) and (Kind < tkIdentStrictReserved);
end;

procedure TToken.OffsetToken(OffsetTokenIndex, OffsetIndex, OffsetColumn, OffsetLine: Integer);
begin
  Inc(FTokenIndex, OffsetTokenIndex);
  Inc(FColumn, OffsetColumn);
  Inc(FLine, OffsetLine);
  Inc(FIndex, OffsetIndex);
end;

{ TDelphiLexer }

constructor TDelphiLexer.Create(const AFilename: string; const AText: AnsiString;
  OwnTokens: Boolean; StartLine: Integer; StartColumn: Integer);
begin
  inherited Create;
  FTokens := TObjectList.Create(OwnTokens);
  FFilename := AFilename;
  FText := AText;
  FLine := StartLine;
  FIndex := 1;
  FColumn := StartColumn;
  if FColumn < 1 then
    FColumn := 1;
  {TODO: BOM detection }
end;

destructor TDelphiLexer.Destroy;
begin
  FTokens.Free;
  inherited Destroy;
end;

function TDelphiLexer.GetTokenProp(Index: Integer): TToken;
begin
  Result := TToken(FTokens[Index]);
end;

function TDelphiLexer.GetCount: Integer;
begin
  Result := FTokens.Count;
end;

procedure TDelphiLexer.DeleteToken(Token: TToken);
var
  TokenIndex, Len: Integer;
  I: Integer;
  Tk: TToken;
  Line: Integer;
begin
  if Token <> nil then
  begin
    { There is no line break change by deleting one token }
    Len := Length(Token.Value);
    Delete(FText, Token.Index, Len);
    TokenIndex := Token.TokenIndex;
    Line := Token.Line;
    for I := TokenIndex + 1 to FTokens.Count - 1 do
    begin
      Tk := TToken(FTokens[I]);
      if Tk.Line = Line then
        Tk.OffsetToken(-1, -Len, 0, 0)
      else
        Tk.OffsetToken(-1, -Len, -Len, 0);
    end;
    FTokens.Delete(TokenIndex);
    if FCurTokenIndex > 0 then
      Dec(FCurTokenIndex);
    FModified := True;
  end;
end;

procedure TDelphiLexer.DeleteTokens(StartToken, EndToken: TToken);
var
  I: Integer;
  StartIndex, EndIndex: Integer;
  StartTokenIndex, EndTokenIndex: Integer;
  OffsetLine, Len: Integer;
  TokenDeleteCount: Integer;
  Tk: TToken;
  LineStartIndex: Integer;
begin
  if (StartToken <> nil) or (EndToken <> nil) then
  begin
    if StartToken = nil then
      DeleteToken(EndToken)
    else if EndToken = nil then
      DeleteToken(StartToken)
    else
    begin
      StartIndex := StartToken.Index;
      EndIndex := EndToken.Index + Length(EndToken.Value);
      Delete(FText, StartIndex, EndIndex - StartIndex);
      StartTokenIndex := StartToken.TokenIndex;
      EndTokenIndex := EndToken.TokenIndex;
      TokenDeleteCount := EndTokenIndex - StartTokenIndex + 1;
      Len := EndIndex - StartIndex + 1;

      LineStartIndex := FindLineStart(FText, EndToken.Index - 1);
      if LineStartIndex < StartIndex then
        LineStartIndex := StartIndex;

      OffsetLine := EndToken.Line - StartToken.Line;
      Dec(FLine, OffsetLine);
      for I := EndTokenIndex + 1 to FTokens.Count - 1 do
      begin
        Tk := TToken(FTokens[I]);
        if Tk.Line = Line then
          Tk.OffsetToken(-TokenDeleteCount, -Len, -(EndIndex - LineStartIndex), -OffsetLine)
        else
          Tk.OffsetToken(-TokenDeleteCount, -Len, 0, -OffsetLine);
      end;
      for I := EndTokenIndex - 1 downto StartTokenIndex do
        FTokens.Delete(I);

      if FCurTokenIndex >= TokenDeleteCount then
        Dec(FCurTokenIndex, TokenDeleteCount);
      FModified := True;
    end;
  end;
end;

procedure TDelphiLexer.ReplaceToken(Token: TToken; const Text: string);
var
  Index: Integer;
begin
  { TODO: optimize }
  Index := Token.Index;
  DeleteToken(Token);
  InsertText(Index, Text);
  FModified := True;
end;

procedure TDelphiLexer.DiscardTokens(StartIndex: Integer);
var
  I: Integer;
  Tk: TToken;
begin
  for I := FTokens.Count - 1 downto 0 do
  begin
    Tk := TToken(FTokens[I]);
    if Tk.Index >= StartIndex then
      FTokens.Delete(I)
    else
    begin
      if Tk.Index + Length(Tk.Value) > StartIndex then
      begin
        Self.FIndex := Tk.Index;
        Self.FLine := Tk.Line;
        FTokens.Delete(I);
      end
      else
      begin
        Self.FIndex := Tk.Index + Length(Tk.Value);
        Self.FLine := Tk.Line;
      end;
      if FCurTokenIndex > FTokens.Count then
        FCurTokenIndex := FTokens.Count;
      Break;
    end;
  end;
  FModified := True;
end;

procedure TDelphiLexer.DeleteText(StartIndex, Len: Integer);
begin
  if Len > 0 then
  begin
    Delete(FText, StartIndex, Len);
    DiscardTokens(StartIndex);
    FModified := True;
  end;
end;

procedure TDelphiLexer.InsertText(StartIndex: Integer; const Text: string);
begin
  if Text <> '' then
  begin
    Insert(Text, FText, StartIndex);
    DiscardTokens(StartIndex);
    FModified := True;
  end;
end;

procedure TDelphiLexer.ReplaceText(StartIndex, Len: Integer; const Text: string);
begin
  { TODO: optimize }
  DeleteText(StartIndex, Len);
  InsertText(StartIndex, Text);
  FModified := True;
end;

procedure TDelphiLexer.RestartLexer;
begin
  FCurTokenIndex := 0;
end;

procedure TDelphiLexer.RewindLastToken;
begin
  if FCurTokenIndex > 0 then
    Dec(FCurTokenIndex);
end;

function TDelphiLexer.LookAhead: TToken;
var
  Idx: Integer;
begin
  Idx := FCurTokenIndex;
  Result := NextToken;
  FCurTokenIndex := Idx;
end;

function TDelphiLexer.LookAheadNoComment: TToken;
var
  Idx: Integer;
begin
  Idx := FCurTokenIndex;
  Result := NextTokenNoComment;
  FCurTokenIndex := Idx;
end;

function TDelphiLexer.NextToken: TToken;
begin
  Result := nil;
  while FTokens.Count <= FCurTokenIndex do
  begin
    Result := GetToken;
    if Result = nil then
      Break;
  end;
  if FCurTokenIndex < FTokens.Count then
  begin
    Result := TToken(FTokens[FCurTokenIndex]);
    Inc(FCurTokenIndex);
  end;
end;

function TDelphiLexer.NextToken(out Token: TToken): Boolean;
begin
  Token := NextToken;
  Result := Token <> nil;
end;

function TDelphiLexer.NextTokenNoComment: TToken;
begin
  repeat
    Result := NextToken;
    if (Result <> nil) and (Result.Kind = tkDirective) then
    begin
      if StartsText(Result.Value, '{INCLUDE ') or
         StartsText(Result.Value, '{I ') or
         StartsText(Result.Value, '(*INCLUDE ') or
         StartsText(Result.Value, '(*I ') then
        Break;
    end;
  until (Result = nil) or not (Result.Kind in [tkComment, tkDirective]);
end;

function TDelphiLexer.NextTokenNoComment(out Token: TToken): Boolean;
begin
  Token := NextTokenNoComment;
  Result := Token <> nil;
end;

function TDelphiLexer.ParseIdentifierName: string;
var
  Token: TToken;
begin
  Result := '';
  Token := CurrentToken;
  if Token <> nil then
  begin
    repeat
      while (Token <> nil) and (Token.Kind in [tkComment, tkDirective]) do
        Token := NextToken;
      if not Token.IsTypeIdent then
        Exit;
      Result := Result + Token.Value;
      Token := NextToken;
      while (Token <> nil) and (Token.Kind in [tkComment, tkDirective]) do
        Token := NextToken;

      if Token.Kind <> tkQualifier then
        Break;
      Result := Result + '.';
    until not NextTokenNoComment(Token);
  end;
  // leave: token after last identifier part
end;

function TDelphiLexer.PreTokenOf(Token: TToken): TToken;
begin
  Result := nil;
  if Token.TokenIndex > 0 then
    Result := Tokens[Token.TokenIndex - 1];
end;

function TDelphiLexer.PreTokenNoCommentOf(Token: TToken): TToken;
begin
  Result := Token;
  while Result.TokenIndex > 0 do
  begin
    Result := Tokens[Result.TokenIndex - 1];
    if Result.Kind <> tkComment then
      Exit;
  end;
  Result := nil;
end;

function TDelphiLexer.NextTokenOf(Token: TToken): TToken;
begin
  Result := nil;
  if Token.TokenIndex + 1 < Count then
    Result := Tokens[Token.TokenIndex + 1];
end;

function TDelphiLexer.NextTokenNoCommentOf(Token: TToken): TToken;
begin
  Result := Token;
  while Result.TokenIndex + 1 < Count do
  begin
    Result := Tokens[Result.TokenIndex + 1];
    if Result.Kind <> tkComment then
      Exit;
  end;
  Result := nil;
end;

function TDelphiLexer.GetCurrentToken: TToken;
begin
  if FCurTokenIndex - 1 < FTokens.Count then
    Result := TToken(FTokens[FCurTokenIndex - 1])
  else
    Result := nil;
end;

function TDelphiLexer.GetFullStringNext: string;
var
  Token: TToken;
begin
  Token := CurrentToken;
  if (Token <> nil) and (Token.Kind = tkString) then
  begin
    Result := Token.Value;
    while NextTokenNoComment(Token) and (Token.Kind = tkPlus) do
    begin
      Result := Result + ' + ';
      if NextTokenNoComment(Token) and ((Token.Kind = tkString) or (Token.IsIdent)) then
        Result := Result + Token.Value
      else
        Break;
    end;
  end;
end;

function TDelphiLexer.GetPreviousToken: TToken;
begin
  if (FCurTokenIndex > 0) and (FCurTokenIndex - 1 - 1 < FTokens.Count) then
    Result := TToken(FTokens[FCurTokenIndex - 1 - 1])
  else
    Result := nil;
end;

function TDelphiLexer.GetToken(out Token: TToken): Boolean;
begin
  Token := GetToken;
  Result := Token <> nil;
end;

function TDelphiLexer.GetToken: TToken;
var
  Data, P, F: PChar;
  ch1, ch2: Char;
  S: string;

  IndexAdd: Integer;
  IsDecimal: Boolean;
  IsExp: Boolean;
  IsExpSign: Boolean;
  StartLine, StartColumn, Line, Column: Integer;
  Kind: TTokenKind;
  StartIndex: Integer;
begin
  Result := nil;
  if FIndex > Length(FText) then
    Exit;
  Data := Pointer(Text);
  P := Data + FIndex - 1;

  // skip white chars
  Column := FColumn;
  Line := FLine;
  ch1 := P[0];
  while ch1 in WhiteChars do
  begin
    if ch1 = #10 then
    begin
      Inc(Line);
      Column := 0;
    end;
    Inc(P);
    ch1 := P[0];
    if ch1 <> #13 then
      Inc(Column);
  end;

  if ch1 = #0 then
  begin
    FLine := Line;
    FColumn := Column;
    Exit;
  end;

  StartLine := Line;
  StartColumn := Column;
  StartIndex := P - Data + 1;

  F := P;
  IndexAdd := 0;
  if ch1 = '''' then
  begin
    Inc(P);
    Inc(Column);
    // string
    while True do
    begin
      case P[0] of
        #0:
          Break;
        '''':
          begin
            if P[1] = '''' then
              Inc(P)
            else
              Break;
          end;
        #10, #13:
          begin
            Dec(P);
            Column := 0;
            Break; // line end is string end in pascal
          end;
      end;
      Inc(P);
      Inc(Column);
    end;
    if P[0] <> #0 then
    begin
      Inc(P); // include P[0] which is now P[-1]
      Inc(Column);
    end;
    Kind := tkString;
  end
  else if ch1 = '{' then
  begin
    // comment { ... } -> find comment end
    Inc(P);
    Inc(Column);
    if P[0] = '$' then
    begin
      Kind := tkDirective;
      Inc(P);
      Inc(Column);
    end
    else
      Kind := tkComment;

    while True do
    begin
      case P[0] of
        #0, '}':
          Break;
        #10:
          begin
            Inc(Line);
            Column := 0;
          end;
      end;
      Inc(P);
      if P[0] <> #13 then
        Inc(Column);
    end;

    if P[0] <> #0 then
    begin
      Inc(P); // include P[0] which is now P[-1]
      Inc(Column);
    end;
  end
  else if (ch1 = '(') and (P[1] = '*') then
  begin
    // comment (* ... *) -> find comment end
    Inc(P, 2);
    Inc(Column, 2);
    if P[0] = '$' then
    begin
      Kind := tkDirective;
      Inc(P);
      Inc(Column);
    end
    else
      Kind := tkComment;

    ch1 := P[0];
    while (ch1 <> #0) and not ((ch1 = '*') and (P[1] = ')')) do
    begin
      if ch1 = #10 then
      begin
        Inc(Line);
        Column := 0;
      end;
      Inc(P);
      ch1 := P[0];
      if ch1 <> #13 then
        Inc(Column);
    end;

    if ch1 <> #0 then
    begin
      Inc(P, 2); // include P[0],P[1] which is now P[-2],P[-1]
      Inc(Column, 2);
    end;
  end
  else if (ch1 = '/') and (P[1] = '/') then
  begin
    // comment "// ..." -> find comment end
    Inc(P, 2);
    Inc(Column, 2);
    while not (P[0] in [#0, #10, #13]) do
    begin
      Inc(P);
      Inc(Column);
    end;
    Kind := tkComment;
    if P[0] <> #0 then
    begin
      if P[0] = #13 then
        Inc(IndexAdd); {do not parse the #13 again}
      Inc(Line);
      Inc(IndexAdd); {do not parse the #10 again}
      Column := 1;
    end;
  end
  else if ch1 in IdentFirstChars then
  begin
    // identifier
    Inc(P);
    Inc(Column);
    while P[0] in IdentChars do
    begin
      Inc(P);
      Inc(Column);
    end;
    SetString(S, F, P - F);
    Kind := FindTokenIdentKind(S);
  end
  else if (ch1 = '&') and (P[1] in IdentFirstChars) then
  begin
    // identifier with leading '&'
    Inc(P, 2);
    Inc(Column, 2);
    while P[0] in IdentChars do
    begin
      Inc(P);
      Inc(Column);
    end;
    SetString(S, F, P - F);
    Kind := FindTokenIdentKind(S);
  end
  else if ch1 in NumberChars then
  begin
    // number
    Inc(P);
    Inc(Column);

    IsDecimal := False;
    IsExp := False;
    IsExpSign := False;
    repeat
      case P[0] of
        '0'..'9': ;

        '.':
          begin
            if P[1] = '.' then  // '..' "range symbol"
              Break;
            if IsDecimal or IsExp then
              Break
            else
              IsDecimal := True;
          end;

        '+', '-':
          if not IsExp or IsExpSign then
            Break
          else
            IsExpSign := True;

        'e', 'E':
          if IsDecimal or IsExp then
            Break
          else
            IsExp := True;

      else
        Break;
      end;
      Inc(P);
      Inc(Column);
    until False;
    if IsExp or IsDecimal then
      Kind := tkFloat
    else
      Kind := tkInt;
  end
  else if (ch1 = '$') and (P[1] in HexNumberChars) then
  begin
    // hex number
    Inc(P, 2);
    Inc(Column, 2);
    while P[0] in HexNumberChars do
    begin
      Inc(P);
      Inc(Column);
    end;
    Kind := tkInt;
  end
  else if (ch1 = '#') and ((P[1] = '$') or (P[1] in NumberChars)) then
  begin
    // char
    Inc(P, 2);
    Inc(Column, 2);
    if P[-1] = '$' then
    begin
      while P[0] in HexNumberChars do
      begin
        Inc(P);
        Inc(Column);
      end;
    end
    else
    begin
      while P[0] in NumberChars do
      begin
        Inc(P);
        Inc(Column);
      end;
    end;
    Kind := tkString;
  end
  else
  if (ch1 = '$') and (P[1] = '_') and (P[2] = '_') then // macro support
  begin
    // identifier
    Inc(P, 2);
    Inc(Column, 2);
    while P[0] in IdentChars do
    begin
      Inc(P);
      Inc(Column);
    end;
    SetString(S, F, P - F);
    Kind := tkMacro;
  end
  else {if ch1 in SymbolChars then}
  begin
    Inc(P);
    Inc(Column);
    ch2 := P[0];
    case ch1 of
      ';': Kind := tkSemicolon;
      ',': Kind := tkComma;
      '^': Kind := tkPointer;
      '@': Kind := tkAddr;
      '&': Kind := tkAmp;
      '[': Kind := tkLBracket;
      ']': Kind := tkRBracket;
      '+': Kind := tkPlus;
      '-': Kind := tkMinus;
      '*': Kind := tkMultiply;
      '/': Kind := tkDivide;
      '=': Kind := tkEqual;
      ':': if ch2 = '=' then Kind := tkAssign else Kind := tkColon;
      '<': if ch2 = '=' then
             Kind := tkLessEqualThan
           else if ch2 = '>' then
             Kind := tkNotEqual
           else
             Kind := tkLessThan;
      '>': if ch2 = '=' then Kind := tkGreaterEqualThan else Kind := tkGreaterThan;
      '.': if ch2 = '.' then
             Kind := tkRange
           else if ch2 = ')' then
           begin
             Kind := tkRBracket; // '.)' => ']'
             Inc(P);
             Inc(Column);
           end
           else
             Kind := tkQualifier;
      '(': if ch2 = '.' then
           begin
             Kind := tkLBracket; // '(.' => '['
             Inc(P);
             Inc(Column);
           end
           else
             Kind := tkLParan;
      ')': Kind := tkRParan;
    else
      Kind := tkSymbol;
    end;
    if Kind >= tkSymbolLevel2 then
    begin
      Inc(P);
      Inc(Column);
    end;
  end;
  FIndex := P - Data + 1;
  FLine := Line;
  FColumn := Column;

  SetString(S, F, P - F);
  Result := TToken.Create(FTokens.Count, Kind, S, FFilename, StartLine, StartColumn, StartIndex);
  FTokens.Add(Result);

  Inc(FIndex, IndexAdd); // skip some chars if necessary
end;

initialization
  InitTokenStrings;

finalization
  FreeAndNil(TokenStrings);

end.
