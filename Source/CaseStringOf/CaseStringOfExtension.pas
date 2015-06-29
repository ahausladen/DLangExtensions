unit CaseStringOfExtension;

{ case-string-of Extension

The case-string-of extension introduces a case-of that uses strings instead of ordinal-value.

Internally the case-string-of uses a hashtable.

  case string/UnicodeString/AnsiString/RawByteString/UTF8String/WideString VarName of
       /  string/UnicodeString/AnsiString/RawByteString/UTF8String/WideString(VarName) of
    'str1': ;
    'str2': ;
    ConstantStr: ;
    Variable: ;
    string/UnicodeString/AnsiString/RawByteString/UTF8String/WideString('str3'):
  else
  end;

The case-string-of extension automatically generates an include file. This file is included
immediatelly after the implementation keyword or after the implementation's uses list. It is possible
to move the automatically inserted $INCLUDE by using the "$INCLUDE CASE-STRING-OF" compiler directive.
The "$INCLUDE CASE-STRING-OF" meight be also necessary if the implementation's uses list contains
a complicated $IFDEF/$IF condition.

Limitations:
  * Only literal strings and contants are allowed.
  * constants or string concatinations makes the case-string-of slower because no hash table
    can be used.
  * The comparision is case sensitive.
}

interface

{$R CaseStringOfExtension.res CaseStringOfExtension.rc}

uses
  Windows, SysUtils, Classes, Contnrs, LanguageExtension, DelphiLexer, LexerImplInclude,
  WideStrUtils;

type
  TCaseStringOf = class;

  TStringType = (stUnicodeString, stAnsiString, stWideString);

  TCaseStringOfExtension = class(TLanguageExtension, ILexerImplInclude)
  private
    FIncludeFileName: string;
    FCaseStringOfs: TObjectList;
    procedure ParseCaseOf(Lexer: TDelphiLexer);
    procedure ParseCaseOfBody(Item: TCaseStringOf; Lexer: TDelphiLexer);
    procedure ParseCaseList(Item: TCaseStringOf; Lexer: TDelphiLexer);
    procedure ParseTry(Lexer: TDelphiLexer);
    procedure ParseCodeBlock(Lexer: TDelphiLexer);
    procedure ParseInnerBlockStatement(Lexer: TDelphiLexer; const EndKinds: TTokenKindSet);
    procedure ParseStatement(Lexer: TDelphiLexer; const EndKinds: TTokenKindSet);
    procedure ParseExpression(Lexer: TDelphiLexer; const EndKinds: TTokenKindSet);
  protected
    procedure AlterLexer(Lexer: TDelphiLexer); override;

    procedure Parse(Lexer: TDelphiLexer);
    function IncludeFileNeeded: Boolean;
    procedure GenerateIncludeContent(Lines: TStrings);
    function GetIncludeFileName: string;
  public
    procedure Initialize; override;
    destructor Destroy; override;
    property IncludeFileName: string read FIncludeFileName;
  end;

  TCaseStringOfItem = class(TObject)
  private
    FValue: string;
    FStartToken: TToken;
    FEndToken: TToken;
    FHash: LongWord;
    FIndex: Integer;
    FSimpleToken: Boolean;
  public
    constructor Create(const AValue: string; AStartToken, AEndToken: TToken; ASimpleToken: Boolean);
    procedure ReplaceTokenByIndex(Lexer: TDelphiLexer);

    property Value: string read FValue;
    property StartToken: TToken read FStartToken;
    property EndToken: TToken read FEndToken;
    property SimpleToken: Boolean read FSimpleToken;
    property Hash: LongWord read FHash write FHash;
    property Index: Integer read FIndex write FIndex;
  end;

  TCaseStringOf = class(TObject)
  private
    FList: TObjectList;
    FHelperFunction: string;
    FItems: TObjectList;
    FStringType: TStringType;
    function GetCount: Integer;
    function GetItem(Index: Integer): TCaseStringOfItem;
  public
    constructor Create(AList: TObjectList; AStringType: TStringType);
    destructor Destroy; override;
    procedure GenerateHash(Lexer: TDelphiLexer; StringToken, OfToken: TToken);
    procedure GenerateIncludeCode(Lines: TStrings);

    procedure Add(Lexer: TDelphiLexer; AStartToken, AEndToken: TToken; ASimpleToken: Boolean);
    procedure Clear;
    function IndexOf(const Value: string): Integer;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TCaseStringOfItem read GetItem; default;

    property HelperFunction: string read FHelperFunction;
    property StringType: TStringType read FStringType;
  end;

implementation

uses Utils, AnsiStrings, StrUtils;

{ TCaseStringOfExtension }

procedure TCaseStringOfExtension.Initialize;
begin
  inherited Initialize;
  FIncludeFileName := MakeDLangExtFilename(Filename) + '.casestringof.inc';
  FCaseStringOfs := TObjectList.Create;
end;

destructor TCaseStringOfExtension.Destroy;
begin
  FCaseStringOfs.Free;
  inherited Destroy;
end;

function TCaseStringOfExtension.GetIncludeFileName: string;
begin
  Result := IncludeFileName;
end;

function TCaseStringOfExtension.IncludeFileNeeded: Boolean;
begin
  Result := FCaseStringOfs.Count > 0;
end;

procedure TCaseStringOfExtension.AlterLexer(Lexer: TDelphiLexer);
begin
  { Include files aren't supported }
  if not SameText(ExtractFileExt(Lexer.Filename), '.pas') then
    Exit;

  FindImplIncludeToken(Lexer, Self, 'CASE-STRING-OF');
end;

procedure TCaseStringOfExtension.ParseTry(Lexer: TDelphiLexer);
// try ::= "try" <inner-block-statement> "finally" <inner-block-statement> "end"
//     ::= "try" <inner-block-statement> "expect" [{<on-except-block>}* | <inner-block-statement>] "end"
begin
  // Enter "try"
  Lexer.NextTokenNoComment;
  ParseInnerBlockStatement(Lexer, [tkI_end, tkI_finally, tkI_except]);
  Lexer.NextTokenNoComment;
  ParseInnerBlockStatement(Lexer, [tkI_end]);
  // Leave "end"
end;

procedure TCaseStringOfExtension.ParseCodeBlock(Lexer: TDelphiLexer);
// block-statement ::= "begin" <inner-block-statement> "end"
//                 ::= <statement>
var
  Token: TToken;
begin
  // Enter ["begin" | "asm" | ... ]
  Token := Lexer.CurrentToken;

  case Token.Kind of
    tkI_begin, tkI_asm:
      begin
        Lexer.NextTokenNoComment;
        ParseInnerBlockStatement(Lexer, [tkI_end]);
      end;
  else
    ParseStatement(Lexer, [tkSemicolon, tkI_end]);
    Token := Lexer.CurrentToken;
    if (Token <> nil) and (Token.Kind = tkI_end) then
    begin
      Lexer.RewindLastToken; // handle the " begin statement end" where the semicolon is missing
      Token := Lexer.CurrentToken;
    end;

    // return token before ";"
    if (Token <> nil) and (Token.Kind = tkSemicolon) then
      Lexer.RewindLastToken;
  end;
  // Leave: "end" | token before ";"
end;

procedure TCaseStringOfExtension.ParseInnerBlockStatement(Lexer: TDelphiLexer; const EndKinds: TTokenKindSet);
// inner-block-statement ::= [ "" | <statement>] {";" <statement>}* [ "" | ";" ]
//                       ::= <block-statement>
var
  Token: TToken;
begin
  Token := Lexer.CurrentToken;
  if Token <> nil then
  begin
    case Token.Kind of
      tkI_begin, tkI_asm:
        ParseCodeBlock(Lexer);
    else
      while (Token <> nil) and not (Token.Kind in EndKinds) do
      begin
        ParseStatement(Lexer, EndKinds);
        Token := Lexer.CurrentToken;
      end;
    end;
  end;
  // Leave: EndKinds
end;

procedure TCaseStringOfExtension.ParseStatement(Lexer: TDelphiLexer; const EndKinds: TTokenKindSet);
// statement ::= [ <if> | <while> | <repeat> | <for> | <case-of> | <try> ]
//           ::= ";"
//           ::= <function-call>
//           ::= <assignment>
var
  Token: TToken;
begin
  Token := Lexer.CurrentToken;
  while (Token <> nil) and not (Token.Kind in EndKinds) do
  begin
    case Token.Kind of
      tkI_case:
        ParseCaseOf(Lexer);
      tkI_try:
        ParseTry(Lexer);

      // shortcut for "for", "while", "with"
      tkI_begin, tkI_asm:
        ParseCodeBlock(Lexer);
    end;
    Token := Lexer.NextTokenNoComment;
  end;
  // Leave: EndKinds | ";"
end;

procedure TCaseStringOfExtension.ParseExpression(Lexer: TDelphiLexer; const EndKinds: TTokenKindSet);
var
  Token: TToken;
begin
  Token := Lexer.CurrentToken;
  while (Token <> nil) and not (Token.Kind in EndKinds) do
    Token := Lexer.NextTokenNoComment;
  // Leave: EndKinds
end;

procedure TCaseStringOfExtension.ParseCaseList(Item: TCaseStringOf; Lexer: TDelphiLexer);
var
  Token: TToken;
  LookToken: TToken;
begin
  { <Addon> }
  repeat
    Token := Lexer.CurrentToken;
    LookToken := Lexer.LookAheadNoComment;
    if LookToken <> nil then
    begin
      if Token.Kind = tkString then
      begin
        if not (LookToken.Kind in [tkComma, tkColon]) then
        begin
          Lexer.GetFullStringNext;
          if Item <> nil then
            Item.Add(Lexer, Token, Lexer.PreviousToken, False);
        end
        else
        begin
          if Item <> nil then
            Item.Add(Lexer, Token, Token, True);
          Lexer.NextTokenNoComment;
        end;
      end
      else if Token.IsIdent then
      begin
        while (LookToken <> nil) and not (LookToken.Kind in [tkComma, tkColon]) do
          LookToken := Lexer.NextTokenNoComment;
        LookToken := Lexer.PreviousToken;
        if LookToken <> nil then
        begin
          if Item <> nil then
            Item.Add(Lexer, Token, LookToken, False);
        end;
      end
      else if Token.Kind = tkI_else then
        Exit
      else
        Break;
      Token := Lexer.CurrentToken;
    end
    else
      Break;
    if (Token = nil) or not (Token.Kind in [tkComma, tkColon]) then
      Break;
    if Token.Kind = tkColon then
      Exit;
    Lexer.NextTokenNoComment;
  until False;

  if Item <> nil then
    Item.Clear;
  { </Addon> }

  ParseExpression(Lexer, [tkColon]);
  // Leave: ":"
end;

procedure TCaseStringOfExtension.ParseCaseOf(Lexer: TDelphiLexer);
// case-of ::= "case" <case-of-expression> "of" <case-of-body> "end"
var
  Token, StringToken, OfToken: TToken;
  Item: TCaseStringOf;
begin
  // Enter "case"
  Token := Lexer.NextTokenNoComment;
  if Token = nil then
    Exit;

  { <Addon> }
  Item := nil;
  StringToken := nil;
  OfToken := nil;
  if (Token.Kind = tkI_string) or
     ((Token.Kind = tkIdent) and
      (SameText(Token.Value, 'AnsiString') or
       SameText(Token.Value, 'UnicodeString') or
       SameText(Token.Value, 'RawByteString') or SameText(Token.Value, 'zzzRBS') or
       SameText(Token.Value, 'UTF8String') or
       SameText(Token.Value, 'WideString'))) then
  begin
    if Token.Kind = tkI_string then
      {$IFDEF UNICODE}
      Item := TCaseStringOf.Create(FCaseStringOfs, stUnicodeString)
      {$ELSE}
      Item := TCaseStringOf.Create(FCaseStringOfs, stAnsiString)
      {$ENDIF UNICODE}
    else
    if SameText(Token.Value, 'UnicodeString') then
      Item := TCaseStringOf.Create(FCaseStringOfs, stUnicodeString)
    else
    if SameText(Token.Value, 'WideString') then
      Item := TCaseStringOf.Create(FCaseStringOfs, stWideString)
    else
      Item := TCaseStringOf.Create(FCaseStringOfs, stAnsiString);

    StringToken := Token;
  end;
  { </Addon> }

  ParseExpression(Lexer, [tkI_of]);

  { <Addon> }
  Token := Lexer.CurrentToken;
  if (Token <> nil) and (Item <> nil) then
    OfToken := Token;
  { </Addon> }

  Token := Lexer.NextTokenNoComment;
  if Token <> nil then
    ParseCaseOfBody(Item, Lexer);

  { <Addon> }
  if Item <> nil then
  begin
    if Item.Count = 0 then
      Item.Free
    else
      Item.GenerateHash(Lexer, StringToken, OfToken); // replaces the case-tokens
  end
  { </Addon> }
  // Leave: "end"
end;

procedure TCaseStringOfExtension.ParseCaseOfBody(Item: TCaseStringOf; Lexer: TDelphiLexer);
// case-of-body ::= {<constant> {"," <constant>}* ":" <block-statement> ";"}*
//              ::= "else" <inner-block-statement>
var
  Token: TToken;
begin
  Token := Lexer.CurrentToken;
  while (Token <> nil) and not (Token.Kind in [tkI_else, tkI_end]) do
  begin
    ParseCaseList(Item, Lexer);
    Lexer.NextTokenNoComment;

    { <Addon> }
    if (Item <> nil) and (Item.Count = 0) then
      Item := nil;
    { </Addon> }

    ParseCodeBlock(Lexer);
    Token := Lexer.NextTokenNoComment;
    if (Token = nil) or (Token.Kind <> tkSemicolon) then
      Break;
    Token := Lexer.NextTokenNoComment;
  end;

  if (Token <> nil) and (Token.Kind = tkI_else) then
  begin
    Lexer.NextTokenNoComment;
    ParseInnerBlockStatement(Lexer, [tkI_end]);
  end;

  // Leave: "end"
end;

procedure TCaseStringOfExtension.Parse(Lexer: TDelphiLexer);
var
  Token: TToken;
begin
  Token := Lexer.CurrentToken;
  if (Token = nil) or (Token.Kind <> tkI_case) then
    Exit;

  ParseCaseOf(Lexer);
end;

procedure TCaseStringOfExtension.GenerateIncludeContent(Lines: TStrings);
var
  I: Integer;
  Item: TCaseStringOf;
  Stream: TResourceStream;
begin
  { Read base class code from the resource }
  Stream := TResourceStream.Create(HInstance, 'CaseStringOfFunctions', RT_RCDATA);
  try
    Lines.LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
  Lines.Insert(0, '{$IFNDEF __zzCaseStringOf_Include_Protection__}');
  Lines.Insert(1, '{$DEFINE __zzCaseStringOf_Include_Protection__}');
  Lines.Insert(2, '');

  Lines.Add('');
  Lines.Add('{=============================================================================}');
  Lines.Add('');

  for I := 0 to FCaseStringOfs.Count - 1 do
  begin
    Item := TCaseStringOf(FCaseStringOfs[I]);

    Item.GenerateIncludeCode(Lines);
    Lines.Add('');
    Lines.Add('{-----------------------------------------------------------------------------}');
    Lines.Add('');
  end;
  Lines.Add('{$ENDIF __zzCaseStringOf_Include_Protection__}');
end;

{ TCaseStringOf }

procedure TCaseStringOf.Clear;
begin
  FItems.Clear;
end;

constructor TCaseStringOf.Create(AList: TObjectList; AStringType: TStringType);
var
  Index: Integer;
begin
  inherited Create;
  FList := AList;
  FStringType := AStringType;
  FItems := TObjectList.Create;
  Index := FList.Add(Self);
  FHelperFunction := 'zzCaseStringOf_' + IntToStr(Index);
end;

destructor TCaseStringOf.Destroy;
begin
  FList.Extract(Self);
  FItems.Free;
  inherited Destroy;
end;

function TCaseStringOf.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TCaseStringOf.GetItem(Index: Integer): TCaseStringOfItem;
begin
  Result := TCaseStringOfItem(FItems[Index]);
end;

function TCaseStringOf.IndexOf(const Value: string): Integer;
begin
  for Result := 0 to Count - 1 do
    if Items[Result].Value = Value then
      Exit;
  Result := -1;
end;

procedure TCaseStringOf.Add(Lexer: TDelphiLexer; AStartToken, AEndToken: TToken; ASimpleToken: Boolean);
var
  S: UTF8String;
begin
  S := Copy(Lexer.Text, AStartToken.Index, AEndToken.EndIndex - AStartToken.Index + 1);
  FItems.Add(TCaseStringOfItem.Create(UTF8ToString(S), AStartToken, AEndToken, ASimpleToken));
end;

procedure TCaseStringOf.GenerateHash(Lexer: TDelphiLexer; StringToken, OfToken: TToken);

  // KEEP IN SYNC with case-string-of resource
  function HashStringW(const S: UnicodeString): Cardinal;
  var
    I: Integer;
  begin
    Result := Length(S);
    if Result > 0 then
      for I := 0 to Length(S) - 1 do
        Inc(Result, Ord(S[I + 1]));
  end;

  function HashStringA(const S: RawByteString): Cardinal;
  var
    I: Integer;
  begin
    Result := Length(S);
    if Result > 0 then
      for I := 0 to Length(S) - 1 do
        Inc(Result, Ord(S[I + 1]));
  end;

  function SortCompare(Item1, Item2: TCaseStringOfItem): Integer;
  begin
    if Item1.SimpleToken and Item2.SimpleToken then
      Result := Item1.Hash - Item2.Hash
    else
    if Item1.SimpleToken then
      Result := -1
    else if Item2.SimpleToken then
      Result := 1
    else
      Result := 0;
  end;

var
  I: Integer;
  List: TList;
  StrList: string;
  HasSimple, HasComplex: Boolean;
begin
  List := TList.Create;
  try
    HasSimple := False;
    HasComplex := False;
    for I := 0 to Count - 1 do
    begin
      List.Add(Items[I]);
      if Items[I].SimpleToken then
      begin
        HasSimple := True;
        if StringType = stAnsiString then
          Items[I].Hash := HashStringA(AnsiStrings.AnsiDequotedStr(AnsiString(Items[I].Value), ''''))
        else
          Items[I].Hash := HashStringW(SysUtils.AnsiDequotedStr(Items[I].Value, ''''))
      end
      else
        HasComplex := True;
    end;
    if HasSimple then
      FItems.Sort(@SortCompare);

    { Set Index and find duplicates }
    for I := 0 to Count - 1 do
      Items[I].Index := IndexOf(Items[I].Value);

    { Replace tokens by the index (revers declaration order is required => List[] instead of Items[]) }
    for I := List.Count - 1 downto 0 do
      TCaseStringOfItem(List[I]).ReplaceTokenByIndex(Lexer);
  finally
    List.Free;
  end;

  if HasComplex then
  begin
    { Append the constants and concatinations by the open string array }
    StrList := '';
    for I := 0 to Count - 1 do
    begin
      if not Items[I].SimpleToken then
      begin
        if StrList <> '' then
          StrList := StrList + ', ' + RemoveLineBreaks(Items[I].Value)
        else
          StrList := RemoveLineBreaks(Items[I].Value);
      end;
    end;
    StrList := ', [' + StrList + ']';
    Lexer.ReplaceToken(OfToken, StrList + ') of');
  end
  else
    Lexer.ReplaceToken(OfToken, ') of');

  if HasSimple then
    Lexer.ReplaceToken(StringToken, HelperFunction + '(')
  else
  begin
    case StringType of
      stUnicodeString:
        Lexer.ReplaceToken(StringToken, 'zzCaseStringOfVariable(');
      stAnsiString:
        Lexer.ReplaceToken(StringToken, 'zzCaseStringOfVariableA(');
      stWideString:
        Lexer.ReplaceToken(StringToken, 'zzCaseStringOfVariableW(');
    end;
  end;

  Lexer.RewindLastToken; // start parsing from "case" in the next parser-iteration
end;

procedure TCaseStringOf.GenerateIncludeCode(Lines: TStrings);
const
  SimpleCountIfSwitch = 3;
var
  I: Integer;
  SimpleCount: Integer;
  ComplexCount: Integer;
  LString: string;
begin
  ComplexCount := 0;
  SimpleCount := 0;
  for I := 0 to Count - 1 do
  begin
    if Items[I].SimpleToken then
      Inc(SimpleCount)
    else
      Inc(ComplexCount);
  end;

  if SimpleCount = 0 then
    Exit; // nothing to do, we use the predefined zzCaseStringOfVariable() method

  case StringType of
    stUnicodeString:
      LString := 'UnicodeString';
    stAnsiString:
      LString := 'RawByteString';
    stWideString:
      LString := 'WideString';
  end;

  if ComplexCount > 0 then
    Lines.Add(Format('function %s(const Value: %s; const Values: array of %1:s): Integer;', [HelperFunction, LString]))
  else
    Lines.Add(Format('function %s(const Value: %s): Integer;', [HelperFunction, LString]));

  if SimpleCount > SimpleCountIfSwitch then
  begin
    Lines.Add('type');
    Lines.Add('  zzTCaseStringOfHashItem = record');
    Lines.Add('    Hash: Integer;');
    Lines.Add(Format('    Value: %s;', [LString]));
    Lines.Add('  end;');
    Lines.Add('const');
    Lines.Add('  HashTbl: array[0..' + IntToStr(SimpleCount - 1) + '] of zzTCaseStringOfHashItem = (');
    for I := 0 to SimpleCount - 1 do
    begin
      if I <> SimpleCount - 1 then
        Lines.Add('    (Hash: ' + IntToStr(Items[I].Hash) + '; Value: ' + Items[I].Value + '),')
      else
        Lines.Add('    (Hash: ' + IntToStr(Items[I].Hash) + '; Value: ' + Items[I].Value + ')');
    end;
    Lines.Add('  );');
  end;
  Lines.Add('begin');
  if SimpleCount > SimpleCountIfSwitch then
  begin
    case StringType of
      stUnicodeString:
        Lines.Add('  Result := zzInternalCaseStringOf(Value, @HashTbl, High(HashTbl));');
      stAnsiString:
        Lines.Add('  Result := zzInternalCaseStringOfA(Value, @HashTbl, High(HashTbl));');
      stWideString:
        Lines.Add('  Result := zzInternalCaseStringOfW(Value, @HashTbl, High(HashTbl));');
    end;
  end;

  if (SimpleCount > 0) and (SimpleCount <= SimpleCountIfSwitch) then
  begin
    for I := 0 to SimpleCount - 1 do
    begin
      Lines.Add('  if Value = ' + Items[I].Value + ' then');
      Lines.Add('    Result := ' + IntToStr(i));
      Lines.Add('  else');
    end;
    if ComplexCount = 0 then
      Lines.Add('    Result := -1');
  end;

  if ComplexCount > 0 then
  begin
    if SimpleCount > SimpleCountIfSwitch then
      Lines.Add('  if Result = -1 then');
    Lines.Add('  begin');
    Lines.Add('    for Result := ' + IntToStr(SimpleCount) + ' to High(Values) + ' + IntToStr(SimpleCount) + ' do');
    Lines.Add('      if Values[Result - ' + IntToStr(SimpleCount) + '] = Value then');
    Lines.Add('        Exit;');
    Lines.Add('    Result := -1;');
    Lines.Add('  end;');
  end;
  Lines.Add('end;');
end;

{ TCaseStringOfItem }

constructor TCaseStringOfItem.Create(const AValue: string; AStartToken, AEndToken: TToken; ASimpleToken: Boolean);
begin
  inherited Create;
  FValue := AValue;
  FStartToken := AStartToken;
  FEndToken := AEndToken;
  FSimpleToken := ASimpleToken;
  FIndex := -1;
end;

procedure TCaseStringOfItem.ReplaceTokenByIndex(Lexer: TDelphiLexer);

  function SkipComment(Lexer: TDelphiLexer; Token: TToken): TToken;
  begin
    while (Token <> nil) and (Token.Kind in [tkComment, tkDirective]) do
      Token := Lexer.NextTokenOf(Token);
    Result := Token;
  end;

  function FillString(const S: string; Len: Integer): string;
  var
    OrgLen: Integer;
    I: Integer;
  begin
    Result := S;
    OrgLen := Length(Result);
    if OrgLen < Len then
    begin
      SetLength(Result, Len);
      for I := OrgLen + 1 to Len do
        Result[I] := ' ';
    end;
  end;

var
  StartIndex, Len, DelLen: Integer;
  LineOffset: Integer;
  S: string;
begin
  if (StartToken <> nil) and (FIndex <> -1) then
  begin
    Len := EndToken.EndIndex + 1 - StartToken.Index;
    StartIndex := StartToken.Index;
    DelLen := EndToken.EndIndex + 1 - StartToken.Index;
    LineOffset := EndToken.Line - StartToken.Line;

    Lexer.DeleteText(StartIndex, DelLen);
    // Tokens are now invalid
    FStartToken := nil;
    FEndToken := nil;

    { Insert missing linebreaks }
    if LineOffset > 0 then
    begin
      S := '';
      while LineOffset > 0 do
      begin
        S := S + sLineBreak;
        Dec(LineOffset);
      end;
      Lexer.InsertText(StartIndex, S);
    end;
    Inc(Len, DelLen);
    Lexer.InsertText(StartIndex, FillString(IntToStr(Index), Len));
  end;
end;

end.