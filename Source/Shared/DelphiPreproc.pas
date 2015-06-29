{******************************************************************************}
{*                                                                            *}
{* Delphi Preprocessor                                                        *}
{*                                                                            *}
{* (C) 2005 Andreas Hausladen                                                 *}
{*                                                                            *}
{******************************************************************************}

unit DelphiPreproc;

interface

uses
  SysUtils, Classes, Contnrs, DelphiParserContainers, DelphiLexer;

type
  TValueKind = (vkIsInt, vkIsFloat, vkIsString{, vkIsSet});

  TExprNodeValueRec = record
    Typ: TValueKind;
    ValueInt: Int64;
    ValueFloat: Double;
    ValueStr: string;
    Error: string;
  end;

  TTokenEvent = procedure(Sender: TObject; Token: TToken) of object;
  TIncludeEvent = procedure(Sender: TObject; const Name: string) of object;
  TPreprocessorErrorEvent = procedure(Sender: TObject; const Msg: string; Token: TToken) of object;
  TGetConstBoolValueEvent = procedure(Sender: TObject; const Name: string; var Value: Boolean) of object;
  TGetConstValueEvent = procedure(Sender: TObject; const Name: string; var Kind: TTokenKind; var Value: string) of object;
  TGetDeclaredEvent = procedure(Sender: TObject; const Name: string; var Value: Boolean) of object;

  TTokenParserMethod = procedure(Token: TToken; Lexer: TDelphiLexer) of object;

  EDelphiPreprocessor = class(Exception);

  TTokenParserItem = class(TObject)
  private
    FMethod: TTokenParserMethod;
  public
    constructor Create(AMethod: TTokenParserMethod);
    property Method: TTokenParserMethod read FMethod;
  end;

  TTokenParserList = class(THashtable)
  private
    function GetMethod(const Name: string): TTokenParserMethod;
  public
    procedure Add(const Name: string; Method: TTokenParserMethod);
    property Methods[const Name: string]: TTokenParserMethod read GetMethod; default;
  end;

  TIfdefStackItem = class(TObject)
  private
    FSkip: Boolean;
    procedure SetSkip(const Value: Boolean);
  public
    IsIfdef: Boolean;
    ElseProceeded: Boolean;
    Processed: Boolean;

    property Skip: Boolean read FSkip write SetSkip;
  end;

  TIfdefStack = class(TObject)
  private
    FStack: TStack;
    FSkip: Boolean;
    function GetCount: Integer;
    function GetInElse: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Ifdef(ASkip: Boolean);
    procedure _Else;
    function EndIf: Boolean;
    procedure ElseIf(ASkip: Boolean);
    procedure _If(ASkip: Boolean);
    function IfEnd: Boolean;

    property Skip: Boolean read FSkip;
    property Count: Integer read GetCount;
    property InElse: Boolean read GetInElse;
  end;

  TDelphiPreprocessor = class(TObject)
  private
    FLexers: TObjectList;
    FCurLexer: TDelphiLexer;
    FTokens: TObjectList;

    FIncludeFiles: TStrings;
    FOnDirective: TTokenEvent;
    FOnComment: TTokenEvent;
    FDefines: THashtable;
    FOnIncludeFile: TIncludeEvent;
    FOnError: TPreprocessorErrorEvent;
    FPreprocToken: TToken;

    FCompilerDirectives: TTokenParserList;
    FIfdefStack: TIfdefStack;
    FOnGetDeclared: TGetDeclaredEvent;
    FOnGetConstBoolValue: TGetConstBoolValueEvent;
    FOnGetConstValue: TGetConstValueEvent;

    function GetCurrentToken: TToken;
    function GetTokenCount: Integer;
    function GetTokenProp(Index: Integer): TToken;
  protected
    procedure InitCompilerDirectives; virtual;
    procedure ProcessDirective(Token: TToken); virtual;
    procedure Error(const Fmt: string; const Args: array of const; Token: TToken);
    function GetConstBoolValue(const Name: string): Boolean; virtual;
    procedure GetConstValue(const Name: string; out Kind: TTokenKind; out Value: string); virtual;
    function GetDeclared(const Name: string): Boolean; virtual;

    property CompilerDirectives: TTokenParserList read FCompilerDirectives;

    procedure ParseDefine(Token: TToken; L: TDelphiLexer);
    procedure ParseUndef(Token: TToken; L: TDelphiLexer);
    procedure ParseIfdef(Token: TToken; L: TDelphiLexer);
    procedure ParseIfndef(Token: TToken; L: TDelphiLexer);
    procedure ParseIfopt(Token: TToken; L: TDelphiLexer);
    procedure ParseEndif(Token: TToken; L: TDelphiLexer);
    procedure ParseElse(Token: TToken; L: TDelphiLexer);
    procedure ParseElseif(Token: TToken; L: TDelphiLexer);
    procedure ParseIf(Token: TToken; L: TDelphiLexer);
    procedure ParseIfEnd(Token: TToken; L: TDelphiLexer);
    procedure ParseInclude(Token: TToken; L: TDelphiLexer);
  public
    constructor Create(const AFilename: string; const Text: UTF8String);
    destructor Destroy; override;

    procedure AddInclude(const Filename: string; const Text: UTF8String); overload;
    procedure Define(const Name: string);
    procedure Undefine(const Name: string);
    function GetToken(AllowComments: Boolean = False): TToken;

    property IncludeFiles: TStrings read FIncludeFiles;
    property Defines: THashtable read FDefines;

    property OnComment: TTokenEvent read FOnComment write FOnComment;
    property OnDirective: TTokenEvent read FOnDirective write FOnDirective;
    property OnIncludeFile: TIncludeEvent read FOnIncludeFile write FOnIncludeFile;
    property OnError: TPreprocessorErrorEvent read FOnError write FOnError;
    property OnGetConstBoolValue: TGetConstBoolValueEvent read FOnGetConstBoolValue write FOnGetConstBoolValue;
    property OnGetConstValue: TGetConstValueEvent read FOnGetConstValue write FOnGetConstValue;
    property OnGetDeclared: TGetDeclaredEvent read FOnGetDeclared write FOnGetDeclared;

    property CurrentToken: TToken read GetCurrentToken;

    property TokenCount: Integer read GetTokenCount;
    property Tokens[Index: Integer]: TToken read GetTokenProp;
  end;

function CompareFileName(const Filename1, Filename2: string): Boolean;

implementation

uses
  DelphiExpr;

resourcestring
  RsIncludeRecursion = 'Include recursion detected in file %s';
  RsUnknownCompilerDirective = 'Unknown compiler directive "%s"';
  RsIdentifierExpected = 'Identifier expected';
  RsFilenameExpected = 'Filename expected';
  RsStringEndMissing = 'String end is missing';
  RsNoIfdefOpen = 'No conditinal block open';
  RsExpected = '%s expected';
  RsMultipleElseNotAllowed = '$ELSE already used';

function CompareFileName(const Filename1, Filename2: string): Boolean;
{$IFDEF MSWINDOWS}
var
  Fn1, Fn2: string;
{$ENDIF MSWINDOWS}
begin
  {$IFDEF LINUX}
  Result := CompareStr(Filename1, Filename2) = 0;
  {$ENDIF LINUX}
  {$IFDEF MSWINDOWS}
  if (Pos('~', Filename1) > 0) or (Pos('~', Filename2) > 0) then
  begin
    Fn1 := ExtractShortPathName(Filename1);
    Fn2 := ExtractShortPathName(Filename2);
    Result := (Fn1 <> '') and (Fn2 <> '') and (CompareText(Fn1, Fn2) = 0);
  end
  else
    Result := CompareText(Fn1, Fn2) = 0;
  {$ENDIF MSWINDOWS}
end;

type
  MatchFail = (mfYes, mfNo);

  TPreprocessorIfParser = class(TObject)
  private
    Look: TToken;
    FPreprocessor: TDelphiPreprocessor;
    FLexer: TDelphiLexer;
    FRescanTokens: TObjectList;
    FRescanIndex: Integer;
  public
    constructor Create(APreprocessor: TDelphiPreprocessor; ALexer: TDelphiLexer);
    destructor Destroy; override;
  private
    function NextCache: TToken; {$IFDEF COMPILER10_UP} inline; {$ENDIF}
    procedure Next;
    procedure Rescan(token: TToken);
    procedure Error(const msg: string);
    function Match(const tokens: array of TTokenKind): Boolean; overload;
    function Match(fail: MatchFail; const tokens: array of TTokenKind): Boolean; overload;
    function Expression: IExprNode;
    function Term: IExprNode;
    function Factor: IExprNode;

    function BoolExpression: IBoolNode;
    function BoolTerm: IBoolNode;
    function BoolNotFactor: IBoolNode;
    function IsRelOp: Boolean;
    function BoolIdent: IBoolNode;
    function RelOp: IBoolNode;
    function BoolFactor: IBoolNode;
  public
    function Parse: Boolean;
  end;

{ TPreprocessorIfParser }

constructor TPreprocessorIfParser.Create(APreprocessor: TDelphiPreprocessor; ALexer: TDelphiLexer);
begin
  inherited Create;
  FPreprocessor := APreprocessor;
  FLexer := ALexer;
  FRescanTokens := TObjectList.Create(False);
end;

destructor TPreprocessorIfParser.Destroy;
begin
  FRescanTokens.Free;
  inherited Destroy;
end;

function TPreprocessorIfParser.NextCache: TToken;
begin
  Result := nil;
  if FRescanIndex < FRescanTokens.Count then
    Result := TToken(FRescanTokens[FRescanIndex]);
  Inc(FRescanIndex);
end;

procedure TPreprocessorIfParser.Next;
begin
  look := NextCache;
  if look = nil then
  begin
    look := FLexer.GetToken;
    FRescanTokens.Add(look);
  end;
end;

procedure TPreprocessorIfParser.Rescan(token: TToken);
begin
  FRescanIndex := FRescanTokens.IndexOf(token);
  if (FRescanIndex = -1) then
    FPreprocessor.Error('Internal error 0100: Cannot rescan token.', [], token);
end;

procedure TPreprocessorIfParser.Error(const msg: string);
begin
  FPreprocessor.Error(msg, [], FPreprocessor.FPreprocToken);
end;

function TPreprocessorIfParser.Match(const tokens: array of TTokenKind): Boolean;
var
  i: Integer;
begin
  if look <> nil then
  begin
    for i := 0 to High(tokens) do
      if look.Kind = tokens[i] then
      begin
        Result := True;
        Exit;
      end;
  end;
  Result := False;
end;

function TPreprocessorIfParser.Match(fail: MatchFail; const tokens: array of TTokenKind): Boolean;
begin
  if fail = mfNo then
  begin
    Result := Match(tokens);
    Exit;
  end;
  if not Match(tokens) then
  begin
    if look <> nil then
      Error(Format('Unexpected token: %s', [Look.Value]))
    else
      Error('Unexpected end of file');
  end;
  Result := True;
end;

(*
	<expression>  ::=  [ "+" | "-" | "not" | € ] <term> { [ "+" | "-" | "or" | "xor" ] <term> }*

  <term>  ::=  <factor> { [ "*" | "/" | "mod" | "and" ] <factor> }*

  <factor>  ::=  <number>  | <ident> | "(" <expression> ")"
*)
function TPreprocessorIfParser.Expression: IExprNode;
var
  op: TToken;
  exp: IExprNode;
  bitNeg: Boolean;
  negative: Boolean;
begin
  negative := False;
  bitNeg := False;
  case look.Kind of
    tkPlus, tkMinus:
      begin
        negative := True;
        Next;
      end;
    tkI_Not:
      begin
        bitNeg := True;
        Next;
      end;
  end;
  exp := Term;
  if negative then
    exp := TExprNodeUnaryMinus.Create(exp);
  if bitNeg then
    exp := TExprNodeBitOpNeg.Create(exp);
  while Match(mfNo, [tkPlus, tkMinus, tkI_Or, tkI_Xor]) do
  begin
    op := look;
    Next;
    exp := TExprNodeBinOp.Create(op.Kind, op.Value, exp, Term);
  end;
  Result := exp;
end;

function TPreprocessorIfParser.Term: IExprNode;
var
  op: TToken;
  t: IExprNode;
begin
  t := Factor;
  while Match(mfNo, [tkMultiply, tkDivide, tkI_Mod, tkI_And]) do
  begin
    op := look;
    Next;
    t := TExprNodeBinOp.Create(op.Kind, op.Value, t, Factor);
  end;
  Result := t;
end;

function TPreprocessorIfParser.Factor: IExprNode;
var
  fact: IExprNode;
  ident: TToken;
  kind: TTokenKind;
  S: string;
begin
  fact := nil;
  Match(mfYes, [tkInt, tkFloat, tkIdent, tkLParan]);

  case look.Kind of
    tkInt, tkFloat:
      fact := TExprNodeConst.Create(look.Kind, look.Value);
    tkIdent:
      begin
        ident := look;
        if SameText(ident.Value, 'True') or SameText(ident.Value, 'False') then
          fact := TExprNodeConst.Create(ident.Kind, ident.Value)
        else
        begin
          if Match(mfNo, [tkLParan]) then
            Error(Format('Call to undefined function: %s', [ident.Value]))
          else
          begin
            FPreprocessor.GetConstValue(ident.Value, Kind, S);
            fact := TExprNodeConst.Create(Kind, S);
          end;
        end;
      end;
    tkLParan:
      begin
        Next();
        fact := Expression();
        Match(mfYes, [tkRParan]);
      end;
  end;
  Next;
  Result := fact;
end;

(*
  <b-expression>  ::=  <b-term> { [ "or" | "xor" ] <b-term> }*

  <b-term>  ::=  <b-not-factor> { "and" <b-not-factor> }*

  <b-not-factor>  ::=  [ "not" | € ] <b-factor>

  <relop>  ::=  "<" | "<=" | ">" | ">=" | "==" | "!="

  <b-factor>  ::=  <number> | true | false | "(" <b-expression> ")"
              ::=  "defined" "(" <ident> ")"
              ::=  "declared" "(" <ident> ")"
              ::=  <ident>
              ::=  <expression> <relop> <expression>
*)
function TPreprocessorIfParser.BoolExpression: IBoolNode;
var
  right: IBoolNode;
  op: TToken;
  left: IBoolNode;
begin
  left := BoolTerm;
  while Match(mfNo, [tkI_Or, tkI_Xor]) do
  begin
    op := look;
    Next;
    right := BoolTerm;
    left := TBoolNodeOp.Create(op.Kind, op.Value, left, right);
  end;
  Result := left;
end;

function TPreprocessorIfParser.BoolTerm: IBoolNode;
var
  right: IBoolNode;
  op: TToken;
  left: IBoolNode;
begin
  left := BoolNotFactor;
  while Match(mfNo, [tkI_And]) do
  begin
    op := look;
    Next;
    right := BoolNotFactor;
    left := TBoolNodeOp.Create(op.Kind, op.Value, left, right);
  end;
  if (look <> nil) then
    Match(mfYes, [tkRParan, tkI_Or, tkI_Not, tkI_Xor]);
  Result := left;
end;

function TPreprocessorIfParser.BoolNotFactor: IBoolNode;
begin
  if Match(mfNo, [tkI_Not]) then
  begin
    Next;
    Result := TBoolNodeNot.Create(BoolFactor);
  end
  else
    Result := BoolFactor;
end;

function TPreprocessorIfParser.IsRelOp: Boolean;
begin
  Result := Match(mfNo, [tkLessThan, tkGreaterThan, tkLessEqualThan,
                         tkGreaterEqualThan, tkEqual, tkNotEqual]);
end;

function TPreprocessorIfParser.BoolIdent: IBoolNode;
var
  node: IBoolNode;
  macro: TToken;
  ident: TToken;
begin
  ident := look;
  if SameText(ident.Value, 'defined') then
  begin
    Next;
    Match(mfNo, [tkLParan]);
    Next;
    macro := look;
    if macro = nil then
      Error('Invalid defined() syntax')
    else
      if (macro.Kind <> tkIdent) then
        Error(Format('defined() requires an identifier: %s', [macro.Value]));
    node := TBoolNodeConst.Create(FPreprocessor.Defines.Contains(macro.Value));
    Next;
    Match(mfYes, [tkRParan]);
    Next;
    Result := node;
  end
  else if SameText(ident.Value, 'declared') then
  begin
    Next;
    Match(mfNo, [tkLParan]);
    Next;
    macro := look;
    if macro = nil then
      Error('Invalid declared() syntax')
    else
      if (macro.Kind <> tkIdent) then
        Error(Format('declared() requires an identifier: %s', [macro.Value]));
    node := TBoolNodeConst.Create(FPreprocessor.GetDeclared(macro.Value));
    Next;
    Match(mfYes, [tkRParan]);
    Next;
    Result := node;
  end
  else
  begin
    Next;
    if SameText(ident.Value, 'True') then
      Result := TBoolNodeConst.Create(True)
    else if SameText(ident.Value, 'false') then
      Result := TBoolNodeConst.Create(False)
    else
      Result := TBoolNodeConst.Create(FPreprocessor.GetConstBoolValue(ident.Value))
  end;
end;

function TPreprocessorIfParser.RelOp: IBoolNode;
var
  exp2: IExprNode;
  Op: TToken;
  exp: IExprNode;
begin
  exp := Expression;
  if IsRelOp then
  begin
    Op := look;
    Next;
    exp2 := Expression;
    Result := TBoolNodeRelOp.Create(Op.Kind, Op.Value, exp, exp2);
  end
  else
    Result := TBoolNodeExp.Create(exp);
end;

function TPreprocessorIfParser.BoolFactor: IBoolNode;
var
  node: IBoolNode;
  lparan, ident: TToken;
begin
  Match(mfYes, [tkInt, tkFloat, tkLParan, tkIdent]);
  case look.Kind of
    tkLParan:
      begin
        lparan := look;
        Next;
        node := BoolExpression;
        Match(mfYes, [tkRParan]);
        Next;
        if IsRelOp then
        begin
          Rescan(lparan);
          Next;
          node := RelOp;
        end;
        Result := node;
      end;

    tkIdent:
      begin
        // it could be a simple ident or a part of an expression
        ident := look;
        Next;
        if IsRelOp or Match(mfNo, [tkPlus, tkMinus, tkI_Or, tkI_Xor,
                                   tkMultiply, tkDivide, tkI_Mod, tkI_And]) then
        begin
          Rescan(ident);
          Next;
          node := RelOp;
        end
        else
        begin
          Rescan(ident);
          Next;
          node := BoolIdent;
        end;
        Result := node;
      end;

    tkInt, tkFloat:
      Result := RelOp;
  else
    Result := nil; // never reached but the compiler does not now that Match raises an exception
  end;
end;

function TPreprocessorIfParser.Parse: Boolean;
var
  b: Boolean;
  bn: IBoolNode;
  boolError: string;
begin
  Next;
  boolError := '';
  bn := BoolExpression;
  b := bn.GetValue(boolError);
  if boolError <> '' then
    Error(boolError);
  Result := b;
end;

{ TTokenParserList }

procedure TTokenParserList.Add(const Name: string; Method: TTokenParserMethod);
begin
  inherited Add(Name, TTokenParserItem.Create(Method));
end;

function TTokenParserList.GetMethod(const Name: string): TTokenParserMethod;
var
  Item: TTokenParserItem;
begin
  Item := TTokenParserItem(Values[Name]);
  if Item <> nil then
    Result := Item.Method
  else
    Result := nil;
end;

{ TTokenParserItem }

constructor TTokenParserItem.Create(AMethod: TTokenParserMethod);
begin
  inherited Create;
  FMethod := AMethod;
end;

{ TDelphiPreprocessor }

constructor TDelphiPreprocessor.Create(const AFilename: string; const Text: UTF8String);
begin
  inherited Create;
  FLexers := TObjectList.Create;
  FLexers.Add(TDelphiLexer.Create(AFilename, Text, False));
  FCurLexer := TDelphiLexer(FLexers[0]);
  FDefines := THashtable.Create(False);
  FIncludeFiles := TStringList.Create;
  FTokens := TObjectList.Create;
  FIfdefStack := TIfdefStack.Create;

  FCompilerDirectives := TTokenParserList.Create(False);
  InitCompilerDirectives;
  Define('CONDITIONALEXPRESSIONS');
end;

destructor TDelphiPreprocessor.Destroy;
begin
  FIfdefStack.Free;
  FCompilerDirectives.Free;
  FIncludeFiles.Free;
  FDefines.Free;
  FLexers.Free;
  FTokens.Free; // from here on all tokens are invalid
  inherited Destroy;
end;

procedure TDelphiPreprocessor.AddInclude(const Filename: string; const Text: UTF8String);
var
  InclCount, i: Integer;
begin
  // detect include recursion
  InclCount := 0;
  for i := 0 to FLexers.Count - 1 do
    if CompareFileName(TDelphiLexer(FLexers[i]).Filename, Filename) then
      Inc(InclCount);
  if InclCount > 50 then
  begin
    Error(RsIncludeRecursion, [ExtractFileName(Filename)], FPreprocToken);
    Exit;
  end;

  FCurLexer := TDelphiLexer.Create(Filename, Text);
  FCurLexer.SupportMacroTokens := TDelphiLexer(FLexers[0]).SupportMacroTokens;
  FLexers.Add(FCurLexer);

  if FIncludeFiles.IndexOf(Filename) < 0 then
    FIncludeFiles.Add(Filename);
end;

function TDelphiPreprocessor.GetCurrentToken: TToken;
begin
  if FTokens.Count > 0 then
    Result := TToken(FTokens[FTokens.Count - 1])
  else
    Result := nil;
end;

procedure TDelphiPreprocessor.Define(const Name: string);
begin
  if not FDefines.Contains(Name) then
    FDefines.Add(Name, nil);
end;

procedure TDelphiPreprocessor.Undefine(const Name: string);
begin
  FDefines.Remove(Name);
end;

procedure TDelphiPreprocessor.InitCompilerDirectives;
begin
  CompilerDirectives.Add('define', ParseDefine);
  CompilerDirectives.Add('undef', ParseUndef);
  CompilerDirectives.Add('ifdef', ParseIfdef);
  CompilerDirectives.Add('ifopt', ParseIfopt);
  CompilerDirectives.Add('ifndef', ParseIfndef);
  CompilerDirectives.Add('endif', ParseEndif);
  CompilerDirectives.Add('else', ParseElse);
  CompilerDirectives.Add('if', ParseIf);
  CompilerDirectives.Add('ifend', ParseIfend);
  CompilerDirectives.Add('elseif', ParseElseif);

  CompilerDirectives.Add('i', ParseInclude);
  CompilerDirectives.Add('include', ParseInclude);
end;

function TDelphiPreprocessor.GetToken(AllowComments: Boolean): TToken;
begin
  if Assigned(FCurLexer) then
  begin
    Result := nil;
    repeat
      Result.Free;
      Result := FCurLexer.GetToken;
      if Result = nil then
      begin
        // lexer has no further tokens, return to previous lexer
        FLexers.Delete(FLexers.Count - 1);
        if FLexers.Count > 0 then
        begin
          FCurLexer := TDelphiLexer(FLexers[FLexers.Count - 1]);
          Continue; // next token
        end
        else
        begin
          FCurLexer := nil;
          // there are no more lexers available
          Break; // ignore FIfdefStack.Skip and return
        end;
      end
      else
      begin
        // evaluate preprocessor tokens and remove comments
        if Result.Kind = tkComment then
        begin
          if Assigned(FOnComment) then
            FOnComment(Self, Result);
          if not AllowComments then
            Continue; // next token
        end
        else if Result.Kind = tkDirective then
        begin
          ProcessDirective(Result);
          Continue; // next token
        end;
      end;

      if not FIfdefStack.Skip then
        Break;
    until False; // for "continue"

    if Result <> nil then
      FTokens.Add(Result);
  end
  else
    Result := nil;
end;

procedure TDelphiPreprocessor.ProcessDirective(Token: TToken);
var
  ps: Integer;
  Opt: string;
  S: string;
  L: TDelphiLexer;
  NameToken: TToken;
  TokenParser: TTokenParserMethod;
begin
  FPreprocToken := Token;
  try
    if Assigned(FOnDirective) then
      FOnDirective(Self, Token);

    S := Token.Value;
    ps := Pos('$', S);
    Opt := Copy(S, ps + 1, Length(S) - ps * 2 + 1);

    L := TDelphiLexer.Create('', UTF8Encode(Opt), True);
    try
      NameToken := L.GetToken;
      if (NameToken = nil) or (NameToken.Kind < tkIdent) then
      begin
        Error(RsUnknownCompilerDirective,[], Token);
        Exit;
      end;

      TokenParser := CompilerDirectives[NameToken.Value];
      if Assigned(TokenParser) then
        TokenParser(Token, L)
      else
        { TODO : reactivate when all directives are parsed };
        //Error(RsUnknownCompilerDirective, [NameToken.Value], Token);
    finally
      L.Free;
    end;

  finally
    FPreprocToken := nil;
  end;
end;

procedure TDelphiPreprocessor.Error(const Fmt: string; const Args: array of const;
  Token: TToken);
begin
  Assert(Token <> nil);
  if Assigned(FOnError) then
    FOnError(Self, Format(Fmt, Args), Token)
  else
    raise EDelphiPreprocessor.CreateFmt('%s(%d): %s',
            [Token.Filename, (Token.Line + 1), Format(Fmt, Args)]);
end;

procedure TDelphiPreprocessor.ParseInclude(Token: TToken; L: TDelphiLexer);
var
  S, Filename: string;
  i: Integer;
  Len: Integer;
begin
  Filename := Trim(Copy(UTF8ToString(L.Text), L.Index, MaxInt));
  if Filename = '' then
    Error(RsFilenameExpected, [], Token);
  Len := Length(Filename);
  if Filename[1] = '''' then
  begin
    S := Filename;
    Filename := '';
    for i := 2 to Len do
      if S[i] = '''' then
      begin
        Filename := Copy(S, 2, i - 2);
        Break;
      end;
    if Filename = '' then
    begin
      Error(RsStringEndMissing, [], Token);
      Exit;
    end;
  end
  else
  begin
    i := 2;
    while (i <= Len) and (Filename[i] > ' ') do
      Inc(i);
    Filename := Copy(Filename, 1, i - 1);
  end;

  if Assigned(FOnIncludeFile) then
    FOnIncludeFile(Self, Filename);
end;

procedure TDelphiPreprocessor.ParseIfEnd(Token: TToken; L: TDelphiLexer);
begin
  if FIfdefStack.Count = 0 then
    Error(RsNoIfdefOpen, [], Token)
  else if not FIfdefStack.IfEnd then
    Error(RsExpected, ['$ENDIF'], Token);
end;

procedure TDelphiPreprocessor.ParseEndif(Token: TToken; L: TDelphiLexer);
begin
  if FIfdefStack.Count = 0 then
    Error(RsNoIfdefOpen, [], Token)
  else if not FIfdefStack.Endif then
    Error(RsExpected, ['$IFEND'], Token);
end;

procedure TDelphiPreprocessor.ParseIf(Token: TToken; L: TDelphiLexer);
var
  p: TPreprocessorIfParser;
begin
  p := TPreprocessorIfParser.Create(Self, L);
  try
    FIfdefStack._If(not p.Parse);
  finally
    p.Free;
  end;
end;

procedure TDelphiPreprocessor.ParseIfdef(Token: TToken; L: TDelphiLexer);
var
  t: TToken;
begin
  t := L.GetToken;
  if (t = nil) or (t.Kind < tkIdent) then
  begin
    Error(RsIdentifierExpected, [], Token);
    FIfdefStack.Ifdef(False); // default to undefined if no exception was raised
    Exit;
  end;

  FIfdefStack.Ifdef(not FDefines.Contains(t.Value));
end;

procedure TDelphiPreprocessor.ParseIfndef(Token: TToken; L: TDelphiLexer);
var
  t: TToken;
begin
  t := L.GetToken;
  if (t = nil) or (t.Kind < tkIdent) then
  begin
    Error(RsIdentifierExpected, [], Token);
    FIfdefStack.Ifdef(True); // default to undefined if no exception was raised
    Exit;
  end;

  FIfdefStack.Ifdef(FDefines.Contains(t.Value));
end;

procedure TDelphiPreprocessor.ParseIfopt(Token: TToken; L: TDelphiLexer);
var
  t: TToken;
begin
  t := L.GetToken;
  if (t = nil) or (t.Kind < tkIdent) then
  begin
    Error(RsIdentifierExpected, [], Token);
    FIfdefStack.Ifdef(False); // default to undefined if no exception was raised
    Exit;
  end;
  // $X+/- ???
  FIfdefStack.Ifdef(FCompilerDirectives.Contains(t.Value));
end;

procedure TDelphiPreprocessor.ParseElse(Token: TToken; L: TDelphiLexer);
begin
  if FIfdefStack.Count = 0 then
    Error(RsNoIfdefOpen, [], Token)
  else if FIfdefStack.InElse then
    Error(RsMultipleElseNotAllowed, [], Token)
  else
    FIfdefStack._Else;
end;

procedure TDelphiPreprocessor.ParseElseif(Token: TToken; L: TDelphiLexer);
var
  p: TPreprocessorIfParser;
begin
  if FIfdefStack.Count = 0 then
    Error(RsNoIfdefOpen, [], Token)
  else
  begin
    p := TPreprocessorIfParser.Create(Self, L);
    try
      FIfdefStack.ElseIf(not p.Parse);
    finally
      p.Free;
    end;
  end;
end;

procedure TDelphiPreprocessor.ParseDefine(Token: TToken; L: TDelphiLexer);
var
  t: TToken;
begin
  t := L.GetToken;
  if (t = nil) or (t.Kind < tkIdent) then
  begin
    Error(RsIdentifierExpected, [], Token);
    Exit;
  end;
  Define(t.Value);
end;

procedure TDelphiPreprocessor.ParseUndef(Token: TToken; L: TDelphiLexer);
var
  t: TToken;
begin
  t := L.GetToken;
  if (t = nil) or (t.Kind < tkIdent) then
  begin
    Error(RsIdentifierExpected, [], Token);
    Exit;
  end;
  Undefine(t.Value);
end;

function TDelphiPreprocessor.GetConstBoolValue(const Name: string): Boolean;
begin
  Result := False;
  if Assigned(FOnGetConstBoolValue) then
    FOnGetConstBoolValue(Self, Name, Result);
end;

function TDelphiPreprocessor.GetDeclared(const Name: string): Boolean;
begin
  Result := False;
  if Assigned(FOnGetDeclared) then
    FOnGetDeclared(Self, Name, Result);
end;

function TDelphiPreprocessor.GetTokenCount: Integer;
begin
  Result := FTokens.Count;
end;

function TDelphiPreprocessor.GetTokenProp(Index: Integer): TToken;
begin
  Result := TToken(FTokens[Index]);
end;

procedure TDelphiPreprocessor.GetConstValue(const Name: string;
  out Kind: TTokenKind; out Value: string);
begin
  Value := '';
  Kind := tkString;
  if Assigned(FOnGetConstValue) then
    FOnGetConstValue(Self, Name, Kind, Value);
end;

{ TIfdefStackItem }

procedure TIfdefStackItem.SetSkip(const Value: Boolean);
begin
  FSkip := Value;
  if not Value then
    Processed := True;
end;

{ TIfdefStack }

constructor TIfdefStack.Create;
begin
  inherited Create;
  FSkip := False;
  FStack := TObjectStack.Create;
end;

destructor TIfdefStack.Destroy;
begin
  while FStack.Count > 0 do // there shouldn't be anything but keep memory leaks low
    TObject(FStack.Pop).Free;
  FStack.Free;
  inherited Destroy;
end;

function TIfdefStack.GetCount: Integer;
begin
  Result := FStack.Count;
end;

procedure TIfdefStack._If(ASkip: Boolean);
var
  Item: TIfdefStackItem;
begin
  Item := TIfdefStackItem.Create;
  Item.IsIfdef := False;
  Item.Skip := ASkip;
  FStack.Push(Item);
  FSkip := ASkip;
end;

function TIfdefStack.IfEnd: Boolean;
begin
  with TIfdefStackItem(FStack.Pop) do
  begin
    Result := not IsIfdef;
    Free;
  end;
  if FStack.Count > 0 then
    FSkip := TIfdefStackItem(FStack.Peek).Skip
  else
    FSkip := False;
end;

procedure TIfdefStack.ElseIf(ASkip: Boolean);
var
  Item: TIfdefStackItem;
begin
  Item := TIfdefStackItem(FStack.Peek);
  //Assert(not Item.ElseProceeded);
  if not Item.Processed then
  begin
    FSkip := ASkip;
    Item.Skip := FSkip;
  end;
  Item.IsIfdef := False; // switch to $IF mode
end;

procedure TIfdefStack.Ifdef(ASkip: Boolean);
var
  Item: TIfdefStackItem;
begin
  Item := TIfdefStackItem.Create;
  Item.IsIfdef := True;
  Item.Skip := ASkip;
  FStack.Push(Item);
  FSkip := ASkip;
end;

procedure TIfdefStack._Else;
var
  Item: TIfdefStackItem;
begin
  Item := TIfdefStackItem(FStack.Peek);
  FSkip := Item.Processed;
  Item.Skip := FSkip;
  Item.ElseProceeded := True;
end;

function TIfdefStack.EndIf: Boolean;
begin
  with TIfdefStackItem(FStack.Pop) do
  begin
    Result := IsIfdef;
    Free;
  end;
  if FStack.Count > 0 then
    FSkip := TIfdefStackItem(FStack.Peek).Skip
  else
    FSkip := False;
end;

function TIfdefStack.GetInElse: Boolean;
begin
  if FStack.Count > 0 then
    Result := TIfdefStackItem(FStack.Peek).ElseProceeded
  else
    Result := False;
end;

end.
