unit Macros;

{ FileMacros:

This extension supports some file information macros:

$__FILE__      string: full qualified filename of the current file
$__PROJECT__   string: name of the project (dpr/dpk) without path and without suffix
$__LINE__      Integer: current source code line
$__TIME__      TDateTime: compilation timestamp

Exit(expression)     leaves a function and sets Result to expression (Delphi 2009 introduced "Exit(retvalue)"
     The "Exit" is the ResultExit if it is
       - "System" "." "Exit"
       - "Borland" "." "Delphi" "." "System" "." "Exit"
       - ";" "Exit"
       - ":" "Exit"
       - <reserved word/identifier> "Exit"



TODO:
(*$MACROS ON/OFF*)
(*$DEFINE macroname(paramlist) := expression*)
$macroname(params)

}

interface

uses
  SysUtils, Classes, LanguageExtension, DelphiLexer;

type
  TMacros = class(TLanguageExtension)
  private
    FFilename: string;
    class var ProjectName: string;
    class var CompileTime: TDateTime;
  protected
    class procedure CompileProject(const AFilename: string; AIsCodeInsight: Boolean); override;
    procedure AlterLexer(Lexer: TDelphiLexer); override;

    procedure ParseMacro(Lexer: TDelphiLexer; Token: TToken);
    procedure ParseResultExit(Lexer: TDelphiLexer; ExitToken: TToken);
  end;

implementation

uses
  StrUtils, DelphiLexerUtils;

{ TFileMacros }

class procedure TMacros.CompileProject(const AFilename: string; AIsCodeInsight: Boolean);
begin
  inherited CompileProject(AFilename, AIsCodeInsight);
  TMacros.ProjectName := QuotedStr(ChangeFileExt(ExtractFileName(AFilename), ''));
  TMacros.CompileTime := Now;
end;

procedure TMacros.AlterLexer(Lexer: TDelphiLexer);
var
  Token: TToken;
  InAssembler: Boolean;
begin
  inherited;
  FFilename := QuotedStr(Lexer.Filename);

  InAssembler := False;
  while Lexer.NextToken(Token) do
  begin
    if not InAssembler and (Token.Kind = tkI_asm) then
      InAssembler := True
    else if InAssembler and (Token.Kind = tkI_end) then
      InAssembler := False;

    if Token.Kind = tkMacro then
      ParseMacro(Lexer, Token)
    else
    {$IF CompilerVersion < 200}
    // Delphi 2009 introduced "Exit(retvalue)"
    if not InAssembler and (Token.Kind = tkIdent) then
    begin
      if SameText(Token.Value, 'Exit') then
        ParseResultExit(Lexer, Token);
    end;
    {$IFEND}
  end;
end;

procedure TMacros.ParseMacro(Lexer: TDelphiLexer; Token: TToken);
var
  MacroName: string;
begin
  MacroName := UpperCase(Token.Value);
  // default Macro
  if Token.Value = '$__FILE__' then
    Lexer.ReplaceToken(Token, QuotedStr(Filename))
  else if Token.Value = '$__PROJECT__' then
    Lexer.ReplaceToken(Token, QuotedStr(TMacros.ProjectName))
  else if Token.Value = '$__LINE__' then
    Lexer.ReplaceToken(Token, IntToStr(Token.Line))
  else if Token.Value = '$__TIME__' then
    Lexer.ReplaceToken(Token, FloatToStr(TMacros.CompileTime));
end;

procedure TMacros.ParseResultExit(Lexer: TDelphiLexer; ExitToken: TToken);
var
  Token, NamespaceToken: TToken;
  Parans: Integer;
  Namespace: string;
begin
  Token := Lexer.PreTokenNoCommentOf(ExitToken);

  { Obtain used namespace } 
  NamespaceToken := nil;
  Namespace := '';
  if Token <> nil then
  begin
    { The "Exit" is the ResultExit if it is
       - "System" "." "Exit"
       - "Borland" "." "Delphi" "." "System" "." "Exit"
       - ";" "Exit"
       - ":" "Exit"
       - <reserved word/identifier> "Exit"
    }
    if Token.Kind = tkQualifier then
    begin
      NamespaceToken := Token;
      Namespace := ReadPreviousNamespace(Lexer, NamespaceToken);
      if not SameText(Namespace, 'System.') and not SameText(Namespace, 'Borland.Delphi.System.') then
        Exit;
    end
    else if (Token.Kind < tkIdent) and (Token.Kind <> tkSemicolon) and (Token.Kind <> tkColon) then
      Exit;
  end;

  { parse expression }
  Token := Lexer.NextTokenNoComment;
  if Token.Kind = tkLParan then
  begin
    Parans := 1;
    while (Parans > 0) and Lexer.NextTokenNoComment(Token) do
    begin
      case Token.Kind of
        tkMacro:
          ParseMacro(Lexer, Token);
        tkLParan:
          Inc(Parans);
        tkRParan:
          Dec(Parans);
      end;
    end;
    if Parans = 0 then
    begin
      Lexer.InsertTextAfter(Token, '; ' + Namespace + 'Exit; end');
      Lexer.ReplaceToken(ExitToken, 'begin Result := ');
      if NamespaceToken <> nil then
        Lexer.DeleteTokens(NamespaceToken, Lexer.PreTokenNoCommentOf(ExitToken));
    end;
  end
  else
    Lexer.RewindLastToken;
end;

end.
