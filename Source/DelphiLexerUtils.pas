unit DelphiLexerUtils;

interface

uses
  SysUtils, Classes, DelphiLexer, StrUtils;

{ ReadPreviousNamespace parses the previous tokens for the namespace. The returned
  string is either empty or it ends with a '.' }
function ReadPreviousNamespace(Lexer: TDelphiLexer; var Token: TToken): string;

implementation

function ReadPreviousNamespace(Lexer: TDelphiLexer; var Token: TToken): string;
var
  Tk: TToken;
begin
  Assert( Token.Kind = tkQualifier );

  Result := '';
  Tk := Token;
  while Token.Kind = tkQualifier do
  begin
    Token := Lexer.PreTokenNoCommentOf(Token);
    if Token = nil then
      Break;
    if Token.Kind >= tkIdent then
      Result := Token.Value + '.' + Result;
    Tk := Token;
    Token := Lexer.PreTokenNoCommentOf(Token);
  end;
  Token := Tk;
end;


end.
