unit DelphiParserHelper;

interface

uses
  SysUtils, Classes, Contnrs, DelphiLexer;

type
  TParserHelper = class(TObject)
    procedure ParseIf(Lexer: TDelphiLexer); virtual;
    procedure ParseWhile(Lexer: TDelphiLexer); virtual;
    procedure ParseRepeat(Lexer: TDelphiLexer); virtual;
    procedure ParseFor(Lexer: TDelphiLexer); virtual;
    procedure ParseTry(Lexer: TDelphiLexer); virtual;
    procedure ParseStatement(Lexer: TDelphiLexer); virtual;
    procedure ParseInnerBlockStatement(Lexer: TDelphiLexer); virtual;
    procedure ParseBlockStatement(Lexer: TDelphiLexer); virtual;
    procedure ParseAsmBlockStatement(Lexer: TDelphiLexer); virtual;
    procedure ParseCaseOfBody(Lexer: TDelphiLexer); virtual;
    procedure ParseCaseOfExpression(Lexer: TDelphiLexer); virtual;
    procedure ParseCaseOf(Lexer: TDelphiLexer); virtual;
  end;

implementation

{ TParserHelper }

procedure TParserHelper.ParseCaseOf(Lexer: TDelphiLexer);
// case-of ::= "case" <case-of-expression> "of" <case-of-body> "end"
begin

end;

procedure TParserHelper.ParseCaseOfExpression(Lexer: TDelphiLexer);
// case-of-expression ::= <expression>
begin

end;

procedure TParserHelper.ParseCaseOfBody(Lexer: TDelphiLexer);
// case-of-body ::= <constant> {"," <constant>}* ":" <block-statement>
//              ::= "else" <inner-block-statement>
begin

end;

procedure TParserHelper.ParseAsmBlockStatement(Lexer: TDelphiLexer);
// asm-block-statement ::= "asm" {<asm-statement>}* "end"
begin

end;

procedure TParserHelper.ParseBlockStatement(Lexer: TDelphiLexer);
// block-statement ::= "begin" <inner-block-statement> "end"
//                 ::= <statement>
begin

end;

procedure TParserHelper.ParseInnerBlockStatement(Lexer: TDelphiLexer);
// inner-block-statement ::= [ "" | <statement>] {";" <statement>}* [ "" | ";" ]
//                       ::= <block-statement>
begin

end;

procedure TParserHelper.ParseStatement(Lexer: TDelphiLexer);
// statement ::= [ <if> | <while> | <repeat> | <for> | <case-of> | <try> ]
//           ::= ";"
//           ::= <function-call>
//           ::= <assignment>
begin

end;

procedure TParserHelper.ParseTry(Lexer: TDelphiLexer);
// try ::= "try" <inner-block-statement> "finally" <inner-block-statement> "end"
//     ::= "try" <inner-block-statement> "expect" [{<on-except-block>}* | <inner-block-statement>] "end"
begin

end;

procedure TParserHelper.ParseWhile(Lexer: TDelphiLexer);
// while ::= "while" <bool-expression> "do" <block-statement>
begin

end;

procedure TParserHelper.ParseFor(Lexer: TDelphiLexer);
// for ::= "for" <variable> ":=" <expression> ["to" | "downto"] <expression> "do" <block-statement>
begin

end;

procedure TParserHelper.ParseIf(Lexer: TDelphiLexer);
// if ::= "if" <bool-expression> "then" <block-expression
//    ::= "if" <bool-expression> "then" <block-expression> "else" <block-expression> "end"
begin

end;

procedure TParserHelper.ParseRepeat(Lexer: TDelphiLexer);
// while ::= "repeat" <inner-block-statement> "until" <bool-expression>
begin

end;

end.
