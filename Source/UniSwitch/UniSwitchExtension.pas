unit UniSwitchExtension;

(*
  UniSwitch - Unicode Switch

  Enabled/Disabled by the switch
  {%ANSISTRINGS ON/ACTIVE/OFF}
     ON    : undefines UNICODE, adds alias types, enables AnsiStrings
     ACTIVE: undefines UNICODE, enables AnsiStrings, CodeInsight and Compiler column info can be wrong
     OFF   : defines UNICODE, disables AnsiStrings

  Action:
    - The switch changes the meaning of "string", "Char" and "PChar" to
      "RawByteString", "AnsiChar" and "PAnsiChar".
    - Constant strings and Chars are typecasted to "RawByteString", "AnsiChar" what forces the
      compiler to use the AnsiString versions of System functions like "Pos", "Insert", "Delete".
      It also prevents implicit type casts.

  Limitations:
    - The {%ANSISTRINGS ON} must be inserted below the "uses" in the "interface" and
      "implementation". Thus it can't be used between "begin" and "end".
      This is because the switch must declare type aliases to keep the column information correct
      {%ANSISTRINGS ACTIVE} doesn't have this limitation.
    - The switch cannot be IFDEFed. IFDEFs are ignored.
    - The switch does not "ansify" your code it just changes the meaning of "string", "Char" and "PChar"
      to "RawByteString", "AnsiChar" and "PAnsiChar". Nothing more, nothing less.
*)

interface

uses
  Windows, AnsiStrings, SysUtils, Classes, Contnrs, LanguageExtension, DelphiLexer;

type
  TUniSwitchExtension = class(TLanguageExtension)
  protected
    procedure AlterLexer(Lexer: TDelphiLexer); override;
  end;

implementation

{ TUniSwitchExtension }

procedure TUniSwitchExtension.AlterLexer(Lexer: TDelphiLexer);
{$IFDEF UNICODE} // Delphi 2009+
var
  Token, Tk: TToken;
  Body: string;
  SubLexer: TDelphiLexer;
  S: string;
  I, Parts: Integer;
  IsCharToken: Boolean;
  Ext: string;
  IsIncludeFile: Boolean;
  AnsiStringEnabled: Boolean;
  AliasesInserted: Boolean;
{$ENDIF UNICODE}
begin
  {$IFDEF UNICODE} // Delphi 2009+
  AnsiStringEnabled := False;
  AliasesInserted := False;
  I := Pos(UTF8String('%'), Lexer.Text);
  if I > 0 then
  begin
    if PosEx(UTF8String('%ANSISTRINGS'), AnsiStrings.UpperCase(RawByteString(Lexer.Text)), I) = 0 then
      Exit; // nothing to do
  end
  else
    Exit; // nothing to do

  Token := Lexer.NextToken;

  Ext := ExtractFileExt(Filename);
  IsIncludeFile := not (SameText(Ext, '.pas') or SameText(Ext, '.dpr'));

  while Token <> nil do
  begin
    if Token.Kind = tkComment then
    begin
      Body := Token.GetCommentBody;
      if AsciiStartsText('%ANSISTRINGS', Body) then
      begin
        SubLexer := TDelphiLexer.Create('', UTF8Encode(Body));
        try
          SubLexer.SupportMacroTokens := Lexer.SupportMacroTokens;
          Tk := SubLexer.NextToken;
          Assert( (Tk <> nil) and (Tk.Kind = tkSymbol) );
          Tk := SubLexer.NextToken;
          if (Tk <> nil) and SameText(Tk.Value, 'ANSISTRINGS') then
          begin
            Tk := SubLexer.NextToken;
            if (Tk <> nil) and Tk.IsIdent then
            begin
              if SameText(Tk.Value, 'ON') then
              begin
                AnsiStringEnabled := True;
                S := '{$UNDEF UNICODE}';
                if not AliasesInserted and not IsIncludeFile then
                begin
                  AliasesInserted := True;
                  S := S + ' type zzzRBS = RawByteString; zzzA = AnsiChar; zzzPA = PAnsiChar;';
                end;
                Lexer.ReplaceToken(Token, S);
                Lexer.NextToken;
              end
              else if SameText(Tk.Value, 'ACTIVE') then
              begin
                AnsiStringEnabled := True;
                Lexer.ReplaceToken(Token, '{$UNDEF UNICODE}');
                Lexer.NextToken;
              end
              else if SameText(Tk.Value, 'OFF') then
              begin
                AnsiStringEnabled := False;
                Lexer.ReplaceToken(Token, '{$DEFINE UNICODE}');
                Lexer.NextToken;
              end;
            end;
          end;
        finally
          SubLexer.Free;
        end;
      end;
    end
    else if AnsiStringEnabled then
    begin
      if (Token.Kind = tkString) and not Manager.IsCodeInsight then
      begin
        IsCharToken := False;
        Parts := Lexer.GetCombinedStringConstantNext(S);
        if Parts = 1 then
        begin
          if S[1] = '#' then
            IsCharToken := True
          else if S[1] = '''' then
          begin
            if Length(S) = 3 then
              IsCharToken := True;
          end;
        end;

        if Parts > 0 then
          Lexer.RewindLastToken; // back to my token

        if IsCharToken then
          Lexer.ReplaceText(Token.Index, Lexer.CurrentToken.EndIndex - Token.Index + 1,
                            'AnsiChar(' + S + ')')
        else
          Lexer.ReplaceText(Token.Index, Lexer.CurrentToken.EndIndex - Token.Index + 1,
                           'RawByteString(' + S + ')');

        Lexer.NextToken; // 'RawByteString' / 'AnsiChar'
        Lexer.NextToken; // '('
        Lexer.NextToken; //  Token.Value
        Lexer.GetCombinedStringConstantNext(S); // ')'
      end
      else if Token.IsIdent then
      begin
        if Token.Kind = tkI_string then
        begin
          Tk := Lexer.PreTokenNoCommentOf(Token);
          if (Tk = nil) or (Tk.Kind <> tkColon) or (Lexer.LookAheadNoComment <> nil) and (Lexer.LookAheadNoComment.Kind <> tkLBracket) then
          begin // do not replace ": string[12]"
            if AliasesInserted then
              Lexer.ReplaceToken(Token, 'zzzRBS')
            else
              Lexer.ReplaceToken(Token, 'RawByteString');
          end;
        end
        else if Token.Kind = tkIdent then
        begin
          if SameText(Token.Value, 'Char') then
          begin
            if AliasesInserted then
              Lexer.ReplaceToken(Token, 'zzzA')
            else
              Lexer.ReplaceToken(Token, 'AnsiChar')
          end
          else
          if SameText(Token.Value, 'PChar') then
          begin
            if AliasesInserted then
              Lexer.ReplaceToken(Token, 'zzzPA')
            else
              Lexer.ReplaceToken(Token, 'PAnsiChar')
          end;
        end;
      end;
    end;
    Token := Lexer.NextToken;
  end;
  {$ENDIF UNICODE}
end;

end.
