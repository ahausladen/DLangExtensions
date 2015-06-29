unit LexerImplInclude;

interface

uses
  Windows, SysUtils, Classes, StrUtils, DelphiLexer;

type
  ILexerImplInclude = interface
    procedure Parse(Lexer: TDelphiLexer);
    function IncludeFileNeeded: Boolean;
    procedure GenerateIncludeContent(Lines: TStrings);
    function GetIncludeFileName: string;
  end;

procedure FindImplIncludeToken(Lexer: TDelphiLexer; Intf: ILexerImplInclude; const IncludeDirective: string);

implementation

uses
  LanguageExtension;

type
  TString4Array = array[0..3] of string;

function ExtractDLangExtFilename(const Filename: string): string;
begin
  Result := '__DLangExt' + PathDelim + ExtractFileName(Filename);
end;

procedure InsertInclude(Lexer: TDelphiLexer; Token: TToken; Intf: ILexerImplInclude;
  const Directives: TString4Array);
begin
  if (Token.Kind = tkDirective) and
     (SameText(Directives[0], Token.Value) or SameText(Directives[1], Token.Value) or
      SameText(Directives[2], Token.Value) or SameText(Directives[3], Token.Value)) then
  begin
    if not Intf.IncludeFileNeeded  then
      Lexer.DeleteToken(Token)
    else
    begin
      if Token.Value[1] = '{' then
        Lexer.ReplaceToken(Token, '{$INCLUDE ''' + ExtractDLangExtFilename(Intf.GetIncludeFileName) + '''}')
      else
        Lexer.ReplaceToken(Token, '(*$INCLUDE ''' + ExtractDLangExtFilename(Intf.GetIncludeFileName) + '''*)');
    end;
  end
  else
  if Intf.IncludeFileNeeded then
    Lexer.InsertTextAfter(Token, '{$INCLUDE ''' + ExtractDLangExtFilename(Intf.GetIncludeFileName) + '''}');
end;

procedure GenerateIncludeFile(Intf: ILexerImplInclude);
var
  Lines, OldLines: TStrings;
  Exists: Boolean;
  Modified: Boolean;
  IncludeFileName: string;
  I: Integer;
begin
  IncludeFileName := Intf.GetIncludeFileName;

  Exists := FileExists(IncludeFileName);
  if not Intf.IncludeFileNeeded then
  begin

    LanguageExtensionManager.UnregisterVirtualDiskFile(IncludeFileName);
    { Delete obsolete file }
    if Exists then
    begin
      SetFileAttributes(PChar(IncludeFileName), 0);
      DeleteFile(IncludeFileName);
      RemoveDirectory(PChar(ExtractFileDir(IncludeFileName)));
    end;
    Exit;
  end;
  LanguageExtensionManager.RegisterVirtualDiskFile(IncludeFileName);

  Lines := TStringList.Create;
  try
    Intf.GenerateIncludeContent(Lines);


    { Has the file changed. If so save it otherwise do not rewrite the file }
    Modified := True;
    if Exists then
    begin
      OldLines := TStringList.Create;
      try
        OldLines.LoadFromFile(IncludeFileName);
        Modified := Lines.Count <> OldLines.Count;
        if not Modified then
          for I := 0 to Lines.Count - 1 do
            if Lines[I] <> OldLines[I] then
            begin
              Modified := True;
              Break;
            end;
      finally
        OldLines.Free;
      end;
    end;

    if Modified then
    begin
      SetFileAttributes(PChar(IncludeFileName), 0);
      ForceDirectories(ExtractFileDir(IncludeFileName));
      Lines.SaveToFile(IncludeFileName);
      SetFileAttributes(PChar(IncludeFileName), FILE_ATTRIBUTE_READONLY);
    end;
  finally
    Lines.Free;
  end;
end;


procedure FindImplIncludeToken(Lexer: TDelphiLexer; Intf: ILexerImplInclude; const IncludeDirective: string);
var
  Directives: TString4Array;
  Token: TToken;
  IncludeToken: TToken;
  UsesIncludeTokens: array of TToken;
  I: Integer;
  PreToken: TToken;
begin
  Directives[0] := '{$I ' + IncludeDirective + '}';
  Directives[1] := '{$INCLUDE ' + IncludeDirective + '}';
  Directives[2] := '(*$I ' + IncludeDirective + '*)';
  Directives[3] := '(*$INCLUDE ' + IncludeDirective + '*)';
  while Lexer.NextTokenNoComment(Token) do
  begin
    { only operator after "implementation", "progam" or "library" }

    if Token.Kind in [tkI_implementation, tkI_program, tkI_library] then
    begin
      if Token.Kind <> tkI_implementation then
      begin
        { Get the ";" token }
        while Lexer.NextToken(Token) and (Token.Kind <> tkSemicolon) do
          ;
      end;
      IncludeToken := Token; // this token is needed if there is no "uses" token
      PreToken := nil;
      while Lexer.NextToken(Token) and ((Token.Kind = tkComment) or IsEndIfToken(Token)) do
        PreToken := Token;
      if (PreToken <> nil) and (IsEndIfToken(PreToken)) then
        IncludeToken := PreToken;

      while Token <> nil do
      begin
        if Token.Kind = tkI_uses then
        begin
          { "uses" overrides the "implementation/program/library" include insertion point }

          { Get the ";" token }
          while Lexer.NextToken(Token) and (Token.Kind <> tkSemicolon) do
            ;
          IncludeToken := Token;
          PreToken := nil;
          while (Lexer.NextToken(Token)) and ((Token.Kind = tkComment) or IsEndIfToken(Token)) do
            PreToken := Token;
          if (PreToken <> nil) and (IsEndIfToken(PreToken)) then
            IncludeToken := PreToken;
          SetLength(UsesIncludeTokens, Length(UsesIncludeTokens) + 1);
          UsesIncludeTokens[High(UsesIncludeTokens)] := IncludeToken;
        end;

        if (Token.Kind = tkDirective) and
           (SameText(Directives[0], Token.Value) or SameText(Directives[1], Token.Value) or
            SameText(Directives[2], Token.Value) or SameText(Directives[3], Token.Value)) then
        begin
          { The developer gave us a hint where to put the "FOR-IN" include file }
          IncludeToken := Token;
          SetLength(UsesIncludeTokens, 0);
        end
        else
          Intf.Parse(Lexer);

        { Parse next token }
        Token := Lexer.NextToken;
      end;

      { Insert the DIRECTIVE include file at the specified code location }
      if IncludeToken <> nil then
        InsertInclude(Lexer, IncludeToken, Intf, Directives); // removes the {$I DIRECTIVE} if not used

      if Intf.IncludeFileNeeded then
      begin
        for I := High(UsesIncludeTokens) - 1 downto 0 do
          if UsesIncludeTokens[I] <> IncludeToken then
            InsertInclude(Lexer, UsesIncludeTokens[I], Intf, Directives);
      end;

      GenerateIncludeFile(Intf); // create or delete .casestringof.inc file

      Break; // file has been altered.
    end;
  end;
end;


end.
