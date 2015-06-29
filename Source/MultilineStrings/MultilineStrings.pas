unit MultilineStrings;

{ Multi line strings:

A multi line string is started by "<<" followed (without a space) by a user defined
terminator token which must not contain a space and is ended by a line break. The next
line is the first multi line string. The multi line string is terminated by a new line
that starts with the terminator token. If this token is indented, the indention is
removed from all multi line string lines. This allows to indent the whole string
without having the indention in the resulting string.
The "#<code>" token has a special meaning in the multi line string. It inlines source
code into the string what is technically just a concatination, so the Lvalue must be
a string. The "#" chars in the string must be escaped by prepanding a "#". Otherwise
it would be treated as inlining code.


Examples:

S := <<MARKER
multi line
string
MARKER;

ShowMessage(<<@@
  multi line
  string
  @@); // this removes the two space indention from the multi line string

S := <<@@ // comments are allowed in this line
##1. I = #< IntToStr(I) >
##2. Value = #< VarToStr(Value) >
@@;
}

interface

uses
  Utf8StringUtils,
  SysUtils, Classes, Contnrs, LanguageExtension;

type
  TMultiLineString = class(TLanguageExtension)
  protected
    function AlterContent(var Content: UTF8String): Boolean; override;
  end;

implementation

const
  MultiLineStringInitator = UTF8String('<<');
  FirstMultiLineStringInitatorChar = '<';
  MultiLineStringInitatorLen = Length(MultiLineStringInitator);

  MultiLineStringLineBreak = '#13#10 +';
  MultiLineStringLineBreakLen = Length(MultiLineStringLineBreak);

function GetStringLine(var P: PAnsiChar; FindEndToken: Boolean = False): UTF8String;
var
  X: PAnsiChar;
  ps: Integer;
begin
  X := P;
  while (X[0] <> #0) and (X[0] <> #10) and (X[0] <> #13) do
    Inc(X);
  SetString(Result, P, X - P);
  if (X[0] = #13) and (X[1] = #10) then
    Inc(X);
  P := X + 1;
  if FindEndToken then
  begin
    ps := Pos(UTF8String('//'), Result);
    if ps = 0 then
    begin
      ps := Pos(AnsiChar('{'), Result);
      if ps = 0 then
        ps := Pos(UTF8String('(*'), Result);
    end;
    if ps > 0 then
      Result := Copy(Result, 1, ps - 1);
    Result := TrimRightA(Result);
  end;
end;

function ConcatBlocks(Blocks: TStringsA): UTF8String;
var
  I, Len: Integer;
begin
  Len := 0;
  for I := 0 to Blocks.Count - 1 do
    Inc(Len, Length(Blocks[I]));
  SetLength(Result, Len);
  Len := 0;
  for I := 0 to Blocks.Count - 1 do
  begin
    if Blocks[I] <> '' then
      Move(Blocks[I][1], Result[Len + 1], Length(Blocks[I]));
    Inc(Len, Length(Blocks[I]));
  end;
end;

function AlterMultiLineStringLine(const S: UTF8String): UTF8String;
var
  I, Len, BraceCount: Integer;
  InStr: Boolean;
begin
  Result := S;
  Len := Length(Result);
  I := 1;
  while I <= Len do
  begin
    case Result[I] of
      '''':
        begin
          Insert('''', Result, I);
          Inc(I);
          Inc(Len);
        end;
      '#':
        begin
          if I < Len - 1 then
          begin
            if Result[I + 1] = '#' then
            begin // escaped #
              Delete(Result, I + 1, 1);
              Dec(Len);
            end
            else if Result[I + 1] = '<' then
            begin
              Result[I] := ''''; // string end
              Result[I + 1] := '+';
              Inc(I, 2);

              BraceCount := 1;
              InStr := False;
              while (BraceCount > 0) and (I <= Len) do
              begin
                case Result[I] of
                  '''':
                    begin
                      if InStr then
                      begin
                        if Result[I + 1] <> '''' then
                          InStr := False
                        else
                          Inc(I);
                      end
                      else
                        InStr := True;
                    end;
                  '<':
                    Inc(BraceCount);
                  '>':
                    Dec(BraceCount);
                end;
                Inc(I);
              end;
              Dec(I);
              if BraceCount = 0 then
              begin
                Result[I] := '+';
                Insert('''', Result, I + 1);
                Inc(I);
                Inc(Len);
              end;
            end;
          end;
        end;
    end;

    Inc(I);
  end;

  Result := '''' + Result + '''';
end;

function IsEndToken(const EndToken: UTF8String; P, LastP: PAnsiChar): PAnsiChar;
var
  Len: Integer;
begin
  Len := Length(EndToken);
  if P + Len < LastP then
  begin
    while (P < LastP) and (P[0] <= ' ') do
      Inc(P);
    if (P + Len < LastP) and (StrLIComp(P, PAnsiChar(EndToken), Len) = 0) then
      Result := P
    else
      Result := nil;
  end
  else
    Result := nil;
end;

function DeleteIndention(const S: UTF8String; Indention: Integer): UTF8String;
var
  I: Integer;
begin
  I := 0;
  if Length(S) > Indention then
  begin
    while (I < Indention) and (S[I + 1] <= ' ') do
      Inc(I);
    if I = 0 then
      Result := S
    else
      Result := Copy(S, I + 1, MaxInt);
  end
  else
    Result := S;
end;

function FindInitiator(P: PAnsiChar): PAnsiChar;
var
  InStr: Boolean;
  Found: Boolean;
  I: Integer;
begin
  InStr := False;
  while True do
  begin
    case P[0] of
      #0:
        begin
          Result := nil;
          Exit;
        end;
      #10:
        InStr := False;
      '''':
        InStr := not InStr;
      '{':
        if not InStr then
        begin
          Inc(P);
          while (P[0] <> #0) and (P[0] <> '}') do
            Inc(P);
        end;
      '(':
        if not InStr and (P[1] = '*') then
        begin
          Inc(P, 2);
          while (P[0] <> #0) and not ((P[0] = '*') and (P[1] = ')')) do
            Inc(P);
        end;
      '/':
        if not InStr and (P[1] = '/') then
        begin
          Inc(P, 2);
          while (P[0] <> #0) and (P[0] <> #10) and (P[0] <> #13) do
            Inc(P);
          while (P[0] = #10) or (P[0] = #13) do
            Inc(P);
        end;
      FirstMultiLineStringInitatorChar:
        begin
          if not InStr then
          begin
            Found := True;
            for I := 1 to MultiLineStringInitatorLen - 1 do
            begin
              if P[I] <> MultiLineStringInitator[I + 1] then
              begin
                Found := False;
                Break;
              end;
            end;
            if Found then
              Break;
          end;
        end;
    end;
    Inc(P);
  end;
  Result := P;
end;

{ TMultiLineString }

function TMultiLineString.AlterContent(var Content: UTF8String): Boolean;
var
  EndToken, S: UTF8String;
  P, Start, F, X: PAnsiChar;
  Blocks, NewLines: TStringsA;
  I: Integer;
begin
  Blocks := TStringListA.Create;
  NewLines := TStringListA.Create;
  try
    P := PAnsiChar(Content);
    Start := P;
    repeat
      P := FindInitiator(P);
      if P <> nil then
      begin
        F := P;
        Inc(P, MultiLineStringInitatorLen);
        EndToken := GetStringLine(P, True);
        if Pos(AnsiChar(' '), EndToken) = 0 then // it is a valid "end token"
        begin
          SetString(S, Start, F - Start);
          Blocks.Add(S); // save unmodified block

          NewLines.Clear;
          NewLines.Add(''); // add first LineBreak
          while P[0] <> #0 do
          begin
            F := P;
            S := GetStringLine(P);
            X := IsEndToken(EndToken, F, P);
            if X <> nil then
            begin
              if X <> F then
              begin
                { Remove indention from the strings }
                for I := 1 to NewLines.Count - 1 do
                  NewLines[I] := DeleteIndention(NewLines[I], X - F);
              end;
              P := X + Length(EndToken);
              Break;
            end;
            NewLines.Add(S);
          end;
          if NewLines.Count > 0 then
          begin
            { Alter multi string lines, skip first LineBreak } 
            for I := 1 to NewLines.Count - 1 - 1 do
              NewLines[I] := AlterMultiLineStringLine(NewLines[I]) + MultiLineStringLineBreak;
            if NewLines.Count > 1 then
              NewLines[NewLines.Count - 1] := AlterMultiLineStringLine(NewLines[NewLines.Count - 1]); // last line has no '#13#10 +'
            Blocks.Add(NewLines.Text);
          end;
          Start := P;
        end;
      end;
    until P = nil;

    if Blocks.Count > 0 then
    begin
      P := PAnsiChar(Content) + Length(Content);
      SetString(S, Start, P - Start);
      Blocks.Add(S); // save unmodified block
      Content := ConcatBlocks(Blocks);
      Result := True;
    end
    else
      Result := False;
  finally
    NewLines.Free;
    Blocks.Free;
  end;
end;

end.
