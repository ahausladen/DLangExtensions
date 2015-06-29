unit ForInExtension;

{ Extended for-in loop.

The extended for-in loop allows you to iterator over an array or every object that has a Count
property and a default array-property. Furthermore it can be used with classes that have a
GetEnumerator method. The for-in iterator variable's scope is limited to the for-in loop.

for-in code		                              Description
for I: DataType in List[] do ;		          List is an instance of a class that has a Count property
                                            and a default array-property (like TList, TStrings).
                                            This is not type safe because it uses a hard typecast to
                                            DataType.

for object C: ClassType in List[] do ;		  List is an instance of a class that has a Count property
                                            and a default array-property (like TList, TStrings).
                                            This is type safe because it uses the as-operator for the
                                            typecast to ClassType.

for I: DataType in array List do ;		      List is a static/dynamic array or a string.
                                            This is not type safe because it uses a hard typecast to
                                            DataType.

for object C: ClassType in array List do ;	List is a static/dynamic array or a string.
                                            This is type safe because it uses the as-operator for the
                                            typecast to ClassType.

for I: DataType in List do ;		            List is an instance of a class that has a GetEnumerator
                                            method.
                                            This is not type safe because it uses a hard typecast to
                                            DataType.

for object C: ClassType in List do ;		    List is an instance of a class that has a GetEnumerator
                                            method.
                                            This is type safe because it uses the as-operator for the
                                            typecast to ClassType.

The for-in extension automatically generates an include file. This file is included immediatelly
after the implementation keyword or after the implementation's uses list. This restricts the usage
of the iterator-types to those that are declared in the interface block or in the imported units.
But it is possible to move the automatically inserted $INCLUDE by using the "$INCLUDE FOR-IN"
compiler directive. The "$INCLUDE FOR-IN" meight be also necessary if the implementation's uses
list contains a complicated $IFDEF/$IF condition.


GetEnumerator:
The GetEnumerator method must return a new instance of a class that has the following methods:

    function GetCurrent: DataType;
    function MoveNext: Boolean;


Limitations:
    * The "for [object] It: DataType in List[]" and the "for [object] It: DataType in array List"
      loops access the List very often. So the List should be a variable an not a function call.
      This does not apply to the GetEnumerator for-in loops.
    * If the DataType is an interface, you should set the iterator variable to NIL after each
      iteration. Otherwise you wouldn't be able to clear the interface after the for-in loop has
      finished because the iterator variable isn't visible anymore. The iterator variable will be
      always cleared when the surrounding function is left.
    * for-in in include files isn't supported 
}

interface

{$R ForInExtension.res ForInExtension.rc}

uses
  Windows, SysUtils, Classes, Contnrs, LanguageExtension, DelphiLexer, StrUtils,
  LexerImplInclude;

type
  TForInExtension = class(TLanguageExtension, ILexerImplInclude)
  private
    FForInLoops: TObjectList;
    FIncludeFileName: string;
  protected
    procedure Parse(Lexer: TDelphiLexer);
    function IncludeFileNeeded: Boolean;
    procedure GenerateIncludeContent(Lines: TStrings);
    function GetIncludeFileName: string;

    property IncludeFileName: string read FIncludeFileName;
  protected
    procedure AlterLexer(Lexer: TDelphiLexer); override;
  public
    procedure Initialize; override;
    destructor Destroy; override;
  end;

  TForInLoop = class(TObject)
  private
    FIteratorName: string;
    FIteratorType: string;
    FObjectLoop: Boolean;
    FArrayPropertyLoop: Boolean;
    FArrayLoop: Boolean;

    FHelperName: string;
    FListName: string;
  public
    constructor Create(AIteratorName, AIteratorType, AListName: string;
      AObjectLoop, AArrayPropertyLoop, AArrayLoop: Boolean);
    function GenerateCode: string;
    procedure GenerateIncludeCode(Lines: TStrings);

    property HelperName: string read FHelperName;

    property IteratorName: string read FIteratorName;
    property IteratorType: string read FIteratorType;
    property ListName: string read FListName;
    property ObjectLoop: Boolean read FObjectLoop;
    property ArrayPropertyLoop: Boolean read FArrayPropertyLoop;
    property ArrayLoop: Boolean read FArrayLoop;
  end;

implementation

{ TForInExtension }

procedure TForInExtension.Initialize;
begin
  inherited Initialize;
  FForInLoops := TObjectList.Create;
  FIncludeFileName := MakeDLangExtFilename(Filename) + '.forin.inc';
end;

destructor TForInExtension.Destroy;
begin
  FForInLoops.Free;
  inherited Destroy;
end;

procedure TForInExtension.GenerateIncludeContent(Lines: TStrings);
var
  I: Integer;
  ForInLoop: TForInLoop;
  K: Integer;                                      
  AlreadyCreated: Boolean;
  Stream: TResourceStream;
begin
  { Read base class code from the resource }
  Stream := TResourceStream.Create(HInstance, 'ForInBaseClasses', RT_RCDATA);
  try
    Lines.LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
  Lines.Insert(0, '{$IFNDEF __zzForIn_Include_Protection__}');
  Lines.Insert(1, '{$DEFINE __zzForIn_Include_Protection__}');
  Lines.Insert(2, '');

  Lines.Add('');
  Lines.Add('{=============================================================================}');
  Lines.Add('');

  for I := 0 to FForInLoops.Count - 1 do
  begin
    ForInLoop := TForInLoop(FForInLoops[I]);

    { Check if there is already a iterator class for the "HelperName" } 
    AlreadyCreated := False;
    for K := 0 to I - 1 do
    begin
      if SameText(ForInLoop.HelperName, TForInLoop(FForInLoops[K]).HelperName) then
      begin
        AlreadyCreated := True;
        Break;
      end;
    end;

    if not AlreadyCreated then
    begin
      ForInLoop.GenerateIncludeCode(Lines);
      Lines.Add('');
      Lines.Add('{-----------------------------------------------------------------------------}');
      Lines.Add('');
    end;
  end;
  Lines.Add('{$ENDIF __zzForIn_Include_Protection__}');
end;

function TForInExtension.IncludeFileNeeded: Boolean;
begin
  Result := FForInLoops.Count > 0;
end;

function TForInExtension.GetIncludeFileName: string;
begin
  Result := IncludeFileName;
end;

procedure TForInExtension.AlterLexer(Lexer: TDelphiLexer);
begin
  { Include files aren't supported }
  if not SameText(ExtractFileExt(Lexer.Filename), '.pas') then
    Exit;

  FindImplIncludeToken(Lexer, Self, 'FOR-IN');
end;

procedure TForInExtension.Parse(Lexer: TDelphiLexer);
var
  ForToken: TToken;
  Token, PreToken, PrePreToken: TToken;
  IteratorName, IteratorType, ListName: string;
  ObjectLoop, ArrayLoop, ArrayPropertyLoop: Boolean;
  ForInLoop: TForInLoop;
  LineOffset: Integer;
  InsertIndex: Integer;
  NewCode: string;
begin
  ObjectLoop := False;
  ForToken := Lexer.CurrentToken;
  if (ForToken = nil) or (ForToken.Kind <> tkI_for) then
    Exit;


  { object/IteratorName }
  if Lexer.NextTokenNoComment(Token) then
  begin
    { for object ...}
    ObjectLoop := Token.Kind = tkI_object;
    if ObjectLoop then
      if not Lexer.NextTokenNoComment(Token) then
        Exit;

    { IteratorName }
    if (Token.Kind < tkIdent) or (Token.Kind >= tkIdentStrictReserved) then
      Exit; // maybe an error, let the compiler check this
    IteratorName := Token.Value;
  end;

  { Check for extended for-in loop }
  if Lexer.NextTokenNoComment(Token) then
  begin
    if Token.Kind <> tkColon then
      Exit; // no type declaration => not an extended for-in loop
  end;

  { IteratorType }
  while Lexer.NextTokenNoComment(Token) and Token.IsTypeIdent do
  begin
    IteratorType := IteratorType + Token.Value;
    if Lexer.NextTokenNoComment(Token) then
      if Token.Kind <> tkDot then
        Break;
  end;

  { Check for extended for-in loop }
  if (Token = nil) or (Token.Kind <> tkI_in) then
    Exit; // no "in" => not an extended for-in loop

  { for ... in array ... do }
  ArrayLoop := Lexer.NextTokenNoComment(Token) and (Token.Kind = tkI_array);
  ListName := '';
  if not ArrayLoop then
    ListName := Token.Value;
    
  PrePreToken := nil;
  PreToken := nil;
  while Lexer.NextTokenNoComment(Token) and (Token.Kind <> tkI_do) do
  begin
    ListName := ListName + ' ' + Token.Value;
    PrePreToken := PreToken;
    PreToken := Token;
  end;
  if Token <> nil then
  begin
    { for ... in ...[] do }
    ArrayPropertyLoop := (PreToken <> nil) and (PrePreToken <> nil) and
                         (PrePreToken.Kind = tkLBracket) and (PreToken.Kind = tkRBracket); // "...[] do"
    if ArrayPropertyLoop then
      ListName := Copy(ListName, 1, Length(ListName) - Length(' [ ]'));


    { Create TForInLoop object that holds the extended for-in loop }
    ForInLoop := TForInLoop.Create(IteratorName, IteratorType, Trim(ListName), ObjectLoop, ArrayPropertyLoop, ArrayLoop);
    FForInLoops.Add(ForInLoop);

    { Replace tokens with new code }
    LineOffset := Token.Line - ForToken.Index;
    InsertIndex := ForToken.Index;

    NewCode := ForInLoop.GenerateCode();
    while LineOffset > 0 do
    begin
      NewCode := NewCode + sLineBreak;
      Dec(LineOffset);
    end;

    Lexer.DeleteTokens(ForToken, Token);
    Lexer.InsertText(InsertIndex, NewCode);
  end;
end;

{ TForInLoop }

constructor TForInLoop.Create(AIteratorName, AIteratorType, AListName: string; AObjectLoop,
  AArrayPropertyLoop, AArrayLoop: Boolean);
begin
  //inherited Create;
  FIteratorName := AIteratorName;
  FIteratorType := AIteratorType;
  FListName := AListName;
  FObjectLoop := AObjectLoop;
  FArrayPropertyLoop := AArrayPropertyLoop;
  FArrayLoop := AArrayLoop;

  if ArrayLoop or ArrayPropertyLoop then
    FHelperName := 'zzForInIteratorArray' + Mangle(IteratorType) + Mangle(IteratorName)
  else
    FHelperName := 'zzForInIterator' + Mangle(IteratorType) + Mangle(IteratorName);
end;

function TForInLoop.GenerateCode: string;
begin
  if ArrayLoop or ArrayPropertyLoop then
  begin // array or array-property
    if ArrayLoop then
      Result := 'if {$IFDEF CLR}Borland.Delphi.{$ENDIF}System.Length(' + ListName + ') > 0 then ' +
                'with ' + HelperName + 'New({$IFDEF CLR}Borland.Delphi.{$ENDIF}System.Length(' + ListName + ')) do '
    else
      Result := 'if (' + ListName + ' <> nil) and (' + ListName + '.Count > 0) then ' +
                'with ' + HelperName + 'New(' + ListName + '.Count) do ';
    if ObjectLoop then
      Result := Result + 'while zzIterator.MoveNext and zzIterator.AssignValue(' + ListName + '[zzIterator.Index] as ' + IteratorType + ') do'
    else
      Result := Result + 'while zzIterator.MoveNext and zzIterator.AssignValue(' + IteratorType + '(' + ListName + '[zzIterator.Index])) do';
  end
  else // Enumerator
  begin
    Result := 'if (' + ListName + ' <> nil) then ' +
              'with ' + ListName + '.GetEnumerator, ' + HelperName + '.Create{$IFNDEF CLR}(MoveNext){$ENDIF}.Data do ' +
      '{$IFNDEF CLR}while zzIterator.TryFinally do{$ENDIF} ';
    if ObjectLoop then
      Result := Result + 'while MoveNext and zzIterator.AssignValue(GetCurrent as ' + IteratorType +') do'
    else
      Result := Result + 'while MoveNext and zzIterator.AssignValue(' + IteratorType + '(GetCurrent)) do';
  end;
end;

procedure TForInLoop.GenerateIncludeCode(Lines: TStrings);
var
  InlineFlag: string;
begin
  InlineFlag := ' {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}';

  if ArrayLoop or ArrayPropertyLoop then
  begin
    Lines.Add('type');
    Lines.Add('  {$IFDEF CLR}');
    Lines.Add('  ' + HelperName + ' = class;');
    Lines.Add('  {$ENDIF CLR}');
    Lines.Add('');
    Lines.Add('  ' + HelperName + 'Object = {$IFDEF CLR}class{$ELSE}object{$ENDIF}(zzForInBaseEnumeratorArrayIterator)');
    Lines.Add('    {$IFDEF CLR}');
    Lines.Add('    Data: ' + HelperName + ';');
    Lines.Add('    {$ENDIF CLR}');
    Lines.Add('    function AssignValue(const Value: ' + IteratorType + '): Boolean;' + InlineFlag);
    Lines.Add('  end;');
    Lines.Add('');
    Lines.Add('  ' + HelperName + ' = {$IFDEF CLR}class{$ELSE}packed record{$ENDIF}');
    Lines.Add('    zzIterator: ' + HelperName + 'Object;');
    Lines.Add('    ' + IteratorName + ': ' + IteratorType + ';');
    Lines.Add('  end;');
    Lines.Add('');
    Lines.Add('function ' + HelperName + 'Object.AssignValue(const Value: ' + IteratorType + '): Boolean;');
    Lines.Add('begin');
    Lines.Add('  {$IFDEF CLR}');
    Lines.Add('  Data.' + IteratorName + ' := Value;');
    Lines.Add('  {$ELSE}');
    Lines.Add('  ' + IteratorType + '(Pointer(Integer(@Self) + SizeOf(Self))^) := Value; // access outer-class data field');
    Lines.Add('  {$ENDIF CLR}');
    Lines.Add('  Result := True;');
    Lines.Add('end;');
    Lines.Add('');
    Lines.Add('function ' + HelperName + 'New(Count: Integer): ' + HelperName + ';' + InlineFlag);
    Lines.Add('begin');
    Lines.Add('  {$IFDEF CLR}');
    Lines.Add('  Result := ' + HelperName + '.Create;');
    Lines.Add('  Result.zzIterator := ' + HelperName + 'Object.Create;');
    Lines.Add('  Result.zzIterator.Data := Result;');
    Lines.Add('  {$ENDIF CLR}');
    Lines.Add('  Result.zzIterator.Index := -1;');
    Lines.Add('  Result.zzIterator.Count := Count;');
    Lines.Add('end;');
  end
  else
  begin
    Lines.Add('type');
    Lines.Add('  ' + HelperName + ' = class;');
    Lines.Add('  ' + HelperName + 'Record = {$IFDEF CLR}class{$ELSE}record{$ENDIF}');
    Lines.Add('    ' + IteratorName + ': ' + IteratorType + ';');
    Lines.Add('    zzIterator: ' + HelperName + ';');
    Lines.Add('  end;');
    Lines.Add('');
    Lines.Add('  ' + HelperName + ' = class(zzForInBaseEnumeratorIterator)');
    Lines.Add('  private');
    Lines.Add('    Data: ' + HelperName + 'Record;');
    Lines.Add('  public');
    Lines.Add('    constructor Create{$IFNDEF CLR}(AMoveNext: zzForInMoveNextMethod){$ENDIF};');
    Lines.Add('    function AssignValue(const Value: ' + IteratorType + '): Boolean;' + InlineFlag);
    Lines.Add('  end;');
    Lines.Add('');
    Lines.Add('constructor ' + HelperName + '.Create;');
    Lines.Add('begin');
    Lines.Add('  inherited Create{$IFNDEF CLR}(AMoveNext){$ENDIF};');
    Lines.Add('  {$IFDEF CLR}');
    Lines.Add('  Data := ' + HelperName + 'Record.Create;');
    Lines.Add('  {$ENDIF CLR}');
    Lines.Add('  Data.zzIterator := Self;');
    Lines.Add('end;');
    Lines.Add('');
    Lines.Add('function ' + HelperName + '.AssignValue(const Value: ' + IteratorType + '): Boolean;');
    Lines.Add('begin');
    Lines.Add('  Data.' + IteratorName + ' := Value;');
    Lines.Add('  Result := True;');
    Lines.Add('end;');
  end;
end;

end.
