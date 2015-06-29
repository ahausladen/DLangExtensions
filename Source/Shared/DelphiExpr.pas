{******************************************************************************}
{*                                                                            *}
{* Delphi Expression Parser                                                   *}
{*                                                                            *}
{* (C) 2005 Andreas Hausladen                                                 *}
{*                                                                            *}
{******************************************************************************}

unit DelphiExpr;

interface

uses
  SysUtils, Classes, Contnrs, DelphiLexer, DelphiPreproc;

type
  EInvalidOp = class(Exception);

  IExprNode = interface
    function ToString: string;
    function GetValue(out Value: TExprNodeValueRec): Boolean;
  end;

  TExprNode = class(TInterfacedObject, IExprNode)
  public
    function GetValue(out Value: TExprNodeValueRec): Boolean; virtual; abstract;
  end;

  TExprNodeUnaryMinus = class(TExprNode)
  private
    FOperand: IExprNode;
  public
    constructor Create(AOperand: IExprNode);
    function ToString: string; override;
    function GetValue(out Value: TExprNodeValueRec): Boolean; override;
  end;

  TExprNodeBitOpNeg = class(TExprNode)
  private
    FOperand: IExprNode;
  public
    constructor Create(AOperand: IExprNode);
    function ToString: string; override;
    function GetValue(out Value: TExprNodeValueRec): Boolean; override;
  end;

  TExprNodeBinOp = class(TExprNode)
  private
    FOperation: TTokenKind;
    FOpName: string;
    FOperand1: IExprNode;
    FOperand2: IExprNode;
  public
    constructor Create(AOperation: TTokenKind; const AOpName: string; AOperand1, AOperand2: IExprNode);
    function ToString: string; override;
    function GetValue(out Value: TExprNodeValueRec): Boolean; override;
  end;

  TExprNodeConst = class(TExprNode)
  private
    FKind: TTokenKind;
    FValue: string;
  public
    constructor Create(AKind: TTokenKind; const AValue: string);
    function ToString: string; override;
    function GetValue(out Value: TExprNodeValueRec): Boolean; override;
  end;

  IBoolNode = interface
    function ToString: string;
    function GetValue(var Error: string): Boolean;
  end;

  TBoolNode = class(TInterfacedObject, IBoolNode)
  public
    function GetValue(var Error: string): Boolean; virtual; abstract;
  end;

  TBoolNodeOp = class(TBoolNode)
  private
    FOperation: TTokenKind;
    FOpName: string;
    FLeft: IBoolNode;
    FRight: IBoolNode;
  public
    constructor Create(AOperation: TTokenKind; const AOpName: string; ALeft, ARight: IBoolNode);
    function ToString: string; override;
    function GetValue(var Error: string): Boolean; override;
  end;

  TBoolNodeNot = class(TBoolNode)
  private
    FNode: IBoolNode;
  public
    constructor Create(ANode: IBoolNode);
    function ToString: string; override;
    function GetValue(var Error: string): Boolean; override;
  end;

  TBoolNodeConst = class(TBoolNode)
  private
    FValue: Boolean;
  public
    constructor Create(AValue: Boolean);
    function ToString: string; override;
    function GetValue(var Error: string): Boolean; override;
  end;

{  TBoolNodeIdent = class(TBoolNode)
  private
    FIdent: string;
    FPreProc: TDelphiPreprocessor;
  public
    constructor Create(APreProc: TDelphiPreprocessor; const AIdent: string);
    function ToString: string; override;
    function GetValue(var Error: string): Boolean; override;
  end;}

  TBoolNodeExp = class(TBoolNode)
  private
    FExpr: IExprNode;
  public
    constructor Create(AExpr: IExprNode);
    function ToString: string; override;
    function GetValue(var Error: string): Boolean; override;
  end;

  TBoolNodeRelOp = class(TBoolNode)
  private
    FRelOperation: TTokenKind;
    FRelOpName: string;
    FLeft: IExprNode;
    FRight: IExprNode;
  public
    constructor Create(ARelOperation: TTokenKind; const ARelOpName: string;
      ALeft, ARight: IExprNode);
    function ToString: string; override;
    function GetValue(var Error: string): Boolean; override;
  end;

implementation

{.$AUTOBOX ON}
{.$HINTS OFF}
{.$WARNINGS OFF}

constructor TExprNodeUnaryMinus.Create(AOperand: IExprNode);
begin
  inherited Create;
  FOperand := AOperand;
end;

function TExprNodeUnaryMinus.ToString: string;
begin
  Result := '-' + FOperand.ToString;
end;

function TExprNodeUnaryMinus.GetValue(out Value: TExprNodeValueRec): Boolean;
var
  calculatable: Boolean;
begin
  calculatable := FOperand.GetValue(Value);
  if calculatable then
  begin
    case Value.typ of
      vkIsInt:
        Value.valueInt := -Value.valueInt;
      vkIsFloat:
        Value.valueFloat := -Value.valueFloat;
      vkIsString:
        // TODO: invalid operation
        Value.Error := Format('Invalid operation %s', ['-']);
    end;
  end;
  Result := calculatable;
end;

constructor TExprNodeBitOpNeg.Create(AOperand: IExprNode);
begin
  inherited Create;
  FOperand := AOperand;
end;

function TExprNodeBitOpNeg.ToString: string;
begin
  Result := '~' + FOperand.ToString;
end;

function TExprNodeBitOpNeg.GetValue(out Value: TExprNodeValueRec): Boolean;
var
  calculatable: Boolean;
begin
  calculatable := FOperand.GetValue(Value);
  if calculatable then
  begin
    case Value.typ of
      vkIsInt:
        Value.valueInt := not Value.valueInt;
      vkIsFloat:
        begin
          Value.Error := 'Operation ~ cannot be applied to float value';
          Result := False;
          Exit;
        end;
      vkIsString:
        // TODO: invalid operation
        Value.Error := Format('Invalid operation %s', ['not']);
    end;
  end;
  Result := calculatable;
end;

constructor TExprNodeBinOp.Create(AOperation: TTokenKind; const AOpName: string; AOperand1, AOperand2: IExprNode);
begin
  inherited Create;
  FOperation := AOperation;
  FOpName := AOpName;
  FOperand1 := AOperand1;
  FOperand2 := AOperand2;
end;

function TExprNodeBinOp.ToString: string;
begin
  Result := '(' + FOperand1.ToString + ' ' + FOpName + ' ' + FOperand2.ToString + ')';
end;

function TExprNodeBinOp.GetValue(out Value: TExprNodeValueRec): Boolean;
var
  calculatable2: Boolean;
  calculatable1: Boolean;
  v2: Double;
  v1: Double;
  value2: TExprNodeValueRec;
  value1: TExprNodeValueRec;
begin
  calculatable1 := FOperand1.GetValue(value1);
  calculatable2 := FOperand2.GetValue(value2);
  if (calculatable1 = False) then
    Value.Error := value1.Error
  else
    Value.Error := value2.Error;
  Value.valueFloat := 0;
  Value.valueInt := 0;
  Value.typ := vkIsInt;
  if (calculatable1 and calculatable2) then
  begin
    if ((value1.typ = vkIsString) and (value2.typ = vkIsString)) then
    begin
      Value.typ := vkIsString;
      Result := True;
      // calculate
      case FOperation of
        tkPlus:
          Value.valueStr := value1.valueStr + value2.valueStr;
      else
        // TODO: invalid operation
        Value.Error := Format('Invalid operation %s', [FOpName]);
        Result := False;
      end;
    end
    else if ((value1.typ = vkIsInt) and (value2.typ = vkIsInt)) then
    begin
      Value.typ := vkIsInt;
      Result := True;
      // calculate
      case FOperation of
        tkPlus:
          Value.valueInt := value1.valueInt + value2.valueInt;
        tkMinus:
          Value.valueInt := value1.valueInt - value2.valueInt;
        tkMultiply:
          Value.valueInt := value1.valueInt * value2.valueInt;
        tkDivide:
          Value.valueInt := value1.valueInt div value2.valueInt; // warning: division by 0
        tkI_Mod:
          Value.valueInt := value1.valueInt mod value2.valueInt; // warning: division by 0
        tkI_Or:
          Value.valueInt := value1.valueInt or value2.valueInt;
        tkI_Xor:
          Value.valueInt := value1.valueInt xor value2.valueInt;
        tkI_And:
          Value.valueInt := value1.valueInt and value2.valueInt;
      else
        Result := False;
      end;
    end
    else
    begin
      v1 := 0;
      v2 := 0;
      Value.typ := vkIsFloat;
      if ((value1.typ = vkIsFloat) and (value2.typ = vkIsInt)) then
      begin
        v1 := value1.valueFloat;
        v2 := value2.valueInt;
      end
      else if ((value1.typ = vkIsInt) and (value2.typ = vkIsFloat)) then
      begin
        v1 := value1.valueInt;
        v2 := value2.valueFloat;
      end
      else if ((value1.typ = vkIsFloat) and (value2.typ = vkIsFloat)) then
      begin
        v1 := value1.valueFloat;
        v2 := value2.valueFloat;
      end;
      Result := True;
      // calculate
      case FOperation of
        tkPlus:
          Value.valueFloat := v1 + v2;
        tkMinus:
          Value.valueFloat := v1 - v2;
        tkMultiply:
          Value.valueFloat := v1 * v2;
        tkDivide:
          if v2 = 0 then
          begin
            Value.Error := 'division by zero';
            Result := False;
          end
          else
            Value.valueFloat := v1 / v2;
        tkI_Mod:
          if v2 = 0 then
          begin
            Value.Error := 'division by zero';
            Result := False;
          end
          else
            Value.valueFloat := Frac(v1 / v2);
        tkI_Or, tkI_Xor, tkI_And:
          begin
            // TODO: invalid operation
            Value.Error := Format('Invalid operation %s', [FOpName]);
            Result := False;
          end;
      end;
    end;
  end
  else
    Result := False;
end;

constructor TExprNodeConst.Create(AKind: TTokenKind; const AValue: string);
begin
  inherited Create;
  FKind := AKind;
  FValue := AValue;
end;

function TExprNodeConst.ToString: string;
begin
  Result := FValue;
end;

function TExprNodeConst.GetValue(out Value: TExprNodeValueRec): Boolean;
var
  ErrCode: Integer;
begin
  Value.Error := '';
  Value.ValueInt := 0;
  Value.ValueFloat := 0;
  Value.ValueStr := '';
  Value.Typ := vkIsInt;
  case FKind of
    tkIdent:
      begin
        Value.typ := vkIsInt;
        if SameText(FValue, 'true') then
          Value.ValueInt := 1
        else if SameText(FValue, 'false') then
          Value.ValueInt := 0
        else
          raise Exception.Create('Internal error: Unknown identifier found');
      end;
    tkInt:
      begin
        Value.Typ := vkIsInt;
        Value.ValueInt := StrToInt(FValue);
      end;
    tkFloat:
      begin
        Value.Typ := vkIsFloat;
        Val(FValue, Value.ValueFloat, ErrCode);
        if ErrCode > 0 then
        begin
          Value.Error := Format('%s is no valid float value', [FValue]);
          Result := False;
          Exit;
        end;
      end;
    tkString:
      begin
        Value.Typ := vkIsString;
        Value.ValueStr := FValue;
      end;
  end;
  Result := True;
end;

constructor TBoolNodeOp.Create(AOperation: TTokenKind; const AOpName: string;
  ALeft: IBoolNode; ARight: IBoolNode);
begin
  inherited Create;
  FOperation := AOperation;
  FOpName := AOpName;
  FLeft := ALeft;
  FRight := ARight;
end;

function TBoolNodeOp.ToString: string;
begin
  Result := '(' + FLeft.ToString + ' ' + FOpName + ' ' + FRight.ToString + ')';
end;

function TBoolNodeOp.GetValue(var Error: string): Boolean;
var
  l: Boolean;
begin
  l := FLeft.GetValue(Error);
  if Error <> '' then
  begin
    Result := False;
    Exit;
  end;
  case FOperation of
    tkI_And:
      Result := l and FRight.GetValue(Error);
    tkI_Or:
      Result := l or FRight.GetValue(Error);
    tkI_Xor:
      Result := l xor FRight.GetValue(Error);
  else
    Error := Format('Unknown operation %s', [FOpName]);
    Result := False;
  end;
end;

constructor TBoolNodeNot.Create(ANode: IBoolNode);
begin
  inherited Create;
  FNode := ANode;
end;

function TBoolNodeNot.ToString: string;
begin
  Result := 'not ' + FNode.ToString;
end;

function TBoolNodeNot.GetValue(var Error: string): Boolean;
begin
  Result := not FNode.GetValue(Error);
end;

constructor TBoolNodeConst.Create(AValue: Boolean);
begin
  inherited Create;
  FValue := AValue;
end;

function TBoolNodeConst.ToString: string;
const
  BoolToStr: array[Boolean] of string = ('False', 'True');
begin
  Result := BoolToStr[FValue];
end;

function TBoolNodeConst.GetValue(var Error: string): Boolean;
begin
  Result := FValue;
end;

{constructor TBoolNodeIdent.Create(APreProc: TDelphiPreprocessor; const AIdent: string);
begin
  inherited Create;
  FIdent := AIdent;
  FPreProc := APreProc;
end;

function TBoolNodeIdent.ToString: string;
begin
  Result := FIdent;
end;

function TBoolNodeIdent.GetValue(var Error: string): Boolean;
begin
  Result := FPreProc.Defines.Contains(FIdent);
end;}

constructor TBoolNodeExp.Create(AExpr: IExprNode);
begin
  inherited Create;
  FExpr := AExpr;
end;

function TBoolNodeExp.ToString: string;
begin
  Result := '(' + FExpr.ToString + ')';
end;

function TBoolNodeExp.GetValue(var Error: string): Boolean;
var
  Value: TExprNodeValueRec;
begin

  if FExpr.GetValue(Value) then
    if Value.typ = vkIsFloat then
      Error := 'Float Value cannot be evaluated to true or false.'
    else if Value.typ = vkIsString then
      Error := 'String Value cannot be evaluated to true or false.'
    else if Value.typ = vkIsInt then
    begin
      Result := (Value.valueInt <> 0);
      Exit;
    end
    else
      Error := 'Unknown number type.'
  else
    Error := Value.Error;
  Result := False;
end;

constructor TBoolNodeRelOp.Create(ARelOperation: TTokenKind; const ARelOpName: string;
  ALeft, ARight: IExprNode);
begin
  inherited Create;
  FRelOperation := ARelOperation;
  FRelOpName := ARelOpName;
  FLeft := ALeft;
  FRight := ARight;
end;

function TBoolNodeRelOp.ToString: string;
begin
  Result := '(' + FLeft.ToString + ' ' + FRelOpName + ' ' + FRight.ToString + ')';
end;

function TBoolNodeRelOp.GetValue(var Error: string): Boolean;
var
  d1, d2: Double;
  v1, v2: TExprNodeValueRec;
  s1, s2: string;
begin
  Result := False;
  v1.Error := '';
  v2.Error := '';
  if FLeft.GetValue(v1) and FRight.GetValue(v2) then
  begin
    d1 := 0;
    d2 := 0;
    if v1.Typ = vkIsInt then d1 := v1.ValueInt
    else if v1.Typ = vkIsFloat then d1 := v1.ValueFloat
    else s1 := v1.ValueStr;

    if v2.Typ = vkIsInt then d2 := v2.ValueInt
    else if v1.Typ = vkIsFloat then d2 := v2.ValueFloat
    else s2 := v2.ValueStr;

    case FRelOperation of
      tkEqual: // ==
        if (v1.Typ = vkIsString) and (v2.Typ = vkIsString) then
          Result := s1 = s2
        else if (v1.Typ = vkIsInt) and (v2.Typ = vkIsInt) then
          Result := v1.ValueInt = v2.ValueInt
        else
          Result := d1 = d2;

      tkNotEqual: // !=
        if (v1.Typ = vkIsString) and (v2.Typ = vkIsString) then
          Result := s1 <> s2
        else if (v1.Typ = vkIsInt) and (v2.Typ = vkIsInt) then
          Result := v1.ValueInt <> v2.ValueInt
        else
          Result := d1 <> d2;

      tkLessThan: // <
        if (v1.Typ = vkIsString) and (v2.Typ = vkIsString) then
          Result := s1 < s2
        else if (v1.Typ = vkIsInt) and (v2.Typ = vkIsInt) then
          Result := v1.ValueInt < v2.ValueInt
        else
          Result := d1 < d2;

      tkGreaterThan: // >
        if (v1.Typ = vkIsString) and (v2.Typ = vkIsString) then
          Result := s1 > s2
        else if (v1.Typ = vkIsInt) and (v2.Typ = vkIsInt) then
          Result := v1.ValueInt > v2.ValueInt
        else
          Result := d1 > d2;

      tkLessEqualThan: // <=
        if (v1.Typ = vkIsString) and (v2.Typ = vkIsString) then
          Result := s1 <= s2
        else if (v1.Typ = vkIsInt) and (v2.Typ = vkIsInt) then
          Result := v1.ValueInt <= v2.ValueInt
        else
          Result := d1 <= d2;

      tkGreaterEqualThan: // >=
        if (v1.Typ = vkIsString) and (v2.Typ = vkIsString) then
          Result := s1 >= s2
        else if (v1.Typ = vkIsInt) and (v2.Typ = vkIsInt) then
          Result := v1.ValueInt >= v2.ValueInt
        else
          Result := d1 >= d2;
    else
      Error := Format('Unknown relation operation: %s', [FRelOpName]);
    end;
  end
  else
    Error := (v1.Error + '. ' + v2.Error);
end;

end.
