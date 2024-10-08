{ google protobuf files compiler to FPC class

  Copyright (C) 2018-2024 Lagunov Aleksey alexs@yandex.ru

  This source is free software; you can redistribute it and/or modify it under the terms of the GNU General Public
  License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later
  version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web at
  <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing to the Free Software Foundation, Inc., 51
  Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}

unit EnumObject;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ProtoParser;

type
  TEnumValueListEnumerator = class;

  { TEnumValue }

  TEnumValue = class
  private
    FCaption: string;
    FValue: integer;
  public
    constructor Create;
    destructor Destroy;override;
    procedure Assign(ASource:TEnumValue);
    property Caption:string read FCaption write FCaption;
    property Value:integer read FValue write FValue;
  end;

  { TEnumValueList }

  TEnumValueList = class
  private
    FList:TFPList;
    function GetCount: integer;
    function GetItem(AIndex: integer): TEnumValue;
    procedure Clear;
  public
    constructor Create;
    destructor Destroy;override;
    procedure Assign(ASource:TEnumValueList);
    function GetEnumerator: TEnumValueListEnumerator;
    function Add(ACaption:string):TEnumValue;
    function EnumByValue(AValue:Integer):TEnumValue;
    property Item[AIndex:integer]:TEnumValue read GetItem; default;
    property Count:integer read GetCount;
  end;

  { TEnumValueListEnumerator }

  TEnumValueListEnumerator = class
  private
    FList: TEnumValueList;
    FPosition: Integer;
  public
    constructor Create(AList: TEnumValueList);
    function GetCurrent: TEnumValue;
    function MoveNext: Boolean;
    property Current: TEnumValue read GetCurrent;
  end;


  { TEnum }

  TEnum = class(TProtoObject)
  private
    FAllowAlias: Boolean;
    FCurField:TEnumValue;
    FCurOption:string;
    FValues: TEnumValueList;
  protected
    procedure InitParserTree;override;
    procedure InternalProcessChildToken(AParser:TProtoParser; AToken:TProtoToken; AWord:string); override;

  public
    constructor Create;override;
    destructor Destroy;override;
    procedure Clear; override;
    procedure GenerateInterfaceSection(AModule:TStrings); override;
    procedure Assign(ASource:TProtoObject); override;
    property Values:TEnumValueList read FValues;
    property AllowAlias:Boolean read FAllowAlias write FAllowAlias;
  end;

implementation

{ TEnumValueList }

function TEnumValueList.GetCount: integer;
begin
  Result:=FList.Count;
end;

function TEnumValueList.GetItem(AIndex: integer): TEnumValue;
begin
  Result:=TEnumValue(FList[AIndex]);
end;

procedure TEnumValueList.Clear;
var
  i: Integer;
begin
  for i:=0 to FList.Count-1 do
    TEnumValue(FList[i]).Free;
  FList.Clear;
end;

constructor TEnumValueList.Create;
begin
  inherited Create;
  FList:=TFPList.Create;
end;

destructor TEnumValueList.Destroy;
begin
  FList.Clear;
  FList.Free;
  inherited Destroy;
end;

procedure TEnumValueList.Assign(ASource: TEnumValueList);
var
  P: TEnumValue;
begin
  if not Assigned(ASource) then Exit;
  for P in ASource do Add('').Assign(P);
end;

function TEnumValueList.GetEnumerator: TEnumValueListEnumerator;
begin
  Result:=TEnumValueListEnumerator.Create(Self);
end;

function TEnumValueList.Add(ACaption: string): TEnumValue;
begin
  Result:=TEnumValue.Create;
  Result.Caption:=ACaption;
  FList.Add(Result);
end;

function TEnumValueList.EnumByValue(AValue: Integer): TEnumValue;
var
  i: Integer;
  V: TEnumValue;
begin
  Result:=nil;
  for i:=0 to FList.Count-1 do
  begin
    V:=TEnumValue(FList[i]);
    if V.FValue = AValue then
      Exit(V);
  end;
end;

{ TEnumValueListEnumerator }

constructor TEnumValueListEnumerator.Create(AList: TEnumValueList);
begin
  FList := AList;
  FPosition := -1;
end;

function TEnumValueListEnumerator.GetCurrent: TEnumValue;
begin
  Result := FList[FPosition];
end;

function TEnumValueListEnumerator.MoveNext: Boolean;
begin
  Inc(FPosition);
  Result := FPosition < FList.Count;
end;

{ TEnumValue }

constructor TEnumValue.Create;
begin
  inherited Create;
end;

destructor TEnumValue.Destroy;
begin
  inherited Destroy;
end;

procedure TEnumValue.Assign(ASource: TEnumValue);
begin
  if not Assigned(ASource) then Exit;
  FCaption:=ASource.FCaption;
  FValue:=ASource.FValue;
end;

{ TEnum }

procedure TEnum.InitParserTree;
var
  FStart, T, TN, TSymb, TV, TOpt, TOpt1, TOpt1_1, TOpt1_2, TRes,
    TRes1, TRes2, TRes1_1, TRes1_2: TProtoToken;
begin
  (*
  enum Corpus {
    UNIVERSAL = 0;
    WEB = 1;
    IMAGES = 2;
    LOCAL = 3;
    NEWS = 4;
    PRODUCTS = 5;
    VIDEO = 6;
  }

  enum EnumAllowingAlias {
    option allow_alias = true;
    UNKNOWN = 0;
    STARTED = 1;
    RUNNING = 1;
  }

  enum Foo {
    reserved 2, 15, 9 to 11, 40 to max;
    reserved "FOO", "BAR";
  }
  *)
  FStart:=AddToken(stKeyword, nil, 'enum', [toHeaderStart, toHeaderEnd]);
  T:=AddToken(stIdentificator, FStart, '', [], 1);
  T:=AddToken(stSymbol, T, '{', []);

    TRes:=AddToken(stKeyword, T, 'reserved', []);
      TRes1:=AddToken(stInteger, TRes, '', []);

      TRes1_1:=AddToken(stKeyword, TRes1, 'TO', []);
      TRes1_2:=AddToken(stKeyword, TRes1_1, 'max', []);
      TRes1_1:=AddToken(stInteger, TRes1_1, '', []);

      TSymb:=AddToken(stSymbol, [TRes1, TRes1_1, TRes1_2], ',', []);
      TSymb.AddChildToken(TRes1);

      TRes2:=AddToken(stString, TRes, '', []);
      TSymb:=AddToken(stSymbol, [TRes2], ',', []);
      TSymb.AddChildToken(TRes2);

    TOpt:=AddToken(stKeyword, T, 'option', []);
    TOpt1:=AddToken(stIdentificator, TOpt, '', [], 5);
    TOpt1:=AddToken(stSymbol, TOpt1, '=', []);
    TOpt1_1:=AddToken(stKeyword, TOpt1, 'true', [], 6);
    TOpt1_2:=AddToken(stKeyword, TOpt1, 'false', [], 7);

    TN:=AddToken(stIdentificator, T, '', [], 2);
    TSymb:=AddToken(stSymbol, TN, '=', []);
    TV:=AddToken(stInteger, TSymb, '', [], 3);
    TSymb:=AddToken(stSymbol, [TV, TOpt1_1, TOpt1_2, TRes1, TRes1_1, TRes1_2, TRes2], ';', [], 4);
    TSymb.AddChildToken([TN, TOpt, TRes]);
  T:=AddToken(stSymbol, TSymb, '}', [], -1);
end;

procedure TEnum.InternalProcessChildToken(AParser: TProtoParser;
  AToken: TProtoToken; AWord: string);
var
  V: TEnumValue;
begin
  inherited InternalProcessChildToken(AParser, AToken, AWord);
  case AToken.Code of
    2:FCurField:=FValues.Add(AWord);
    3:begin
        if not AllowAlias then
        begin
          V:=FValues.EnumByValue(StrToInt(AWord));
          if Assigned(V) and (V<>FCurField) then
            raise Exception.CreateFmt('duplicate enum value : %s = %d',[FCurField.Caption, StrToInt(AWord)]);
        end;
        if Assigned(FCurField) then FCurField.FValue:=StrToInt(AWord);

      end;
    4:FCurField:=nil;
    5:FCurOption:=UpperCase( AWord);
    6:begin
        if FCurOption = 'ALLOW_ALIAS' then
          FAllowAlias:=true;
        FCurOption:='';
      end;
    7:begin
        if FCurOption = 'ALLOW_ALIAS' then
          FAllowAlias:=false;
        FCurOption:='';
      end;
  end;
end;

procedure TEnum.GenerateInterfaceSection(AModule: TStrings);
var
  V: TEnumValue;
  S: String;
  i: Integer;
begin
  if Values.Count = 0 then exit;
  AModule.Add('T'+Caption + ' = (');
  for i:=0 to Values.Count-1 do
  begin
    V:=Values[i];
    S:=Format('  %s = %d', [V.Caption, V.Value]);
    if i<Values.Count-1 then S:=S + ',';
    AModule.Add(S);
  end;
  AModule.Add(');');
  AModule.Add('T'+Caption + 'Array = array of T'+Caption+';');
end;

constructor TEnum.Create;
begin
  inherited Create;
  FValues:=TEnumValueList.Create;
end;

destructor TEnum.Destroy;
begin
  Clear;
  FreeAndNil(FValues);
  inherited Destroy;
end;

procedure TEnum.Clear;
begin
  if Assigned(FValues) then
    FValues.Clear;
  FAllowAlias:=false;
  inherited Clear;
end;

procedure TEnum.Assign(ASource: TProtoObject);
begin
  inherited Assign(ASource);
end;

initialization
  RegisterProtoObject(TEnum);
end.

