{ XSD files compiler to FPC class

  Copyright (C) 2019 Lagunov Aleksey alexs@yandex.ru

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

unit XsdElementTypesUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TXSDSimpleType = class;
  TXSDComplexType = class;
  TXSDComplexTypes = class;
  TXSDModule = class;
  TXSDComplexTypesEnumerator = class;
  TXSDSimpleTypesEnumerator = class;
  TPropertyItemsEnumerator = class;

  TPropertyItemType = (pitClass, pitAttribute, pitSimpleType);

  { TPropertyItem }

  TPropertyItem = class
  private
    FBaseType: string;
    FDescription: string;
    FIsRequired: boolean;
    FItemType: TPropertyItemType;
    FName: string;
    FPascalName: string;
    FValuesList: TStringList;
    FXSDSimpleType: TXSDSimpleType;
    FXSDComplexType: TXSDComplexType;
    FOwner: TXSDComplexType;
    FMinLength:Integer;
    FMaxLength:Integer;
    FMinOccurs:Integer;
    FMaxOccurs:Integer;
  public
    constructor Create(AOwner:TXSDComplexType);
    destructor Destroy; override;
    function PascalBaseType:string;

    function PascalMinLength:string;
    function PascalMaxLength:string;
    function PascalName:string;

    procedure UpdatePascalNames;
    property Name:string read FName write FName;
    property BaseType:string read FBaseType write FBaseType;
    property ItemType:TPropertyItemType read FItemType write FItemType;
    property Description:string read FDescription write FDescription;
    property XSDSimpleType:TXSDSimpleType read FXSDSimpleType;
    property MinOccurs:Integer read FMinOccurs write FMinOccurs;
    property MaxOccurs:Integer read FMaxOccurs write FMaxOccurs;
    property IsRequired:boolean read FIsRequired write FIsRequired;
    property ValuesList:TStringList read FValuesList;
  end;

  { TPropertyItems }

  TPropertyItems = class
  private
    FList:TFPList;
    FOwner: TXSDComplexType;
    function GetCount: integer;
    function GetItems(AIndex: Integer): TPropertyItem;
  public
    constructor Create(AOwner:TXSDComplexType);
    destructor Destroy; override;
    procedure Clear;
    procedure UpdatePascalNames;
    function GetEnumerator: TPropertyItemsEnumerator;
    function Add(AItemType:TPropertyItemType):TPropertyItem;
    function FindProperty(AItemName:string):TPropertyItem;
    property Count:integer read GetCount;
    property Items[AIndex:Integer]:TPropertyItem read GetItems; default;
  end;

  { TPropertyItemsEnumerator }

  TPropertyItemsEnumerator = class
  private
    FList: TPropertyItems;
    FPosition: Integer;
  public
    constructor Create(AList: TPropertyItems);
    function GetCurrent: TPropertyItem;
    function MoveNext: Boolean;
    property Current: TPropertyItem read GetCurrent;
  end;


  { TXSDComplexType }

  TXSDComplexType = class
  private
    FDescription: string;
    FInheritedType: string;
    FMainRoot: boolean;
    FPascalTypeName: string;
    FPropertys: TPropertyItems;
    FTypeName: string;
    FOwner: TXSDComplexTypes;
    FInheritedXSDComplexType:TXSDComplexType;
  private
  public
    constructor Create(AOwner:TXSDComplexTypes);
    destructor Destroy; override;
    procedure UpdatePascalNames;
    function InheritedTypeName:string;
    property TypeName:string read FTypeName write FTypeName;
    property PascalTypeName:string read FPascalTypeName write FPascalTypeName;
    property Propertys:TPropertyItems read FPropertys;
    property MainRoot:boolean read FMainRoot write FMainRoot;
    property Description:string read FDescription write FDescription;
    property InheritedType:string read FInheritedType write FInheritedType;
  end;

  { TXSDComplexTypes }

  TXSDComplexTypes = class
  private
    FList:TFPList;
    FOwner: TXSDModule;
    function GetCount: integer;
    function GetItems(AIndex: Integer): TXSDComplexType;
  private
    procedure SortInheritedList;
  public
    constructor Create(AOwner:TXSDModule);
    destructor Destroy; override;
    procedure Clear;
    procedure UpdatePascalNames;
    function GetEnumerator: TXSDComplexTypesEnumerator;
    function Add(ATypeName:string):TXSDComplexType;
    function FindType(const ATypeName:string):TXSDComplexType;
    property Count:integer read GetCount;
    property Items[AIndex:Integer]:TXSDComplexType read GetItems; default;
  end;

  { TXSDComplexTypesEnumerator }

  TXSDComplexTypesEnumerator = class
  private
    FList: TXSDComplexTypes;
    FPosition: Integer;
  public
    constructor Create(AList: TXSDComplexTypes);
    function GetCurrent: TXSDComplexType;
    function MoveNext: Boolean;
    property Current: TXSDComplexType read GetCurrent;
  end;

  { TXSDSimpleType }

  TXSDSimpleType = class
  private
    FBaseName: string;
    FDescription: string;
    FMaxLength: integer;
    FMinLength: integer;
    FPasBaseName: string;
    FPasTypeName: string;
    FTypeName: string;
    FOwner: TXSDModule;
    FValuesList: TStringList;
  public
    constructor Create(AOwner:TXSDModule);
    destructor Destroy; override;
    procedure UpdatePascalNames;
    property TypeName:string read FTypeName write FTypeName;
    property BaseName:string read FBaseName write FBaseName;
    property MaxLength:integer read FMaxLength write FMaxLength;
    property MinLength:integer read FMinLength write FMinLength;
    property Description:string read FDescription write FDescription;
    property PasBaseName:string read FPasBaseName write FPasBaseName;
    property PasTypeName:string read FPasTypeName write FPasTypeName;
    property ValuesList:TStringList read FValuesList;
    property ValuePattern:string read FValuePattern write FValuePattern;
  end;

  { TXSDSimpleTypes }

  TXSDSimpleTypes = class
  private
    FList:TFPList;
    FOwner: TXSDModule;
    function GetCount: integer;
    function GetItems(AIndex: Integer): TXSDSimpleType;
  public
    constructor Create(AOwner:TXSDModule);
    destructor Destroy; override;
    procedure Clear;
    function GetEnumerator: TXSDSimpleTypesEnumerator;
    function Add(ATypeName:string):TXSDSimpleType;
    function FindType(const ATypeName:string):TXSDSimpleType;
    property Count:integer read GetCount;
    procedure UpdatePascalNames;
    property Items[AIndex:Integer]:TXSDSimpleType read GetItems; default;
  end;

  { TXSDSimpleTypesEnumerator }

  TXSDSimpleTypesEnumerator = class
  private
    FList: TXSDSimpleTypes;
    FPosition: Integer;
  public
    constructor Create(AList: TXSDSimpleTypes);
    function GetCurrent: TXSDSimpleType;
    function MoveNext: Boolean;
    property Current: TXSDSimpleType read GetCurrent;
  end;

  { TXSDModule }

  TXSDModule = class
  private
    FComplexTypes: TXSDComplexTypes;
    FSimpleTypes: TXSDSimpleTypes;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure UpdatePascalNames;
    property ComplexTypes:TXSDComplexTypes read FComplexTypes;
    property SimpleTypes:TXSDSimpleTypes read FSimpleTypes;
  end;

implementation

uses xsdutils;

{ TPropertyItem }

constructor TPropertyItem.Create(AOwner: TXSDComplexType);
begin
  inherited Create;
  FValuesList:=TStringList.Create;
  FOwner:=AOwner;
  FMinLength:=-1;
  FMaxLength:=-1;
end;

destructor TPropertyItem.Destroy;
begin
  FreeAndNil(FValuesList);
  inherited Destroy;
end;

function TPropertyItem.PascalBaseType: string;
begin
  if Assigned(FXSDSimpleType) then
    Result:=FXSDSimpleType.PasTypeName
  else
  if Assigned(FXSDComplexType) then
  begin
    Result:=FXSDComplexType.PascalTypeName;
    if (MaxOccurs>1) or (MaxOccurs<0) then
      Result:=Result + 'List';
  end
  else
  begin
    Result:=GetSimpleType(FBaseType);
    if Result= '' then
      Result:=FBaseType;
  end;
end;

function TPropertyItem.PascalMinLength: string;
begin
  if Assigned(FXSDSimpleType) then
    Result:=IntToStr(FXSDSimpleType.FMinLength)
  else
    Result:=IntToStr(FMinLength)
end;

function TPropertyItem.PascalMaxLength: string;
begin
  if Assigned(FXSDSimpleType) then
    Result:=IntToStr(FXSDSimpleType.FMaxLength)
  else
    Result:=IntToStr(FMaxLength)
end;

function TPropertyItem.PascalName: string;
begin
  Result:=FPascalName;
end;

procedure TPropertyItem.UpdatePascalNames;
var
  S: String;
  I: Integer;
begin
  if not IsSimpleType(FBaseType) then
  begin
    FXSDSimpleType:=FOwner.FOwner.FOwner.SimpleTypes.FindType(FBaseType);
    if not Assigned(FXSDSimpleType) then
    begin
      FXSDComplexType:=FOwner.FOwner.FOwner.ComplexTypes.FindType(FBaseType);
      if Assigned(FXSDComplexType) then
        ItemType:=pitClass;
{      else
        ItemType:=pitAttribute;}
    end
{    else
      ItemType:=pitAttribute;}
  end;

  FPascalName:=Name;
  I:=0;
  while IsKeyword(FPascalName) or (Assigned(FOwner.Propertys.FindProperty(FPascalName))) do
  begin
    Inc(I);
    FPascalName:=Name+IntToStr(i);
  end;
end;

{ TXSDSimpleType }

constructor TXSDSimpleType.Create(AOwner: TXSDModule);
begin
  inherited Create;
  FValuesList:=TStringList.Create;
  FOwner:=AOwner;
  FMinLength:=-1;
  FMinLength:=-1;
end;

destructor TXSDSimpleType.Destroy;
begin
  FValuesList.Free;
  inherited Destroy;
end;

procedure TXSDSimpleType.UpdatePascalNames;
begin
  FPasTypeName:='T'+FTypeName;
end;

{ TXSDComplexType }

constructor TXSDComplexType.Create(AOwner: TXSDComplexTypes);
begin
  inherited Create;
  FOwner:=AOwner;
  FPropertys:=TPropertyItems.Create(Self);
end;

destructor TXSDComplexType.Destroy;
begin
  FreeAndNil(FPropertys);
  inherited Destroy;
end;

procedure TXSDComplexType.UpdatePascalNames;
var
  P: TPropertyItem;
begin
  for P in Propertys do P.UpdatePascalNames;
  PascalTypeName:='T'+TypeName;

  if InheritedType <> '' then
    FInheritedXSDComplexType:=FOwner.FindType(InheritedType);
end;

function TXSDComplexType.InheritedTypeName: string;
begin
  if Assigned(FInheritedXSDComplexType) then
    Result:=FInheritedXSDComplexType.PascalTypeName
  else
    Result:='TXmlSerializationObject';
end;

{ TPropertyItemsEnumerator }

constructor TPropertyItemsEnumerator.Create(AList: TPropertyItems);
begin
  FList := AList;
  FPosition := -1;
end;

function TPropertyItemsEnumerator.GetCurrent: TPropertyItem;
begin
  Result := FList[FPosition];
end;

function TPropertyItemsEnumerator.MoveNext: Boolean;
begin
  Inc(FPosition);
  Result := FPosition < FList.Count;
end;

{ TPropertyItems }

function TPropertyItems.GetCount: integer;
begin
  Result:=FList.Count;
end;

function TPropertyItems.GetItems(AIndex: Integer): TPropertyItem;
begin
  Result:=TPropertyItem(FList[AIndex]);
end;

constructor TPropertyItems.Create(AOwner: TXSDComplexType);
begin
  inherited Create;
  FOwner:=AOwner;
  FList:=TFPList.Create;
end;

destructor TPropertyItems.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TPropertyItems.Clear;
var
  i: Integer;
begin
  for i:=0 to FList.Count-1 do
    TPropertyItem(FList[i]).Free;
  FList.Clear;
end;

procedure TPropertyItems.UpdatePascalNames;
begin

end;

function TPropertyItems.GetEnumerator: TPropertyItemsEnumerator;
begin
  Result:=TPropertyItemsEnumerator.Create(Self);
end;

function TPropertyItems.Add(AItemType: TPropertyItemType): TPropertyItem;
begin
  Result:=TPropertyItem.Create(FOwner);
  Result.ItemType:=AItemType;
  FList.Add(Result);
end;

function TPropertyItems.FindProperty(AItemName: string): TPropertyItem;
begin
  Result:=nil;
  //AI
  //for P in Self ;
end;

{ TXSDSimpleTypesEnumerator }

constructor TXSDSimpleTypesEnumerator.Create(AList: TXSDSimpleTypes);
begin
  FList := AList;
  FPosition := -1;
end;

function TXSDSimpleTypesEnumerator.GetCurrent: TXSDSimpleType;
begin
  Result := FList[FPosition];
end;

function TXSDSimpleTypesEnumerator.MoveNext: Boolean;
begin
  Inc(FPosition);
  Result := FPosition < FList.Count;
end;

{ TXSDSimpleTypes }

function TXSDSimpleTypes.GetCount: integer;
begin
  Result:=FList.Count;
end;

function TXSDSimpleTypes.GetItems(AIndex: Integer): TXSDSimpleType;
begin
  Result:=TXSDSimpleType(FList[AIndex]);
end;

constructor TXSDSimpleTypes.Create(AOwner: TXSDModule);
begin
  inherited Create;
  FOwner:=AOwner;
  FList:=TFPList.Create;
end;

destructor TXSDSimpleTypes.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TXSDSimpleTypes.Clear;
var
  I: Integer;
begin
  for I:=0 to FList.Count-1 do
    TXSDSimpleType(FList[i]).Free;
  FList.Clear;
end;

function TXSDSimpleTypes.GetEnumerator: TXSDSimpleTypesEnumerator;
begin
  Result:=TXSDSimpleTypesEnumerator.Create(Self);
end;

function TXSDSimpleTypes.Add(ATypeName: string): TXSDSimpleType;
begin
  Result:=TXSDSimpleType.Create(FOwner);
  Result.TypeName:=ATypeName;
  FList.Add(Result);
end;

function TXSDSimpleTypes.FindType(const ATypeName: string): TXSDSimpleType;
var
  ST: TXSDSimpleType;
begin
  Result:=nil;
  for ST in Self do
    if ST.TypeName = ATypeName then
      Exit(ST);
end;

procedure TXSDSimpleTypes.UpdatePascalNames;
var
  ST: TXSDSimpleType;
begin
  for ST in Self do ST.UpdatePascalNames;
end;

{ TXSDComplexTypesEnumerator }

constructor TXSDComplexTypesEnumerator.Create(AList: TXSDComplexTypes);
begin
  FList := AList;
  FPosition := -1;
end;

function TXSDComplexTypesEnumerator.GetCurrent: TXSDComplexType;
begin
  Result := FList[FPosition];
end;

function TXSDComplexTypesEnumerator.MoveNext: Boolean;
begin
  Inc(FPosition);
  Result := FPosition < FList.Count;
end;

{ TXSDModule }

constructor TXSDModule.Create;
begin
  inherited Create;
  FComplexTypes:=TXSDComplexTypes.Create(Self);
  FSimpleTypes:=TXSDSimpleTypes.Create(Self);
end;

destructor TXSDModule.Destroy;
begin
  Clear;
  FComplexTypes.Free;
  FSimpleTypes.Free;
  inherited Destroy;
end;

procedure TXSDModule.Clear;
begin
  FComplexTypes.Clear;
  FSimpleTypes.Clear;
end;

procedure TXSDModule.UpdatePascalNames;
begin
  SimpleTypes.UpdatePascalNames;
  ComplexTypes.UpdatePascalNames;
end;

{ TXSDComplexTypes }

function TXSDComplexTypes.GetCount: integer;
begin
  Result:=FList.Count;
end;

function TXSDComplexTypes.GetItems(AIndex: Integer): TXSDComplexType;
begin
  Result:=TXSDComplexType(FList[AIndex]);
end;

procedure TXSDComplexTypes.SortInheritedList;
var
  CP, CP1: TXSDComplexType;
  i, J: Integer;
  Prop: TPropertyItem;
begin
(*  for i:=0 to FList.Count-2 do
  begin
    CP:=TXSDComplexType(FList[i]);
    for J:=i+1 to FList.Count-1 do
    begin
      CP1:=TXSDComplexType(FList[j]);
      for Prop in CP.Propertys do
      begin
        if Prop.FXSDComplexType = CP1 then
        begin
          FList[i]:=FList[j];
          FList[j]:=CP;
          CP:=TXSDComplexType(FList[i]);
          Break;
        end;
      end;
    end;
  end;
*)
  for i:=0 to FList.Count-2 do
  begin
    CP:=TXSDComplexType(FList[i]);
    for J:=i+1 to FList.Count-1 do
    begin
      CP1:=TXSDComplexType(FList[j]);
      if CP.FInheritedXSDComplexType = CP1  then
      begin
        FList[i]:=FList[j];
        FList[j]:=CP;
        CP:=TXSDComplexType(FList[i]);
        Continue;
      end;
    end;
  end;
(*
  for i:=0 to FList.Count-2 do
  begin
    CP:=TXSDComplexType(FList[i]);
    for J:=i+1 to FList.Count-1 do
    begin
      CP1:=TXSDComplexType(FList[j]);
      for Prop in CP.Propertys do
      begin
        if Prop.FXSDComplexType = CP1 then
        begin
          FList[i]:=FList[j];
          FList[j]:=CP;
          CP:=TXSDComplexType(FList[i]);
          Break;
        end;
      end;
    end;
  end;
*)
end;

constructor TXSDComplexTypes.Create(AOwner: TXSDModule);
begin
  inherited Create;
  FList:=TFPList.Create;
  FOwner:=AOwner;
end;

destructor TXSDComplexTypes.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

procedure TXSDComplexTypes.Clear;
var
  i: Integer;
begin
  for i:=0 to FList.Count-1 do
    TXSDComplexType(FList[i]).Free;
  FList.Clear;
end;

procedure TXSDComplexTypes.UpdatePascalNames;
var
  CT: TXSDComplexType;
begin
  for CT in Self do CT.UpdatePascalNames;
  SortInheritedList;
end;

function TXSDComplexTypes.GetEnumerator: TXSDComplexTypesEnumerator;
begin
  Result:=TXSDComplexTypesEnumerator.Create(Self);
end;

function TXSDComplexTypes.Add(ATypeName: string): TXSDComplexType;
begin
  Result:=TXSDComplexType.Create(Self);
  FList.Add(Result);
  Result.FTypeName:=ATypeName;
end;

function TXSDComplexTypes.FindType(const ATypeName: string): TXSDComplexType;
var
  CT: TXSDComplexType;
begin
  Result:=nil;
  for CT in Self do
    if CT.TypeName = ATypeName then
      Exit(CT);
end;

end.

