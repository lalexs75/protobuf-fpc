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
  TXSDComplexTypesEnumerator = class;
  TXSDSimpleTypesEnumerator = class;
  TPropertyItemsEnumerator = class;

  TPropertyItemType = (pitClass, pitAttribute, pitSimpleType);

  { TPropertyItem }

  TPropertyItem = class
  private
    FBaseType: string;
    FDescription: string;
    FItemType: TPropertyItemType;
    FName: string;
  published
    property Name:string read FName write FName;
    property BaseType:string read FBaseType write FBaseType;
    property ItemType:TPropertyItemType read FItemType write FItemType;
    property Description:string read FDescription write FDescription;
  end;

  { TPropertyItems }

  TPropertyItems = class
  private
    FList:TFPList;
    function GetCount: integer;
    function GetItems(AIndex: Integer): TPropertyItem;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function GetEnumerator: TPropertyItemsEnumerator;
    function Add(AItemType:TPropertyItemType):TPropertyItem;
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
    FMainRoot: boolean;
    FPropertys: TPropertyItems;
    FTypeName: string;
  public
    constructor Create;
    destructor Destroy; override;
    property TypeName:string read FTypeName write FTypeName;
    property Propertys:TPropertyItems read FPropertys;
    property MainRoot:boolean read FMainRoot write FMainRoot;
    property Description:string read FDescription write FDescription;
  end;

  { TXSDComplexTypes }

  TXSDComplexTypes = class
  private
    FList:TFPList;
    function GetCount: integer;
    function GetItems(AIndex: Integer): TXSDComplexType;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function GetEnumerator: TXSDComplexTypesEnumerator;
    function Add(ATypeName:string):TXSDComplexType;
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
    FTypeName: string;
  public
    property TypeName:string read FTypeName write FTypeName;
    property BaseName:string read FBaseName write FBaseName;
    property PasBaseName:string read FPasBaseName write FPasBaseName;
    property MaxLength:integer read FMaxLength write FMaxLength;
    property MinLength:integer read FMinLength write FMinLength;
    property Description:string read FDescription write FDescription;
  end;

  { TXSDSimpleTypes }

  TXSDSimpleTypes = class
  private
    FList:TFPList;
    function GetCount: integer;
    function GetItems(AIndex: Integer): TXSDSimpleType;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function GetEnumerator: TXSDSimpleTypesEnumerator;
    function Add(ATypeName:string):TXSDSimpleType;
    property Count:integer read GetCount;
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
    property ComplexTypes:TXSDComplexTypes read FComplexTypes;
    property SimpleTypes:TXSDSimpleTypes read FSimpleTypes;
  end;

implementation

{ TXSDComplexType }

constructor TXSDComplexType.Create;
begin
  inherited Create;
  FPropertys:=TPropertyItems.Create;
end;

destructor TXSDComplexType.Destroy;
begin
  FreeAndNil(FPropertys);
  inherited Destroy;
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

constructor TPropertyItems.Create;
begin
  inherited Create;
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

function TPropertyItems.GetEnumerator: TPropertyItemsEnumerator;
begin
  Result:=TPropertyItemsEnumerator.Create(Self);
end;

function TPropertyItems.Add(AItemType: TPropertyItemType): TPropertyItem;
begin
  Result:=TPropertyItem.Create;
  Result.ItemType:=AItemType;
  FList.Add(Result);
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

constructor TXSDSimpleTypes.Create;
begin
  inherited Create;
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
  Result:=TXSDSimpleType.Create;
  Result.TypeName:=ATypeName;
  FList.Add(Result);
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
  FComplexTypes:=TXSDComplexTypes.Create;
  FSimpleTypes:=TXSDSimpleTypes.Create;
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

{ TXSDComplexTypes }

function TXSDComplexTypes.GetCount: integer;
begin
  Result:=FList.Count;
end;

function TXSDComplexTypes.GetItems(AIndex: Integer): TXSDComplexType;
begin
  Result:=TXSDComplexType(FList[AIndex]);
end;

constructor TXSDComplexTypes.Create;
begin
  inherited Create;
  FList:=TFPList.Create;
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

function TXSDComplexTypes.GetEnumerator: TXSDComplexTypesEnumerator;
begin
  Result:=TXSDComplexTypesEnumerator.Create(Self);
end;

function TXSDComplexTypes.Add(ATypeName: string): TXSDComplexType;
begin
  Result:=TXSDComplexType.Create;
  FList.Add(Result);
  Result.FTypeName:=ATypeName;
end;

end.

