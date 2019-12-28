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

unit XsdPasCodegenUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, XsdElementTypesUnit;

type
  TCodeGenDescribeOption = (cgdoDescribeTypes, cgdoDescribeClasses, cgdoDescribeClassProperty);
  TCodeGenDescribeOptions = set of TCodeGenDescribeOption;

  { TXsdPasCodegen }

  TXsdPasCodegen = class
  private
    FDescribeOptions: TCodeGenDescribeOptions;
    FPasUnitDescription: string;
    FPasUnitName: string;
    FXSDModule: TXSDModule;
    function DoGenUnitHeader:string;
    function DoGenInterfaceUses:string;
    function DoGenImplementationUses:string;
    function DoGenClasseInterface:string;
    function DoGenClasseImplementation:string;
    function DoGenSimpleTypes:string;
  public
    constructor Create(AXSDModule:TXSDModule);
    destructor Destroy; override;
    function GeneratePasCode:string;
    property PasUnitName:string read FPasUnitName write FPasUnitName;
    property PasUnitDescription:string read FPasUnitDescription write FPasUnitDescription;
    property DescribeOptions:TCodeGenDescribeOptions read FDescribeOptions write FDescribeOptions;
  end;

implementation

{ TXsdPasCodegen }

function TXsdPasCodegen.DoGenUnitHeader: string;
begin
  Result:='unit '+PasUnitName + ';'+LineEnding+LineEnding+'{$mode objfpc}{$H+}'+LineEnding+LineEnding + 'interface'+LineEnding+LineEnding;
end;

function TXsdPasCodegen.DoGenInterfaceUses: string;
begin
  Result:='uses'+LineEnding+'  Classes, SysUtils, xmlobject;'+LineEnding+LineEnding;
end;

function TXsdPasCodegen.DoGenImplementationUses: string;
begin
  Result:='implementation' + LineEnding + LineEnding

end;

function TXsdPasCodegen.DoGenClasseInterface: string;
var
  CT: TXSDComplexType;
  PT: TPropertyItem;
begin
  Result:='';
  for CT in FXSDModule.ComplexTypes do
  begin
    Result:=Result + '  T' + CT.TypeName + ' = class;'+LineEnding;
  end;
  Result:=Result + LineEnding;

  for CT in FXSDModule.ComplexTypes do
  begin
    Result:=Result + '  {  T' + CT.TypeName + '  }'+LineEnding;

    if (cgdoDescribeClasses in FDescribeOptions)  and (CT.Description <> '') then
      Result:=Result + '  {  ' + TrimRight(CT.Description) + '  }'+LineEnding;

    Result:=Result + '  T' + CT.TypeName + ' = class(TXmlSerializationObject)'+LineEnding + '  private' + LineEnding;
    for PT in CT.Propertys do
      Result:=Result + '    F' + PT.Name + ':' + PT.BaseType + ';'+LineEnding;

    for PT in CT.Propertys do
      if PT.ItemType in [pitAttribute, pitSimpleType] then
        Result:=Result + '    procedure Set' + PT.Name + '( AValue:' + PT.BaseType + ');'+LineEnding;

    Result:=Result+
    '  protected'+LineEnding+
    '    procedure InternalRegisterPropertys; override;'+LineEnding+
    '    procedure InternalInitChilds; override;'+LineEnding;

    if CT.MainRoot then
      Result:=Result+'    function RootNodeName:string; override;'+LineEnding;

    Result:=Result+
    '  public'+LineEnding+
    '    destructor Destroy; override;'+LineEnding+
    '  published'+LineEnding;

    for PT in CT.Propertys do
    begin
      //if (cgdoDescribeClassProperty in FDescribeOptions) and (PT.Description <> '') then Result:=Result + '    {' + TrimRight(PT.Description) + '}' + LineEnding;
      Result:=Result + '    property '+PT.Name + ':'+PT.BaseType + ' read F'+PT.Name;
      if PT.ItemType in [pitAttribute, pitSimpleType] then
        Result:=Result + ' write Set' + PT.Name;

      if (cgdoDescribeClassProperty in FDescribeOptions) and (PT.Description <> '') then Result:=Result + '    {' + TrimRight(PT.Description) + '}';

      Result:=Result+ ';'+LineEnding;
    end;
    Result:=Result +
    '  end;'+LineEnding+
    '  T' + CT.TypeName +'s = specialize GXMLSerializationObjectList<T'+CT.TypeName+'>;' + LineEnding + LineEnding;
  end;
end;

function TXsdPasCodegen.DoGenClasseImplementation: string;
var
  CT: TXSDComplexType;
  PT: TPropertyItem;
  SAttr: String;
begin
  Result:='';
  for CT in FXSDModule.ComplexTypes do
  begin
     Result:=Result + '  {  T'+CT.TypeName + '  }'+LineEnding;

     for PT in CT.Propertys do
     begin
       if PT.ItemType in [pitAttribute, pitSimpleType] then
       begin
         Result:=Result +
          'procedure T'+CT.TypeName+'.Set'+PT.Name+'(AValue: '+PT.BaseType+');'+LineEnding +
          'begin'+LineEnding+
          '  if F'+PT.Name+'=AValue then Exit;'+LineEnding+
          '  F'+PT.Name+':=AValue;'+LineEnding+
          '  ModifiedProperty('''+PT.Name+''');'+LineEnding+
          'end;'+LineEnding+LineEnding;
       end;
     end;

     Result:=Result +
      'procedure T'+CT.TypeName+'.InternalRegisterPropertys;'+LineEnding +
      'begin'+LineEnding;
     for PT in CT.Propertys do
     begin

       SAttr:='';
       if PT.ItemType = pitSimpleType then
         SAttr:='xsaSimpleObject';
       //xsaRequared

       Result:=Result +
        '  RegisterProperty('''+PT.Name+''', '''+PT.Name+''', ['+SAttr+'], '''', 0, 250);'+LineEnding;
     end;
     Result:=Result + 'end;'+LineEnding+LineEnding;

     Result:=Result +
      'procedure T'+CT.TypeName+'.InternalInitChilds;'+LineEnding +
      'begin'+LineEnding;
     for PT in CT.Propertys do
     begin
       if PT.ItemType = pitClass then
        Result:=Result +
         '  F'+PT.Name+':=T'+PT.Name+'.Create;'+LineEnding;
     end;
     Result:=Result + 'end;'+LineEnding+LineEnding;

     Result:=Result +
      'destructor T'+CT.TypeName+'.Destroy;'+LineEnding +
      'begin'+LineEnding;
     for PT in CT.Propertys do
     begin
       if PT.ItemType = pitClass then
        Result:=Result +
         '  F'+PT.Name+'.Free;'+LineEnding;
     end;
     Result:=Result + '  inherited Destroy;'+LineEnding+'end;'+LineEnding+LineEnding;

    if CT.MainRoot then
      Result:=Result+'function T'+CT.TypeName+'.RootNodeName:string;'+LineEnding+
      'begin'+LineEnding+
      '  Result:='''+CT.TypeName+''';'+LineEnding+
      'end;'+LineEnding+LineEnding;
  end;
end;

function TXsdPasCodegen.DoGenSimpleTypes: string;
var
  ST: TXSDSimpleType;
begin
  Result:='';
  for ST in FXSDModule.SimpleTypes do
  begin
    if (cgdoDescribeTypes in FDescribeOptions) and (ST.Description <> '') then
       Result:=Result + '{'+ST.Description+ '}' + LineEnding;
    Result:=Result + '  T' + ST.TypeName + ' = ' + ST.PasBaseName + ';' + LineEnding;
  end;
  if Result <> '' then
    Result:='type' + LineEnding + Result;
end;

constructor TXsdPasCodegen.Create(AXSDModule: TXSDModule);
begin
  inherited Create;
  FXSDModule:=AXSDModule;
end;

destructor TXsdPasCodegen.Destroy;
begin
  inherited Destroy;
end;

function TXsdPasCodegen.GeneratePasCode: string;
var
  S: String;
begin
  Result:=DoGenUnitHeader + DoGenInterfaceUses;

  S:=DoGenClasseInterface;

  if S<>'' then
    S:='type'+LineEnding + S;

  Result:=Result + DoGenSimpleTypes +
    S +
    DoGenImplementationUses + DoGenClasseImplementation + 'end.';
end;

end.

