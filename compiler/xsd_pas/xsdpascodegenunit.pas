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
{$DEFINE XSD_FORWARD_DECLARE}
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
    FLicenseHeader: string;
    FPasUnitDescription: string;
    FPasUnitName: string;
    FPasUnitOutputFolder: string;
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
    procedure GeneratePasCodeToFile(AFileName:string);

    property XSDModule:TXSDModule read FXSDModule write FXSDModule;
    property PasUnitName:string read FPasUnitName write FPasUnitName;
    property PasUnitOutputFolder:string read FPasUnitOutputFolder write FPasUnitOutputFolder;
    property PasUnitDescription:string read FPasUnitDescription write FPasUnitDescription;
    property DescribeOptions:TCodeGenDescribeOptions read FDescribeOptions write FDescribeOptions;
    property LicenseHeader:string read FLicenseHeader write FLicenseHeader;
  end;

implementation
uses LazFileUtils, xsdutils, rxstrutils;

{ TXsdPasCodegen }

function TXsdPasCodegen.DoGenUnitHeader: string;
var
  S: String;
begin
  S:=FileToString(FLicenseHeader);
  if S<>'' then S:=S + LineEnding;
  Result:=S + 'unit '+PasUnitName + ';'+LineEnding+LineEnding+'{$mode objfpc}{$H+}'+LineEnding+LineEnding + 'interface'+LineEnding+LineEnding;
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
  {$IFDEF XSD_FORWARD_DECLARE}
  for CT in FXSDModule.ComplexTypes do
    Result:=Result + '  ' + CT.PascalTypeName + ' = class;'+LineEnding;
  for CT in FXSDModule.ComplexTypes do
    Result:=Result + '  ' + CT.PascalTypeName +'List = specialize GXMLSerializationObjectList<'+CT.PascalTypeName+'>;' + LineEnding;
  {$ENDIF}
  Result:=Result + LineEnding;

  for CT in FXSDModule.ComplexTypes do
  begin
    Result:=Result + '  {  ' + CT.PascalTypeName + '  }'+LineEnding;

    if (cgdoDescribeClasses in FDescribeOptions)  and (CT.Description <> '') then
      Result:=Result + GenerateTypeDescription(CT.Description) {'  {  ' + TrimRight(CT.Description) + '  }'+LineEnding};

    Result:=Result + '  ' + CT.PascalTypeName + ' = class(' + CT.InheritedTypeName + ')'+LineEnding + '  private' + LineEnding;
    for PT in CT.Propertys do
      Result:=Result + '    F' + PT.PascalName + ':' + PT.PascalBaseType + ';'+LineEnding;

    for PT in CT.Propertys do
      if PT.ItemType in [pitAttribute, pitSimpleType] then
        Result:=Result + '    procedure Set' + PT.PascalName + '( AValue:' + PT.PascalBaseType + ');'+LineEnding;

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
      if (cgdoDescribeClassProperty in FDescribeOptions) and (PT.Description <> '') then Result:=Result + GenerateTypeDescription(PT.Description, 4);
      Result:=Result + '    property '+PT.PascalName + ':'+PT.PascalBaseType + ' read F'+PT.PascalName;
      if PT.ItemType in [pitAttribute, pitSimpleType] then
        Result:=Result + ' write Set' + PT.PascalName;

//      if (cgdoDescribeClassProperty in FDescribeOptions) and (PT.Description <> '') then Result:=Result + '    {' + TrimRight(PT.Description) + '}';

      Result:=Result+ ';'+LineEnding;
    end;
    Result:=Result + '  end;'+ LineEnding
    {$IFNDEF XSD_FORWARD_DECLARE}
      + '  ' + CT.PascalTypeName +'List = specialize GXMLSerializationObjectList<'+CT.PascalTypeName+'>;' + LineEnding
    {$ENDIF}
      + LineEnding;
  end;
end;

function TXsdPasCodegen.DoGenClasseImplementation: string;
var
  CT: TXSDComplexType;
  PT: TPropertyItem;
  SAttr, S: String;
begin
  Result:='';
  for CT in FXSDModule.ComplexTypes do
  begin
     Result:=Result + '  {  '+CT.PascalTypeName + '  }'+LineEnding;

     for PT in CT.Propertys do
     begin
       if PT.ItemType in [pitAttribute, pitSimpleType] then
       begin
         Result:=Result +
          'procedure '+CT.PascalTypeName+'.Set'+PT.PascalName+'(AValue: '+PT.PascalBaseType+');'+LineEnding +
          'begin'+LineEnding+
          '  if F'+PT.PascalName+'=AValue then Exit;'+LineEnding+
          '  F'+PT.PascalName+':=AValue;'+LineEnding;

         if PT.PascalValuesListCount > 0 then
           Result:=Result +
             '  CheckLockupValue('''+PT.PascalName+''', AValue);'+LineEnding;
         if PT.PascalMinLength > -1 then
           Result:=Result +
             '  CheckStrMinSize('''+PT.PascalName+''', AValue);'+LineEnding;
         if PT.PascalMaxLength > -1 then
           Result:=Result +
             '  CheckStrMaxSize('''+PT.PascalName+''', AValue);'+LineEnding;
         Result:=Result +
          '  ModifiedProperty('''+PT.PascalName+''');'+LineEnding+
          'end;'+LineEnding+LineEnding;
       end;
     end;

     Result:=Result +
      'procedure '+CT.PascalTypeName+'.InternalRegisterPropertys;'+LineEnding +
      'var'+LineEnding +
      '  P: TPropertyDef;'+LineEnding +
      'begin'+LineEnding +
      '  inherited InternalRegisterPropertys;' + LineEnding;
     for PT in CT.Propertys do
     begin

       SAttr:='';
       if PT.ItemType = pitSimpleType then
         SAttr:='xsaSimpleObject, ';

       if PT.IsRequired then
         SAttr:=SAttr + 'xsaRequared, ';

       Result:=Result +
        '  P:=RegisterProperty('''+PT.PascalName+''', '''+PT.Name+''', ['+Copy(SAttr, 1, Length(SAttr)-2)+'], '''', '+ IntToStr(PT.PascalMinLength)+', '+IntToStr(PT.PascalMaxLength)+');'+LineEnding;
       if Assigned(PT.XSDSimpleType) then
       begin
         if PT.XSDSimpleType.ValuesList.Count > 0 then
           for S in PT.XSDSimpleType.ValuesList do
             Result:=Result +
               '    P.ValidList.Add('+QuotedStr(S)+');' + LineEnding;
         if PT.XSDSimpleType.TotalDigits > 0 then
         begin
           Result:=Result +
           '    P.TotalDigits := '+IntToStr(PT.XSDSimpleType.TotalDigits)+';' + LineEnding +
           '    P.FractionDigits := '+IntToStr(PT.XSDSimpleType.FractionDigits)+';' + LineEnding;
         end;
       end;

       if PT.ItemType in [pitAttribute, pitSimpleType] then
       begin
         if not Assigned(PT.XSDSimpleType) then
         begin
           if PT.ValuesList.Count > 0 then
             for S in PT.ValuesList do
               Result:=Result +
                 '    P.ValidList.Add('+QuotedStr(S)+');' + LineEnding;
           if PT.TotalDigits > 0 then
             Result:=Result +
             '    P.TotalDigits := '+IntToStr(PT.TotalDigits)+';' + LineEnding +
             '    P.FractionDigits := '+IntToStr(PT.FractionDigits)+';' + LineEnding;
         end;
         if PT.DefaultValue <> '' then
           Result:=Result + '    P.DefaultValue:='+QuotedStr(PT.DefaultValue)+';'+LineEnding;
       end;
     end;
     Result:=Result + 'end;'+LineEnding+LineEnding;

     Result:=Result +
      'procedure '+CT.PascalTypeName+'.InternalInitChilds;'+LineEnding +
      'begin'+LineEnding+
      '  inherited InternalInitChilds;'+LineEnding;
     for PT in CT.Propertys do
     begin
       if PT.ItemType = pitClass then
        Result:=Result +
         '  F'+PT.PascalName+':='+PT.PascalBaseType + '.Create;'+LineEnding;
     end;
     Result:=Result + 'end;'+LineEnding+LineEnding;

     Result:=Result +
      'destructor '+CT.PascalTypeName+'.Destroy;'+LineEnding +
      'begin'+LineEnding;
     for PT in CT.Propertys do
     begin
       if PT.ItemType = pitClass then
        Result:=Result +
         '  F'+PT.PascalName+'.Free;'+LineEnding;
     end;
     Result:=Result + '  inherited Destroy;'+LineEnding+'end;'+LineEnding+LineEnding;

    if CT.MainRoot then
      Result:=Result+'function '+CT.PascalTypeName+'.RootNodeName:string;'+LineEnding+
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
       Result:=Result + GenerateTypeDescription(ST.Description) {'{'+ST.Description+ '}' + LineEnding};
    Result:=Result + '  ' + ST.PasTypeName + ' = ' + ST.PasBaseName + ';' + LineEnding;
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

procedure TXsdPasCodegen.GeneratePasCodeToFile(AFileName: string);
var
  F: TFileStream;
  S: String;
begin
  if AFileName = '' then
  begin
    if FPasUnitOutputFolder<>'' then
      AFileName:=AppendPathDelim(FPasUnitOutputFolder);
    AFileName:=AFileName + PasUnitName + '.pas';
  end;
  F:=TFileStream.Create(AFileName, fmCreate);
  try
    S:=GeneratePasCode;
    if S<>'' then
      F.Write(S[1], Length(S));
  finally
    F.Free;
  end;
end;

end.

