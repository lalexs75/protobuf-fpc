unit XsdPasCodegenUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, XsdElementTypesUnit;

type

  { TXsdPasCodegen }

  TXsdPasCodegen = class
  private
    FPasUnitDescription: string;
    FPasUnitName: string;
    FXSDModule: TXSDModule;
    function DoGenUnitHeader:string;
    function DoGenInterfaceUses:string;
    function DoGenImplementationUses:string;
    function DoGenClasseInterface:string;
    function DoGenClasseImplementation:string;

  public
    constructor Create(AXSDModule:TXSDModule);
    destructor Destroy; override;
    function GeneratePasCode:string;
    property PasUnitName:string read FPasUnitName write FPasUnitName;
    property PasUnitDescription:string read FPasUnitDescription write FPasUnitDescription;
  end;

implementation

{ TXsdPasCodegen }

function TXsdPasCodegen.DoGenUnitHeader: string;
begin
  Result:='unit '+PasUnitName + ';'+LineEnding+LineEnding+'{$mode objfpc}{$H+}'+LineEnding+LineEnding + 'interface'+LineEnding+LineEnding;
end;

function TXsdPasCodegen.DoGenInterfaceUses: string;
begin

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
    Result:=Result + '  {  T' + CT.TypeName + '  }'+LineEnding;
    if CT.Description <> '' then
    Result:=Result + '  {  ' + TrimRight(CT.Description) + '  }'+LineEnding;

    Result:=Result + '  T' + CT.TypeName + ' = class(TXmlSerializationObject);'+LineEnding + '  private' + LineEnding;
    for PT in CT.Propertys do
      Result:=Result + '    F' + PT.Name + ':' + PT.BaseType + ';'+LineEnding;

    for PT in CT.Propertys do
      if PT.ItemType = pitAttribute then
        Result:=Result + '    procedure Set' + PT.Name + '( AValue:' + PT.BaseType + ');'+LineEnding;

    Result:=Result+
    '  protected'+LineEnding+
    '    procedure InternalRegisterPropertys; override;'+LineEnding+
    '    procedure InternalInitChilds; override;'+LineEnding+
    '  public'+LineEnding+
    '    destructor Destroy; override;'+LineEnding+
    '  published'+LineEnding;

    for PT in CT.Propertys do
    begin
      if PT.Description <> '' then Result:=Result + '    {' + TrimRight(PT.Description) + '}' + LineEnding;
      Result:=Result + '    property '+PT.Name + ':'+PT.BaseType + ' read F'+PT.Name;
      if PT.ItemType = pitAttribute then
        Result:=Result + ' write Set' + PT.Name;
      Result:=Result+ ';'+LineEnding;
    end;
    Result:=Result +
    '  end;'+LineEnding+
    '  TInvoiceItems = specialize GXMLSerializationObjectList<'+CT.TypeName+'>;' + LineEnding + LineEnding;
  end;
end;

function TXsdPasCodegen.DoGenClasseImplementation: string;
var
  CT: TXSDComplexType;
  PT: TPropertyItem;
begin
  Result:='';
  for CT in FXSDModule.ComplexTypes do
  begin
     Result:=Result + '  {  T'+CT.TypeName + '  }'+LineEnding;

     for PT in CT.Propertys do
     begin
       if PT.ItemType = pitAttribute then
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
      Result:=Result +
      '  RegisterProperty('''+PT.Name+''', '''+PT.Name+''', '''', '''', 0, 250);'+LineEnding;
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
  end;
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

  Result:=Result + S + DoGenImplementationUses + DoGenClasseImplementation + 'end.';
end;

end.

