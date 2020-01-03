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

unit XsdProcessorUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DOM, XsdElementTypesUnit;

type
  TXSDProcessor = class;

  TOnProcessNodeEvent = procedure(Sender:TXSDProcessor; ANodeName:string; AMessage:string) of object;
  { TXSDProcessor }

  TXSDProcessor = class
  private
    FDoc: TXMLDocument;
    FSchema: TDOMNode;
    FOnProcessNodeEvent: TOnProcessNodeEvent;
    FXSDModule: TXSDModule;
    function FindSchemaElent(AName:string):TDOMNode;
  protected
    procedure DoProcessNodeMsg(ANodeName:string; AMessage:string);

    procedure ProcessSchema(ANode:TDOMNode);
    procedure ProcessElement(ANode:TDOMNode);
    procedure ProcessComplexElement(ANode, AContext:TDOMNode; AComplexType: TXSDComplexType);
    procedure ProcessSimpleType(AContext: TDOMNode; ASimpleType: TXSDSimpleType);
    function GetAnnotation(AContext:TDOMNode):string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure LoadFromFile(AFileName:string);
    function ExecuteProcessor:TXSDModule;
    property OnProcessNodeEvent:TOnProcessNodeEvent read FOnProcessNodeEvent write FOnProcessNodeEvent;
  end;

implementation
uses XMLRead, xsdutils;

{ TXSDProcessor }

function TXSDProcessor.FindSchemaElent(AName: string): TDOMNode;
var
  N, R: TDOMNode;
  I: Integer;
  S: DOMString;
begin
  Result:=nil;

  for I:=0 to FSchema.ChildNodes.Count-1 do
  begin
    N:=FSchema.ChildNodes[i];
    S:=N.NodeName;
    if (S = 'xs:element') or (S = 'xs:attribute') then
      R:=N.Attributes.GetNamedItem('name')
    else
      R:=nil;

    if Assigned(R) and (R.NodeValue = AName) then
      Exit(N);
  end;

  raise Exception.CreateFmt('Not find element "%s" in schema', [AName]);
end;

procedure TXSDProcessor.DoProcessNodeMsg(ANodeName: string; AMessage: string);
begin
  if Assigned(FOnProcessNodeEvent) then
    FOnProcessNodeEvent(Self, ANodeName, AMessage);
end;

procedure TXSDProcessor.ProcessSchema(ANode: TDOMNode);
var
  i: Integer;
  N, R: TDOMNode;
  S: DOMString;
  CT: TXSDComplexType;
begin
  if not Assigned(ANode) then Exit;
  //load simple type list
  for i:=0 to ANode.ChildNodes.Count - 1 do
  begin
    N:=ANode.ChildNodes[i];
    S:=N.NodeName;
    if (S = 'xs:simpleType') then
    begin
      R:=N.Attributes.GetNamedItem('name');
      DoProcessNodeMsg(R.NodeName, R.NodeValue);
      ProcessSimpleType(N, FXSDModule.SimpleTypes.Add(R.NodeValue));
    end;
  end;

  //load clases
  for i:=0 to ANode.ChildNodes.Count - 1 do
  begin
    N:=ANode.ChildNodes[i];
    S:=N.NodeName;
    if (S <> 'xs:simpleType') then
    begin
      DoProcessNodeMsg(S, N.NodeValue);
      if (S = 'xs:element')  then
      begin
        ProcessElement(N)
      end
      else
      if (S = 'xs:complexType') then
      begin
        R:=N.Attributes.GetNamedItem('name');
        DoProcessNodeMsg(R.NodeName, R.NodeValue);
        CT:=FXSDModule.ComplexTypes.Add(R.NodeValue);
        ProcessComplexElement( N, N, CT);
      end;
    end;
  end;
end;

procedure TXSDProcessor.ProcessElement(ANode: TDOMNode);
var
  R, RName: TDOMNode;
  FComplexType: TXSDComplexType;
begin
  RName:=ANode.Attributes.GetNamedItem('name');
  if Assigned(RName) then
    DoProcessNodeMsg(RName.NodeName, RName.NodeValue);

  R:=ANode.Attributes.GetNamedItem('type');
  if Assigned(R) then
    DoProcessNodeMsg(R.NodeName, R.NodeValue)
  else
  begin
    R:=ANode.FindNode('xs:complexType');
    if Assigned(R) then
    begin
      FComplexType:=FXSDModule.ComplexTypes.Add(RName.NodeValue);
      FComplexType.MainRoot:=true;
      ProcessComplexElement(ANode, R, FComplexType)
    end;
  end;
end;

procedure TXSDProcessor.ProcessComplexElement(ANode, AContext: TDOMNode;
  AComplexType: TXSDComplexType);

procedure ProcessAttribute(FA:TDOMNode);
var
  Prop: TPropertyItem;
  R, R1, FC: TDOMNode;
begin
  R:=FA.Attributes.GetNamedItem('ref'); //ref=QName
  if Assigned(R) then
    FC:=FindSchemaElent(R.NodeValue)
  else
    FC:=FA;

  Prop:=AComplexType.Propertys.Add(pitAttribute);
  Prop.Name:=FC.Attributes.GetNamedItem('name').NodeValue;
  Prop.Description:=GetAnnotation(FC);

  R:=FC.Attributes.GetNamedItem('type');
  if Assigned(R) then
    Prop.BaseType:=R.NodeValue
  else
  begin
    R:=FC.FindNode('xs:simpleType');
    if Assigned(R) then
    begin
      R1:=R.FindNode('xs:restriction');
      if Assigned(R1) then
      begin
        Prop.BaseType:=R1.Attributes.GetNamedItem('base').NodeValue;
        if IsSimpleType(Prop.BaseType) then
          Prop.BaseType:=GetSimpleType(Prop.BaseType);
          //Prop.BaseType:=R1.Attributes.GetNamedItem('base');
      end
    end
    else
      Prop.BaseType:='String'; //GetSimpleType(Prop.BaseType);
  end;


     //default=строка
     //fixed=строка
     //form=qualified | unqualified
     //id=идентификатор
     //name=NCName
     //type=QName
  R:=FA.Attributes.GetNamedItem('use'); //use=optional | prohibited | required
  if Assigned(R) then
    Prop.IsRequired:=R.NodeValue = 'required';
end;

procedure ProcessAS(RAll:TDOMNode);

function DoAttributeProp(FA: TDOMNode):TPropertyItem;
var
  R: TDOMNode;
begin
  R:=FA.Attributes.GetNamedItem('type');
  if Assigned(R) then
  begin
    if IsSimpleType(R.NodeValue) then
    begin
      Result:=AComplexType.Propertys.Add(pitSimpleType);
      Result.BaseType:=GetSimpleType(R.NodeValue);
    end
    else
    begin
      Result:=AComplexType.Propertys.Add(pitClass);
      Result.BaseType:=R.NodeValue;
      //Result.IsArray:= RAll.NodeName = 'xs:sequence';
    end;
    Result.Name:=FA.Attributes.GetNamedItem('name').NodeValue;
    Result.Description:=GetAnnotation(FA);
  end
  else
  begin
    R:=FA.FindNode('xs:complexType');
    if Assigned(R) then
    begin
      Result:=AComplexType.Propertys.Add(pitClass);
      Result.BaseType:=FA.Attributes.GetNamedItem('name').NodeValue;
      //Result.IsArray:= RAll.NodeName = 'xs:sequence';
      Result.Name:=FA.Attributes.GetNamedItem('name').NodeValue;
      Result.Description:=GetAnnotation(FA);
    end;
  end;
end;

function DoComplexType(FA, FC: TDOMNode):TPropertyItem; //TXSDComplexType;
var
  S:string;
  Prop: TPropertyItem;
  CT:TXSDComplexType;
begin
  S:=AComplexType.TypeName  +  '_' + FA.Attributes.GetNamedItem('name').NodeValue;
  CT:=FXSDModule.ComplexTypes.Add(S);
  ProcessComplexElement(FC, FC, CT);

  Result:=AComplexType.Propertys.Add(pitClass);
  Result.BaseType:=S;
  Result.Name:=FA.Attributes.GetNamedItem('name').NodeValue;
end;

var
  i, FMaxOccurs, FMinOccurs: Integer;
  FA, FC, R, RMinOccurs, RMaxOccurs: TDOMNode;
  Prop: TPropertyItem;
begin
  for i:=0 to RAll.ChildNodes.Count-1 do
  begin
    Prop:=nil;
    FA:=RAll.ChildNodes[i];
    if FA.NodeName = 'xs:element' then
    begin
      RMaxOccurs:=FA.Attributes.GetNamedItem('maxOccurs');
      if Assigned(RMaxOccurs) then
      begin
        if RMaxOccurs.NodeValue = 'unbounded' then
          FMaxOccurs:=-1
        else
          FMaxOccurs:=StrToIntDef(RMaxOccurs.NodeValue, 1);
      end
      else
        FMaxOccurs:=1;

      RMinOccurs:=FA.Attributes.GetNamedItem('minOccurs');
      if Assigned(RMinOccurs) then
        FMinOccurs:=StrToIntDef(RMinOccurs.NodeValue, 1)
      else
        FMinOccurs:=1;

      FC:=FA.Attributes.GetNamedItem('ref');
      if Assigned(FC) then
      begin
        FA:=FindSchemaElent(FC.NodeValue);
        Prop:=DoAttributeProp(FA)
      end
      else
      begin
        FC:=FA.FindNode('xs:complexType');
        if Assigned(FC) then
          Prop:=DoComplexType(FA, FC)
        else
        begin
          Prop:=DoAttributeProp(FA);
          Prop.ItemType:=pitSimpleType;
        end;
      end;

      if Assigned(Prop) then
      begin
        Prop.MaxOccurs:=FMaxOccurs;
        Prop.MinOccurs:=FMinOccurs;
      end;
       //id=идентификатор
       //substitutionGroup=QName
       //default=string
       //fixed=string
       //form=qualified | unqualified
       //nillable=true|false
       //abstract=true|false
       //block=(#all | список (extension|restriction))
       //final=(#all | список (extension|restriction))
    end;
  end;
end;

var
  FA , FC1: TDOMNode;
  i: Integer;
  S, S1: DOMString;
begin
  DoProcessNodeMsg(ANode.NodeName, ANode.NodeValue);
  AComplexType.Description:=GetAnnotation(AContext);

  for i:=0 to AContext.ChildNodes.Count-1 do
  begin
    FA:=AContext.ChildNodes[i];
    S:=FA.NodeValue;
    S1:=FA.NodeName;
    if FA.NodeName = 'xs:attribute' then
      ProcessAttribute(FA)
    else
    if FA.NodeName = 'xs:sequence' then
      ProcessAS(FA)
    else
    if FA.NodeName = 'xs:all' then
      ProcessAS(FA)
    else
    if FA.NodeName = 'xs:choice' then
      ProcessAS(FA)
    else
    if FA.NodeName = 'xs:complexContent' then
    begin;
      FC1:=FA.FindNode('xs:extension');
      if Assigned(FC1) then
      begin
        AComplexType.InheritedType:=FC1.Attributes.GetNamedItem('base').NodeValue;
        ProcessComplexElement(ANode, FC1, AComplexType);
      end;
    end
    else
    if FA.NodeName <> 'xs:annotation' then
      raise Exception.CreateFmt('Uknow element type - %s', [FA.NodeName]);
  end;
end;

procedure TXSDProcessor.ProcessSimpleType(AContext: TDOMNode;
  ASimpleType: TXSDSimpleType);
var
  R, M: TDOMNode;
begin
  ASimpleType.Description:=GetAnnotation(AContext);
  R:=AContext.FindNode('xs:restriction');
  if Assigned(R) then
  begin
    ASimpleType.BaseName:=R.Attributes.GetNamedItem('base').NodeValue;
    ASimpleType.PasBaseName:=GetSimpleType(ASimpleType.BaseName);
    M:=R.FindNode('xs:length');
    if Assigned(M) then
    begin
      ASimpleType.MinLength:=StrToIntDef(M.Attributes.GetNamedItem('value').NodeValue, -1);
      ASimpleType.MaxLength:=StrToIntDef(M.Attributes.GetNamedItem('value').NodeValue, -1);
    end
    else
    begin
      M:=R.FindNode('xs:minLength');
      if Assigned(M) then
        ASimpleType.MinLength:=StrToIntDef(M.Attributes.GetNamedItem('value').NodeValue, -1);
      M:=R.FindNode('xs:maxLength');
      if Assigned(M) then
        ASimpleType.MaxLength:=StrToIntDef(M.Attributes.GetNamedItem('value').NodeValue, -1);
    end;
    //minExclusive
    //minInclusive
    //maxExclusive
    //maxInclusive
    //totalDigits
    //fractionDigits
    //length
    //enumeration
    //whiteSpace
    //pattern
  end;
end;

function TXSDProcessor.GetAnnotation(AContext: TDOMNode): string;
var
  R: TDOMNode;
begin
  Result:='';
  R:=AContext.FindNode('xs:annotation');
  if Assigned(R) then
    R:=R.FindNode('xs:documentation');
  if Assigned(R) then
    Result:=R.TextContent;
end;

constructor TXSDProcessor.Create;
begin
  inherited Create;
end;

destructor TXSDProcessor.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TXSDProcessor.Clear;
begin
  FSchema:=nil;
  if Assigned(FDoc) then
    FreeAndNil(FDoc);
end;

procedure TXSDProcessor.LoadFromFile(AFileName: string);
begin
  Clear;
  ReadXMLFile(FDoc, AFileName);
end;

function TXSDProcessor.ExecuteProcessor: TXSDModule;
begin
  FXSDModule:=nil;
  if Assigned(FDoc) then
  begin
    FXSDModule:=TXSDModule.Create;
    FSchema:=FDoc.FindNode('xs:schema');
    if Assigned(FSchema) then
    begin
      ProcessSchema(FSchema);
      FXSDModule.UpdatePascalNames;
    end
    else
      raise Exception.Create('Not find schema in document');
  end;
  Result:=FXSDModule;
end;

end.

