{ XSD files compiler to FPC class

  Copyright (C) 2019-2020 Lagunov Aleksey alexs@yandex.ru

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
    FMainDoc: TXMLDocument;
    FIncludeFolders: TStrings;
    FMainSchema: TDOMNode;
    FOnProcessNodeEvent: TOnProcessNodeEvent;
    FXSDModule: TXSDModule;
    function FindSchemaElent(AName: string; FSchema: TDOMNode): TDOMNode;
  protected
    procedure DoProcessNodeMsg(ANodeName:string; AMessage:string);
    procedure DoLoadXMLIncludeDoc(AFileName:string);

    procedure ProcessSchema(ANode:TDOMNode);
    procedure ProcessElement(ANode, FSchema:TDOMNode);
    procedure ProcessComplexElement(ANode, AContext, FSchema: TDOMNode;
      AComplexType: TXSDComplexType);
    procedure ProcessSimpleType(AContext: TDOMNode; ASimpleType: TXSDSimpleType);
    function GetAnnotation(AContext:TDOMNode):string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure LoadFromFile(AFileName:string);
    function ExecuteProcessor:TXSDModule;
    property IncludeFolders:TStrings read FIncludeFolders;
    property OnProcessNodeEvent:TOnProcessNodeEvent read FOnProcessNodeEvent write FOnProcessNodeEvent;
  end;

implementation
uses StrUtils, XMLRead, xsdutils, xsdconsts, LazFileUtils;

{ TXSDProcessor }

function TXSDProcessor.FindSchemaElent(AName: string; FSchema:TDOMNode): TDOMNode;
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

  raise Exception.CreateFmt(sNotFoundElementInSchema, [AName]);
end;

procedure TXSDProcessor.DoProcessNodeMsg(ANodeName: string; AMessage: string);
begin
  if Assigned(FOnProcessNodeEvent) then
    FOnProcessNodeEvent(Self, ANodeName, AMessage);
end;

procedure TXSDProcessor.DoLoadXMLIncludeDoc(AFileName: string);
var
  FIncDoc: TXMLDocument;
  FIncSchema: TDOMNode;
begin
  ReadXMLFile(FIncDoc, AFileName);
  try
    if Assigned(FIncDoc) then
    begin
      FIncSchema:=FIncDoc.FindNode('xs:schema');
      if Assigned(FIncSchema) then
        ProcessSchema(FIncSchema);
    end
    else
      raise Exception.Create(sNotFoundSchemaInDocument);
  finally
    FIncDoc.Free;
  end;
end;

procedure TXSDProcessor.ProcessSchema(ANode: TDOMNode);
var
  i, C: Integer;
  N, R: TDOMNode;
  S: DOMString;
  S1 : string;
  CT: TXSDComplexType;
  ST: TXSDSimpleType;
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
      ST:=FXSDModule.SimpleTypes.Add(R.NodeValue);
      ST.InludedType:=ANode <>  FMainSchema;
      ProcessSimpleType(N, ST);
    end
    else
    if (S = 'xs:include') then
    begin
      R:=N.Attributes.GetNamedItem('schemaLocation');
      C:=FIncludeFolders.Count;
      S1:=ExpandXSDFileName(ExtractFileName(R.NodeValue), FIncludeFolders);
      if S1<>'' then
      begin
        FXSDModule.IncludeFiles.Add(ExtractFileNameOnly(R.NodeValue));
        DoLoadXMLIncludeDoc(S1)
      end
      else
        raise Exception.CreateFmt(sNotFoundIncludeFile, [ExtractFileName(R.NodeValue)]);
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
        ProcessElement(N, ANode)
      end
      else
      if (S = 'xs:complexType') then
      begin
        R:=N.Attributes.GetNamedItem('name');
        DoProcessNodeMsg(R.NodeName, R.NodeValue);
        CT:=FXSDModule.ComplexTypes.Add(R.NodeValue);
        CT.InludedType:=ANode <> FMainSchema;
        ProcessComplexElement( N, N, ANode, CT);
      end;
    end;
  end;
end;

procedure TXSDProcessor.ProcessElement(ANode, FSchema: TDOMNode);
var
  R, RName: TDOMNode;
  FComplexType: TXSDComplexType;
begin
  RName:=ANode.Attributes.GetNamedItem('name');
  if Assigned(RName) then
    DoProcessNodeMsg(RName.NodeName, RName.NodeValue);

  R:=ANode.Attributes.GetNamedItem('type');
  if Assigned(R) then
  begin
    DoProcessNodeMsg(R.NodeName, R.NodeValue);

    FComplexType:=FXSDModule.ComplexTypes.Add(RName.NodeValue + '_element');
    FComplexType.MainRoot:=true;
    FComplexType.MainRootName:=RName.NodeValue;
    FComplexType.InheritedType:=R.NodeValue;
    //ProcessComplexElement(ANode, R, FComplexType)
  end
  else
  begin
    R:=ANode.FindNode('xs:complexType');
    if Assigned(R) then
    begin
      FComplexType:=FXSDModule.ComplexTypes.Add(RName.NodeValue);
      FComplexType.MainRoot:=true;
      FComplexType.MainRootName:=RName.NodeValue;
      ProcessComplexElement(ANode, R, FSchema, FComplexType)
    end;
  end;
end;

procedure TXSDProcessor.ProcessComplexElement(ANode, AContext, FSchema: TDOMNode;
  AComplexType: TXSDComplexType);

procedure ProcessAttribute(FA:TDOMNode);
var
  Prop: TPropertyItem;
  R, R1, FC, M: TDOMNode;
  i: Integer;
  S: String;
begin
  R:=FA.Attributes.GetNamedItem('ref'); //ref=QName
  if Assigned(R) then
    FC:=FindSchemaElent(R.NodeValue, FSchema)
  else
    FC:=FA;

  Prop:=AComplexType.Propertys.Add(pitAttribute);
  Prop.Name:=FC.Attributes.GetNamedItem('name').NodeValue;

  if Prop.Name = 'HyphenRevisionDate' then
  begin
    S:=Prop.Name;
  end;

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
        for i:=0 to R1.ChildNodes.Count-1 do
        begin
          M:=R1.ChildNodes[i];
          if M.NodeName = 'xs:enumeration' then
            Prop.ValuesList.Add(M.Attributes.GetNamedItem('value').NodeValue)
          else
          if M.NodeName = 'xs:length' then
          begin
            Prop.MinLength:=StrToIntDef(M.Attributes.GetNamedItem('value').NodeValue, -1);
            Prop.MaxLength:=StrToIntDef(M.Attributes.GetNamedItem('value').NodeValue, -1);
          end
          else
          if M.NodeName = 'xs:minLength' then
            Prop.MinLength:=StrToIntDef(M.Attributes.GetNamedItem('value').NodeValue, -1)
          else
          if M.NodeName = 'xs:maxLength' then
            Prop.MaxLength:=StrToIntDef(M.Attributes.GetNamedItem('value').NodeValue, -1)
          else
          if M.NodeName = 'xs:pattern' then
            Prop.ValuePattern:=M.Attributes.GetNamedItem('value').NodeValue
          else
          if M.NodeName = 'xs:totalDigits' then
            Prop.TotalDigits:=StrToIntDef(M.Attributes.GetNamedItem('value').NodeValue, -1)
          else
          if M.NodeName = 'xs:fractionDigits' then
            Prop.FractionDigits:=StrToIntDef(M.Attributes.GetNamedItem('value').NodeValue, -1)
        end;
      end
    end
    else
      Prop.BaseType:='String'; //GetSimpleType(Prop.BaseType);
  end;


     //default=строка
     //form=qualified | unqualified
     //id=идентификатор
     //name=NCName
     //type=QName
  R:=FA.Attributes.GetNamedItem('use'); //use=optional | prohibited | required
  if Assigned(R) then
    Prop.IsRequired:=R.NodeValue = 'required';

  R:=FA.Attributes.GetNamedItem('default');
  if Assigned(R) then
    Prop.DefaultValue:=R.NodeValue;

  R:=FA.Attributes.GetNamedItem('fixed');
  if Assigned(R) then
    Prop.FixedValue:=R.NodeValue;
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
  ProcessComplexElement(FC, FC, FSchema, CT);

  Result:=AComplexType.Propertys.Add(pitClass);
  Result.BaseType:=S;
  Result.Name:=FA.Attributes.GetNamedItem('name').NodeValue;
end;

var
  i, FMaxOccurs, FMinOccurs: Integer;
  FA, FC, R, RMinOccurs, RMaxOccurs: TDOMNode;
  Prop: TPropertyItem;
  ST: TXSDSimpleType;
  S: DOMString;
begin
  for i:=0 to RAll.ChildNodes.Count-1 do
  begin
    Prop:=nil;
    FA:=RAll.ChildNodes[i];
    S:=FA.NodeName;
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
        FA:=FindSchemaElent(FC.NodeValue, FSchema);
        Prop:=DoAttributeProp(FA)
      end
      else
      begin
        FC:=FA.FindNode('xs:complexType');
        if Assigned(FC) then
          Prop:=DoComplexType(FA, FC)
        else
        begin
          FC:=FA.FindNode('xs:simpleType');
          if Assigned(FC) then
          begin
            ST:=FXSDModule.SimpleTypes.Add(FA.Attributes.GetNamedItem('name').NodeValue);
            ProcessSimpleType(FC, ST);
            ST.UpdateUniqueName;

            Prop:=AComplexType.Propertys.Add(pitSimpleType);
            Prop.BaseType:=ST.TypeName;
            Prop.Name:=FA.Attributes.GetNamedItem('name').NodeValue;
            Prop.Description:=GetAnnotation(FC);

          end
          else
          begin
            Prop:=DoAttributeProp(FA);
          end;
          Prop.ItemType:=pitSimpleType;
        end;
      end;

      if Assigned(Prop) then
      begin
        Prop.MaxOccurs:=FMaxOccurs;
        Prop.MinOccurs:=FMinOccurs;
        Prop.Description:=GetAnnotation(FA);
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
    end
    else
    if FA.NodeName = 'xs:choice' then
    begin
      ProcessAS(FA)
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
        ProcessComplexElement(ANode, FC1, FSchema, AComplexType);
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
  i: Integer;
begin
  ASimpleType.Description:=GetAnnotation(AContext);
  R:=AContext.FindNode('xs:restriction');
  if Assigned(R) then
  begin
    ASimpleType.BaseName:=R.Attributes.GetNamedItem('base').NodeValue;
    //ASimpleType.PasBaseName:=GetSimpleType(ASimpleType.BaseName);
    ASimpleType.PasBaseName:=ASimpleType.BaseName;
    for i:=0 to R.ChildNodes.Count-1 do
    begin
      M:=R.ChildNodes[i];
      if M.NodeName = 'xs:enumeration' then
        ASimpleType.ValuesList.Add(M.Attributes.GetNamedItem('value').NodeValue)
      else
      if M.NodeName = 'xs:length' then
      begin
        ASimpleType.MinLength:=StrToIntDef(M.Attributes.GetNamedItem('value').NodeValue, -1);
        ASimpleType.MaxLength:=StrToIntDef(M.Attributes.GetNamedItem('value').NodeValue, -1);
      end
      else
      if M.NodeName = 'xs:minLength' then
        ASimpleType.MinLength:=StrToIntDef(M.Attributes.GetNamedItem('value').NodeValue, -1)
      else
      if M.NodeName = 'xs:maxLength' then
        ASimpleType.MaxLength:=StrToIntDef(M.Attributes.GetNamedItem('value').NodeValue, -1)
      else
      if M.NodeName = 'xs:pattern' then
        ASimpleType.ValuePattern:=M.Attributes.GetNamedItem('value').NodeValue
      else
      if M.NodeName = 'xs:totalDigits' then
        ASimpleType.TotalDigits:=StrToIntDef(M.Attributes.GetNamedItem('value').NodeValue, -1)
      else
      if M.NodeName = 'xs:fractionDigits' then
        ASimpleType.FractionDigits:=StrToIntDef(M.Attributes.GetNamedItem('value').NodeValue, -1)
      else
      if M.NodeName = 'xs:minExclusive' then
        ASimpleType.minExclusive:=M.Attributes.GetNamedItem('value').NodeValue
      else
      if M.NodeName = 'xs:minInclusive' then
        ASimpleType.minInclusive:=M.Attributes.GetNamedItem('value').NodeValue
      else
      if M.NodeName = 'xs:maxExclusive' then
        ASimpleType.maxExclusive:=M.Attributes.GetNamedItem('value').NodeValue
      else
      if M.NodeName = 'xs:maxInclusive' then
        ASimpleType.maxInclusive:=M.Attributes.GetNamedItem('value').NodeValue
    end;
    //totalDigits
    //fractionDigits
    //whiteSpace
  end;
end;

function TXSDProcessor.GetAnnotation(AContext: TDOMNode): string;
var
  R, R1: TDOMNode;
  i: Integer;
begin
  Result:='';
  R:=AContext.FindNode('xs:annotation');
  if Assigned(R) then
  begin
    for i:=0 to R.ChildNodes.Count-1 do
    begin
      R1:=R.ChildNodes[i];
      if (R1.NodeName = 'xs:documentation') or (R1.NodeName = 'xs:appinfo') then
        Result:=Result + R.TextContent + LineEnding;
    end;
  end;
  if Assigned(R) then
    Result:=R.TextContent;
end;

constructor TXSDProcessor.Create;
begin
  inherited Create;
  FIncludeFolders:=TStringList.Create;
end;

destructor TXSDProcessor.Destroy;
begin
  Clear;
  FreeAndNil(FIncludeFolders);
  inherited Destroy;
end;

procedure TXSDProcessor.Clear;
begin
  FIncludeFolders.Clear;
  FMainSchema:=nil;
  if Assigned(FMainDoc) then
    FreeAndNil(FMainDoc);
end;

procedure TXSDProcessor.LoadFromFile(AFileName: string);
begin
  Clear;
  ReadXMLFile(FMainDoc, AFileName);
end;

function TXSDProcessor.ExecuteProcessor: TXSDModule;
var
  S: DOMString;
begin
  FXSDModule:=nil;
  if Assigned(FMainDoc) then
  begin
    S:=FMainDoc.NamespaceURI;
    FXSDModule:=TXSDModule.Create;
    FMainSchema:=FMainDoc.FindNode('xs:schema');
    if Assigned(FMainSchema) then
    begin
      ProcessSchema(FMainSchema);
      FXSDModule.UpdatePascalNames;
    end
    else
      raise Exception.Create(sNotFoundSchemaInDocument);
  end;
  Result:=FXSDModule;
end;

end.

