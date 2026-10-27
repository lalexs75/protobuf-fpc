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
  Classes, SysUtils, DOM, XsdElementTypesUnit, Contnrs;

type
  TXSDProcessor = class;

  TOnProcessNodeEvent = procedure(Sender:TXSDProcessor; ANodeName:string; AMessage:string) of object;
  { TXSDProcessor }

  TXSDProcessor = class
  private
    FMainDoc: TXMLDocument;
    FLoadedDocs: TObjectList;
    FIncludeFolders: TStrings;
    FMainSchema: TDOMNode;
    FNameAliases: TStrings;
    FOnProcessNodeEvent: TOnProcessNodeEvent;
    FXSDModule: TXSDModule;
    function FindSchemaElent(AName: string): TDOMNode;
  protected
    procedure DoProcessNodeMsg(ANodeName:string; AMessage:string);
    procedure DoLoadXMLIncludeDoc(AFileName:string);

    procedure ProcessSchema(ANode:TDOMNode; AInclude:boolean);
    procedure ProcessElement(ANode, FSchema:TDOMNode; AInclude:boolean);
    procedure ProcessComplexElement(ANode, AContext, FSchema: TDOMNode; AComplexType: TXSDComplexType; AInclude:boolean);
    procedure ProcessSimpleType(AContext: TDOMNode; ASimpleType: TXSDSimpleType; AInclude:boolean);
    function GetAnnotation(AContext:TDOMNode):string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure LoadFromFile(AFileName:string);
    function ExecuteProcessor:TXSDModule;
    property IncludeFolders:TStrings read FIncludeFolders;
    property NameAliases:TStrings read FNameAliases;

    property OnProcessNodeEvent:TOnProcessNodeEvent read FOnProcessNodeEvent write FOnProcessNodeEvent;
  end;

implementation
uses StrUtils, XMLRead, xsdutils, xsdconsts, LazFileUtils;

{ TXSDProcessor }

function TXSDProcessor.FindSchemaElent(AName: string): TDOMNode;
var
  Doc: TXMLDocument;
  SchemaNode, N, R: TDOMNode;
  I, J: Integer;
  S: DOMString;
begin
  Result:=nil;

  // Перебираем все загруженные XML-документы
  for I := 0 to FLoadedDocs.Count - 1 do
  begin
    Doc := TXMLDocument(FLoadedDocs[I]);
    //SchemaNode := Doc.FindNode('xs:schema');
    SchemaNode := FindXsdNode(Doc, 'schema');
    if not Assigned(SchemaNode) then
      Continue;

    // Ищем элемент или атрибут в текущей схеме
    for J:=0 to SchemaNode.ChildNodes.Count-1 do
    begin
      N:=SchemaNode.ChildNodes[J];
      S:=N.NodeName;
      //if (S = 'xs:element') or (S = 'xs:attribute') then
      if IsXsdNode(N, 'element') or IsXsdNode(N, 'attribute') then
        R:=N.Attributes.GetNamedItem('name')
      else
        R:=nil;

      if Assigned(R) and (R.NodeValue = AName) then
        Exit(N);
    end;
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
      FLoadedDocs.Add(FIncDoc); // <--- ДОБАВЛЕНО: Добавляем в общий список
      //FIncSchema:=FIncDoc.FindNode('xs:schema');
      FIncSchema:=FindXsdNode(FIncDoc, 'schema');
      if Assigned(FIncSchema) then
        ProcessSchema(FIncSchema, true);
    end
    else
      raise Exception.Create(sNotFoundSchemaInDocument);
  finally
    // FIncDoc.Free;  <--- УБРАТЬ! Теперь объект владеет FLoadedDocs
  end;
end;

procedure TXSDProcessor.ProcessSchema(ANode: TDOMNode; AInclude: boolean);
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
    //if (S = 'xs:simpleType') then
    if IsXsdNode(N, 'simpleType') then
    begin
      R:=N.Attributes.GetNamedItem('name');
      DoProcessNodeMsg(R.NodeName, R.NodeValue);

      S1:=R.NodeValue;
      if FNameAliases.Values[S1]<>'' then
        S1:=FNameAliases.Values[S1];
      ST:=FXSDModule.SimpleTypes.Add(S1);
//      ST:=FXSDModule.SimpleTypes.Add(R.NodeValue);
      ST.IncludedType:=AInclude;
      ProcessSimpleType(N, ST, AInclude);
    end
    else
    //if (S = 'xs:include') then
    if IsXsdNode(N, 'include') then
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
//    if (S <> 'xs:simpleType') then
    if not IsXsdNode(N, 'simpleType') then
    begin
      DoProcessNodeMsg(S, N.NodeValue);
      if IsXsdNode(N, 'element') then
//    if (S = 'xs:element')  then
      begin
        ProcessElement(N, ANode, AInclude)
      end
      else
      //if (S = 'xs:complexType') then
      if IsXsdNode(N, 'complexType') then
      begin
        R:=N.Attributes.GetNamedItem('name');
        DoProcessNodeMsg(R.NodeName, R.NodeValue);
        CT:=FXSDModule.ComplexTypes.Add(R.NodeValue);
        CT.IncludedType:=AInclude;
        ProcessComplexElement( N, N, ANode, CT, AInclude);
      end;
    end;
  end;
end;

procedure TXSDProcessor.ProcessElement(ANode, FSchema: TDOMNode;
  AInclude: boolean);
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
    if not AInclude then
    begin
      FComplexType.MainRoot:=true;
      FComplexType.MainRootName:=RName.NodeValue;
      FComplexType.InheritedType:=R.NodeValue;
    end;
    FComplexType.IncludedType:=AInclude;
    //ProcessComplexElement(ANode, R, FComplexType)
  end
  else
  begin
    //R:=ANode.FindNode('xs:complexType');
    R:=FindXsdNode(ANode, 'complexType');
    if Assigned(R) then
    begin
      FComplexType:=FXSDModule.ComplexTypes.Add(RName.NodeValue);
      if not AInclude then
      begin
        FComplexType.MainRoot:=true;
        FComplexType.MainRootName:=RName.NodeValue;
      end;
      FComplexType.IncludedType:=AInclude;
      ProcessComplexElement(ANode, R, FSchema, FComplexType, AInclude)
    end;
  end;
end;

procedure TXSDProcessor.ProcessComplexElement(ANode, AContext,
  FSchema: TDOMNode; AComplexType: TXSDComplexType; AInclude: boolean);

procedure ProcessAttribute(FA:TDOMNode);
var
  Prop: TPropertyItem;
  R, R1, FC, M: TDOMNode;
  i: Integer;
  S: String;
begin
  R:=FA.Attributes.GetNamedItem('ref'); //ref=QName
  if Assigned(R) then
    FC:=FindSchemaElent(R.NodeValue{, FSchema})
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
    //R:=FC.FindNode('xs:simpleType');
    R:=FindXsdNode(FC, 'simpleType');
    if Assigned(R) then
    begin
      //R1:=R.FindNode('xs:restriction');
      R1:=FindXsdNode(R, 'restriction');
      if Assigned(R1) then
      begin
        Prop.BaseType:=R1.Attributes.GetNamedItem('base').NodeValue;
//        if IsSimpleType(Prop.BaseType) then
//          Prop.BaseType:=GetSimpleType(Prop.BaseType);

          //Prop.BaseType:=R1.Attributes.GetNamedItem('base');
        for i:=0 to R1.ChildNodes.Count-1 do
        begin
          M:=R1.ChildNodes[i];
          //if M.NodeName = 'xs:enumeration' then
          if IsXsdNode(M, 'enumeration') then
            Prop.ValuesList.Add(M.Attributes.GetNamedItem('value').NodeValue)
          else
          //if M.NodeName = 'xs:length' then
          if IsXsdNode(M, 'length') then
          begin
            Prop.MinLength:=StrToIntDef(M.Attributes.GetNamedItem('value').NodeValue, -1);
            Prop.MaxLength:=StrToIntDef(M.Attributes.GetNamedItem('value').NodeValue, -1);
          end
          else
          //if M.NodeName = 'xs:minLength' then
          if IsXsdNode(M, 'minLength') then
            Prop.MinLength:=StrToIntDef(M.Attributes.GetNamedItem('value').NodeValue, -1)
          else
          //if M.NodeName = 'xs:maxLength' then
          if IsXsdNode(M, 'maxLength') then
            Prop.MaxLength:=StrToIntDef(M.Attributes.GetNamedItem('value').NodeValue, -1)
          else
          //if M.NodeName = 'xs:pattern' then
          if IsXsdNode(M, 'pattern') then
            Prop.ValuePattern:=M.Attributes.GetNamedItem('value').NodeValue
          else
          //if M.NodeName = 'xs:totalDigits' then
          if IsXsdNode(M, 'totalDigits') then
            Prop.TotalDigits:=StrToIntDef(M.Attributes.GetNamedItem('value').NodeValue, -1)
          else
          //if M.NodeName = 'xs:fractionDigits' then
          if IsXsdNode(M, 'fractionDigits') then
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
{      Result:=AComplexType.Propertys.Add(pitSimpleType);
      Result.BaseType:=GetSimpleType(R.NodeValue);}

      Result:=AComplexType.Propertys.Add(pitSimpleType);
      Result.BaseType:=R.NodeValue; // ✅ ПРАВИЛЬНО: сохраняем 'xsd:string' или 'xs:string'
      // БЫЛО ОШИБКОЙ: Result.BaseType:=GetSimpleType(R.NodeValue);
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
    //R:=FA.FindNode('xs:complexType');
    R:=FindXsdNode(FA, 'complexType');
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
  S, S1:string;
  Prop: TPropertyItem;
  CT:TXSDComplexType;
begin
  S1:=FA.Attributes.GetNamedItem('name').NodeValue;
  if FNameAliases.Values[S1]<>'' then
    S1:=FNameAliases.Values[S1];

  //S:=AComplexType.TypeName  +  '_' + FA.Attributes.GetNamedItem('name').NodeValue;
  S:=AComplexType.TypeName  +  '_' + S1;

  //CT:=FXSDModule.ComplexTypes.Add(S1);
  CT:=FXSDModule.ComplexTypes.Add(S);

  CT.IncludedType:=AInclude;
  ProcessComplexElement(FC, FC, FSchema, CT, AInclude);
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
    if IsXsdNode(FA, 'element') then
//    if FA.NodeName = 'xs:element' then
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
        FA:=FindSchemaElent(FC.NodeValue{, FSchema});
        Prop:=DoAttributeProp(FA)
      end
      else
      begin
        //FC:=FA.FindNode('xs:complexType');
        FC := FindXsdNode(FA, 'complexType');
        if Assigned(FC) then
          Prop:=DoComplexType(FA, FC)
        else
        begin
          //FC:=FA.FindNode('xs:simpleType');
          FC := FindXsdNode(FA, 'simpleType');
          if Assigned(FC) then
          begin
            ST:=FXSDModule.SimpleTypes.Add(FA.Attributes.GetNamedItem('name').NodeValue);
            ST.IncludedType:=AInclude;
            ProcessSimpleType(FC, ST, AInclude);
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
          //Prop.ItemType:=pitSimpleType;
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
    //if FA.NodeName = 'xs:choice' then
    if IsXsdNode(FA, 'choice') then
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
    //if FA.NodeName = 'xs:attribute' then
    if IsXsdNode(FA,'attribute') then
      ProcessAttribute(FA)
    else
    //if FA.NodeName = 'xs:sequence' then
    if IsXsdNode(FA,'sequence') then
      ProcessAS(FA)
    else
    //if FA.NodeName = 'xs:all' then
    if IsXsdNode(FA, 'all') then
      ProcessAS(FA)
    else
    //if FA.NodeName = 'xs:choice' then
    if IsXsdNode(FA, 'choice') then
      ProcessAS(FA)
    else
    //if FA.NodeName = 'xs:complexContent' then
    if IsXsdNode(FA, 'complexContent') then
    begin;
      //FC1:=FA.FindNode('xs:extension');
      FC1:=FindXsdNode(FA, 'extension');
      if Assigned(FC1) then
      begin
        AComplexType.InheritedType:=FC1.Attributes.GetNamedItem('base').NodeValue;
        ProcessComplexElement(ANode, FC1, FSchema, AComplexType, AInclude);
      end;
    end
    else
    //if FA.NodeName = 'xs:simpleContent' then
    if IsXsdNode(FA, 'simpleContent') then
    begin;
      //FC1:=FA.FindNode('xs:extension');
      FC1:=FindXsdNode(FA, 'extension');
      if Assigned(FC1) then
      begin
        AComplexType.InheritedType:=FC1.Attributes.GetNamedItem('base').NodeValue;
        ProcessComplexElement(ANode, FC1, FSchema, AComplexType, AInclude);
      end;
    end
    else
//    if (FA.NodeName <> 'xs:annotation') and (FA.NodeName <> '#comment') then
    if FA.NodeType = ELEMENT_NODE then
      raise Exception.CreateFmt('Uknow element type - %s', [FA.NodeName]);
  end;
end;

procedure TXSDProcessor.ProcessSimpleType(AContext: TDOMNode;
  ASimpleType: TXSDSimpleType; AInclude: boolean);
var
  R, M: TDOMNode;
  i: Integer;
begin
  ASimpleType.Description:=GetAnnotation(AContext);
  //R:=AContext.FindNode('xs:restriction');
  R := FindXsdNode(AContext, 'restriction');
  if Assigned(R) then
  begin
    ASimpleType.BaseName:=R.Attributes.GetNamedItem('base').NodeValue;
    //ASimpleType.PasBaseName:=GetSimpleType(ASimpleType.BaseName);
    ASimpleType.PasBaseName:=ASimpleType.BaseName;
    for i:=0 to R.ChildNodes.Count-1 do
    begin
      M:=R.ChildNodes[i];
      //if M.NodeName = 'xs:enumeration' then
      if  IsXsdNode(M, 'enumeration') then
        ASimpleType.ValuesList.Add(M.Attributes.GetNamedItem('value').NodeValue)
      else
      //if M.NodeName = 'xs:length' then
      if IsXsdNode(M, 'length') then
      begin
        ASimpleType.MinLength:=StrToIntDef(M.Attributes.GetNamedItem('value').NodeValue, -1);
        ASimpleType.MaxLength:=StrToIntDef(M.Attributes.GetNamedItem('value').NodeValue, -1);
      end
      else
      //if M.NodeName = 'xs:minLength' then
      if IsXsdNode(M, 'minLength') then
        ASimpleType.MinLength:=StrToIntDef(M.Attributes.GetNamedItem('value').NodeValue, -1)
      else
      //if M.NodeName = 'xs:maxLength' then
      if IsXsdNode(M, 'maxLength') then
        ASimpleType.MaxLength:=StrToIntDef(M.Attributes.GetNamedItem('value').NodeValue, -1)
      else
      //if M.NodeName = 'xs:pattern' then
      if IsXsdNode(M, 'pattern') then
        ASimpleType.ValuePattern:=M.Attributes.GetNamedItem('value').NodeValue
      else
      //if M.NodeName = 'xs:totalDigits' then
      if IsXsdNode(M, 'totalDigits') then
        ASimpleType.TotalDigits:=StrToIntDef(M.Attributes.GetNamedItem('value').NodeValue, -1)
      else
      //if M.NodeName = 'xs:fractionDigits' then
      if IsXsdNode(M, 'fractionDigits') then
        ASimpleType.FractionDigits:=StrToIntDef(M.Attributes.GetNamedItem('value').NodeValue, -1)
      else
      //if M.NodeName = 'xs:minExclusive' then
      if IsXsdNode(M, 'minExclusive') then
        ASimpleType.minExclusive:=M.Attributes.GetNamedItem('value').NodeValue
      else
      //if M.NodeName = 'xs:minInclusive' then
      if IsXsdNode(M, 'minInclusive') then
        ASimpleType.minInclusive:=M.Attributes.GetNamedItem('value').NodeValue
      else
      //if M.NodeName = 'xs:maxExclusive' then
      if IsXsdNode(M, 'maxExclusive') then
        ASimpleType.maxExclusive:=M.Attributes.GetNamedItem('value').NodeValue
      else
      //if M.NodeName = 'xs:maxInclusive' then
      if IsXsdNode(M, 'maxInclusive') then
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
  //R:=AContext.FindNode('xs:annotation');
  R := FindXsdNode(AContext, 'annotation');
  if Assigned(R) then
  begin
    for i:=0 to R.ChildNodes.Count-1 do
    begin
      R1:=R.ChildNodes[i];
      //if (R1.NodeName = 'xs:documentation') or (R1.NodeName = 'xs:appinfo') then
      if IsXsdNode(R1, 'documentation') or IsXsdNode(R1, 'appinfo') then
        Result:=Result + R1.TextContent + LineEnding;
    end;
    Result:=Result + R.TextContent;
  end;
end;

constructor TXSDProcessor.Create;
begin
  inherited Create;
  FIncludeFolders:=TStringList.Create;
  FNameAliases:=TStringList.Create;
  FLoadedDocs := TObjectList.Create(True);
end;

destructor TXSDProcessor.Destroy;
begin
  Clear;
  FreeAndNil(FIncludeFolders);
  FreeAndNil(FNameAliases);
  FreeAndNil(FLoadedDocs);
  inherited Destroy;
end;

procedure TXSDProcessor.Clear;
begin
  FIncludeFolders.Clear;
  FNameAliases.Clear;
  FMainSchema:=nil;
{  if Assigned(FMainDoc) then
    FreeAndNil(FMainDoc);}

  if Assigned(FLoadedDocs) then
    FLoadedDocs.Clear; // Очистит и освободит все TXMLDocument, включая FMainDoc
end;

procedure TXSDProcessor.LoadFromFile(AFileName: string);
begin
  Clear;
  ReadXMLFile(FMainDoc, AFileName);
  if Assigned(FMainDoc) then
    FLoadedDocs.Add(FMainDoc); // <--- ДОБАВЛЕНО: Сохраняем основной документ
end;

function TXSDProcessor.ExecuteProcessor: TXSDModule;
begin
  FXSDModule:=nil;
  if Assigned(FMainDoc) then
  begin
    FXSDModule:=TXSDModule.Create;
    FXSDModule.NameAliases.Assign(FNameAliases);
    //FMainSchema:=FMainDoc.FindNode('xs:schema');
    FMainSchema := FindXsdNode(FMainDoc, 'schema');
    if Assigned(FMainSchema) then
    begin
      ProcessSchema(FMainSchema, false);
      FXSDModule.UpdatePascalNames;
    end
    else
      raise Exception.Create(sNotFoundSchemaInDocument);
  end;
  Result:=FXSDModule;
end;

end.

