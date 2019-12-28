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
    FOnProcessNodeEvent: TOnProcessNodeEvent;
    FXSDModule: TXSDModule;
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
uses XMLRead;
function IsSimpleType(ATypeName:string):Boolean;
begin
  Result:=
    (ATypeName = 'xs:string') or
    (ATypeName = 'xs:decimal') or
    (ATypeName = 'xs:integer') or
    (ATypeName = 'xs:boolean') or
    (ATypeName = 'xs:date') or
    (ATypeName = 'xs:time');
end;

function GetSimpleType(ATypeName:string):string;
begin
  if (ATypeName = 'xs:string') then
    Result:='String'
  else
  if (ATypeName = 'xs:decimal') then
    Result:='Double'
  else
  if (ATypeName = 'xs:integer') then
    Result:='Integer'
  else
  if (ATypeName = 'xs:boolean') then
    Result:='Boolean'
  else
  if  (ATypeName = 'xs:date') then
    Result:='TTime'
  else
  if (ATypeName = 'xs:time') then
    Result:='TDate'
  else
    Result:='';
end;

{ TXSDProcessor }

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
  for i:=0 to ANode.ChildNodes.Count - 1 do
  begin
    N:=ANode.ChildNodes[i];
    S:=N.NodeName;
    DoProcessNodeMsg(S, N.NodeValue);
    if (S = 'xs:element')  then
      ProcessElement(N)
    else
    if (S = 'xs:complexType') then
    begin
      R:=N.Attributes.GetNamedItem('name');
      DoProcessNodeMsg(R.NodeName, R.NodeValue);
      CT:=FXSDModule.ComplexTypes.Add(R.NodeValue);
      ProcessComplexElement( N, N, CT);
    end
    else
    if (S = 'xs:simpleType') then
    begin
      R:=N.Attributes.GetNamedItem('name');
      DoProcessNodeMsg(R.NodeName, R.NodeValue);
      ProcessSimpleType(N, FXSDModule.SimpleTypes.Add(R.NodeValue));
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
var
  RAll, FA, R, FC: TDOMNode;
  i: Integer;
  S, S1, ST1: String;
  Prop: TPropertyItem;
  FComplexType, FComplexType1: TXSDComplexType;
begin
  DoProcessNodeMsg(ANode.NodeName, ANode.NodeValue);
  AComplexType.Description:=GetAnnotation(AContext);

  for i:=0 to AContext.ChildNodes.Count-1 do
  begin
    FA:=AContext.ChildNodes[i];
    S:=FA.NodeName;
    if S = 'xs:attribute' then
    begin
      Prop:=AComplexType.Propertys.Add(pitAttribute);
      S1:=FA.Attributes.GetNamedItem('name').NodeValue;
      Prop.Name:=S1;
      Prop.Description:=GetAnnotation(FA);
      R:=FA.Attributes.GetNamedItem('type');
      if Assigned(R) then
        Prop.BaseType:=R.NodeValue;
    end
  end;


  RAll:=AContext.FindNode('xs:sequence');
  if not Assigned(RAll) then
    RAll:=AContext.FindNode('xs:all');

  if Assigned(RAll) then
  begin
    for i:=0 to RAll.ChildNodes.Count-1 do
    begin
      FA:=RAll.ChildNodes[i];
      S:=FA.NodeName;
      if S = 'xs:element' then
      begin
        FC:=FA.FindNode('xs:complexType');
        if Assigned(FC) then
        begin

          ST1:=AComplexType.TypeName  +  '_' + FA.Attributes.GetNamedItem('name').NodeValue;
          FComplexType1:=FXSDModule.ComplexTypes.Add(ST1);
          ProcessComplexElement(FC, FC, FComplexType1);

          Prop:=AComplexType.Propertys.Add(pitClass);
          Prop.BaseType:=ST1;
          Prop.Name:=FA.Attributes.GetNamedItem('name').NodeValue;
        end
        else
        begin
          S1:=FA.Attributes.GetNamedItem('name').NodeValue;
          R:=FA.Attributes.GetNamedItem('type');
          if Assigned(R) then
          begin
            if IsSimpleType(R.NodeValue) then
            begin
              Prop:=AComplexType.Propertys.Add(pitSimpleType);
              Prop.BaseType:=GetSimpleType(R.NodeValue);
            end
            else
            begin
              Prop:=AComplexType.Propertys.Add(pitClass);
              Prop.BaseType:=R.NodeValue;
            end;
            Prop.Name:=S1;
            Prop.Description:=GetAnnotation(FA);
          end;
        end;
      end;
    end;
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
    M:=R.FindNode('xs:minLength');
    if Assigned(M) then
      ASimpleType.MinLength:=StrToIntDef(M.Attributes.GetNamedItem('value').NodeValue, -1);
    M:=R.FindNode('xs:maxLength');
    if Assigned(M) then
      ASimpleType.MinLength:=StrToIntDef(M.Attributes.GetNamedItem('value').NodeValue, -1);
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
  if Assigned(FDoc) then
    FreeAndNil(FDoc);
end;

procedure TXSDProcessor.LoadFromFile(AFileName: string);
begin
  Clear;
  ReadXMLFile(FDoc, AFileName);
end;

function TXSDProcessor.ExecuteProcessor: TXSDModule;
var
  i: Integer;
  S: DOMString;
begin
  FXSDModule:=nil;
  if Assigned(FDoc) then
  begin
    FXSDModule:=TXSDModule.Create;
    for i:=0 to FDoc.ChildNodes.Count-1 do
    begin
      S:=FDoc.ChildNodes[i].NodeName;
      DoProcessNodeMsg(S, FDoc.ChildNodes[i].NodeValue);
      if S = 'xs:schema' then
        ProcessSchema(FDoc.ChildNodes[i]);
    end;
  end;
  Result:=FXSDModule;
end;

end.

