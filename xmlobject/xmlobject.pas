{ interface library for FPC and Lazarus

  Copyright (C) 2019-2020 Lagunov Aleksey alexs75@yandex.ru

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit xmlobject;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DOM, TypInfo, AbstractSerializationObjects;

type


  { TXmlSerializationObject }

  TXmlSerializationObject = class(TAbstractSerializationObject)
  private
    procedure InternalRead(AElement: TDOMNode);
    procedure DoLoadAtributes(AElement: TDOMNode);
    procedure DoLoadChild(AElement: TDOMNode);

    procedure InternalWrite(FXML: TXMLDocument; AElement: TDOMElement);
    procedure InternalWriteChild(FXML: TXMLDocument; AChild:TObject; AElement: TDOMElement; P: TPropertyDef);
    procedure SetAtribute(P: TDOMElement; AttribName, AttribValue:DOMString; Prop:TPropertyDef);
    function CreateElement(FXML: TXMLDocument; AParent:TDOMNode; AName:string):TDOMElement;
    procedure InternalWriteDynArrayXML(FXML: TXMLDocument; P: TDOMElement; AXmlProp:TPropertyDef; AProp:PPropInfo);
    procedure InternalReadDynArray(AXmlProp:TPropertyDef; AProp:PPropInfo; ATextContent:string);
  protected
    procedure InternalWriteString(P: TPropertyDef; AValue:string); override;
    procedure InternalWriteBoolean(P: TPropertyDef; AValue:Boolean); override;
    procedure InternalWriteInteger(P: TPropertyDef; AValue:Integer); override;
    procedure InternalWriteInt64(P: TPropertyDef; AValue:Int64); override;
    procedure InternalWriteQWord(P: TPropertyDef; AValue:QWord); override;
    procedure InternalWriteDateTime(P: TPropertyDef; AValue:TDateTime); override;
    procedure InternalWriteDate(P: TPropertyDef; AValue:TDate); override;
    procedure InternalWriteTime(P: TPropertyDef; AValue:TTime); override;
    procedure InternalWriteFloat(P: TPropertyDef; AValue:Double); override;
    procedure InternalWriteDynArray(P: TPropertyDef; AProp:PPropInfo); override;

    procedure InternalWriteClass(P: TPropertyDef; AObject:TAbstractSerializationObject); override;
    procedure InternalWriteClassCollection(P: TPropertyDef; AObjects:TXmlSerializationObjectList); override;
  protected
    function RootNodeName:string; virtual;
  public
    procedure LoadFromStream(AStream:TStream); override;
    procedure SaveToStream(AStream:TStream); override;

    procedure LoadFromStr(AStr:string); virtual;
    function SaveToStr:string; virtual;

    procedure SaveToXML(const XML: TXMLDocument);
    procedure LoadFromXML(const XML: TXMLDocument);
  end;

implementation
uses LazUTF8, XMLRead, XMLWrite, {$IFDEF WINDOWS} xmliconv_windows {$ELSE} xmliconv {$ENDIF}, xmlobject_resource;

type
  TXmlSerializationObjectListHack = class(TXmlSerializationObjectList);

{ TXmlSerializationObject }

procedure TXmlSerializationObject.InternalRead(AElement: TDOMNode);
begin
  if not Assigned(AElement) then Exit;

  DoLoadAtributes(AElement);
  DoLoadChild(AElement);
end;

procedure TXmlSerializationObject.InternalWrite(FXML: TXMLDocument;
  AElement: TDOMElement);
var
  P: TPropertyDef;
  FProp: PPropInfo;
  E: TDOMElement;
  S, TN:string;
  D:TDateTime;
  i: Integer;
  K: TTypeKind;
  PP: TObject;
begin
  ValidateRequared;

  for P in PropertyList do
  begin

    FProp:=GetPropInfo(Self, P.PropertyName);
    if not Assigned(FProp) then
      raise Exception.CreateFmt(sPropertyNotFound, [ClassName, P.PropertyName, P.Caption]);

    K:=FProp^.PropType^.Kind;
    TN:=FProp^.PropType^.Name;
    case FProp^.PropType^.Kind of
      tkChar,
      tkAString,
      tkWString,
      tkSString,
      tkLString :
        if P.Modified then
        begin
          if xsaSimpleObject in P.Attribs then
          begin
            E:=CreateElement(FXML, AElement, P.XMLName);
            E.TextContent:=GetStrProp(Self, P.PropertyName);
          end
          else
            SetAtribute(AElement, P.XMLName, GetStrProp(Self, P.PropertyName), P);
        end;
      tkBool :
        begin
          if xsaSimpleObject in P.Attribs then
          begin
            E:=CreateElement(FXML, AElement, P.XMLName);
            E.TextContent:=BoolToStr(GetOrdProp(Self, P.PropertyName) = 1, 'true', 'false');
          end
          else
            SetAtribute(AElement, P.XMLName, BoolToStr(GetOrdProp(Self, P.PropertyName) = 1, 'true', 'false'), P);
        end;
//      tkQWord : SetOrdProp(Self, FProp, Ord(ABuf.ReadAsQWord));

      tkInt64 ,
      tkInteger :
        if P.Modified then
        begin
          if xsaSimpleObject in P.Attribs then
          begin
            E:=CreateElement(FXML, AElement, P.XMLName);
            E.TextContent:=IntToStr( GetInt64Prop(Self, P.PropertyName));
          end
          else
            SetAtribute(AElement, P.XMLName, IntToStr( GetInt64Prop(Self, P.PropertyName)), P);
        end;
      tkFloat :
        if P.Modified then
        begin
          if TN = 'TTime' then
          begin
            D:=GetFloatProp(Self, P.PropertyName);
            S:=FormatDateTime('HH:NN:SS', D);
          end
          else
          if TN = 'TDate' then
          begin
            D:=GetFloatProp(Self, P.PropertyName);
            S:=FormatDateTime('YYYY-MM-DD', D);
          end
          else
          if TN = 'TDateTime' then
          begin
            D:=GetFloatProp(Self, P.PropertyName);
            S:=FormatDateTime('YYYY-MM-DD''T''HH:NN:SS''Z''', D);
          end
          else
          begin
            if P.TotalDigits > 0 then
              Str(GetFloatProp(Self, P.PropertyName):P.TotalDigits:P.FractionDigits, S)
            else
              Str(GetFloatProp(Self, P.PropertyName):15:4, S);

            if (Length(S)>0) and (Pos('.', S) > 0) then
            begin
              i:=Length(S);
              while (I>0) and (S[i] = '0') { and (S[i-1]<>'.')} do Dec(i);
              if (I>0) and (S[i]='.') then Dec(i);
              S:=Copy(S, 1, i);
            end;
          end;

          if xsaSimpleObject in P.Attribs then
          begin
            E:=CreateElement(FXML, AElement, P.XMLName);
            E.TextContent:=Trim(S)
          end
          else
            SetAtribute(AElement, P.XMLName, Trim(S), P);
        end;
      tkClass: InternalWriteChild(FXML, TObject(PtrInt( GetOrdProp(Self, FProp))), AElement, P);
      tkDynArray:
        begin
          InternalWriteDynArrayXML(FXML, AElement, P, FProp);
          //PP:=GetObjectProp(Self, FProp);
          //L:=DynArraySize(PP);
          //PDT:=GetTypeData(Info^.PropType);
        end
    else
      raise exception.CreateFmt(sUknowPropertyType, [P.PropertyName]);
    end;
  end;
end;

procedure TXmlSerializationObject.DoLoadAtributes(AElement: TDOMNode);
var
  i, C: Integer;
  A: TDOMNode;
  S1, NV, TN, S:string;
  P: TPropertyDef;
  FProp: PPropInfo;
  D:Extended;
  K: TTypeKind;
  DT: TDateTime;
begin
  if not Assigned(AElement) then Exit;
  for i:=0 to AElement.Attributes.Length-1 do
  begin
    A:=AElement.Attributes.Item[I];
    S1:=A.NodeName;
    NV:=A.NodeValue;
    if (Copy(S1, 1, 6) <>'xmlns:') and (Copy(S1, 1, 4) <> 'xsi:') then
    begin
      P:=PropertyList.PropertyByXMLName(S1);
      if not Assigned(P) then
        P:=PropertyList.PropertyByAlias(S1);

      if Assigned(P) then
      begin
        FProp:=GetPropInfo(Self, P.PropertyName);
        if not Assigned(FProp) then
          raise Exception.CreateFmt(sPropertyNotFound, [ClassName, P.PropertyName, P.Caption]);
        K:=FProp^.PropType^.Kind;
        TN:=FProp^.PropType^.Name;

        if TN='Unit1' then
        begin
          S:=TN;
        end;

        case K of
          tkChar,
          tkAString,
          tkWString,
          tkSString,
          tkLString : SetStrProp(Self, FProp, NV);
          tkBool : SetOrdProp(Self, FProp, Ord(StrToBool(NV)));
//          tkQWord : SetOrdProp(Self, FProp, Ord(ABuf.ReadAsQWord));
          tkInt64 : SetInt64Prop(Self, FProp, StrToInt64(NV));
          tkInteger : SetOrdProp(Self, FProp, StrToInt(NV));

//          tkSet                       : SetSetProp(t,PropInfo,S);
          tkFloat :
            begin
              if TN = 'TTime' then
              begin
                DT:=StrToTime(NV); //FormatDateTime('HH:NN:SS', D);
                SetFloatProp(Self, FProp, DT);
              end
              else
              if TN = 'TDate' then
              begin
                DT:=StrToDate(NV); //FormatDateTime('YYYY-MM-DD', D);
                SetFloatProp(Self, FProp, DT);
              end
              else
              if TN = 'TDateTime' then
              begin
                DT:=StrToDateTime(NV); //FormatDateTime('YYYY-MM-DD''T''HH:NN:SS', D);
                SetFloatProp(Self, FProp, DT);
              end
              else
              begin
                Val(NV, D, C);
                if C = 0 then
                  SetFloatProp(Self, FProp, D);
              end;
            end
//          tkEnumeration : SetOrdProp(Self, FProp, Ord(ABuf.ReadAsInteger));
//          tkDynArray:LoadBytes(FProp, P);
        else
          raise exception.CreateFmt(sUknowPropertyType, [P.PropertyName]);
        end;
      end
      else
        raise exception.CreateFmt(sNotFoundPropertyForField, [ClassName, S1]);
    end;
  end;
end;

procedure TXmlSerializationObject.DoLoadChild(AElement: TDOMNode);
var
  i, C: Integer;
  FNode: TDOMNode;
  P: TPropertyDef;
  FProp: PPropInfo;
  K: TTypeKind;
  FInst: TObject;
  R: TXmlSerializationObject;
  TN, NV, DS, TS, S: String;
  D:Extended;
  DT:TDateTime;
  FS: TFormatSettings;
begin
  for i:=0 to AElement.ChildNodes.Count-1 do
  begin
    FNode:=AElement.ChildNodes.Item[I];

    P:=PropertyList.PropertyByXMLName(FNode.NodeName);
    if not Assigned(P) then
      P:=PropertyList.PropertyByAlias(FNode.NodeName);

    if Assigned(P) then
    begin
      S:=UpperCase(P.PropertyName);
      if S = 'UNIT' then
      begin
        S:='';
      end;

      FProp:=GetPropInfo(Self, P.PropertyName); //Retreive property informations
      if not Assigned(FProp) then
        raise Exception.CreateFmt(sPropertyNotFound2, [P.PropertyName]);

      K:=FProp^.PropType^.Kind;
      TN:=FProp^.PropType^.Name;

      if TN='UNIT1' then
      begin
        S:=TN;
      end;

      if (xsaSimpleObject in P.Attribs) or (K <> tkClass) then
      begin
        NV:=FNode.TextContent;
        case K of
          tkChar,
          tkAString,
          tkWString,
          tkSString,
          tkLString   : SetStrProp(Self, FProp, NV);
          tkInt64,
          tkInteger : SetInt64Prop(Self, FProp, StrToInt64(NV));
          tkBool : SetOrdProp(Self, FProp, Ord(StrToBool(NV)));
          tkFloat :
            begin
              if TN = 'TTime' then
              begin
                DT:=StrToTime(NV, {'HH:NN:SS',} ':');
                SetFloatProp(Self, FProp, DT);
              end
              else
              if TN = 'TDate' then
              begin
                DT:=StrToDate(NV, 'YYYY-MM-DD', '-');
                SetFloatProp(Self, FProp, DT);
              end
              else
              if TN = 'TDateTime' then
              begin
                C:=Pos('T', NV);
                DS:=Copy(NV, 1, C-1);
                TS:=Copy(NV, C+1, Length(NV));
                DT:=StrToDate(DS, 'YYYY-MM-DD', '-') + StrToTime(TS, ':');
                //DT:=StrToDateTime(NV, FS); //FormatDateTime('YYYY-MM-DD''T''HH:NN:SS', D);
                SetFloatProp(Self, FProp, DT);
              end
              else
              begin
                Val(NV, D, C);
                if C = 0 then
                  SetFloatProp(Self, FProp, D);
              end;
            end;
          tkClass:if TN = 'TStringList' then
          begin
            FInst := TObject(PtrInt( GetOrdProp(Self, FProp)));
            TStringList(FInst).Add(NV)
          end;
          tkDynArray:InternalReadDynArray(P, FProp, NV);
        else
          raise exception.CreateFmt(sUknowPropertyType, [P.PropertyName]);
        end;
      end
      else
      begin
        if K <> tkClass then
          raise Exception.CreateFmt(sPropertyIsNotClassType, [P.PropertyName]);

        FInst := TObject(PtrInt( GetOrdProp(Self, FProp)));
        if not Assigned(FInst) then
          raise Exception.CreateFmt(sClassPropertyNotInit, [P.PropertyName]);

        if FInst is TXmlSerializationObject then
          TXmlSerializationObject(FInst).InternalRead(FNode)
        else
        if FInst is TXmlSerializationObjectList then
        begin
          R:=TXmlSerializationObjectListHack(FInst).InternalAddObject as TXmlSerializationObject;
          R.InternalRead(FNode)
        end;
      end;
    end
    else
      raise exception.CreateFmt(sUnknowClassProperty, [ClassName, FNode.NodeName]);
  end;
end;

procedure TXmlSerializationObject.SetAtribute(P: TDOMElement; AttribName,
  AttribValue: DOMString; Prop: TPropertyDef);
begin
  if (Prop.MaxSize > 0) and (UTF8Length(AttribValue) > Prop.MaxSize) then
    raise Exception.CreateFmt(sValueExpectedRange, [ClassName, Prop.PropertyName, AttribValue, Prop.MaxSize]);
  P.SetAttribute(AttribName, AttribValue);
end;

function TXmlSerializationObject.CreateElement(FXML: TXMLDocument;
  AParent: TDOMNode; AName: string): TDOMElement;
begin
  Result:=FXML.CreateElement(AName);
  if Assigned(AParent) then
    AParent.AppendChild(Result);
end;

procedure TXmlSerializationObject.InternalWriteDynArrayXML(FXML: TXMLDocument;
  P: TDOMElement; AXmlProp: TPropertyDef; AProp: PPropInfo);
var
  vDinArray: Pointer;
  L: tdynarrayindex;
  PDT: PTypeData;
  K: TTypeKind;
  KN, sValue: String;
  EL: PTypeInfo;
  O: TOrdType;
  i: Integer;
  E: TDOMElement;
//  EL: PTypeInfo;
begin
  vDinArray:=GetObjectProp(Self, AProp);
  L:=DynArraySize(vDinArray);
  PDT:=GetTypeData(AProp^.PropType);
  O:=PDT^.OrdType;
  EL:=PDT^.ElType2;
  K:=EL^.Kind;
  KN:=EL^.Name;
  if not (K in [tkInteger, tkString, tkAString]) then
    raise exception.CreateFmt(sUknowPropertyType, [AXmlProp.PropertyName]);

  for i:=0 to L-1 do
  begin
    case K of
      tkInteger:
      begin
        case O of
          //  otSByte,otUByte,otSWord,otUWord,
            otSLong:sValue:=IntToStr(TXSDIntegerArray(vDinArray)[i]);
            //otULong,otSQWord,otUQWor
        else
          raise exception.CreateFmt(sUknowPropertyType, [AXmlProp.PropertyName]);
        end;
      end;
      tkAString,
      tkString:sValue:=TXSDStringArray(vDinArray)[i];
    else
      raise exception.CreateFmt(sUknowPropertyType, [AXmlProp.PropertyName]);
    end;
    E:=CreateElement(FXML, P, AXmlProp.XMLName);
    E.TextContent:=sValue;
  end;
end;

procedure TXmlSerializationObject.InternalReadDynArray(AXmlProp: TPropertyDef;
  AProp: PPropInfo; ATextContent: string);
var
  vDinArray: Pointer;
  L: tdynarrayindex;
  PDT: PTypeData;
  O: TOrdType;
  EL: PTypeInfo;
  K: TTypeKind;
  KN: String;
begin
  vDinArray:=GetObjectProp(Self, AProp);
  L:=DynArraySize(vDinArray);
  PDT:=GetTypeData(AProp^.PropType);
  O:=PDT^.OrdType;
  EL:=PDT^.ElType2;
  K:=EL^.Kind;
  KN:=EL^.Name;

  L:=L+1;
  DynArraySetLength(vDinArray, AProp^.PropType, 1, @L);

  case K of
    tkInteger:
    begin
      case O of
        //  otSByte,otUByte,otSWord,otUWord,
          otSLong:TXSDIntegerArray(vDinArray)[L-1]:=StrToInt(ATextContent);
          //otULong,otSQWord,otUQWor
      else
        raise exception.CreateFmt(sUknowPropertyType, [AXmlProp.PropertyName]);
      end;
    end;
    tkAString,
    tkString:TXSDStringArray(vDinArray)[L-1]:=ATextContent;
  else
    raise exception.CreateFmt(sUknowPropertyType, [AXmlProp.PropertyName]);
  end;
  SetObjectProp(Self, AProp, TObject(vDinArray));
end;

procedure TXmlSerializationObject.InternalWriteString(P: TPropertyDef;
  AValue: string);
begin

end;

procedure TXmlSerializationObject.InternalWriteBoolean(P: TPropertyDef;
  AValue: Boolean);
begin

end;

procedure TXmlSerializationObject.InternalWriteInteger(P: TPropertyDef;
  AValue: Integer);
begin

end;

procedure TXmlSerializationObject.InternalWriteInt64(P: TPropertyDef;
  AValue: Int64);
begin

end;

procedure TXmlSerializationObject.InternalWriteQWord(P: TPropertyDef;
  AValue: QWord);
begin

end;

procedure TXmlSerializationObject.InternalWriteDateTime(P: TPropertyDef;
  AValue: TDateTime);
begin

end;

procedure TXmlSerializationObject.InternalWriteDate(P: TPropertyDef;
  AValue: TDate);
begin

end;

procedure TXmlSerializationObject.InternalWriteTime(P: TPropertyDef;
  AValue: TTime);
begin

end;

procedure TXmlSerializationObject.InternalWriteFloat(P: TPropertyDef;
  AValue: Double);
begin

end;

procedure TXmlSerializationObject.InternalWriteDynArray(P: TPropertyDef;
  AProp: PPropInfo);
begin

end;

procedure TXmlSerializationObject.InternalWriteClass(P: TPropertyDef;
  AObject: TAbstractSerializationObject);
begin

end;

procedure TXmlSerializationObject.InternalWriteClassCollection(P: TPropertyDef;
  AObjects: TXmlSerializationObjectList);
begin

end;

procedure TXmlSerializationObject.InternalWriteChild(FXML: TXMLDocument;
  AChild: TObject; AElement: TDOMElement; P: TPropertyDef);
var
  E: TDOMElement;
  Itm: TXmlSerializationObject;
  i: Integer;
  S: String;
begin
  if not Assigned(AChild) then Exit;
  if AChild is TXmlSerializationObject then
  begin
    if TXmlSerializationObject(AChild).IsEmpty then Exit;
    E:=CreateElement(FXML, AElement, P.XMLName);
    TXmlSerializationObject(AChild).InternalWrite(FXML, E);
  end
  else
  if AChild is TXmlSerializationObjectList then
  begin
    for i:=0 to TXmlSerializationObjectList(AChild).Count-1 do
    begin
      Itm:=TXmlSerializationObjectListHack(AChild).InternalGetItem(I) as TXmlSerializationObject;
      E:=CreateElement(FXML, AElement, P.XMLName);
      Itm.InternalWrite(FXML, E);
    end;
  end
  else
  if AChild is TStrings then
  begin
    for i:=0 to TStrings(AChild).Count-1 do
    begin
      S:=TStrings(AChild)[i];
      E:=CreateElement(FXML, AElement, P.XMLName);
      E.TextContent:=S;
    end;
  end
  else
    raise Exception.CreateFmt(sUnknowObject, [AChild.ClassName]);
end;

function TXmlSerializationObject.RootNodeName: string;
begin
  Result:=ClassName;
end;

procedure TXmlSerializationObject.LoadFromStream(AStream: TStream);
var
  FXML: TXMLDocument;
begin
  inherited LoadFromStream(AStream);
  ReadXMLFile(FXML, AStream);
  InternalRead(FXML.DocumentElement);
  FXML.Free;
end;

procedure TXmlSerializationObject.SaveToStream(AStream: TStream);
var
  FXML: TXMLDocument;
  E: TDOMElement;
begin
  FXML:=TXMLDocument.Create;
  E:=CreateElement(FXML, FXML, RootNodeName);
  InternalWrite(FXML, E);
  WriteXML(FXML, AStream);
  FXML.Free;
end;

procedure TXmlSerializationObject.LoadFromStr(AStr: string);
var
  S: TStringStream;
begin
  S:=TStringStream.Create(AStr);
  LoadFromStream(S);
  S.Free;
end;

function TXmlSerializationObject.SaveToStr: string;
var
  S: TStringStream;
begin
  S:=TStringStream.Create('');
  SaveToStream(S);
  Result:=S.DataString;
  S.Free;
end;

procedure TXmlSerializationObject.SaveToXML(const XML: TXMLDocument);
var
  E: TDOMElement;
begin
  E:=CreateElement(XML, XML, RootNodeName);
  InternalWrite(XML, E);
end;

procedure TXmlSerializationObject.LoadFromXML(const XML: TXMLDocument);
begin
  if Assigned(XML) then
    InternalRead(XML.DocumentElement)
  else
    raise Exception.Create(sNotAssignedXMLFile);
end;

end.

