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

unit JSONObjects;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, xmlobject, TypInfo, fpJSON, AbstractSerializationObjects;

type

  { TJSONSerializationObject }

  TJSONSerializationObject = class(TAbstractSerializationObject)
  private
    //FDoc: TJSONObject;
    FJSONName: string;
    procedure SetJSONName(AValue: string);

    function DefaultObjectList:TXmlSerializationObjectList;
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
  public
    //
    FDoc: TJSONObject;
    procedure InternalReadDoc; override;
    //
    constructor Create; override;
    procedure LoadFromStream(AStream:TStream); override;
    procedure SaveToStream(AStream:TStream); override;

    property JSONName:string read FJSONName write SetJSONName;
  end;


  { TJSONSerializationObjectList }

  TJSONSerializationObjectList = class(TXmlSerializationObjectList)
  private
  protected
  public
    constructor Create(ABaseClass:TAbstractSerializationObjectClass);
    procedure LoadFromStream(AStream:TStream);
    procedure LoadFromFile(AFileName:string);
  end;

  { GJSONSerializationObjectListEnumerator }

  generic GJSONSerializationObjectListEnumerator<GObjList, GObjType> = class
  private
    FList: TJSONSerializationObjectList;
    FPosition: Integer;
  public
    constructor Create(AList: GObjList);
    function GetCurrent: GObjType;
    function MoveNext: Boolean;
    property Current: GObjType read GetCurrent;
  end;

  { GJSONSerializationObjectList }

  generic GJSONSerializationObjectList<GObjType> = class(TJSONSerializationObjectList)
  public type
    TSerializationObjectListEnumerator = specialize GXMLSerializationObjectListEnumerator<TXmlSerializationObjectList, GObjType>;
  private
    function GetItem(AIndex: Integer): GObjType;
  public
    constructor Create;
    function GetEnumerator: TSerializationObjectListEnumerator;
    function AddItem:GObjType;
    property Items[AIndex:Integer]:GObjType read GetItem; default;
  end;

implementation
uses xmlobject_resource, jsonparser;

type
  TXmlSerializationObjectListHack = class(TXmlSerializationObjectList);

{ TJSONSerializationObject }

procedure TJSONSerializationObject.SetJSONName(AValue: string);
begin
  if FJSONName=AValue then Exit;
  FJSONName:=AValue;
end;

function TJSONSerializationObject.DefaultObjectList: TXmlSerializationObjectList;
var
  P: TPropertyDef;
  FInst: TObject;
  FProp: PPropInfo;
  K: TTypeKind;
begin
  Result:=nil;
  for P in PropertyList do
  begin
    if xsaDefault in P.Attribs then
    begin
      FProp:=GetPropInfo(Self, P.PropertyName); //Retreive property informations
      K:=FProp^.PropType^.Kind;
      FInst := TObject(PtrInt( GetOrdProp(Self, FProp)));
      if FInst is TXmlSerializationObjectList then
        Result:=TXmlSerializationObjectList(FInst);
      Exit;
    end;
  end;
end;

procedure TJSONSerializationObject.InternalReadDoc;

procedure DoInObject(AName:string; AList:TXmlSerializationObjectList; J:TJSONObject);
var
  R: TJSONSerializationObject;
begin
  R:=TXmlSerializationObjectListHack(AList).InternalAddObject as TJSONSerializationObject;
  R.JSONName:=AName;
  R.FDoc:=J;
  R.InternalReadDoc;
  R.FDoc:=nil;
end;

var
  J, J1: TJSONData;
  S: String;
  i, n: Integer;
  P: TPropertyDef;
  FProp: PPropInfo;
  K: TTypeKind;
  FInst: TObject;
  JA: TJSONArray;
begin
  for i:=0 to FDoc.Count-1 do
  begin
    S:=FDoc.Names[i];
    J:=FDoc.Items[i];
    if J is TJSONArray then
    begin
      JA:=TJSONArray(J);
      for n:=0 to JA.Count-1 do
      begin
        J1:=JA[n];
        if J1 is TJSONObject then
        begin
          P:=PropertyList.PropertyByXMLName(S);
          if Assigned(P) then
          begin
            FProp:=GetPropInfo(Self, P.PropertyName); //Retreive property informations
            K:=FProp^.PropType^.Kind;

            if K in [tkSString, tkAString] then
              InternalReadString(S, J.FormatJSON)
            else
            begin
              FInst := TObject(PtrInt( GetOrdProp(Self, FProp)));
              if not Assigned(FInst) then
                raise Exception.CreateFmt(sClassPropertyNotInit, [P.PropertyName])
              else
              if FInst is TXmlSerializationObjectList then
                DoInObject('', TXmlSerializationObjectListHack(FInst), J1 as TJSONObject)
              else
                raise Exception.CreateFmt(sClassPropertyNotInit, [P.PropertyName]);
            end;
          end
        end
        else
        if J1 is TJSONArray then
          raise Exception.Create('not implementation')
        else
        begin
          InternalReadString(S, J1.AsString);
        end;
      end;
    end
    else
    if J is TJSONObject then
    begin
      P:=PropertyList.PropertyByXMLName(S);
      if Assigned(P) then
      begin
        FProp:=GetPropInfo(Self, P.PropertyName); //Retreive property informations
        K:=FProp^.PropType^.Kind;

        if K in [tkSString, tkAString] then
          InternalReadString(S, J.FormatJSON)
        else
        begin
          FInst := TObject(PtrInt( GetOrdProp(Self, FProp)));
          if not Assigned(FInst) then
            raise Exception.CreateFmt(sClassPropertyNotInit, [P.PropertyName]);
          if FInst is TJSONSerializationObject then
          begin
            TJSONSerializationObject(FInst).FDoc:=J as TJSONObject;
            TJSONSerializationObject(FInst).InternalReadDoc;
            TJSONSerializationObject(FInst).FDoc:=nil;
          end
          else
          if FInst is TXmlSerializationObjectList then
            DoInObject('', TXmlSerializationObjectListHack(FInst), J as TJSONObject);
        end;
      end
      else
      begin
        if Assigned(DefaultObjectList) then
          DoInObject(S, DefaultObjectList, J as TJSONObject);
      end
    end
    else
    begin
      if not J.IsNull then
        InternalReadString(S, J.AsString);
    end;
  end;
end;

procedure TJSONSerializationObject.InternalWriteString(P: TPropertyDef;
  AValue: string);
begin
  FDoc.Add(P.XMLName, TJSONString.Create(AValue));
end;

procedure TJSONSerializationObject.InternalWriteBoolean(P: TPropertyDef;
  AValue: Boolean);
begin
  FDoc.Add(P.XMLName, TJSONBoolean.Create(AValue));
end;

procedure TJSONSerializationObject.InternalWriteInteger(P: TPropertyDef;
  AValue: Integer);
begin
  FDoc.Add(P.XMLName, TJSONIntegerNumber.Create(AValue));
end;

procedure TJSONSerializationObject.InternalWriteInt64(P: TPropertyDef;
  AValue: Int64);
begin
  FDoc.Add(P.XMLName, TJSONInt64Number.Create(AValue));
end;

procedure TJSONSerializationObject.InternalWriteQWord(P: TPropertyDef;
  AValue: QWord);
begin
  FDoc.Add(P.XMLName, TJSONQWordNumber.Create(AValue));
end;

procedure TJSONSerializationObject.InternalWriteDateTime(P: TPropertyDef;
  AValue: TDateTime);
begin
  FDoc.Add(P.XMLName, TJSONString.Create(DateTimeToStr(AValue)));
end;

procedure TJSONSerializationObject.InternalWriteDate(P: TPropertyDef;
  AValue: TDate);
var
  S: string;
begin
  DateTimeToString(S, 'yyyy-mm-dd', AValue);
  FDoc.Add(P.XMLName, TJSONString.Create(S));
  //FDoc.Add(P.XMLName, TJSONString.Create(DateToStr(AValue)));
end;

procedure TJSONSerializationObject.InternalWriteTime(P: TPropertyDef;
  AValue: TTime);
begin
  FDoc.Add(P.XMLName, TJSONString.Create(TimeToStr(AValue)));
end;

procedure TJSONSerializationObject.InternalWriteFloat(P: TPropertyDef;
  AValue: Double);
begin
  FDoc.Add(P.XMLName, TJSONFloatNumber.Create(AValue));
end;

procedure TJSONSerializationObject.InternalWriteDynArray(P: TPropertyDef;
  AProp: PPropInfo);
var
  FJA: TJSONArray;
  FDinArray: TObject;
  L: tdynarrayindex;
  PDT: PTypeData;
  O: TOrdType;
  K: TTypeKind;
  i: Integer;
begin
  FDinArray:=GetObjectProp(Self, AProp);
  L:=DynArraySize(FDinArray);
  PDT:=GetTypeData(AProp^.PropType);
  O:=PDT^.OrdType;
  K:=PDT^.ElType2^.Kind;
  if not (K in [tkInteger, tkString, tkAString]) then
    raise exception.CreateFmt(sUknowPropertyType, [P.PropertyName]);

  FJA:=TJSONArray.Create;
  FDoc.Add(P.XMLName, FJA);

  for i:=0 to L-1 do
  begin
    case K of
      tkInteger:
      begin
        case O of
          //  otSByte,otUByte,otSWord,otUWord,
            otSLong:FJA.Add(TXSDIntegerArray(FDinArray)[i]);
            //otULong,otSQWord,otUQWor
        else
          raise exception.CreateFmt(sUknowPropertyType, [P.PropertyName]);
        end;
      end;
      tkAString,
      tkString:FJA.Add(TXSDStringArray(FDinArray)[i]);
    else
      raise exception.CreateFmt(sUknowPropertyType, [P.PropertyName]);
    end;
  end;
end;

procedure TJSONSerializationObject.InternalWriteClass(P: TPropertyDef;
  AObject: TAbstractSerializationObject);
begin
  if not Assigned(AObject) then Exit;
  TJSONSerializationObject(AObject).FDoc:=TJSONObject.Create;
  FDoc.Add(P.XMLName, TJSONSerializationObject(AObject).FDoc);
  TJSONSerializationObject(AObject).InternalWriteDoc;
  TJSONSerializationObject(AObject).FDoc:=Nil;
end;

procedure TJSONSerializationObject.InternalWriteClassCollection(
  P: TPropertyDef; AObjects: TXmlSerializationObjectList);
var
  i: Integer;
  FJA: TJSONArray;
  FItem: TJSONSerializationObject;
begin
  if xsaDefault in P.Attribs then
  begin
    for i:=0 to AObjects.Count-1 do
    begin
      FItem:=TXmlSerializationObjectListHack(AObjects).InternalGetItem(i) as TJSONSerializationObject;
      FItem.FDoc:=TJSONObject.Create;
      FDoc.Add(FItem.JSONName, FItem.FDoc);
      FItem.InternalWriteDoc;
      FItem.FDoc:=nil;
    end;
  end
  else
  begin
    FJA:=TJSONArray.Create;
    FDoc.Add(P.XMLName, FJA);

    for i:=0 to AObjects.Count-1 do
    begin
      FItem:=TXmlSerializationObjectListHack(AObjects).InternalGetItem(i) as TJSONSerializationObject;
      FItem.FDoc:=TJSONObject.Create;
      FJA.Add(FItem.FDoc);
      FItem.InternalWriteDoc;
      FItem.FDoc:=nil;
    end;
  end;
end;

constructor TJSONSerializationObject.Create;
begin
  inherited Create;
  //FIgnoreReadUndefProps:=true;
end;

procedure TJSONSerializationObject.LoadFromStream(AStream: TStream);
var
  P: TJSONParser;
begin
  inherited LoadFromStream(AStream);
  P:=TJSONParser.Create(AStream);
  try
    FDoc:=P.Parse as TJSONObject;
    //FDoc:=P.Parse as TJSONData;
    InternalReadDoc;
    FDoc.Free;
  finally
    P.Free;
  end;
end;

procedure TJSONSerializationObject.SaveToStream(AStream: TStream);
var
  S: TJSONStringType;
  JO: TJSONString;
begin
  FDoc:=TJSONObject.Create;
  try
    InternalWriteDoc;
    S:=FDoc.FormatJSON;
    if S<>'' then
      AStream.Write(S[1], Length(S));
  finally
    FreeAndNil(FDoc);
  end;
end;


{ TJSONSerializationObjectList }

constructor TJSONSerializationObjectList.Create(
  ABaseClass: TAbstractSerializationObjectClass);
begin
  inherited Create(ABaseClass)
end;

procedure TJSONSerializationObjectList.LoadFromStream(AStream: TStream);
var
  P: TJSONParser;
  FDoc: TJSONArray;
  P1: TJSONEnum;
  S: TJSONStringType;
  T: TJSONSerializationObject;
begin
  P:=TJSONParser.Create(AStream);
  try
    FDoc:=P.Parse as TJSONArray;
    //FDoc:=P.Parse as TJSONData;
    for P1 in FDoc do
    begin
      S:=P1.Key;
      S:=P1.Value.ClassName;
      if P1.Value is TJSONObject then
      begin
        T:=InternalAddObject as TJSONSerializationObject;
        T.FDoc:=P1.Value as TJSONObject;
        T.InternalReadDoc;
      end;
    end;
    FDoc.Free;
  finally
    P.Free;
  end;
end;

procedure TJSONSerializationObjectList.LoadFromFile(AFileName: string);
var
  F: TFileStream;
begin
  F:=TFileStream.Create(AFileName, fmOpenRead);
  LoadFromStream(F);
  F.Free;
end;

{ GJSONSerializationObjectListEnumerator }

constructor GJSONSerializationObjectListEnumerator.Create(AList: GObjList);
begin
  FList := AList;
  FPosition := -1;
end;

function GJSONSerializationObjectListEnumerator.GetCurrent: GObjType;
begin
  Result := GObjType(FList.FList[FPosition]);
end;

function GJSONSerializationObjectListEnumerator.MoveNext: Boolean;
begin
  Inc(FPosition);
  Result := FPosition < FList.Count;
end;

function GJSONSerializationObjectList.GetItem(AIndex: Integer): GObjType;
begin
  Result:=GObjType(FList[AIndex]);
end;

constructor GJSONSerializationObjectList.Create;
begin
  inherited Create(GObjType);
end;

function GJSONSerializationObjectList.GetEnumerator: TSerializationObjectListEnumerator;
begin
  Result:=TSerializationObjectListEnumerator.Create(Self);
end;

function GJSONSerializationObjectList.AddItem: GObjType;
begin
  Result:=GObjType(InternalAddObject);
end;

end.

