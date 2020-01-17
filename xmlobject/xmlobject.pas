{ interface library for FPC and Lazarus

  Copyright (C) 2019 Lagunov Aleksey alexs75@yandex.ru

  Генерация xml файлов для электронного документооборота

  Структуры данных базируются на основании "Приказ ФНС РФ от 19.12.2018 N ММВ-7-15/820@"

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
  Classes, SysUtils, DOM;

type
  EExchangeDefinitionError = class(Exception);
  TXSAttrib = (xsaSimpleObject, xsaRequared);
  TXSAttribs = set of TXSAttrib;
  TPropertyListEnumerator = class;
  TXmlSerializationObject = class;

  { TPropertyDef }

  TPropertyDef = class
  private
    FAttribs: TXSAttribs;
    FCaption: string;
    FDefaultValue: string;
    FmaxExclusiveFloat: Double;
    FmaxExclusiveInt: Int64;
    FmaxInclusiveFloat: Double;
    FmaxInclusiveInt: Int64;
    FMaxSize: integer;
    FminExclusiveFloat: Double;
    FminExclusiveInt: Int64;
    FminInclusiveFloat: Double;
    FminInclusiveInt: Int64;
    FMinSize: integer;
    FModified: boolean;
    FPropertyName: string;
    FTotalDigits: integer;
    FValidList: TStringList;
    FXMLName: string;
    FAliases:string;
  public
    constructor Create;
    destructor Destroy; override;
    property PropertyName:string read FPropertyName;
    property Caption:string read FCaption;
    property XMLName:string read FXMLName;
    property Modified:boolean read FModified;
    property MinSize:integer read FMinSize;
    property MaxSize:integer read FMaxSize;
    property Attribs:TXSAttribs read FAttribs write FAttribs;
    property ValidList:TStringList read FValidList;
    property DefaultValue:string read FDefaultValue write FDefaultValue;
    property TotalDigits:integer read FTotalDigits write FTotalDigits;
    property FractionDigits:integer read FTotalDigits write FTotalDigits;

    property minExclusiveInt:Int64 read FminExclusiveInt write FminExclusiveInt;
    property maxExclusiveInt:Int64 read FmaxExclusiveInt write FmaxExclusiveInt;
    property minInclusiveInt:Int64 read FminInclusiveInt write FminInclusiveInt;
    property maxInclusiveInt:Int64 read FmaxInclusiveInt write FmaxInclusiveInt;

    property minExclusiveFloat:Double read FminExclusiveFloat write FminExclusiveFloat;
    property maxExclusiveFloat:Double read FmaxExclusiveFloat write FmaxExclusiveFloat;
    property minInclusiveFloat:Double read FminInclusiveFloat write FminInclusiveFloat;
    property maxInclusiveFloat:Double read FmaxInclusiveFloat write FmaxInclusiveFloat;
  end;

  { TPropertyList }

  TPropertyList = class
  private
    FList:TFPList;
    FOwner:TXmlSerializationObject;
    function GetCount: integer;
    function GetItems(AIndex: integer): TPropertyDef;
    procedure ClearModified;
  public
    constructor Create(AOwner:TXmlSerializationObject);
    destructor Destroy; override;

    function PropertyByName(APropertyName:string):TPropertyDef;
    function PropertyByXMLName(AXMLName:string):TPropertyDef;
    function PropertyByAlias(AAliasName:string):TPropertyDef;
    function Add(const APropertyName, AXMLName:string; AAttribs:TXSAttribs; ACaption:string; AMinSize, AMaxSize:integer):TPropertyDef;
    procedure Clear;
    function GetEnumerator: TPropertyListEnumerator;
    property Count:integer read GetCount;
    property Items[AIndex:integer]:TPropertyDef read GetItems; default;
  end;

  { TPropertyListEnumerator }

  TPropertyListEnumerator = class
  private
    FList: TPropertyList;
    FPosition: Integer;
  public
    constructor Create(AList: TPropertyList);
    function GetCurrent: TPropertyDef;
    function MoveNext: Boolean;
    property Current: TPropertyDef read GetCurrent;
  end;


  { TXmlSerializationObject }

  TXmlSerializationObject = class
  private
    FPropertyList:TPropertyList;
    procedure InternalRead(AElement: TDOMNode);
    procedure DoLoadAtributes(AElement: TDOMNode);
    procedure DoLoadChild(AElement: TDOMNode);

    procedure InternalWrite(FXML: TXMLDocument; AElement: TDOMElement);
    procedure InternalWriteChild(FXML: TXMLDocument; AChild:TObject; AElement: TDOMElement; P: TPropertyDef);
    procedure SetAtribute(P: TDOMElement; AttribName, AttribValue:DOMString; Prop:TPropertyDef);
    function CreateElement(FXML: TXMLDocument; AParent:TDOMNode; AName:string):TDOMElement;
  protected
    function IsEmpty:Boolean;
    procedure ValidateRequared;
    function RegisterProperty(APropertyName, AXMLName:string; AAttribs:TXSAttribs; ACaption:string; AMinSize, AMaxSize:integer; Aliases:string = ''):TPropertyDef;
    procedure ModifiedProperty(APropertyName:string);
    procedure InternalRegisterPropertys; virtual;
    procedure InternalInitChilds; virtual;
    function RootNodeName:string; virtual;
  protected

    procedure CheckLockupValue(APropertyName:string; AValue:string);
    procedure CheckLockupValue(APropertyName:string; AValue:Integer); inline;
    procedure CheckStrMinSize(APropertyName:string; AValue:string);
    procedure CheckStrMaxSize(APropertyName:string; AValue:string);
    procedure CheckFixedValue(APropertyName:string; AValue:string);
    procedure CheckFixedValue(APropertyName:string; AValue:Int64);

    procedure CheckMinExclusiveValue(APropertyName:string; AValue:Int64);
    procedure CheckMaxExclusiveValue(APropertyName:string; AValue:Int64);
    procedure CheckMinInclusiveValue(APropertyName:string; AValue:Int64);
    procedure CheckMaxInclusiveValue(APropertyName:string; AValue:Int64);

    procedure CheckMinExclusiveValue(APropertyName:string; AValue:Double);
    procedure CheckMaxExclusiveValue(APropertyName:string; AValue:Double);
    procedure CheckMinInclusiveValue(APropertyName:string; AValue:Double);
    procedure CheckMaxInclusiveValue(APropertyName:string; AValue:Double);
  public
    constructor Create;
    destructor Destroy; override;

    procedure SaveToFile(AFileName:string); virtual;
    procedure LoadFromFile(AFileName:string);

    procedure LoadFromStream(AStream:TStream);
    procedure SaveToStream(AStream:TStream);

    procedure LoadFromStr(AStr:string);
    function SaveToStr:string;

    procedure SaveToXML(const XML: TXMLDocument);
    procedure LoadFromXML(const XML: TXMLDocument);
  end;

  TXmlSerializationObjectClass = class of TXmlSerializationObject;

  { TXmlSerializationObjectList }

  TXmlSerializationObjectList = class
  private
    FList:TFPList;
    FBaseClass:TXmlSerializationObjectClass;
    function GetCount: Integer;
  protected
    function InternalAddObject:TXmlSerializationObject;
    function InternalGetItem(AIndex: Integer):TXmlSerializationObject;
  public
    constructor Create(ABaseClass:TXmlSerializationObjectClass);
    procedure Clear;
    destructor Destroy; override;
    property Count:Integer read GetCount;
  end;

  { GXMLSerializationObjectListEnumerator }

  generic GXMLSerializationObjectListEnumerator<GObjList, GObjType> = class
  private
    FList: TXmlSerializationObjectList;
    FPosition: Integer;
  public
    constructor Create(AList: GObjList);
    function GetCurrent: GObjType;
    function MoveNext: Boolean;
    property Current: GObjType read GetCurrent;
  end;

  { GXMLSerializationObjectList }

  generic GXMLSerializationObjectList<GObjType> = class(TXmlSerializationObjectList)
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
uses XMLRead, XMLWrite, {$IFDEF WINDOWS} xmliconv_windows {$ELSE} xmliconv {$ENDIF}, TypInfo, LazUTF8, xmlobject_resource;

{ TPropertyListEnumerator }

constructor TPropertyListEnumerator.Create(AList: TPropertyList);
begin
  FList := AList;
  FPosition := -1;
end;

function TPropertyListEnumerator.GetCurrent: TPropertyDef;
begin
  Result := FList[FPosition];
end;

function TPropertyListEnumerator.MoveNext: Boolean;
begin
  Inc(FPosition);
  Result := FPosition < FList.Count;
end;

{ TPropertyDef }

constructor TPropertyDef.Create;
begin
  inherited Create;
  FValidList:=TStringList.Create;
  FValidList.Sorted:=true;
end;

destructor TPropertyDef.Destroy;
begin
  FreeAndNil(FValidList);
  inherited Destroy;
end;

{ GXMLSerializationObjectListEnumerator }

constructor GXMLSerializationObjectListEnumerator.Create(AList: GObjList);
begin
  FList := AList;
  FPosition := -1;
end;

function GXMLSerializationObjectListEnumerator.GetCurrent: GObjType;
begin
  Result := GObjType(FList.FList[FPosition]);
end;

function GXMLSerializationObjectListEnumerator.MoveNext: Boolean;
begin
  Inc(FPosition);
  Result := FPosition < FList.Count;
end;

{ GXMLSerializationObjectList }

function GXMLSerializationObjectList.GetItem(AIndex: Integer): GObjType;
begin
  Result:=GObjType(FList[AIndex]);
end;

constructor GXMLSerializationObjectList.Create;
begin
  inherited Create(GObjType);
end;

function GXMLSerializationObjectList.GetEnumerator: TSerializationObjectListEnumerator;
begin
  Result:=TSerializationObjectListEnumerator.Create(Self);
end;

function GXMLSerializationObjectList.AddItem: GObjType;
begin
  Result:=GObjType(InternalAddObject);
end;

{ TXmlSerializationObjectList }

function TXmlSerializationObjectList.GetCount: Integer;
begin
  Result:=FList.Count;
end;

constructor TXmlSerializationObjectList.Create(
  ABaseClass: TXmlSerializationObjectClass);
begin
  inherited Create;
  FList:=TFPList.Create;
  FBaseClass:=ABaseClass;
end;

procedure TXmlSerializationObjectList.Clear;
var
  i: Integer;
begin
  for i:=0 to FList.Count-1 do
    TXmlSerializationObject(FList[i]).Free;
  FList.Clear;
end;

destructor TXmlSerializationObjectList.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited Destroy;
end;

function TXmlSerializationObjectList.InternalAddObject: TXmlSerializationObject;
begin
  Result:=FBaseClass.Create;
  FList.Add(Result);
end;

function TXmlSerializationObjectList.InternalGetItem(AIndex: Integer
  ): TXmlSerializationObject;
begin
  Result:=TXmlSerializationObject(FList[AIndex]);
end;

{ TPropertyList }

function TPropertyList.GetCount: integer;
begin
  Result:=FList.Count;
end;

function TPropertyList.GetItems(AIndex: integer): TPropertyDef;
begin
  Result:=TPropertyDef(FList[AIndex]);
end;

procedure TPropertyList.ClearModified;
var
  i: Integer;
begin
  for i:=0 to FList.Count-1 do
    TPropertyDef(FList[I]).FModified:=false;
end;

constructor TPropertyList.Create(AOwner: TXmlSerializationObject);
begin
  inherited Create;
  FOwner:=AOwner;
  FList:=TFPList.Create;
end;

destructor TPropertyList.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited Destroy;
end;

function TPropertyList.PropertyByName(APropertyName: string): TPropertyDef;
var
  P: TPropertyDef;
  i: Integer;
begin
  Result:=nil;
  APropertyName:=UpperCase(APropertyName);
  for i:=0 to FList.Count-1 do
  begin
    P:=GetItems(I);
    if UpperCase(P.FPropertyName) = APropertyName then
      Exit(P);
  end;

  raise Exception.CreateFmt(sPropertyNotFound1, [FOwner.ClassName, APropertyName]);
end;

function TPropertyList.PropertyByXMLName(AXMLName: string): TPropertyDef;
var
  P: TPropertyDef;
  i: Integer;
begin
  Result:=nil;
  for i:=0 to FList.Count-1 do
  begin
    P:=GetItems(I);
    if P.FXMLName = AXMLName then
      Exit(P);
  end;
end;

function TPropertyList.PropertyByAlias(AAliasName: string): TPropertyDef;
var
  P: TPropertyDef;
  i: Integer;
begin
  Result:=nil;
  for i:=0 to FList.Count-1 do
  begin
    P:=GetItems(I);
    if P.FAliases = AAliasName then
      Exit(P);
  end;
end;

function TPropertyList.Add(const APropertyName, AXMLName: string;
  AAttribs: TXSAttribs; ACaption: string; AMinSize, AMaxSize: integer
  ): TPropertyDef;
begin
  Result:=TPropertyDef.Create;
  FList.Add(Result);
  Result.FAttribs:=AAttribs;
  Result.FPropertyName:=APropertyName;
  Result.FCaption:=ACaption;
  Result.FXMLName:=AXMLName;

  Result.FMaxSize:=AMaxSize;
  Result.FMinSize:=AMinSize;
end;

procedure TPropertyList.Clear;
var
  i: Integer;
begin
  for i:=0 to FList.Count-1 do
    TPropertyDef(FList[I]).Free;
  FList.Clear;
end;

function TPropertyList.GetEnumerator: TPropertyListEnumerator;
begin
  Result:=TPropertyListEnumerator.Create(Self);
end;

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
  K: TTypeKind;
  D:TDateTime;
begin
  ValidateRequared;

  for P in FPropertyList do
  begin

    FProp:=GetPropInfo(Self, P.FPropertyName);
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
//                  SetAtribute(P: TDOMElement; AttribName, AttribValue:string; AMaxLen:Integer);
//      tkBool : SetOrdProp(Self, FProp, Ord(ABuf.ReadAsBoolean));
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
            S:=FormatDateTime('YYYY-MM-DD''T''HH:NN:SS', D);
          end
          else
          begin
            if P.TotalDigits > 0 then
              Str(GetFloatProp(Self, P.PropertyName):P.TotalDigits:P.FractionDigits, S)
            else
              Str(GetFloatProp(Self, P.PropertyName):15:4, S);
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
    else
      raise exception.CreateFmt(sUknowPropertyType, [P.FPropertyName]);
    end;
  end;
end;

procedure TXmlSerializationObject.DoLoadAtributes(AElement: TDOMNode);
var
  i, C: Integer;
  A: TDOMNode;
  S1, NV, TN:string;
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
      P:=FPropertyList.PropertyByXMLName(S1);
      if not Assigned(P) then
        P:=FPropertyList.PropertyByAlias(S1);

      if Assigned(P) then
      begin
        FProp:=GetPropInfo(Self, P.FPropertyName);
        if not Assigned(FProp) then
          raise Exception.CreateFmt(sPropertyNotFound, [ClassName, P.PropertyName, P.Caption]);
        K:=FProp^.PropType^.Kind;
        TN:=FProp^.PropType^.Name;
        case K of
          tkChar,
          tkAString,
          tkWString,
          tkSString,
          tkLString : SetStrProp(Self, FProp, NV);
  (*      tkBool : SetOrdProp(Self, FProp, Ord(ABuf.ReadAsBoolean));
          tkQWord : SetOrdProp(Self, FProp, Ord(ABuf.ReadAsQWord));
  *)
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
          raise exception.CreateFmt(sUknowPropertyType, [P.FPropertyName]);
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
  TN, NV, DS, TS: String;
  D:Extended;
  DT:TDateTime;
  FS: TFormatSettings;
begin
  for i:=0 to AElement.ChildNodes.Count-1 do
  begin
    FNode:=AElement.ChildNodes.Item[I];

    P:=FPropertyList.PropertyByXMLName(FNode.NodeName);
    if not Assigned(P) then
      P:=FPropertyList.PropertyByAlias(FNode.NodeName);

    if Assigned(P) then
    begin
      FProp:=GetPropInfo(Self, P.FPropertyName); //Retreive property informations
      if not Assigned(FProp) then
        raise Exception.CreateFmt(sPropertyNotFound2, [P.FPropertyName]);

      K:=FProp^.PropType^.Kind;
      TN:=FProp^.PropType^.Name;
      if (xsaSimpleObject in P.Attribs) or (K <> tkClass) then
      begin
        NV:=FNode.TextContent;
        case K of
          tkChar,
          tkAString,
          tkWString,
          tkSString,
          tkLString   : SetStrProp(Self, FProp, NV);
          tkInteger : SetInt64Prop(Self, FProp, StrToInt64(NV));
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
        else
          raise exception.CreateFmt(sUknowPropertyType, [P.FPropertyName]);
        end;
      end
      else
      begin
        if K <> tkClass then
          raise Exception.CreateFmt(sPropertyIsNotClassType, [P.FPropertyName]);

        FInst := TObject(PtrInt( GetOrdProp(Self, FProp)));
        if not Assigned(FInst) then
          raise Exception.CreateFmt(sClassPropertyNotInit, [P.FPropertyName]);

        if FInst is TXmlSerializationObject then
          TXmlSerializationObject(FInst).InternalRead(FNode)
        else
        if FInst is TXmlSerializationObjectList then
        begin
          R:=TXmlSerializationObjectList(FInst).InternalAddObject;
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
  if (Prop.FMaxSize > 0) and (UTF8Length(AttribValue) > Prop.FMaxSize) then
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
      Itm:=TXmlSerializationObjectList(AChild).InternalGetItem(I);
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

function TXmlSerializationObject.IsEmpty: Boolean;
var
  i: Integer;
  P: TPropertyDef;
  O: TObject;
  FProp: PPropInfo;
begin
  Result:=true;
  for i:=0 to FPropertyList.Count-1 do
  begin
    P:=FPropertyList[i];

    FProp:=GetPropInfo(Self, P.FPropertyName);
    if not Assigned(FProp) then
      raise Exception.CreateFmt(sPropertyNotFound, [ClassName, P.PropertyName, P.Caption]);

    if FProp^.PropType^.Kind = tkClass then
    begin
      O:=TObject(PtrInt( GetOrdProp(Self, FProp)));
      if Assigned(O) then
      begin
        if O is TXmlSerializationObject then
        begin
          if not TXmlSerializationObject(O).IsEmpty then
            Exit(false);
        end
        else
        if O is TXmlSerializationObjectList then
        begin
          if TXmlSerializationObjectList(O).Count>0 then
            Exit(false);
        end
        else
          raise Exception.CreateFmt(sUknowPropertyType, [P.PropertyName]);
      end
      else
        raise Exception.CreateFmt(sObjectPropertyNotAssigned, [ClassName, P.PropertyName]);
    end
    else
    if P.Modified then
      Exit(false);
  end;
end;

procedure TXmlSerializationObject.ValidateRequared;
var
  P: TPropertyDef;
  FProp: PPropInfo;
begin
  for P in FPropertyList do
  begin
    FProp:=GetPropInfo(Self, P.FPropertyName);
    if Assigned(FProp) and (FProp^.PropType^.Kind <> tkClass) then
      if (not P.Modified) and (xsaRequared in P.Attribs) then
        raise Exception.CreateFmt(sPropertyRequaredValue, [ClassName, P.PropertyName]);
  end;
end;

procedure TXmlSerializationObject.InternalInitChilds;
begin

end;

procedure TXmlSerializationObject.CheckLockupValue(APropertyName: string;
  AValue: string);
var
  P: TPropertyDef;
  i: Integer;
begin
  P:=FPropertyList.PropertyByName(APropertyName);
  if Assigned(P) then
  begin
    if P.ValidList.Count>0 then
      if not P.ValidList.Find(AValue, i) then
        raise Exception.CreateFmt(sVvalueNotInRange, [APropertyName, AValue]);
  end
end;

procedure TXmlSerializationObject.CheckLockupValue(APropertyName: string;
  AValue: Integer); inline;
begin
  CheckLockupValue(APropertyName, IntToStr(AValue));
end;

procedure TXmlSerializationObject.CheckStrMinSize(APropertyName: string;
  AValue: string);
var
  P: TPropertyDef;
begin
  P:=FPropertyList.PropertyByName(APropertyName);
  if Assigned(P) and (P.MinSize>-1) then
    if UTF8Length(AValue) < P.MinSize then
      raise Exception.CreateFmt(sValueShorterThat, [ClassName, APropertyName, AValue, P.MinSize]);
end;

procedure TXmlSerializationObject.CheckStrMaxSize(APropertyName: string;
  AValue: string);
var
  P: TPropertyDef;
begin
  P:=FPropertyList.PropertyByName(APropertyName);
  if Assigned(P) and (P.MaxSize>-1) then
    if UTF8Length(AValue) > P.MaxSize then
      raise Exception.CreateFmt(sValueGreaterThan, [ClassName, APropertyName, AValue, P.MaxSize]);
end;

procedure TXmlSerializationObject.CheckFixedValue(APropertyName: string;
  AValue: string);
var
  P: TPropertyDef;
begin
  P:=FPropertyList.PropertyByName(APropertyName);
  if Assigned(P) then
    if P.DefaultValue <> AValue then
      raise Exception.CreateFmt(sValueNotEqualToFixedValue, [APropertyName, AValue, P.DefaultValue]);
end;

procedure TXmlSerializationObject.CheckFixedValue(APropertyName: string;
  AValue: Int64);
var
  P: TPropertyDef;
begin
  P:=FPropertyList.PropertyByName(APropertyName);
  if Assigned(P) then
    if StrToInt(P.DefaultValue) <> AValue then
      raise Exception.CreateFmt(sValueNotEqualToFixedValueInt, [APropertyName, AValue, P.DefaultValue]);
end;

procedure TXmlSerializationObject.CheckMinExclusiveValue(APropertyName: string;
  AValue: Int64);
var
  P: TPropertyDef;
begin
  P:=FPropertyList.PropertyByName(APropertyName);
  if Assigned(P) then
    if P.minExclusiveInt >= AValue then
      raise Exception.CreateFmt(sValueIsLoweredThatInt, [APropertyName, AValue, P.minExclusiveInt]);
end;

procedure TXmlSerializationObject.CheckMaxExclusiveValue(APropertyName: string;
  AValue: Int64);
var
  P: TPropertyDef;
begin
  P:=FPropertyList.PropertyByName(APropertyName);
  if Assigned(P) then
    if P.maxExclusiveInt <= AValue then
      raise Exception.CreateFmt(sValueIsGreatedInt, [APropertyName, AValue, P.maxExclusiveInt]);
end;

procedure TXmlSerializationObject.CheckMinInclusiveValue(APropertyName: string;
  AValue: Int64);
var
  P: TPropertyDef;
begin
  P:=FPropertyList.PropertyByName(APropertyName);
  if Assigned(P) then
    if P.minInclusiveInt > AValue then
      raise Exception.CreateFmt(sValueIsLoweredThatInt, [APropertyName, AValue, P.minInclusiveInt]);
end;

procedure TXmlSerializationObject.CheckMaxInclusiveValue(APropertyName: string;
  AValue: Int64);
var
  P: TPropertyDef;
begin
  P:=FPropertyList.PropertyByName(APropertyName);
  if Assigned(P) then
    if P.maxInclusiveInt < AValue then
      raise Exception.CreateFmt(sValueIsGreatedInt, [APropertyName, AValue, P.maxInclusiveInt]);
end;

procedure TXmlSerializationObject.CheckMinExclusiveValue(APropertyName: string;
  AValue: Double);
var
  P: TPropertyDef;
begin
  P:=FPropertyList.PropertyByName(APropertyName);
  if Assigned(P) then
    if P.minExclusiveFloat >= AValue then
      raise Exception.CreateFmt(sValueIsLoweredThatFloat, [APropertyName, AValue, P.minExclusiveFloat]);
end;

procedure TXmlSerializationObject.CheckMaxExclusiveValue(APropertyName: string;
  AValue: Double);
var
  P: TPropertyDef;
begin
  P:=FPropertyList.PropertyByName(APropertyName);
  if Assigned(P) then
    if P.maxExclusiveFloat <= AValue then
      raise Exception.CreateFmt(sValueIsGreatedFloat, [APropertyName, AValue, P.maxExclusiveFloat]);
end;

procedure TXmlSerializationObject.CheckMinInclusiveValue(APropertyName: string;
  AValue: Double);
var
  P: TPropertyDef;
begin
  P:=FPropertyList.PropertyByName(APropertyName);
  if Assigned(P) then
    if P.minInclusiveFloat > AValue then
      raise Exception.CreateFmt(sValueIsLoweredThatFloat, [APropertyName, AValue, P.minInclusiveFloat]);
end;

procedure TXmlSerializationObject.CheckMaxInclusiveValue(APropertyName: string;
  AValue: Double);
var
  P: TPropertyDef;
begin
  P:=FPropertyList.PropertyByName(APropertyName);
  if Assigned(P) then
    if P.maxInclusiveFloat < AValue then
      raise Exception.CreateFmt(sValueIsGreatedFloat, [APropertyName, AValue, P.maxInclusiveFloat]);
end;

function TXmlSerializationObject.RootNodeName: string;
begin
  Result:=ClassName;
end;

function TXmlSerializationObject.RegisterProperty(APropertyName,
  AXMLName: string; AAttribs: TXSAttribs; ACaption: string; AMinSize,
  AMaxSize: integer; Aliases: string): TPropertyDef;
begin
  Result:=FPropertyList.Add(APropertyName, AXMLName, AAttribs, ACaption, AMinSize, AMaxSize);
  Result.FAliases:=Aliases;
end;

procedure TXmlSerializationObject.ModifiedProperty(APropertyName: string);
var
  P: TPropertyDef;
begin
  P:=FPropertyList.PropertyByName(APropertyName);
  if Assigned(P) then
    P.FModified:=true
end;

procedure TXmlSerializationObject.InternalRegisterPropertys;
begin

end;

constructor TXmlSerializationObject.Create;
begin
  inherited Create;
  FPropertyList:=TPropertyList.Create(Self);

  InternalInitChilds;
  InternalRegisterPropertys;
end;

destructor TXmlSerializationObject.Destroy;
begin
  FreeAndNil(FPropertyList);
  inherited Destroy;
end;

procedure TXmlSerializationObject.SaveToFile(AFileName: string);
var
  FXML: TXMLDocument;
  E: TDOMElement;
begin
  FXML:=TXMLDocument.Create;
  E:=CreateElement(FXML, FXML, RootNodeName);
  InternalWrite(FXML, E);
  WriteXML(FXML, AFileName);
  FXML.Free;
end;

procedure TXmlSerializationObject.LoadFromFile(AFileName: string);
var
  FXML: TXMLDocument;
begin
  ReadXMLFile(FXML, AFileName);
  InternalRead(FXML.DocumentElement);
  FXML.Free;
end;

procedure TXmlSerializationObject.LoadFromStream(AStream: TStream);
var
  FXML: TXMLDocument;
begin
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

