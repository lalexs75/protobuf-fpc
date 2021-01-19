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

unit AbstractSerializationObjects;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TypInfo;

type
  EExchangeDefinitionError = class(Exception);
  TXSAttrib = (xsaSimpleObject, xsaRequared, xsaDefault);
  TXSAttribs = set of TXSAttrib;
  TPropertyListEnumerator = class;
  TAbstractSerializationObject = class;
  TXmlSerializationObjectList = class;

  TXSDIntegerArray = array of Integer;
  TXSDStringArray = array of String;


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
    //property Aliases:string read FAliases write FAliases;
  end;

  { TPropertyList }

  TPropertyList = class
  private
    FList:TFPList;
    FOwner:TAbstractSerializationObject;
    function GetCount: integer;
    function GetItems(AIndex: integer): TPropertyDef;
    procedure ClearModified;
  public
    constructor Create(AOwner:TAbstractSerializationObject);
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

  { TAbstractSerializationObject }

  TAbstractSerializationObject = class
  private
    FPropertyList:TPropertyList;
  protected
    FIgnoreReadUndefProps:Boolean;
    property PropertyList:TPropertyList read FPropertyList;
    function IsEmpty:Boolean;
    procedure ValidateRequared;
    function RegisterProperty(APropertyName, AXMLName:string; AAttribs:TXSAttribs; ACaption:string; AMinSize, AMaxSize:integer; Aliases:string = ''):TPropertyDef;
    procedure InternalRegisterPropertys; virtual;
    procedure InternalInitChilds; virtual;
    procedure ModifiedProperty(APropertyName:string);

    procedure InternalWriteDoc;
    procedure InternalReadDynArrayElement(P:TPropertyDef; AProp:PPropInfo; AValue:string);
  protected
    procedure InternalReadDoc; virtual; abstract;
    procedure InternalReadString(AName, AValue:string);

    procedure InternalWriteString(P: TPropertyDef; AValue:string); virtual; abstract;
    procedure InternalWriteBoolean(P: TPropertyDef; AValue:Boolean); virtual; abstract;
    procedure InternalWriteInteger(P: TPropertyDef; AValue:Integer); virtual; abstract;
    procedure InternalWriteInt64(P: TPropertyDef; AValue:Int64); virtual; abstract;
    procedure InternalWriteQWord(P: TPropertyDef; AValue:QWord); virtual; abstract;
    procedure InternalWriteDateTime(P: TPropertyDef; AValue:TDateTime); virtual; abstract;
    procedure InternalWriteDate(P: TPropertyDef; AValue:TDate); virtual; abstract;
    procedure InternalWriteTime(P: TPropertyDef; AValue:TTime); virtual; abstract;
    procedure InternalWriteFloat(P: TPropertyDef; AValue:Double); virtual; abstract;
    procedure InternalWriteDynArray(P: TPropertyDef; AProp:PPropInfo); virtual; abstract;

    procedure InternalWriteClass(P: TPropertyDef; AObject:TAbstractSerializationObject); virtual; abstract;
    procedure InternalWriteClassCollection(P: TPropertyDef; AObjects:TXmlSerializationObjectList); virtual; abstract;
  protected
    procedure CheckLockupValue(APropertyName:string; AValue:string);
    procedure CheckLockupValue(APropertyName:string; AValue:Integer); inline;
    procedure CheckStrMinSize(APropertyName:string; AValue:string);
    procedure CheckStrMaxSize(APropertyName:string; AValue:string);
    procedure CheckFixedValue(APropertyName:string; AValue:string);
    procedure CheckFixedValue(APropertyName:string; AValue:Int64);
    procedure CheckFixedValue(APropertyName:string; AValue:Double);

    procedure CheckMinExclusiveValue(APropertyName:string; AValue:Int64);
    procedure CheckMaxExclusiveValue(APropertyName:string; AValue:Int64);
    procedure CheckMinInclusiveValue(APropertyName:string; AValue:Int64);
    procedure CheckMaxInclusiveValue(APropertyName:string; AValue:Int64);

    procedure CheckMinExclusiveValue(APropertyName:string; AValue:Double);
    procedure CheckMaxExclusiveValue(APropertyName:string; AValue:Double);
    procedure CheckMinInclusiveValue(APropertyName:string; AValue:Double);
    procedure CheckMaxInclusiveValue(APropertyName:string; AValue:Double);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;

    procedure SaveToFile(AFileName:string); virtual;
    procedure LoadFromFile(AFileName:string); virtual;
    procedure LoadFromStream(AStream:TStream); virtual;
    procedure SaveToStream(AStream:TStream); virtual;

    property RegistredPropertyList:TPropertyList read FPropertyList;
  end;
  TAbstractSerializationObjectClass = class of TAbstractSerializationObject;

  { TXmlSerializationObjectList }

  TXmlSerializationObjectList = class
  private
    FList:TFPList;
    FBaseClass:TAbstractSerializationObjectClass;
    function GetCount: Integer;
  protected
    function InternalAddObject:TAbstractSerializationObject;
    function InternalGetItem(AIndex: Integer):TAbstractSerializationObject;
  public
    constructor Create(ABaseClass:TAbstractSerializationObjectClass);
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

uses LazUTF8, xmlobject_resource;

{ TAbstractSerializationObject }

function TAbstractSerializationObject.IsEmpty: Boolean;
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

    FProp:=GetPropInfo(Self, P.PropertyName);
    if not Assigned(FProp) then
      raise Exception.CreateFmt(sPropertyNotFound, [ClassName, P.PropertyName, P.Caption]);

    if FProp^.PropType^.Kind = tkClass then
    begin
      O:=TObject(PtrInt( GetOrdProp(Self, FProp)));
      if Assigned(O) then
      begin
        if O is TAbstractSerializationObject then
        begin
          if not TAbstractSerializationObject(O).IsEmpty then
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

procedure TAbstractSerializationObject.ValidateRequared;
var
  P: TPropertyDef;
  FProp: PPropInfo;
begin
  for P in FPropertyList do
  begin
    FProp:=GetPropInfo(Self, P.PropertyName);
    if Assigned(FProp) and (FProp^.PropType^.Kind <> tkClass) then
      if (not P.Modified) and (xsaRequared in P.Attribs) then
        raise Exception.CreateFmt(sPropertyRequaredValue, [ClassName, P.PropertyName]);
  end;
end;

function TAbstractSerializationObject.RegisterProperty(APropertyName,
  AXMLName: string; AAttribs: TXSAttribs; ACaption: string; AMinSize,
  AMaxSize: integer; Aliases: string): TPropertyDef;
begin
  Result:=FPropertyList.Add(APropertyName, AXMLName, AAttribs, ACaption, AMinSize, AMaxSize);
  Result.FAliases:=Aliases;
end;

procedure TAbstractSerializationObject.InternalRegisterPropertys;
begin

end;

procedure TAbstractSerializationObject.InternalInitChilds;
begin

end;

procedure TAbstractSerializationObject.ModifiedProperty(APropertyName: string);
var
  P: TPropertyDef;
begin
  P:=FPropertyList.PropertyByName(APropertyName);
  if Assigned(P) then
    P.FModified:=true
end;

procedure TAbstractSerializationObject.InternalWriteDoc;
var
  P: TPropertyDef;
  FProp: PPropInfo;
  K: TTypeKind;
  TN: String;
  FPropObjValue: TObject;
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
      tkLString : if P.Modified then InternalWriteString(P, GetStrProp(Self, P.PropertyName));
      tkBool : if P.Modified then InternalWriteBoolean(P, GetOrdProp(Self, P.PropertyName) = 1);
      tkQWord : if P.Modified then InternalWriteQWord(P, GetInt64Prop(Self, P.PropertyName));
      tkInt64 : if P.Modified then InternalWriteInt64(P, GetInt64Prop(Self, P.PropertyName));
      tkInteger : if P.Modified then InternalWriteInteger(P, GetInt64Prop(Self, P.PropertyName));
      tkFloat :
        if P.Modified then
        begin
          if TN = 'TTime' then
            InternalWriteTime(P, GetFloatProp(Self, P.PropertyName))
          else
          if TN = 'TDate' then
            InternalWriteDate(P, GetFloatProp(Self, P.PropertyName))
          else
          if TN = 'TDateTime' then
            InternalWriteDateTime(P, GetFloatProp(Self, P.PropertyName))
          else
            InternalWriteFloat(P, GetFloatProp(Self, P.PropertyName));
        end;
      tkClass:
        begin
          FPropObjValue:=TObject(PtrInt( GetOrdProp(Self, FProp)));
          if FPropObjValue is TAbstractSerializationObject then
            InternalWriteClass(P, TAbstractSerializationObject(FPropObjValue))
          else
          if FPropObjValue is TXmlSerializationObjectList then
            InternalWriteClassCollection(P, TXmlSerializationObjectList(FPropObjValue))
          else
            raise exception.CreateFmt(sUknowPropertyType, [P.PropertyName]);
        end;
      tkDynArray:if P.Modified then InternalWriteDynArray(P, FProp);
    else
      raise exception.CreateFmt(sUknowPropertyType, [P.PropertyName]);
    end;
  end;
end;

procedure TAbstractSerializationObject.InternalReadString(AName, AValue: string
  );
var
  P: TPropertyDef;
  FProp: PPropInfo;
  K: TTypeKind;
  TN: String;
  DT: TDateTime;
  D:Extended;
  C:Integer;
begin
  P:=PropertyList.PropertyByXMLName(AName);
  if Assigned(P) then
  begin
    FProp:=GetPropInfo(Self, P.PropertyName);

    if not Assigned(FProp) then
      raise Exception.CreateFmt(sPropertyNotFound, [ClassName, P.PropertyName, P.Caption]);

    K:=FProp^.PropType^.Kind;
    TN:=FProp^.PropType^.Name;

    case K of
      tkChar,
      tkAString,
      tkWString,
      tkSString,
      tkLString : SetStrProp(Self, FProp, AValue);
      tkBool : SetOrdProp(Self, FProp, Ord(StrToBool(AValue)));
//          tkQWord : SetOrdProp(Self, FProp, Ord(ABuf.ReadAsQWord));
      tkInt64 : SetInt64Prop(Self, FProp, StrToInt64(AValue));
      tkInteger : SetOrdProp(Self, FProp, StrToInt(AValue));

//          tkSet                       : SetSetProp(t,PropInfo,S);
      tkFloat :
        begin
          if TN = 'TTime' then
          begin
            DT:=StrToTime(AValue); //FormatDateTime('HH:NN:SS', D);
            SetFloatProp(Self, FProp, DT);
          end
          else
          if TN = 'TDate' then
          begin
            DT:=StrToDate(AValue); //FormatDateTime('YYYY-MM-DD', D);
            SetFloatProp(Self, FProp, DT);
          end
          else
          if TN = 'TDateTime' then
          begin
            DT:=StrToDateTime(AValue); //FormatDateTime('YYYY-MM-DD''T''HH:NN:SS', D);
            SetFloatProp(Self, FProp, DT);
          end
          else
          begin
            Val(AValue, D, C);
            if C = 0 then
              SetFloatProp(Self, FProp, D);
          end;
        end;
      tkDynArray:InternalReadDynArrayElement(P, FProp, AValue);
    else
      raise exception.CreateFmt(sUknowPropertyType, [P.PropertyName]);
    end;
  end
  else
    if not FIgnoreReadUndefProps then
      raise exception.CreateFmt(sNotFoundPropertyForField, [ClassName, AName]);
end;

procedure TAbstractSerializationObject.InternalReadDynArrayElement(
  P: TPropertyDef; AProp: PPropInfo; AValue: string);
var
  vDinArray: Pointer;
  L: tdynarrayindex;
  PDT: PTypeData;
  O: TOrdType;
  EL: PTypeInfo;
  K: TTypeKind;
begin
  vDinArray:=GetObjectProp(Self, AProp);
  L:=DynArraySize(vDinArray);
  PDT:=GetTypeData(AProp^.PropType);
  O:=PDT^.OrdType;
  EL:=PDT^.ElType2;
  K:=EL^.Kind;
//  KN:=EL^.Name;

  L:=L+1;
  DynArraySetLength(vDinArray, AProp^.PropType, 1, @L);

  case K of
    tkInteger:
    begin
      case O of
        //  otSByte,otUByte,otSWord,otUWord,
          otSLong:TXSDIntegerArray(vDinArray)[L-1]:=StrToInt(AValue);
          //otULong,otSQWord,otUQWor
      else
        raise exception.CreateFmt(sUknowPropertyType, [P.PropertyName]);
      end;
    end;
    tkAString,
    tkString:TXSDStringArray(vDinArray)[L-1]:=AValue;
  else
    raise exception.CreateFmt(sUknowPropertyType, [P.PropertyName]);
  end;
  SetObjectProp(Self, AProp, TObject(vDinArray));
end;

constructor TAbstractSerializationObject.Create;
begin
  inherited Create;
  FIgnoreReadUndefProps:=false;
  FPropertyList:=TPropertyList.Create(Self);

  InternalInitChilds;
  InternalRegisterPropertys;
end;

destructor TAbstractSerializationObject.Destroy;
begin
  FreeAndNil(FPropertyList);
  inherited Destroy;
end;

procedure TAbstractSerializationObject.Clear;
begin
  { TODO -oalexs : Необходимо реализовать метод очистки полей }
end;

procedure TAbstractSerializationObject.SaveToFile(AFileName: string);
var
  F: TFileStream;
begin
  F:=TFileStream.Create(AFileName, fmCreate);
  SaveToStream(F);
  F.Free;
end;

procedure TAbstractSerializationObject.LoadFromFile(AFileName: string);
var
  F: TFileStream;
begin
  F:=TFileStream.Create(AFileName, fmOpenRead);
  LoadFromStream(F);
  F.Free;
end;

procedure TAbstractSerializationObject.LoadFromStream(AStream: TStream);
begin
  Clear;
end;

procedure TAbstractSerializationObject.SaveToStream(AStream: TStream);
begin
  //
end;

procedure TAbstractSerializationObject.CheckLockupValue(APropertyName: string;
  AValue: string);
var
  P: TPropertyDef;
  i: Integer;
begin
  P:=PropertyList.PropertyByName(APropertyName);
  if Assigned(P) then
  begin
    if P.ValidList.Count>0 then
      if not P.ValidList.Find(AValue, i) then
        raise Exception.CreateFmt(sVvalueNotInRange, [APropertyName, AValue]);
  end
end;

procedure TAbstractSerializationObject.CheckLockupValue(APropertyName: string;
  AValue: Integer); inline;
begin
  CheckLockupValue(APropertyName, IntToStr(AValue));
end;

procedure TAbstractSerializationObject.CheckStrMinSize(APropertyName: string;
  AValue: string);
var
  P: TPropertyDef;
begin
  P:=PropertyList.PropertyByName(APropertyName);
  if Assigned(P) and (P.MinSize>-1) then
    if UTF8Length(AValue) < P.MinSize then
      raise Exception.CreateFmt(sValueShorterThat, [UnitName +'.'+ ClassName, APropertyName, AValue, P.MinSize]);
end;

procedure TAbstractSerializationObject.CheckStrMaxSize(APropertyName: string;
  AValue: string);
var
  P: TPropertyDef;
begin
  P:=FPropertyList.PropertyByName(APropertyName);
  if Assigned(P) and (P.MaxSize>-1) then
    if UTF8Length(AValue) > P.MaxSize then
      raise Exception.CreateFmt(sValueGreaterThan, [ClassName, APropertyName, AValue, P.MaxSize]);
end;

procedure TAbstractSerializationObject.CheckFixedValue(APropertyName: string;
  AValue: string);
var
  P: TPropertyDef;
begin
  P:=FPropertyList.PropertyByName(APropertyName);
  if Assigned(P) then
    if P.DefaultValue <> AValue then
      raise Exception.CreateFmt(sValueNotEqualToFixedValue, [APropertyName, AValue, P.DefaultValue]);
end;

procedure TAbstractSerializationObject.CheckFixedValue(APropertyName: string;
  AValue: Int64);
var
  P: TPropertyDef;
begin
  P:=FPropertyList.PropertyByName(APropertyName);
  if Assigned(P) then
    if StrToInt(P.DefaultValue) <> AValue then
      raise Exception.CreateFmt(sValueNotEqualToFixedValueInt, [APropertyName, AValue, P.DefaultValue]);
end;

procedure TAbstractSerializationObject.CheckFixedValue(APropertyName: string;
  AValue: Double);
var
  V : Double;
  P: TPropertyDef;
  C: integer;
begin
  P:=FPropertyList.PropertyByName(APropertyName);
  if Assigned(P) then
  begin
    Val(P.DefaultValue, V, C);
    if V <> AValue then
      raise Exception.CreateFmt(sValueNotEqualToFixedValueFloat, [APropertyName, AValue, P.DefaultValue]);
  end;
end;

procedure TAbstractSerializationObject.CheckMinExclusiveValue(APropertyName: string;
  AValue: Int64);
var
  P: TPropertyDef;
begin
  P:=FPropertyList.PropertyByName(APropertyName);
  if Assigned(P) then
    if P.minExclusiveInt >= AValue then
      raise Exception.CreateFmt(sValueIsLoweredThatInt, [APropertyName, AValue, P.minExclusiveInt]);
end;

procedure TAbstractSerializationObject.CheckMaxExclusiveValue(APropertyName: string;
  AValue: Int64);
var
  P: TPropertyDef;
begin
  P:=FPropertyList.PropertyByName(APropertyName);
  if Assigned(P) then
    if P.maxExclusiveInt <= AValue then
      raise Exception.CreateFmt(sValueIsGreatedInt, [APropertyName, AValue, P.maxExclusiveInt]);
end;

procedure TAbstractSerializationObject.CheckMinInclusiveValue(APropertyName: string;
  AValue: Int64);
var
  P: TPropertyDef;
begin
  P:=FPropertyList.PropertyByName(APropertyName);
  if Assigned(P) then
    if P.minInclusiveInt > AValue then
      raise Exception.CreateFmt(sValueIsLoweredThatInt, [APropertyName, AValue, P.minInclusiveInt]);
end;

procedure TAbstractSerializationObject.CheckMaxInclusiveValue(APropertyName: string;
  AValue: Int64);
var
  P: TPropertyDef;
begin
  P:=FPropertyList.PropertyByName(APropertyName);
  if Assigned(P) then
    if P.maxInclusiveInt < AValue then
      raise Exception.CreateFmt(sValueIsGreatedInt, [APropertyName, AValue, P.maxInclusiveInt]);
end;

procedure TAbstractSerializationObject.CheckMinExclusiveValue(APropertyName: string;
  AValue: Double);
var
  P: TPropertyDef;
begin
  P:=FPropertyList.PropertyByName(APropertyName);
  if Assigned(P) then
    if P.minExclusiveFloat >= AValue then
      raise Exception.CreateFmt(sValueIsLoweredThatFloat, [APropertyName, AValue, P.minExclusiveFloat]);
end;

procedure TAbstractSerializationObject.CheckMaxExclusiveValue(APropertyName: string;
  AValue: Double);
var
  P: TPropertyDef;
begin
  P:=FPropertyList.PropertyByName(APropertyName);
  if Assigned(P) then
    if P.maxExclusiveFloat <= AValue then
      raise Exception.CreateFmt(sValueIsGreatedFloat, [APropertyName, AValue, P.maxExclusiveFloat]);
end;

procedure TAbstractSerializationObject.CheckMinInclusiveValue(APropertyName: string;
  AValue: Double);
var
  P: TPropertyDef;
begin
  P:=FPropertyList.PropertyByName(APropertyName);
  if Assigned(P) then
    if P.minInclusiveFloat > AValue then
      raise Exception.CreateFmt(sValueIsLoweredThatFloat, [APropertyName, AValue, P.minInclusiveFloat]);
end;

procedure TAbstractSerializationObject.CheckMaxInclusiveValue(APropertyName: string;
  AValue: Double);
var
  P: TPropertyDef;
begin
  P:=FPropertyList.PropertyByName(APropertyName);
  if Assigned(P) then
    if P.maxInclusiveFloat < AValue then
      raise Exception.CreateFmt(sValueIsGreatedFloat, [APropertyName, AValue, P.maxInclusiveFloat]);
end;

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

constructor TPropertyList.Create(AOwner: TAbstractSerializationObject);
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

{ TXmlSerializationObjectList }

function TXmlSerializationObjectList.GetCount: Integer;
begin
  Result:=FList.Count;
end;

constructor TXmlSerializationObjectList.Create(
  ABaseClass: TAbstractSerializationObjectClass);
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
    TAbstractSerializationObject(FList[i]).Free;
  FList.Clear;
end;

destructor TXmlSerializationObjectList.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited Destroy;
end;

function TXmlSerializationObjectList.InternalAddObject: TAbstractSerializationObject;
begin
  Result:=FBaseClass.Create;
  FList.Add(Result);
end;

function TXmlSerializationObjectList.InternalGetItem(AIndex: Integer
  ): TAbstractSerializationObject;
begin
  Result:=TAbstractSerializationObject(FList[AIndex]);
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
end.

