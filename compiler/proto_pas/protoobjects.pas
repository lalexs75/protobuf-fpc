{ google protobuf files compiler to FPC class

  Copyright (C) 2018-2022 Lagunov Aleksey alexs@yandex.ru

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

unit ProtoObjects;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ProtoParser, EnumObject;

type
  TMessageFieldListEnumerator = class;
  TMessageFieldType = (mftRequired,  mftOptional, mftRepeated, mftEnumDefinition, mftMessageDefinition);
  TProtoDataType = (pdtInternal, pdtEnum, pdtClass);

  { TMessageField }

  TMessageField = class
  private
    FCaption: string;
    FComment: string;
    FDataType: string;
    FDataTypeFlag: TProtoDataType;
    FDefaultValue: string;
    FFieldType: TMessageFieldType;
    FIsPacked: boolean;
    FMessageNumber: integer;
    FProtoObjDef: TProtoObject;
  private
    function IsSetProc:Boolean;
    function IsRealField:Boolean;
    function PascalName:string;
  public
    constructor Create;
    destructor Destroy;override;
    procedure Assign(ASource:TMessageField);
    function PascalDataType:string;
    property Caption:string read FCaption write FCaption;
    property DataType:string read FDataType write FDataType;
    property FieldType:TMessageFieldType read FFieldType write FFieldType;
    property MessageNumber:integer read FMessageNumber write FMessageNumber;
    property Comment:string read FComment write FComment;
    property DataTypeFlag:TProtoDataType read FDataTypeFlag write FDataTypeFlag;
    property IsPacked:boolean read FIsPacked write FIsPacked;
    property DefaultValue:string read FDefaultValue write FDefaultValue;
    property ProtoObjDef:TProtoObject read FProtoObjDef;
  end;

  { TMessageFieldList }

  TMessageFieldList = class
  private
    FList:TFPList;
    function GetCount: integer;
    function GetItem(AIndex: integer): TMessageField;
    procedure Clear;
  public
    constructor Create;
    destructor Destroy;override;
    procedure Assign(ASource:TMessageFieldList);
    function CountEnumDefs:integer;
    function GetEnumerator: TMessageFieldListEnumerator;
    function Add:TMessageField;
    property Item[AIndex:integer]:TMessageField read GetItem; default;
    property Count:integer read GetCount;
  end;

  { TMessageFieldListEnumerator }

  TMessageFieldListEnumerator = class
  private
    FList: TMessageFieldList;
    FPosition: Integer;
  public
    constructor Create(AList: TMessageFieldList);
    function GetCurrent: TMessageField;
    function MoveNext: Boolean;
    property Current: TMessageField read GetCurrent;
  end;

  { TProtoMessage }

  TProtoMessage = class(TProtoObject)
  private
    FCurField:TMessageField;
    FFields: TMessageFieldList;
  protected
    procedure InitParserTree;override;
    procedure InternalProcessChildToken(AParser:TProtoParser; AToken:TProtoToken; AWord:string); override;

  public
    constructor Create;override;
    destructor Destroy;override;
    procedure GenerateInterfaceSection(AModule:TStrings); override;
    procedure GenerateImplementationSection(AModule:TStrings); override;
    procedure Clear; override;
    procedure Assign(ASource:TProtoObject); override;
    property Fields:TMessageFieldList read FFields;
  end;

implementation

type
  TProtoTypeToPas = record
    ProtoType:string;
    PasType:string;
    ArrayType:string;
  end;

const
  SimpleTypesDefs : array of TProtoTypeToPas =
    (
      (ProtoType:'double';
       PasType:'Single';
       ArrayType:'TSingleDynArray';
      ),
      (ProtoType:'float';
       PasType:'Double';
       ArrayType:'TDoubleDynArray';
      ),
      (ProtoType:'int32';
       PasType:'Integer';
       ArrayType:'TIntegerDynArray';
      ),
      (ProtoType:'int64';
       PasType:'Int64';
       ArrayType:'TInt64DynArray';
      ),
      (ProtoType:'uint32';
       PasType:'DWord';
       ArrayType:'TLongWordDynArray';
      ),
      (ProtoType:'uint64';
       PasType:'QWord';
       ArrayType:'TQWordDynArray';
      ),
      (ProtoType:'sint32';
       PasType:'SInt32';
       ArrayType:'TSInt32DynArray';
      ),
      (ProtoType:'sint64';
       PasType:'SInt64';
       ArrayType:'TSInt64DynArray';
      ),
      (ProtoType:'fixed32';
       PasType:'fixed32';
       ArrayType:'TFixed32DynArray';
      ),
      (ProtoType:'fixed64';
       PasType:'fixed64';
       ArrayType:'TFixed64DynArray';
      ),
      (ProtoType:'sfixed32';
       PasType:'sfixed32';
       ArrayType:'TSFixed32DynArray';
      ),
      (ProtoType:'sfixed64';
       PasType:'sfixed64';
       ArrayType:'TSFixed64DynArray';
      ),
      (ProtoType:'bool';
       PasType:'Boolean';
       ArrayType:'TBooleanDynArray';
      ),
      (ProtoType:'string';
       PasType:'String';
       ArrayType:'TDocumentStrings';
      ),
      (ProtoType:'bytes';
       PasType:'TBytes';
       ArrayType:'TBytes64DynArray';
      )
    );

  PasKeyWords : array of string = (
    'and', 'array', 'as', 'asm', 'begin', 'break', 'case', 'class', 'const', 'constructor', 'destructor', 'div',
    'do', 'downto', 'else', 'end', 'except', 'exports', 'false', 'file', 'finalization', 'finally', 'for', 'function',
    'goto', 'if', 'implementation', 'in', 'inherited', 'initialization', 'inline', 'interface', 'is', 'label',
    'library', 'mod', 'nil', 'not', 'object', 'of', 'on', 'operator', 'or', 'out', 'packed', 'procedure', 'program',
    'property', 'raise', 'record', 'repeat', 'self', 'set', 'shl', 'shr', 'string', 'then', 'threadvar', 'to', 'true',
    'try', 'type', 'unit', 'until', 'uses', 'var', 'while', 'with', 'xor');

function IsPasReserwedWord(S:string):Boolean;
var
  I: Integer;
begin
  S:=LowerCase(S);
  Result:=false;
  for I:=1 to High(PasKeyWords) do
    if PasKeyWords[i] = S then
      Exit(true);
end;

  { TMessageField }

function TMessageField.IsSetProc: Boolean;
begin
  Result:=false;
  if FieldType in [mftEnumDefinition, mftMessageDefinition] then Exit;

  Result:=DataTypeFlag in [pdtInternal, pdtEnum];
  if Result and (FFieldType = mftRepeated) then
    Result:=LowerCase(FDataType) <> 'string';
end;

function TMessageField.IsRealField: Boolean;
begin
  Result:=not (FieldType in [mftEnumDefinition, mftMessageDefinition]);
end;

function TMessageField.PascalName: string;
begin
  if IsPasReserwedWord(Caption) then
    Result:=Caption+'Field'
  else
    Result:=Caption;
end;

constructor TMessageField.Create;
begin
  inherited Create;
end;

destructor TMessageField.Destroy;
begin
  if Assigned(FProtoObjDef) then
    FreeAndNil(FProtoObjDef);
  inherited Destroy;
end;

procedure TMessageField.Assign(ASource: TMessageField);
begin
  if not Assigned(ASource) then Exit;
  FCaption:=ASource.FCaption;
  FFieldType:=ASource.FFieldType;
  FDataType:=ASource.FDataType;
  FMessageNumber:=ASource.MessageNumber;
  FDataTypeFlag:=ASource.FDataTypeFlag;
  FIsPacked:=ASource.FIsPacked;
  FDefaultValue:=ASource.FDefaultValue;
end;

function TMessageField.PascalDataType: string;
var
  S: String;
  R: TProtoTypeToPas;
  P: SizeInt;
begin
  if FDataTypeFlag = pdtInternal then
  begin
    S:=LowerCase(FDataType);
    if FFieldType = mftRepeated then
    begin
      for R in SimpleTypesDefs do
        if R.ProtoType = S then
          Exit(R.ArrayType);
    end
    else
    begin
      for R in SimpleTypesDefs do
        if R.ProtoType = S then
          Exit(R.PasType);
    end;
    raise Exception.CreateFmt('Uknow type %s', [FDataType]);
  end
  else
  begin
    P:=Pos('.', FDataType);
    if P > 0 then
    begin
      S:=Copy(FDataType, 1, P);
      S:=S + 'T'+Copy(FDataType, P + 1, Length(FDataType));
    end
    else
      S:='T' + FDataType;
    if (FDataTypeFlag = pdtClass) and (FFieldType = mftRepeated) then
      Result:=S + 's'
    else
    if FFieldType = mftRepeated then
      Result:=S + 'Array'
    else
      Result:=S;

  end;
end;

{ TMessageFieldListEnumerator }

constructor TMessageFieldListEnumerator.Create(AList: TMessageFieldList);
begin
  FList := AList;
  FPosition := -1;
end;

function TMessageFieldListEnumerator.GetCurrent: TMessageField;
begin
  Result := FList[FPosition];
end;

function TMessageFieldListEnumerator.MoveNext: Boolean;
begin
  Inc(FPosition);
  Result := FPosition < FList.Count;
end;

{ TMessageFieldList }

function TMessageFieldList.GetCount: integer;
begin
  Result:=FList.Count;
end;

function TMessageFieldList.GetItem(AIndex: integer): TMessageField;
begin
  Result:=TMessageField(FList[AIndex]);
end;

procedure TMessageFieldList.Clear;
var
  i: Integer;
begin
  for i:=0 to FList.Count-1 do
    TMessageField(FList[i]).Free;
  FList.Clear;
end;

function TMessageFieldList.CountEnumDefs: integer;
var
  i: Integer;
begin
  Result:=0;
  for i:=0 to FList.Count-1 do
    if TMessageField(FList[i]).FieldType = mftEnumDefinition then
      Inc(Result);
end;

constructor TMessageFieldList.Create;
begin
  inherited Create;
  FList:=TFPList.Create;
end;

destructor TMessageFieldList.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

procedure TMessageFieldList.Assign(ASource: TMessageFieldList);
var
  R: TMessageField;
begin
  if not Assigned(ASource) then Exit;
  for R in ASource do
    Add.Assign(R);
end;

function TMessageFieldList.GetEnumerator: TMessageFieldListEnumerator;
begin
  Result:=TMessageFieldListEnumerator.Create(Self);
end;

function TMessageFieldList.Add: TMessageField;
begin
  Result:=TMessageField.Create;
  FList.Add(Result);
end;

{ TProtoMessage }

procedure TProtoMessage.InitParserTree;
var
  FStart, T, T1, TOpt, TRec, TRep, TType1, TType2, TType3, TType4,
    TType5, TType6, TType7, TType8, TType9, TType10, TType11,
    TType12, TType13, TType14, TType15, TType16, TPacked1,
    TPacked2, TPacked21, TPacked22, TDefault1, TDefault2,
    TDefault3, TDefault4, TType16_1, TReserv, TReserv1, TReserv2,
    TReserv3, TReserv4, TEnum1, TMsg1, TDefault5, TDefault6: TProtoToken;
begin
  //message SearchRequest {
  //  required string query = 1;
  //  optional int32 page_number = 2;
  //  optional int32 result_per_page = 3;
  //  repeated int32 samples = 4 [packed=true];
  //}

  //message SearchRequest {
  //  required string query = 1;
  //  optional int32 page_number = 2;
  //  optional int32 result_per_page = 3 [default = 10];
  //  enum Corpus {
  //    UNIVERSAL = 0;
  //    WEB = 1;
  //    IMAGES = 2;
  //    LOCAL = 3;
  //    NEWS = 4;
  //    PRODUCTS = 5;
  //    VIDEO = 6;
  //  }
  //  optional Corpus corpus = 4 [default = UNIVERSAL];
  //}

  //message SearchResponse {
  //  message Result {
  //    required string url = 1;
  //    optional string title = 2;
  //    repeated string snippets = 3;
  //  }
  //  repeated Result result = 1;
  //}
  FStart:=AddToken(stIdentificator, nil, 'message', [toHeaderStart, toHeaderEnd]);
  T:=AddToken(stIdentificator, FStart, '', [], 1);
  T1:=AddToken(stSymbol, T, '{', []);
    TOpt:=AddToken(stKeyword, T1, 'optional', [], 2);
    TRec:=AddToken(stKeyword, T1, 'required', [], 3);
    TRep:=AddToken(stKeyword, T1, 'repeated', [], 4);
      TType1:=AddToken(stKeyword, [TOpt, TRec, TRep], 'double', [], 5);
      TType2:=AddToken(stKeyword, [TOpt, TRec, TRep], 'float', [], 5);
      TType3:=AddToken(stKeyword, [TOpt, TRec, TRep], 'int32', [], 5);
      TType4:=AddToken(stKeyword, [TOpt, TRec, TRep], 'int64', [], 5);
      TType5:=AddToken(stKeyword, [TOpt, TRec, TRep], 'uint32', [], 5);
      TType6:=AddToken(stKeyword, [TOpt, TRec, TRep], 'uint64', [], 5);
      TType7:=AddToken(stKeyword, [TOpt, TRec, TRep], 'sint32', [], 5); //Uses variable-length encoding. Signed int value. These more efficiently encode negative numbers than regular int32s
      TType8:=AddToken(stKeyword, [TOpt, TRec, TRep], 'sint64', [], 5); //Uses variable-length encoding. Signed int value. These more efficiently encode negative numbers than regular int64s
      TType9:=AddToken(stKeyword, [TOpt, TRec, TRep], 'fixed32', [], 5);
      TType10:=AddToken(stKeyword, [TOpt, TRec, TRep], 'fixed64', [], 5);
      TType11:=AddToken(stKeyword, [TOpt, TRec, TRep], 'sfixed32', [], 5);
      TType12:=AddToken(stKeyword, [TOpt, TRec, TRep], 'sfixed64', [], 5);
      TType13:=AddToken(stKeyword, [TOpt, TRec, TRep], 'bool', [], 5);
      TType14:=AddToken(stKeyword, [TOpt, TRec, TRep], 'string', [], 5);
      TType15:=AddToken(stKeyword, [TOpt, TRec, TRep], 'bytes', [], 5);
      TType16:=AddToken(stIdentificator, [TOpt, TRec, TRep], '', [], 9);
      TType16_1:=AddToken(stSymbol, TType16, '.', [], 9);
        TType16_1.AddChildToken(TType16);

    T:=AddToken(stIdentificator, [TType1, TType2, TType3, TType4, TType5, TType6, TType7, TType8,
      TType9, TType10, TType11, TType12, TType13, TType14, TType15, TType16], '', [], 6);
    T:=AddToken(stSymbol, T, '=', []);
    T:=AddToken(stInteger, T, '', [], 7);

      TPacked1:=AddToken(stSymbol, T, '[', []);
      TPacked2:=AddToken(stKeyword, TPacked1, 'packed', []);
      TPacked2:=AddToken(stSymbol, TPacked2, '=', []);
      TPacked21:=AddToken(stKeyword, TPacked2, 'true', [], 10);
      TPacked22:=AddToken(stKeyword, TPacked2, 'false', [], 11);

      TDefault1:=AddToken(stKeyword, TPacked1, 'default', []);
      TDefault1:=AddToken(stSymbol, TDefault1, '=', []);
      TDefault2:=AddToken(stIdentificator, TDefault1, '', [], 12);
      TDefault3:=AddToken(stInteger, TDefault1, '', [], 12);
      TDefault4:=AddToken(stString, TDefault1, '', [], 12);
      TDefault5:=AddToken(stKeyword, TDefault1, 'true', [], 12);
      TDefault6:=AddToken(stKeyword, TDefault1, 'false', [], 12);

      TPacked2:=AddToken(stSymbol, [TPacked21, TPacked22, TDefault2, TDefault3, TDefault4, TDefault5, TDefault6], ']', []);


    TReserv:=AddToken(stKeyword, T1, 'reserved', []);
      TReserv1:=AddToken(stInteger, TReserv, '', []);
      TReserv2:=AddToken(stSymbol, TReserv1, ',', []);
        TReserv2.AddChildToken(TReserv1);
      TReserv2:=AddToken(stKeyword, TReserv1, 'TO', []);
        TReserv2.AddChildToken(TReserv1);
      TReserv3:=AddToken(stString, TReserv, '', []);
      TReserv4:=AddToken(stSymbol, TReserv3, ',', []);
        TReserv4.AddChildToken(TReserv3);

    TEnum1:=AddToken(stKeyword, T1, 'enum', [], 13);
    TMsg1:=AddToken(stIdentificator, T1, 'message', [], 14);
      TEnum1.AddChildToken([TOpt, TRec, TRep, TReserv, TEnum1, TMsg1]);
      TMsg1.AddChildToken([TOpt, TRec, TRep, TReserv, TEnum1, TMsg1]);

    T:=AddToken(stSymbol, [T, TPacked2, TReserv1, TReserv3], ';', [], 8);
    T.AddChildToken([TOpt, TRec, TRep, TReserv, TEnum1, TMsg1]);
  T1:=AddToken(stSymbol, [T, T1], '}', [], -1);
end;

procedure TProtoMessage.InternalProcessChildToken(AParser: TProtoParser;
  AToken: TProtoToken; AWord: string);
begin
  inherited InternalProcessChildToken(AParser, AToken, AWord);
  case AToken.Code of
    2:begin
        if not Assigned(FCurField) then
          FCurField:=Fields.Add;
        FCurField.FFieldType:=mftOptional;
      end;
    3:begin
        if not Assigned(FCurField) then
          FCurField:=Fields.Add;
        FCurField.FFieldType:=mftRequired;
      end;
    4:begin
        if not Assigned(FCurField) then
          FCurField:=Fields.Add;
        FCurField.FFieldType:=mftRepeated;
      end;
    5:begin
        if Assigned(FCurField) then
        begin
          FCurField.DataType:=AWord;
          if FCurField.FieldType <> mftRepeated then
            FCurField.DataTypeFlag:=pdtInternal;
        end;
      end;
    9:begin
        if Assigned(FCurField) then
        begin
          FCurField.DataType:=FCurField.DataType + AWord;
          FCurField.DataTypeFlag:=pdtClass;
        end;
      end;
    6:begin
        if Assigned(FCurField) then
          FCurField.Caption:=AWord;
      end;
    7:begin
        if Assigned(FCurField) then
          FCurField.MessageNumber:=StrToInt(AWord);
      end;
    8:FCurField:=nil;
    10:begin
        if Assigned(FCurField) then
          FCurField.IsPacked:=true;
      end;
    11:if Assigned(FCurField) then
          FCurField.IsPacked:=false;
    12:if Assigned(FCurField) then
          FCurField.DefaultValue:=AWord;
    13, 14:begin
         FCurField:=Fields.Add;
         if AToken.Code = 13 then
         begin
           FCurField.FieldType:=mftEnumDefinition;
           FCurField.FProtoObjDef:=TEnum.Create;
         end
         else
         begin
           FCurField.FieldType:=mftMessageDefinition;
           FCurField.FProtoObjDef:=TProtoMessage.Create;
         end;
         AParser.Position:=AParser.WordPosition;
         AParser.Parse(FCurField.FProtoObjDef, nil);
         if AParser.State = cmsEndOfCmd then
           AParser.State:=cmsNormal;
         FCurField:=nil;
       end;
  end;

end;

procedure TProtoMessage.GenerateInterfaceSection(AModule: TStrings);
var
  F: TMessageField;
  S: String;
begin
  AModule.Add('T%s = class(TSerializationObject)', [Caption]);

{  if Fields.CountEnumDefs > 0 then
    for F in Fields do
      if (F.FieldType = mftEnumDefinition) and Assigned(F.ProtoObjDef) then
        F.FProtoObjDef.GenerateInterfaceSection(AModule);
}
  AModule.Add('private');
  for F in Fields do
    if F.IsRealField then
      AModule.Add('  F%s:%s;', [F.PascalName, F.PascalDataType]);

  for F in Fields do
  begin
    if F.DataTypeFlag = pdtEnum then
    begin
      AModule.Add('  function Get%s:%s;', [F.PascalName, F.PascalDataType]);
      if F.IsSetProc then
        AModule.Add('  procedure Set%s(AValue:%s);', [F.PascalName, F.PascalDataType]);
    end
    else
    if F.IsSetProc then
      AModule.Add('  procedure Set%s(AValue:%s);', [F.PascalName, F.PascalDataType]);
  end;

  AModule.Add('protected');
  AModule.Add('  procedure InternalRegisterProperty; override;');
  AModule.Add('  procedure InternalInit; override;');
  AModule.Add('public');
  AModule.Add('  destructor Destroy; override;');

  for F in Fields do
  begin
    if F.IsRealField and (F.DataTypeFlag = pdtEnum) then
    begin
      S:=Format('  property %s:%s read Get%s', [F.PascalName, F.PascalDataType, F.PascalName]);
      if F.IsSetProc then
      begin
        S:=S + ' write Set'+F.PascalName;
        if F.DefaultValue <> '' then
          S:=S + ' default ' + F.DefaultValue;
      end;
      S:=S+ ';';
      AModule.Add(S);
    end;
  end;

  AModule.Add('published');

  for F in Fields do
  if F.IsRealField and not (F.DataTypeFlag = pdtEnum)  then
  begin
    S:=Format('  property %s:%s read F%s', [F.PascalName, F.PascalDataType, F.PascalName]);
    if F.IsSetProc then
    begin
      S:=S + ' write Set'+F.PascalName;
      if F.DefaultValue <> '' then
        S:=S + ' default ' + F.DefaultValue;
    end;
    S:=S+ ';';
    AModule.Add(S);
  end;
  AModule.Add('end;');
end;

procedure TProtoMessage.GenerateImplementationSection(AModule: TStrings);
var
  F: TMessageField;
  S, S1: String;
  F1:boolean;
begin
  F1:=false;
  AModule.Add('');
  AModule.Add('procedure T'+Caption+'.InternalRegisterProperty;');
  AModule.Add('begin');
  AModule.Add('  inherited InternalRegisterProperty;');

  for F in Fields do
  if not (F.FieldType in [mftEnumDefinition, mftMessageDefinition]) then
  begin
    if F.DataTypeFlag = pdtEnum then
    begin
      S:=Format('  RegisterPropPublic(''%s'', %d, TMethod(@Set%s), TMethod(@Get%s)', [F.PascalName, F.MessageNumber, F.PascalName, F.PascalName]);
    end
    else
      S:='  RegisterProp(''' + F.PascalName + ''', ' + IntToStr(F.MessageNumber);
    if F.FieldType = mftRequired then S:=S + ', true';
    AModule.Add(S + ');');

  end;
  AModule.Add('end;');
  AModule.Add('');
  AModule.Add('procedure T'+Caption+'.InternalInit;');
  AModule.Add('begin');
  AModule.Add('  inherited InternalInit;');
  for F in Fields do
    if F.IsRealField and ((F.DataTypeFlag = pdtClass) or ((LowerCase(F.DataType) = 'string') and (F.FieldType = mftRepeated))) then
      AModule.Add('  F' + F.PascalName + ':= ' + F.PascalDataType + '.Create;');

  // fill default values
  for F in Fields do
    if F.IsRealField and (F.DataTypeFlag in [pdtInternal, pdtEnum]) and (F.DefaultValue <> '') then
    begin
      if not F1 then
      begin
        AModule.Add('');
        F1:=true;
      end;
      AModule.Add('  ' + F.PascalName + ':= ' + F.DefaultValue + ';');
    end;

  AModule.Add('end;');
  AModule.Add('');

  AModule.Add('destructor T'+Caption+'.Destroy;');
  AModule.Add('begin');
  for F in Fields do
    if F.IsRealField and ((F.DataTypeFlag = pdtClass) or ((LowerCase(F.DataType) = 'string') and (F.FieldType = mftRepeated))) then
      AModule.Add('  F' + F.PascalName + '.Free;');
  AModule.Add('  inherited Destroy;');
  AModule.Add('end;');
  AModule.Add('');


  for F in Fields do
  begin
    if F.DataTypeFlag = pdtEnum then
    begin
      AModule.Add('procedure T'+Caption+'.Set'+F.PascalName+'(AValue:'+F.PascalDataType+');');
      AModule.Add('begin');
      AModule.Add('  F'+F.PascalName+ ':=AValue;');
      AModule.Add('  Modified('+IntToStr(F.MessageNumber)+');');
      AModule.Add('end;');
      AModule.Add('');
      AModule.Add('function T'+Caption+'.Get'+F.PascalName+':'+F.PascalDataType+';');
      AModule.Add('begin');
      AModule.Add('  Result:=F'+F.PascalName+ ';');
      AModule.Add('end;');
    end
    else
    if F.IsSetProc then
    begin
      AModule.Add('procedure T'+Caption+'.Set'+F.PascalName+'(AValue:'+F.PascalDataType+');');
      AModule.Add('begin');
      AModule.Add('  F'+F.PascalName+ ':=AValue;');
      AModule.Add('  Modified('+IntToStr(F.MessageNumber)+');');
      AModule.Add('end;');
      AModule.Add('');
    end;
  end;
end;

constructor TProtoMessage.Create;
begin
  inherited Create;
  FFields:=TMessageFieldList.Create;
end;

destructor TProtoMessage.Destroy;
begin
  FFields.Clear;
  FFields.Free;
  inherited Destroy;
end;

procedure TProtoMessage.Clear;
begin
  inherited Clear;
  FFields.Clear;
  FCurField:=nil;
end;

procedure TProtoMessage.Assign(ASource: TProtoObject);
begin
  if ASource is TProtoMessage then
  begin
    FFields.Assign(TProtoMessage(ASource).FFields);
  end;
  inherited Assign(ASource);
end;


initialization
  RegisterProtoObject(TProtoMessage);
end.

