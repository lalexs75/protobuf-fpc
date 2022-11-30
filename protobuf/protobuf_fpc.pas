{ protobuf interface library for FPC and Lazarus

  Copyright (C) 2018-2022 Lagunov Aleksey alexs75@yandex.ru

  base on docs from https://developers.google.com/protocol-buffers/docs/encoding

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

{ Реализация обработки бианрного протокола в формате protobuf  }
{ https://developers.google.com/protocol-buffers/docs/encoding }

unit protobuf_fpc;

{$I protobuf_define.inc}

interface

uses
  Classes, SysUtils, protobuf_fpc_types;

type
  fixed64 = type QWord;
  fixed32 = type DWord;
  sfixed64 = type QWord;
  sfixed32 = type DWord;
  SInt32  = type system.Int32;
  SInt64  = type system.Int64;
  TSInt32DynArray = array of SInt32;
  TSInt64DynArray = array of SInt64;
  TFixed32DynArray = array of fixed32;
  TFixed64DynArray = array of fixed64;
  TSFixed32DynArray = array of sfixed32;
  TSFixed64DynArray = array of sfixed64;

type
  TSerializationBuffer = class;
  TSerializationObject = class;
  TSerializationPropertysEnumerator = class;
  TSerializationObjectClass = class of TSerializationObject;
  ESerializationException = class(Exception);
  TReadPropsProc = procedure(Buf:TSerializationBuffer) of object;
  TSerializationPropertyType = (sptPublished, sptPublic);

  { TSerializationProperty }

  TSerializationProperty = class
  private
    FOjbType: TSerializationObjectClass;
    FOnReadProps: TReadPropsProc;
    FPropName: string;
    FPropNum: integer;
    FRequired:boolean;
    FModified:boolean;
    FSerializationPropertyType: TSerializationPropertyType;
    FSetProp:TMethod;
    FGetProp:TMethod;
  public
    constructor Create(APropName:string; APropNum:integer; AOnReadProps:TReadPropsProc; ARequired:boolean);
    property OjbType:TSerializationObjectClass read FOjbType write FOjbType;
  published
    property PropName:string read FPropName write FPropName;
    property PropNum:integer read FPropNum write FPropNum;
    property OnReadProps:TReadPropsProc read FOnReadProps;
    property SerializationPropertyType:TSerializationPropertyType read FSerializationPropertyType write FSerializationPropertyType;
  end;

  { TSerializationPropertys }

  TSerializationPropertys = class
  private
    FList:TFPList;
    FOwner:TSerializationObject;
    function GetCount: integer;
    function GetItem(AIndex: integer): TSerializationProperty;
    procedure Clear;
    procedure Add(P:TSerializationProperty);
  public
    constructor Create(AOwner:TSerializationObject);
    destructor Destroy; override;
    function Find(APropName:string):TSerializationProperty;
    function FindByNum(APropNum:integer):TSerializationProperty;
    function GetEnumerator: TSerializationPropertysEnumerator;
    property Count:integer read GetCount;
    property Item[AIndex:integer]:TSerializationProperty read GetItem; default;
  end;

  { TSerializationPropertysEnumerator }

  TSerializationPropertysEnumerator = class
  private
    FList: TSerializationPropertys;
    FPosition: Integer;
  public
    constructor Create(AList: TSerializationPropertys);
    function GetCurrent: TSerializationProperty;
    function MoveNext: Boolean;
    property Current: TSerializationProperty read GetCurrent;
  end;

  { TSerializationBuffer }

  TSerializationBuffer = class
  private
    FStream:TMemoryStream;
    FPosition:Cardinal;
    FBuffer:TBytes;
    FPropLen: Integer;
    FPropNum: Integer;
    FPropType: Byte;
    procedure Clear;
    function Eof:boolean;
    function Stream:TStream;
  protected
    function Read(var ABuf; ALen:Cardinal):boolean;
    function CopyFrom(ABuf:TSerializationBuffer; ALen:Cardinal):boolean;

    procedure WriteVarInt(AValue:Integer);
    procedure SkipUknowProperty;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromStream(S:TStream);

    function ReadPropHeader:boolean;
    property PropNum:Integer read FPropNum;
    property PropLen:Integer read FPropLen;
    property PropType:Byte read FPropType;

    function ReadVarInt:integer;
    function ReadInt64:Int64;
    function ReadInt32:Int32;

    function ReadAsString:string;
    function ReadAsBoolean:boolean;
    function ReadAsInteger:Integer;
    function ReadAsInt64:Int64;
    function ReadAsQWord:QWord;

    function ReadAsIntegerZZ:Integer;
    function ReadAsInt64ZZ:Int64;

    procedure WriteAsInteger(P: TSerializationProperty; AValue:Integer);
    procedure WriteAsInt64(P: TSerializationProperty; AValue:Int64);

    procedure WriteAsIntegerZZ(P: TSerializationProperty; AValue:Integer);
    procedure WriteAsInt64ZZ(P: TSerializationProperty; AValue:Int64);

    procedure WriteAsString(P: TSerializationProperty; AValue:String);
    procedure WriteAsQWord(P: TSerializationProperty; AValue:QWord);
    procedure WriteAsBytes(P: TSerializationProperty; AValue:TBytes);
    procedure WriteAsStream(P: TSerializationProperty; AStream:TStream);

  end;

  { TSerializationObject }

  TSerializationObject = class(TPersistent)
  private
    FPropertys:TSerializationPropertys;
    function LoadFromSerializationBuffer(ABuf:TSerializationBuffer):boolean;
    function SaveToSerializationBuffer(ABuf:TSerializationBuffer):boolean;
    function InternalCheckModifiedProp(AProp: TSerializationProperty):Boolean;
    procedure InternalCheckRequired;
  protected
    procedure InternalRegisterProperty; virtual;
    procedure InternalInit; virtual;
    procedure RegisterProp(APropName:string; APropNum:Integer; ARequired:boolean = false; AObjClass:TSerializationObjectClass = nil);
    procedure RegisterPropPublic(APropName:string; APropNum:Integer; ASetProc, AGetProc:TMethod; ARequired:boolean = false);
    function InternalIsModifiedObject:boolean;virtual;
    procedure Modified(APropertyNum:integer);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear; virtual;
    function LoadFromStream(AStream:TStream):boolean;
    function LoadFromFile(AFileName:string):boolean;

    function SaveToStream(AStream:TStream):boolean; overload;
    function SaveToStream:TStream; overload;
    function SaveToFile(AFileName:string):boolean;
  end;

  { TSerializationObjectList }

  TSerializationObjectList = class(TSerializationObject)
  private
    function GetCount: integer;
    function Get(AIndex: Integer): TSerializationObject;
  protected
    FList:TFPList;
    FDataClass:TSerializationObjectClass;
    function InternalIsModifiedObject:boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function CreateItem(ABuf:TSerializationBuffer):boolean;
    procedure Clear; override;
    procedure Add(AItem:TSerializationObject);
    property Count:integer read GetCount;
  end;

  { TSerializationArray }

  TSerializationArray = class
  private
  protected
    procedure InternalAddAsInteger(AValue:Integer); virtual; abstract;
    procedure InternalAddAsString(AValue:String); virtual; abstract;
    function GetCount: integer; virtual;
    procedure SaveToBuf(ABuf:TSerializationBuffer; APropReg: TSerializationProperty); virtual; abstract;
    function Modified:Boolean; virtual; abstract;
  public
    procedure Clear; virtual; abstract;
    property Count:integer read GetCount;
  end;

  { TDocumentStrings }

  TDocumentStrings = class(TSerializationArray)
  private
    FItems:TStringList;
    function GetItem(AIndex:Integer): string;
    function GetText: string;
    procedure SetText(AValue: string);
  protected
    procedure InternalAddAsString(AValue:String); override;
    procedure SaveToBuf(ABuf:TSerializationBuffer; APropReg: TSerializationProperty); override;
    function GetCount: integer; override;
    function Modified:Boolean; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;
    procedure Add(AValue:string);
    function GetEnumerator: TStringsEnumerator;
    property Item[AIndex:Integer]:string read GetItem; default;
    property Items:TStringList read FItems;
    property Text:string read GetText write SetText;
  end;

  { GSerializationObjectListEnumerator }

  generic GSerializationObjectListEnumerator<GObjList, GObjType> = class
  private
    FList: TSerializationObjectList;
    FPosition: Integer;
  public
    constructor Create(AList: GObjList);
    function GetCurrent: GObjType;
    function MoveNext: Boolean;
    property Current: GObjType read GetCurrent;
  end;

  { GSerializationObjectList }

  generic GSerializationObjectList<GObjType> = class(TSerializationObjectList)
  public type
    TSerializationObjectListEnumerator = specialize GSerializationObjectListEnumerator<TSerializationObjectList, GObjType>;
  private
    function GetItem(AIndex: Integer): GObjType;
  public
    constructor Create; override;
    function GetEnumerator: TSerializationObjectListEnumerator;
    function AddItem:GObjType;
    property Items[AIndex:Integer]:GObjType read GetItem; default;
  end;


function EncodeValue32(AValue: Int32):LongWord;
function EncodeValue64(AValue: Int64):QWord;
function DecodeValue32(AValue: LongWord):Int32;
function DecodeValue64(AValue: QWord):Int64;

implementation
uses Types, TypInfo
{$IFDEF DEBUG}
  , rxlogging
{$ENDIF}
;
type
  TGetIntegerProc = function:longint of object;
  TSetIntegerProc = procedure(i:longint) of object;

function EncodeValue32(AValue: Int32):LongWord;
begin
  if AValue < 0 then
    Result:=(AValue shl 1) xor $FFFFFFFF
  else
    Result:=(AValue shl 1);
end;

function EncodeValue64(AValue: Int64):QWord;
begin
  if AValue < 0 then
    Result:=(AValue shl 1) xor $FFFFFFFFFFFFFFFF
  else
    Result:=(AValue shl 1);
end;

function DecodeValue32(AValue: LongWord):Int32;
begin
  Result:=((AValue and $00000001) * $FFFFFFFF) xor (AValue shr 1);
end;

function DecodeValue64(AValue: QWord):Int64;
begin
  Result:=((AValue and $00000001) * $FFFFFFFFFFFFFFFF) xor (AValue shr 1);
end;


{ GSerializationObjectList }

function GSerializationObjectList.GetItem(AIndex: Integer): GObjType;
begin
  Result:=GObjType(FList[AIndex]);
end;

constructor GSerializationObjectList.Create;
begin
  inherited Create;
  FDataClass:=GObjType;
end;

function GSerializationObjectList.GetEnumerator: TSerializationObjectListEnumerator;
begin
  Result:=TSerializationObjectListEnumerator.Create(Self);
end;

function GSerializationObjectList.AddItem: GObjType;
var
  P: TSerializationObject;
begin
  if Assigned(FDataClass) then
  begin
    P:=FDataClass.Create;
    Add(P);
    Result:=GObjType(P);
  end
  else
    Result:=nil;
end;

{ GSerializationObjectListEnumerator }

constructor GSerializationObjectListEnumerator.Create(AList: GObjList);
begin
  FList := AList;
  FPosition := -1;
end;

function GSerializationObjectListEnumerator.GetCurrent: GObjType;
begin
  Result := GObjType(FList.FList[FPosition]);
end;

function GSerializationObjectListEnumerator.MoveNext: Boolean;
begin
  Inc(FPosition);
  Result := FPosition < FList.Count;
end;

{ TSerializationPropertysEnumerator }

constructor TSerializationPropertysEnumerator.Create(
  AList: TSerializationPropertys);
begin
  FList := AList;
  FPosition := -1;
end;

function TSerializationPropertysEnumerator.GetCurrent: TSerializationProperty;
begin
  Result := TSerializationProperty(FList.FList[FPosition]);
end;

function TSerializationPropertysEnumerator.MoveNext: Boolean;
begin
  Inc(FPosition);
  Result := FPosition < FList.Count;
end;

{ TSerializationArray }

function TSerializationArray.GetCount: integer;
begin
  Result:=0;
end;

{ TSerializationObjectList }

function TSerializationObjectList.GetCount: integer;
begin
  Result:=FList.Count;
end;

function TSerializationObjectList.Get(AIndex: Integer): TSerializationObject;
begin

end;

function TSerializationObjectList.InternalIsModifiedObject: boolean;
begin
  Result:=FList.Count > 0;
end;

constructor TSerializationObjectList.Create;
begin
  inherited Create;
  FList:=TFPList.Create;
end;

destructor TSerializationObjectList.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited Destroy;
end;

function TSerializationObjectList.CreateItem(ABuf: TSerializationBuffer
  ): boolean;
var
  P: TSerializationObject;
begin
  if Assigned(FDataClass) then
  begin
    P:=FDataClass.Create;
    Add(P);
    P.LoadFromSerializationBuffer(ABuf);
  end;
end;

procedure TSerializationObjectList.Clear;
var
  P: Pointer;
begin
  for P in FList do
    TSerializationObject(P).Free;
  FList.Clear;
  inherited Clear;
end;

procedure TSerializationObjectList.Add(AItem: TSerializationObject);
begin
  FList.Add(AItem);
end;

{ TSerializationProperty }

constructor TSerializationProperty.Create(APropName: string; APropNum: integer;
  AOnReadProps: TReadPropsProc; ARequired: boolean);
begin
  inherited Create;
  FPropName:=APropName;
  FPropNum:=APropNum;
  FOnReadProps:=AOnReadProps;
  FRequired:=ARequired;
  FSerializationPropertyType:=sptPublished;
end;

{ TSerializationPropertys }

function TSerializationPropertys.GetItem(AIndex: integer
  ): TSerializationProperty;
var
  i: Integer;
begin
  Result:=nil;
  for i:=0 to FList.Count-1 do
    if TSerializationProperty(FList[i]).FPropNum = AIndex then
      Exit(TSerializationProperty(FList[i]));
  //raise ESerializationException.CreateFmt(sProtoBufferPropNotFoundNum, [FOwner.ClassName, AIndex]);
end;

function TSerializationPropertys.GetCount: integer;
begin
  Result:=FList.Count;
end;

procedure TSerializationPropertys.Clear;
var
  i: Integer;
begin
  for i:=0 to FList.Count-1 do
    TSerializationProperty(FList[i]).Free;
  FList.Clear;
end;

procedure TSerializationPropertys.Add(P: TSerializationProperty);
begin
  FList.Add(P);
end;

constructor TSerializationPropertys.Create(AOwner: TSerializationObject);
begin
  inherited Create;
  FList:=TFPList.Create;
  FOwner:=AOwner;
end;

destructor TSerializationPropertys.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited Destroy;
end;

function TSerializationPropertys.Find(APropName: string
  ): TSerializationProperty;
var
  i: Integer;
begin
  APropName:=UpperCase(APropName);
  Result:=nil;
  for i:=0 to FList.Count-1 do
    if UpperCase(TSerializationProperty(FList[i]).FPropName) = APropName then
      Exit(TSerializationProperty(FList[i]));
end;

function TSerializationPropertys.FindByNum(APropNum: integer
  ): TSerializationProperty;
var
  i: Integer;
begin
  Result:=nil;
  for i:=0 to FList.Count-1 do
    if TSerializationProperty(FList[i]).FPropNum = APropNum then
      Exit(TSerializationProperty(FList[i]));
end;

function TSerializationPropertys.GetEnumerator: TSerializationPropertysEnumerator;
begin
  Result:=TSerializationPropertysEnumerator.Create(Self);
end;

{ TSerializationBuffer }

procedure TSerializationBuffer.Clear;
begin
  SetLength(FBuffer, 0);
  FPosition:=0;
  FPropLen:=-1;
  FPropNum:=-1;
  FPropType:=$FF;
end;

function TSerializationBuffer.Eof: boolean;
begin
  Eof:=FPosition >= Length(FBuffer);
end;

function TSerializationBuffer.Stream: TStream;
begin
  if not Assigned(FStream) then
    FStream:=TMemoryStream.Create;
  Result:=FStream;
end;

constructor TSerializationBuffer.Create;
begin
  inherited Create;
  Clear;
end;

destructor TSerializationBuffer.Destroy;
begin
  if Assigned(FStream) then
    FreeAndNil(FStream);
  inherited Destroy;
end;

function TSerializationBuffer.Read(var ABuf; ALen: Cardinal): boolean;
var
  FCnt: Integer;
begin
  if FPosition + ALen > Length(FBuffer) then
  begin
    Result:=false;
    FPosition:=Length(FBuffer);
  end
  else
  begin
    Move(FBuffer[FPosition], ABuf, ALen);
    Inc(FPosition, ALen);
    Result:=true;
  end;
end;

function TSerializationBuffer.CopyFrom(ABuf: TSerializationBuffer;
  ALen: Cardinal): boolean;
begin
  SetLength(FBuffer, ALen);
  Result:=ABuf.Read(FBuffer[0], ALen);
  FPosition:=0;
end;

procedure TSerializationBuffer.WriteVarInt(AValue: Integer);
var
  B: byte;
begin
  repeat
    B:=AValue and %01111111;
    AValue:=AValue shr 7;
    if AValue<>0 then
      B:=B or %10000000;
    Stream.Write(B, SizeOf(Byte));
  until AValue = 0;
end;

procedure TSerializationBuffer.SkipUknowProperty;
begin
  case FPropType of
    0:ReadAsInteger;
    1:ReadAsInt64;
    2:ReadAsString;
    3:; //WriteLog('Start group	groups (deprecated)');
    4:; //WriteLog('End group	groups (deprecated)');
    5:ReadAsInteger;
  end;
end;

procedure TSerializationBuffer.WriteAsInteger(P: TSerializationProperty;
  AValue: Integer);
var
  C: Integer;
begin
  C:=P.FPropNum shl 3 + 0; //varint
  WriteVarInt(C);
  WriteVarInt(AValue);
end;

procedure TSerializationBuffer.WriteAsString(P: TSerializationProperty;
  AValue: String);
var
  C: Integer;
begin
  C:=P.FPropNum shl 3 + 2; //string
  WriteVarInt(C);
  WriteVarInt(Length(AValue));
  if AValue <> '' then
  begin
    Stream.Write(AValue[1], Length(AValue));
  end;
end;

procedure TSerializationBuffer.WriteAsInt64(P: TSerializationProperty;
  AValue: Int64);
var
  C: Integer;
begin
  C:=P.FPropNum shl 3 + 0; //varint
  WriteVarInt(C);
  WriteVarInt(AValue);
end;

procedure TSerializationBuffer.WriteAsIntegerZZ(P: TSerializationProperty;
  AValue: Integer);
var
  C: Integer;
begin
  C:=P.FPropNum shl 3 + 0; //varint
  WriteVarInt(C);
  WriteVarInt(EncodeValue32(AValue));
end;

procedure TSerializationBuffer.WriteAsInt64ZZ(P: TSerializationProperty;
  AValue: Int64);
var
  C: Integer;
begin
  C:=P.FPropNum shl 3 + 0; //varint
  WriteVarInt(C);
  WriteVarInt(EncodeValue64(AValue));
end;

procedure TSerializationBuffer.WriteAsQWord(P: TSerializationProperty;
  AValue: QWord);
begin

end;

procedure TSerializationBuffer.WriteAsBytes(P: TSerializationProperty;
  AValue: TBytes);
var
  C: Integer;
begin
  C:=P.FPropNum shl 3 + 2; //string
  WriteVarInt(C);
  WriteVarInt(Length(AValue));
  if Length(AValue)>0 then
    Stream.Write(AValue[0], Length(AValue));
end;

procedure TSerializationBuffer.WriteAsStream(P: TSerializationProperty;
  AStream: TStream);
var
  C: Integer;
begin
  if not Assigned(AStream) then Exit;
  C:=P.FPropNum shl 3 + 2; //string
  WriteVarInt(C);
  WriteVarInt(AStream.Size);
  if AStream.Size>0 then
  begin
    AStream.Position:=0;
    Stream.CopyFrom(AStream, AStream.Size);
  end;
end;

procedure TSerializationBuffer.LoadFromStream(S: TStream);
var
  L: Int64;
begin
  if Assigned(S) then
  begin
    S.Position:=0;
    L:=S.Size;
    SetLength(FBuffer, S.Size);
    S.Read(FBuffer[0], S.Size);
    FPosition:=0;
  end
  else
    Clear;
end;

function TSerializationBuffer.ReadPropHeader: boolean;
var
  C: Integer;
begin
  Result:=false;

  C:=ReadVarInt;

  FPropLen:=0;
  FPropType:=C and %0111;
  FPropNum:=C shr 3;

  case FPropType of
    0:begin
        //WriteLog('Varint	int32, int64, uint32, uint64, sint32, sint64, bool, enum');

      end;
    1:begin
        //WriteLog('64-bit	fixed64, sfixed64, double');

      end;
    2:begin
        //WriteLog('Length-delimited	string, bytes, embedded messages, packed repeated fields');
        FPropLen:=ReadVarInt;
      end;
    3:begin
        //WriteLog('Start group	groups (deprecated)');

      end;
    4:begin
        //WriteLog('End group	groups (deprecated)');

      end;
    5:begin
        //WriteLog('32-bit	fixed32, sfixed32, float');
      end
  else
    raise ESerializationException.CreateFmt(sProtoBufUnknowPropTypeNum, [FPropType]);
  end;
  Result:=true;
end;

function TSerializationBuffer.ReadVarInt: integer;
var
  B:Byte;
  I: Integer;
begin
  Result:=0;
  I:=0;
  repeat
    Read(B, SizeOf(Byte));
    Result:=Result + ((B and %01111111) shl I);
    Inc(i, 7);
  until ((B and %10000000) = 0) or Eof;
end;

function TSerializationBuffer.ReadAsString: string;
begin
  //fLen:=ReadInteger;
  if not Eof then
  begin
    SetLength(Result, FPropLen);
    Read(Result[1], FPropLen);
  end
  else
    Result:='';

  {$IFDEF DEBUG}
  RxWriteLog(etDebug, 'ReadAsString = %s', [Result]);
  {$ENDIF}
end;

function TSerializationBuffer.ReadInt64: Int64;
begin
  Read(Result, SizeOf(Int64));
end;

function TSerializationBuffer.ReadInt32: Int32;
begin
  Read(Result, SizeOf(Int32));
end;

function TSerializationBuffer.ReadAsBoolean: boolean;
begin
  case FPropType of
    0:Result:=ReadVarInt <> 0; //Varint
    1:Result:=ReadInt64 <> 0; //64-bit
    //2:Length-delimited - string, bytes, embedded messages, packed repeated fields
    //3:Start group - groups (deprecated)
    //4:End group   - groups (deprecated)
    5:Result:=ReadInt32 <> 0; //32-bit
  else
    Result:=false;
    raise ESerializationException.Create(sProtoBufErrorReadBooleanValue);
  end;

  {$IFDEF DEBUG}
  RxWriteLog(etDebug, 'AsBoolean = ' + BoolToStr(Result, true));
  {$ENDIF}
end;

function TSerializationBuffer.ReadAsInteger: Integer;
begin
  //Доработать поддержку знаковых чисел
  case FPropType of
    0:Result:=ReadVarInt; //Varint
    1:Result:=ReadInt64; //64-bit
    //2:Length-delimited - string, bytes, embedded messages, packed repeated fields
    //3:Start group - groups (deprecated)
    //4:End group   - groups (deprecated)
    5:Result:=ReadInt32; //32-bit
  else
    Result:=0;
    raise ESerializationException.Create(sProtoBufErrorReadIntegerValue);
  end;

  {$IFDEF DEBUG}
  RxWriteLog(etDebug, 'ReadAsInteger = %d', [Result]);
  {$ENDIF}
end;

function TSerializationBuffer.ReadAsInt64: Int64;
begin
  case FPropType of
    //0:Result:=ReadVarInt; //Varint
    0:Result:=DecodeValue32(ReadVarInt); //Varint
    1:Result:=ReadInt64; //64-bit
    //2:Length-delimited - string, bytes, embedded messages, packed repeated fields
    //3:Start group - groups (deprecated)
    //4:End group   - groups (deprecated)
    5:Result:=ReadInt32; //32-bit
  else
    Result:=0;
    raise ESerializationException.Create(sProtoBufErrorReadInt64Value);
  end;
end;

function TSerializationBuffer.ReadAsQWord: QWord;
begin
  case FPropType of
    0:Result:=ReadVarInt; //Varint
    1:Result:=ReadInt64; //64-bit
    //2:Length-delimited - string, bytes, embedded messages, packed repeated fields
    //3:Start group - groups (deprecated)
    //4:End group   - groups (deprecated)
    5:Result:=ReadInt32; //32-bit
  else
    Result:=0;
    raise ESerializationException.Create(sProtoBufErrorReadQWordValue);
  end;

  {$IFDEF DEBUG}
  RxWriteLog(etDebug, 'ReadAsCardinal = %d', [Result]);
  {$ENDIF}
end;

function TSerializationBuffer.ReadAsIntegerZZ: Integer;
begin
  //Доработать поддержку знаковых чисел
  case FPropType of
    0:Result:=DecodeValue32(ReadVarInt); //Varint
    1:Result:=ReadInt64; //64-bit
    //2:Length-delimited - string, bytes, embedded messages, packed repeated fields
    //3:Start group - groups (deprecated)
    //4:End group   - groups (deprecated)
    5:Result:=ReadInt32; //32-bit
  else
    Result:=0;
    raise ESerializationException.Create(sProtoBufErrorReadIntegerValue);
  end;

  {$IFDEF DEBUG}
  RxWriteLog(etDebug, 'ReadAsInteger = %d', [Result]);
  {$ENDIF}
end;

function TSerializationBuffer.ReadAsInt64ZZ: Int64;
begin

end;

{ TSerializationObject }

destructor TSerializationObject.Destroy;
begin
  FreeAndNil(FPropertys);
  inherited Destroy;
end;

procedure TSerializationObject.Clear;
var
  P: TSerializationProperty;
begin
  for P in FPropertys do
    P.FModified:=false;
  { TODO : Необходимо дописать очистку всех свойств }
end;

constructor TSerializationObject.Create;
begin
  inherited Create;
  FPropertys:=TSerializationPropertys.Create(Self);
  InternalRegisterProperty;
  InternalInit;
end;

function TSerializationObject.LoadFromStream(AStream: TStream): boolean;
var
  Buf: TSerializationBuffer;
begin
  Result:=false;
  Buf:=TSerializationBuffer.Create;
  try
    Buf.LoadFromStream(AStream);
    LoadFromSerializationBuffer(Buf);
  finally
    Buf.Free;
  end;
  Result:=true;
end;

function TSerializationObject.LoadFromFile(AFileName: string): boolean;
var
  S: TFileStream;
begin
  S:=TFileStream.Create(AFileName, fmOpenRead);
  Result:=LoadFromStream(S);
  S.Free;
end;

function TSerializationObject.LoadFromSerializationBuffer(
  ABuf: TSerializationBuffer): boolean;

procedure LoadClassData(AProp: PPropInfo; APropReg: TSerializationProperty);
var
  FInst: TObject;
  Buf1: TSerializationBuffer;
begin
  FInst := TObject(PtrInt( GetOrdProp(Self, AProp)));
  if not Assigned(FInst) then
  begin
    if Assigned(APropReg.OjbType) then
    begin
      FInst:=APropReg.OjbType.Create;
      SetOrdProp(Self, AProp, PtrInt(FInst));
    end;
  end;


  if Assigned(FInst) then
  begin
    if FInst is TSerializationArray then
    begin
      case ABuf.FPropType of
        0:TSerializationArray(FInst).InternalAddAsInteger(ABuf.ReadVarInt);
        1:TSerializationArray(FInst).InternalAddAsInteger(ABuf.ReadInt64);
        2:TSerializationArray(FInst).InternalAddAsString(ABuf.ReadAsString);
        5:TSerializationArray(FInst).InternalAddAsInteger(ABuf.ReadInt32);
      end;
    end
    else
    begin
      Buf1:=TSerializationBuffer.Create;
      Buf1.CopyFrom(ABuf, ABuf.PropLen);
      if FInst is TSerializationObjectList then
        TSerializationObjectList(FInst).CreateItem(Buf1)
      else
      if FInst is TSerializationObject then
        TSerializationObject(FInst).LoadFromSerializationBuffer(Buf1);
      Buf1.Free;
    end;
  end;
end;

procedure LoadBytes(AProp: PPropInfo; APropReg: TSerializationProperty);
var
  B:TBytes;
begin
  SetLength(B, ABuf.PropLen);
  ABuf.Read(B[0], ABuf.PropLen);
  SetDynArrayProp(Self, AProp, Pointer(B));
end;

var
  P: TSerializationProperty;
  Buf1: TSerializationBuffer;
  FProp: PPropInfo;
  K: TTypeKind;
  SS, TN, KN: String;
  BB, Li: Integer;
  vDinArray: pointer;
  L: tdynarrayindex;
  PDT: PTypeData;
  O: TOrdType;
  EL: PTypeInfo;
begin
  Result:=false;
  while not ABuf.Eof do
  begin
    if ABuf.ReadPropHeader then
    begin
      {$IFDEF DEBUG}
      P:=FPropertys.FindByNum(ABuf.FPropNum);
      if not Assigned(P) then
      begin
        RxWriteLog(etError, '%s: not found prop %d (Type=%d, Len=%d).', [Self.ClassName,  ABuf.PropNum, ABuf.PropType, ABuf.PropLen]);

        case ABuf.PropType of
          0:RxWriteLog(etError, '  Value=%d', [ABuf.ReadVarInt]);
          1:RxWriteLog(etError, '  Value=%d', [ABuf.ReadInt64]);
          2:RxWriteLog(etError, '  Value=%s', [ABuf.ReadAsString]);
          5:RxWriteLog(etError, '  Value=%d', [ABuf.ReadInt32]);
        else
          raise Exception.CreateFmt('unknow type %d', [ABuf.PropType]);
        end;
        Continue;
      end;
{      if ABuf.FPropNum = 71 then
      begin
         P:=nil;
      end;}
      {$ENDIF}

      P:=FPropertys[ABuf.FPropNum];
      if Assigned(P) then
      begin
        {$IFDEF DEBUG}
        RxWriteLog(etDebug, '%s : PropNum=%d, PropType = %d, PropLen = %d, MA = %d, PropName=%s', [Self.ClassName,  P.FPropNum, ABuf.PropType, ABuf.PropLen, Ord(Assigned(P.FOnReadProps)), P.FPropName]);
        {$ENDIF}
        if P.SerializationPropertyType = sptPublished then
        begin
          if Assigned(P.FOnReadProps) then
          begin
            //TODO: Попробовать обработать repetable объекты как коллекции
            if ABuf.PropType <> 2 then
              raise Exception.Create('Error!');

            Buf1:=TSerializationBuffer.Create;
            Buf1.CopyFrom(ABuf, ABuf.PropLen);
            P.OnReadProps(Buf1);
            Buf1.Free;
          end
          else
          begin
            FProp:=GetPropInfo(Self, P.FPropName); //Retreive property informations
            if not Assigned(FProp) then
              raise ESerializationException.CreateFmt(sProtoBufNotFondProperty, [P.PropName]);

            //Необходимо совместить определения типа читаемых данных по признаку из файла и из данных RTTI
            K:=FProp^.PropType^.Kind;
            TN:=UpperCase(FProp^.PropType^.Name);
            case FProp^.PropType^.Kind of
              tkChar,
              tkAString,
              tkWString,
              tkSString,
              tkLString   : SetStrProp(Self, FProp, ABuf.ReadAsString);

              tkBool :
                SetOrdProp(Self, FProp, Ord(ABuf.ReadAsBoolean));

              tkQWord : SetOrdProp(Self, FProp, Ord(ABuf.ReadAsQWord));

              tkInt64   :
                 begin
                   if TN = 'SINT64' then
                     SetOrdProp(Self, FProp, ABuf.ReadAsIntegerZZ)
                   else
                     SetOrdProp(Self, FProp, ABuf.ReadAsInteger);
                 end;
              tkInteger :
                 begin
                   if TN = 'SINT32' then
                     SetOrdProp(Self, FProp, ABuf.ReadAsIntegerZZ)
                   else
                     SetOrdProp(Self, FProp, ABuf.ReadAsInteger);
                 end;
  {
    tkSet                       : SetSetProp(t,PropInfo,S);
              tkFloat                     : SetFloatProp(t,PropInfo, Value);}
              tkEnumeration : SetOrdProp(Self, FProp, Ord(ABuf.ReadAsInteger));
              tkClass : LoadClassData(FProp, P);
              tkDynArray:
                begin
                  if TN = 'TBYTES' then
                    LoadBytes(FProp, P)
                  else
                  begin
                    { TODO : Необходимо обработать динамический массив }
                    //raise Exception.CreateFmt('Property %s.%s. Unknow dyn array type - %s', [ClassName, P.PropName, TN]);
                    //BB:=ABuf.ReadVarInt;


                    vDinArray:=GetObjectProp(Self, FProp);
                    L:=DynArraySize(vDinArray);
                    PDT:=GetTypeData(FProp^.PropType);
                    O:=PDT^.OrdType;
                    EL:=PDT^.ElType2;
                    K:=EL^.Kind;
                    KN:=EL^.Name;

                    L:=L+1;
                    DynArraySetLength(vDinArray, FProp^.PropType, 1, @L);

                    case K of
                      tkInteger,
                      tkEnumeration:
                      begin
                        case O of
                          //  otSByte,otUByte,otSWord,otUWord,
                            otSLong:
                              if (TN = 'SINT32') or (TN='SINT64') then
                                TIntegerDynArray(vDinArray)[L-1]:=ABuf.ReadAsIntegerZZ
                              else
                                TIntegerDynArray(vDinArray)[L-1]:=ABuf.ReadVarInt;
                            //otULong,otSQWord,otUQWor
                        else
                          raise exception.CreateFmt('sUknowPropertyType %s', [P.FPropName]);
                        end;
                      end;
                      //tkAString,
                      //tkString:TXSDStringArray(vDinArray)[L-1]:=ATextContent;
                    else
                      raise exception.CreateFmt('sUknowPropertyType %s.%s', [ClassName, P.PropName]);
                    end;
                    SetObjectProp(Self, FProp, TObject(vDinArray));

                  end;
                end
            else
              raise exception.CreateFmt(sProtoBufUnknowPropType, [P.FPropName]);
            end;
          end;
        end
        else
        if P.SerializationPropertyType = sptPublic then
        begin
          Li:=Ord(ABuf.ReadAsInteger);
          TSetIntegerProc(P.FSetProp)(Li);
        end
        else
          raise exception.CreateFmt(sProtoBufUnknowPropType, [P.FPropName]);
      end
      else
        ABuf.SkipUknowProperty;
    end;
  end;
  Result:=true;
end;

function TSerializationObject.InternalIsModifiedObject: boolean;
var
  P: TSerializationProperty;
begin
  Result:=false;
  for P in FPropertys do
    if InternalCheckModifiedProp(P) then Exit(True);
end;

procedure TSerializationObject.Modified(APropertyNum: integer);
var
  P: TSerializationProperty;
begin
  for P in FPropertys do
    if P.PropNum = APropertyNum then
    begin
      P.FModified:=true;
      Exit;
    end;
  raise Exception.CreateFmt(sProtoBufferPropNotFoundNum, [ClassName, APropertyNum]);
end;

function TSerializationObject.SaveToSerializationBuffer(
  ABuf: TSerializationBuffer): boolean;

procedure SaveClassData(AProp: PPropInfo; APropReg: TSerializationProperty);
var
  Buf1: TSerializationBuffer;
  FInst: TObject;
  i: Integer;
  P: TSerializationObject;
begin
  FInst := TObject(PtrInt( GetOrdProp(Self, AProp)));
  if not Assigned(FInst) then Exit;
  if Assigned(FInst) then
  begin
    if FInst is TSerializationArray then
      TSerializationArray(FInst).SaveToBuf(ABuf, APropReg)
    else
    begin
      if FInst is TSerializationObjectList then
      begin
        if not TSerializationObjectList(FInst).InternalIsModifiedObject then Exit;
        for i:=0 to TSerializationObjectList(FInst).Count-1 do
        begin
          Buf1:=TSerializationBuffer.Create;

          P:=TSerializationObject(TSerializationObjectList(FInst).FList[i]);
          P.SaveToSerializationBuffer(Buf1);

          ABuf.WriteAsStream(APropReg, Buf1.Stream);
          Buf1.Free;
        end;
        //AbstractError
      end
      else
      if FInst is TSerializationObject then
      begin
        if not TSerializationObject(FInst).InternalIsModifiedObject then Exit;
        Buf1:=TSerializationBuffer.Create;
        TSerializationObject(FInst).SaveToSerializationBuffer(Buf1);
        ABuf.WriteAsStream(APropReg, Buf1.Stream);
        Buf1.Free;
      end;

    end;
  end;

end;

var
  P: TSerializationProperty;
  FProp: PPropInfo;
  K: TTypeKind;
  PArr: Pointer;
  L, i: Integer;
  STypeName, PropValue, KN: String;
  vDinArray: TObject;
  PDT: PTypeData;
  O: TOrdType;
  EL: PTypeInfo;
begin
  InternalCheckRequired;
  for P in FPropertys do
  begin
    if P.SerializationPropertyType = sptPublished then
    begin
      FProp:=GetPropInfo(Self, P.FPropName); //Retreive property informations
      if not Assigned(FProp) then
        raise ESerializationException.CreateFmt(sProtoBufNotFondProperty, [P.PropName]);
      K:=FProp^.PropType^.Kind;
      case FProp^.PropType^.Kind of
        tkChar,
        tkAString,
        tkWString,
        tkSString,
        tkLString   :
          begin
            PropValue:=GetStrProp(Self, FProp);
//            if PropValue<>'' then
              ABuf.WriteAsString(P, PropValue);
          end;

        tkBool : ABuf.WriteAsInteger(P, GetOrdProp(Self, FProp));
        tkQWord : ABuf.WriteAsQWord(P, GetOrdProp(Self, FProp));
        //tkInt64   : SetOrdProp(Self, FProp, ABuf.ReadAsInteger);
        tkInt64   :
          begin
            STypeName:=UpperCase(FProp^.PropType^.Name);
            if STypeName = 'SINT64' then
              ABuf.WriteAsInt64ZZ(P, GetInt64Prop(Self, FProp))
            else
              ABuf.WriteAsInt64(P, GetInt64Prop(Self, FProp));
          end;
        tkInteger :
          begin
            STypeName:=UpperCase(FProp^.PropType^.Name);
            if STypeName = 'SINT32' then
              ABuf.WriteAsIntegerZZ(P, GetInt64Prop(Self, FProp))
            else
              ABuf.WriteAsInteger(P, GetInt64Prop(Self, FProp));
          end;
        tkEnumeration : ABuf.WriteAsInteger(P, GetOrdProp(Self, FProp));
        tkDynArray:begin
                     STypeName:=UpperCase(FProp^.PropType^.Name);

                     if STypeName = 'TBYTES' then
                     begin
                       PArr:=GetDynArrayProp(Self, FProp);
                       if Assigned(PArr) then
                         ABuf.WriteAsBytes(P, TBytes(PArr));
                     end
                     else
                     begin
                       vDinArray:=GetObjectProp(Self, FProp);
                       L:=DynArraySize(vDinArray);
                       PDT:=GetTypeData(FProp^.PropType);
                       O:=PDT^.OrdType;
                       EL:=PDT^.ElType2;
                       K:=EL^.Kind;
                       KN:=EL^.Name;

                       for i:=0 to L-1 do
                       begin
                         case K of
                           tkEnumeration,
                           tkInteger:
                             case O of
                              otSLong:ABuf.WriteAsInteger(P, TIntegerDynArray(vDinArray)[i]);
                             else
                               raise Exception.CreateFmt(sProtoBufUnknowPropType, [P.FPropName]);
                             end
                         else
                           raise Exception.CreateFmt(sProtoBufUnknowPropType, [P.FPropName]);
                         end;
                       end;
                     end;
                   end;
        tkClass : SaveClassData(FProp, P);
      else
        raise exception.CreateFmt(sProtoBufUnknowPropType, [P.FPropName]);
      end;
    end
    else
    if P.SerializationPropertyType = sptPublic then
    begin
      //tkEnumeration :
      L:=TGetIntegerProc(P.FGetProp)();
      ABuf.WriteAsInteger(P, L);
    end
    else
      raise Exception.CreateFmt(sProtoBufUnknowPropType, [P.FPropName]);
  end;
  Result:=true;
end;

function TSerializationObject.InternalCheckModifiedProp(
  AProp: TSerializationProperty): Boolean;
var
  FProp: PPropInfo;
  FInst: TObject;
begin
  Result:=false;
  FProp:=GetPropInfo(Self, AProp.FPropName);
  if not Assigned(FProp) then
    raise ESerializationException.CreateFmt(sProtoBufNotFondProperty, [AProp.PropName]);
  if FProp^.PropType^.Kind = tkClass then
  begin
    FInst := TObject(PtrInt( GetOrdProp(Self, AProp.PropName)));
    if not Assigned(FInst) then Exit;
    if FInst is TSerializationArray then
      Result:=TSerializationArray(FInst).Modified
    else
    if FInst is TSerializationObject then
      Result:=TSerializationObject(FInst).InternalIsModifiedObject
    else
      raise Exception.CreateFmt('Uknow object type %s', [AProp.PropName]);
  end
  else
    Result:=AProp.FModified;
end;

procedure TSerializationObject.InternalCheckRequired;
var
  P: TSerializationProperty;
begin
  for P in FPropertys do
    if P.FRequired and not InternalCheckModifiedProp(P) then
      raise Exception.CreateFmt(sProtoBufferPropValueExpected, [P.FPropName]);
end;

function TSerializationObject.SaveToStream(AStream: TStream): boolean;
var
  Buf: TSerializationBuffer;
begin
  Buf:=TSerializationBuffer.Create;
  SaveToSerializationBuffer(Buf);
  if Assigned(Buf.FStream) then
  begin
    Buf.FStream.Position:=0;
    AStream.CopyFrom(Buf.FStream, 0);
  end;
  Buf.Free;
  Result:=True;
end;

function TSerializationObject.SaveToStream: TStream;
begin
  Result:=TMemoryStream.Create;
  SaveToStream(Result);
end;

function TSerializationObject.SaveToFile(AFileName: string): boolean;
var
  S: TFileStream;
begin
  S:=TFileStream.Create(AFileName, fmCreate);
  SaveToStream(S);
  S.Free;
end;

procedure TSerializationObject.InternalRegisterProperty;
begin
  //
end;

procedure TSerializationObject.InternalInit;
begin
  //
end;

procedure TSerializationObject.RegisterProp(APropName:string; APropNum:Integer; ARequired:boolean = false; AObjClass:TSerializationObjectClass = nil);
var
  P: TSerializationProperty;
begin
  if not Assigned(FPropertys.Find(APropName)) then
  begin
    P:=TSerializationProperty.Create(APropName, APropNum, nil, ARequired);
    P.OjbType:=AObjClass;
    FPropertys.Add(P);
  end
  else
    raise ESerializationException.CreateFmt(sPropertyAlreadyRegistered, [APropName]);
end;

procedure TSerializationObject.RegisterPropPublic(APropName: string;
  APropNum: Integer; ASetProc, AGetProc: TMethod; ARequired: boolean);
var
  P: TSerializationProperty;
begin
  if not Assigned(FPropertys.Find(APropName)) then
  begin
    P:=TSerializationProperty.Create(APropName, APropNum, nil, ARequired);
    P.OjbType:=nil; //AObjClass;
    P.SerializationPropertyType:=sptPublic;
    P.FSetProp:=ASetProc;
    P.FGetProp:=AGetProc;
    FPropertys.Add(P);
  end
  else
    raise ESerializationException.CreateFmt(sPropertyAlreadyRegistered, [APropName]);
end;

{ TDocumentStrings }

function TDocumentStrings.GetItem(AIndex: Integer): string;
begin
  if (AIndex < 0) or (AIndex > FItems.Count-1) then
    Result:=''
  else
    Result:=FItems[AIndex];
end;

function TDocumentStrings.GetText: string;
begin
  Result:=FItems.Text;
end;

procedure TDocumentStrings.SetText(AValue: string);
begin
  FItems.Text:=AValue;
end;

procedure TDocumentStrings.InternalAddAsString(AValue: String);
begin
  FItems.Add(AValue);
end;

procedure TDocumentStrings.SaveToBuf(ABuf: TSerializationBuffer;
  APropReg: TSerializationProperty);
var
  i: Integer;
begin
  for i:=0 to FItems.Count-1 do
    ABuf.WriteAsString(APropReg, FItems[i]);
end;

function TDocumentStrings.GetCount: integer;
begin
  Result:=FItems.Count;
end;

function TDocumentStrings.Modified: Boolean;
begin
  Result:=FItems.Count > 0;
end;

constructor TDocumentStrings.Create;
begin
  FItems:=TStringList.Create;
end;

destructor TDocumentStrings.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

procedure TDocumentStrings.Clear;
begin
  FItems.Clear;
end;

procedure TDocumentStrings.Add(AValue: string);
begin
  FItems.Add(AValue);
end;

function TDocumentStrings.GetEnumerator: TStringsEnumerator;
begin
  Result:=TStringsEnumerator.Create(FItems);
end;

end.

