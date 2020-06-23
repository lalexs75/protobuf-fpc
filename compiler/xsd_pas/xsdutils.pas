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

unit xsdutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;
type
  TXSDDataType = (xsdtString, xsdtDateTime, xsdtInteger, xsdtFloat, xsdtBoolean);

  TXSDStdType = class
    PascalName:string;
    QuotedStr:Boolean;
    XSDDataType:TXSDDataType;
  end;

function IsSimpleType(ATypeName:string):Boolean;
function IsTypeQuotedStr(ATypeName:string):Boolean;
function GetSimpleType(ATypeName:string):string;
function IsKeyword(const AKeyword: string): boolean;
function FindPasTypeRec(ATypeName:string):TXSDStdType;
function FindStdTypeByPasName(APasName:string):TXSDStdType;

function GenerateTypeDescription(ADescription:string; ASpacing:integer = 2):string;
function ExpandXSDFileName(AFileName:string; AIncludeFolders:TStrings):string;

const
  charAlpha = ['A'..'Z', 'a'..'z', '_'];
  charAlphaNum = charAlpha + ['0'..'9'];

implementation
uses StrUtils, LazFileUtils;

type
  TStdTypeDef = record
    StdName:string;
    PasName:string;
    QuotedStr:Boolean;
    XSDDataType:TXSDDataType;
  end;

const
  RESERVED_WORDS_TP: array [1..54] of String = (
    'absolute', 'and', 'array', 'asm',
    'begin',
    'case', 'const', 'constructor',
    'destructor', 'div', 'do', 'downto',
    'else', 'end',
    'file', 'for', 'function',
    'goto',
    'if', 'implementation', 'in', 'inherited', 'inline', 'interface',
    'label',
    'mod',
    'nil', 'not',
    'object', 'of', 'on', 'operator', 'or',
    'packed', 'procedure', 'program',
    'record', 'reintroduce', 'repeat',
    'self', 'set', 'shl', 'shr', 'string',
    'then', 'to', 'type',
    'unit', 'until', 'uses',
    'var',
    'while', 'with',
    'xor'
  );

  RESERVED_WORDS_DELPHI: array [1..15] of String = (
    'as',
    'class',
    'except', 'exports',
    'finalization', 'finally',
    'initialization', 'is',
    'library',
    'on', 'out',
    'property',
    'raise',
    'threadvar',
    'try'
  );

  RESERVED_WORDS_FPC: array [1..5] of String = (
    'dispose', 'exit', 'false', 'new', 'true'
  );

  StdTypesArray : array [1..45] of TStdTypeDef = (
    (StdName: 'xs:ENTITIES'; PasName:'String'; QuotedStr:true; XSDDataType:xsdtString),
    (StdName: 'xs:ENTITY'; PasName:'String'; QuotedStr:true; XSDDataType:xsdtString),
    (StdName: 'xs:ID'; PasName:'String'; QuotedStr:true; XSDDataType:xsdtString),                   // Строка, представляющая идентификационный атрибут (используется только с атрибутами схемы
    (StdName: 'xs:IDREF'; PasName:'String'; QuotedStr:true; XSDDataType:xsdtString),                // Строка, представляющая IDREF атрибут (используется только с атрибутами схемы)
    (StdName: 'xs:IDREFS'; PasName:'String'; QuotedStr:true; XSDDataType:xsdtString),               //
    (StdName: 'xs:language'; PasName:'String'; QuotedStr:true; XSDDataType:xsdtString),             // Строка, содержащая корректный идентификатор языка
    (StdName: 'xs:Name'; PasName:'String'; QuotedStr:true; XSDDataType:xsdtString),                 // Строка, содержащая корректное XML имя
    (StdName: 'xs:NCName'; PasName:'String'; QuotedStr:true; XSDDataType:xsdtString),               //
    (StdName: 'xs:NMTOKEN'; PasName:'String'; QuotedStr:true; XSDDataType:xsdtString),              // Строка, представляющая NMTOKEN атрибут (используется только с атрибутами схемы)
    (StdName: 'xs:NMTOKENS'; PasName:'String'; QuotedStr:true; XSDDataType:xsdtString),             //
    (StdName: 'xs:normalizedString'; PasName:'String'; QuotedStr:true; XSDDataType:xsdtString),     //	Строка, которая не содержит символы перевода строки, переноса каретки или табуляции
    (StdName: 'xs:QName'; PasName:'String'; QuotedStr:true; XSDDataType:xsdtString),                //
    (StdName: 'xs:string'; PasName:'String'; QuotedStr:true; XSDDataType:xsdtString),               // Любая строка
    (StdName: 'xs:token'; PasName:'String'; QuotedStr:true; XSDDataType:xsdtString),                // Строка, которая не содержит символы перевода строки, переноса каретки, табуляции, начального и конечного пробелов или множественные пробелы

    (StdName: 'xs:byte'; PasName:'Shortint'; QuotedStr:false; XSDDataType:xsdtInteger),              // 8-битное целочисленное значение со знаком
    (StdName: 'xs:decimal'; PasName:'Double'; QuotedStr:false; XSDDataType:xsdtFloat),             // Десятичное значение
    (StdName:'xs:int'; PasName:'Longint'; QuotedStr:false; XSDDataType:xsdtInteger),                 // 32-битное целочисленное значение со знаком
    (StdName:'xs:integer'; PasName:'Int64'; QuotedStr:false; XSDDataType:xsdtInteger),               // Целочисленное значение
    (StdName:'xs:long'; PasName:'Int64'; QuotedStr:false; XSDDataType:xsdtInteger),                  // 64-битное целочисленное значение со знаком
    (StdName:'xs:negativeInteger'; PasName:'Integer'; QuotedStr:false; XSDDataType:xsdtInteger),     // Целочисленное, содержащее только отрицательные значения (..,-2,-1)
    (StdName:'xs:nonNegativeInteger'; PasName:'Integer'; QuotedStr:false; XSDDataType:xsdtInteger),  // Целочисленное, содержащее только не-отрицательные значения (0,1,2,..)
    (StdName:'xs:nonPositiveInteger'; PasName:'Integer'; QuotedStr:false; XSDDataType:xsdtInteger),  // Целочисленное, содержащее только не-положительные значения (..,-2,-1,0)
    (StdName:'xs:positiveInteger'; PasName:'Integer'; QuotedStr:false; XSDDataType:xsdtInteger),     // Целочисленное, содержащее только положительные значения (1,2,..)
    (StdName:'xs:short'; PasName:'Smallint'; QuotedStr:false; XSDDataType:xsdtInteger),              // 16-битное целочисленное значение со знаком
    (StdName:'xs:unsignedLong'; PasName:'QWord'; QuotedStr:false; XSDDataType:xsdtInteger),          // 64-битное целочисленное значение без знака
    (StdName:'xs:unsignedInt'; PasName:'Longword'; QuotedStr:false; XSDDataType:xsdtInteger),        // 32-битное целочисленное значение без знака
    (StdName:'xs:unsignedShort'; PasName:'Word'; QuotedStr:false; XSDDataType:xsdtInteger),          // 16-битное целочисленное значение без знака
    (StdName:'xs:unsignedByte'; PasName:'Byte'; QuotedStr:false; XSDDataType:xsdtInteger),           // 8-битное целочисленное значение без знака

    (StdName:'xs:date'; PasName:'TDate'; QuotedStr:true; XSDDataType:xsdtDateTime),                   // Определяет дату
    (StdName:'xs:time'; PasName:'TTime'; QuotedStr:true; XSDDataType:xsdtDateTime),                   // Определяет время
    (StdName:'xs:dateTime'; PasName:'TDateTime'; QuotedStr:true; XSDDataType:xsdtDateTime),           // Определяет дату и время
    (StdName:'xs:duration'; PasName:'TDateTime'; QuotedStr:true; XSDDataType:xsdtDateTime),           // Определяет интервал времени
    (StdName:'xs:gDay'; PasName:'Byte'; QuotedStr:false; XSDDataType:xsdtInteger),                   // Определяет часть даты - день (ДД)
    (StdName:'xs:gMonth'; PasName:'Byte'; QuotedStr:false; XSDDataType:xsdtInteger),                 // Определяет часть даты - месяц (MM)
    (StdName:'xs:gMonthDay'; PasName:'Word'; QuotedStr:false; XSDDataType:xsdtInteger),              // Определяет часть даты — месяц и день (MM-ДД)      //TODO:add new type - record with fields MM DD
    (StdName:'xs:gYear'; PasName:'Word'; QuotedStr:false; XSDDataType:xsdtInteger),                  // Определяет часть даты - год (ГГГГ)
    (StdName:'xs:gYearMonth'; PasName:'Longword'; QuotedStr:false; XSDDataType:xsdtInteger),         // Определяет часть даты — год и месяц (ГГГГ-MM) //TODO:add new type - record with fields YYYY MM

    (StdName:'xs:boolean'; PasName:'Boolean'; QuotedStr:false; XSDDataType:xsdtBoolean),             // Логический тип данных

    (StdName:'xs:base64Binary'; PasName:'String'; QuotedStr:true; XSDDataType:xsdtString),          // бинарные данные в кодировке Base64
    (StdName:'xs:hexBinary'; PasName:'String'; QuotedStr:true; XSDDataType:xsdtString),             // бинарные данные в шестнадцатеричной кодировке

    (StdName:'xs:anyURI'; PasName:'String'; QuotedStr:true; XSDDataType:xsdtString),                   // Тип данных anyURI используется для определения URI

    (StdName:'xs:float'; PasName:'Double'; QuotedStr:false; XSDDataType:xsdtFloat),                   //
    (StdName:'xs:double'; PasName:'Double'; QuotedStr:false; XSDDataType:xsdtFloat),                  //
    (StdName:'xs:Qname'; PasName:'String'; QuotedStr:true; XSDDataType:xsdtString),                    //
    (StdName:'xs:NOTATION'; PasName:'String'; QuotedStr:true; XSDDataType:xsdtString)                  //
    );



var
  KeywordsList: TStringList = nil;
  StdTypesList: TStringList = nil;

procedure InitStdTypes;
var
  D: TXSDStdType;
  R: TStdTypeDef;
begin
  if Assigned(StdTypesList) then Exit;
  StdTypesList:=TStringList.Create;
  for R in StdTypesArray do
  begin
    D:=TXSDStdType.Create;
    D.PascalName:=R.PasName;
    D.QuotedStr:=R.QuotedStr;
    D.XSDDataType:=R.XSDDataType;
    StdTypesList.AddObject(R.StdName, D);
  end;
  StdTypesList.Sorted:=true;
end;

function FindStdTypeByPasName(APasName:string):TXSDStdType;
var
  i: Integer;
  D: TXSDStdType;
begin
  for i:=0 to StdTypesList.Count-1 do
  begin
    D:=TXSDStdType(StdTypesList.Objects[i]);
    if D.PascalName = APasName then
      Exit(D);
  end;
  Result:=nil;
end;

procedure DoneStdTypes;
var
  i: Integer;
begin
  if not Assigned(StdTypesList) then Exit;
  for i:=0 to StdTypesList.Count-1 do
    TXSDStdType(StdTypesList.Objects[i]).Free;
  FreeAndNil(StdTypesList);
end;

function IsKeyword(const AKeyword: string): boolean;
var
  i: integer;
begin
  if not Assigned(KeywordsList) then
  begin
    KeywordsList := TStringList.Create;
    for i := 1 to High(RESERVED_WORDS_TP) do
      KeywordsList.Add(RESERVED_WORDS_TP[i]);
    for i := 1 to High(RESERVED_WORDS_DELPHI) do
      KeywordsList.Add(RESERVED_WORDS_DELPHI[i]);
    for i := 1 to High(RESERVED_WORDS_FPC) do
      KeywordsList.Add(RESERVED_WORDS_FPC[i]);
    KeywordsList.Sorted := true;
  end;
  Result := KeywordsList.Find(LowerCase(AKeyword), i);
end;

function FindPasTypeRec(ATypeName: string): TXSDStdType;
var
  I: Integer;
begin
  if not Assigned(StdTypesList) then
    InitStdTypes;

  if StdTypesList.Find(ATypeName, I) then
    Result:=TXSDStdType(StdTypesList.Objects[i])
  else
    Result:=nil;
end;

function GenerateTypeDescription(ADescription: string; ASpacing:integer = 2): string;
var
  ST: TStringList;
  S: String;
begin
  Result:='';
  if Trim(ADescription) = '' then Exit;

  ST:=TStringList.Create;
  ST.Text:=ADescription;
  for S in ST do
    if Trim(S)<>'' then
      Result:=Result + DupeString(' ', ASpacing) + '//'+Trim(S) + LineEnding;
  ST.Free;
end;

function ExpandXSDFileName(AFileName: string; AIncludeFolders: TStrings): string;
var
  S: String;
begin
  if FileExists(AFileName) then
    Exit(AFileName)
  else
    Result:='';
  if not Assigned(AIncludeFolders) then Exit;
  for S in AIncludeFolders do
    if FileExists(AppendPathDelim(S) + AFileName) then
      Result:=AppendPathDelim(S) + AFileName;
end;

function IsSimpleType(ATypeName:string):Boolean;
var
  I: Integer;
begin
  if not Assigned(StdTypesList) then
    InitStdTypes;

  Result:=StdTypesList.Find(ATypeName, I);
end;

function IsTypeQuotedStr(ATypeName: string): Boolean;
var
  I: Integer;
  S: String;
begin
  if not Assigned(StdTypesList) then
    InitStdTypes;
  Result:=false;
  if StdTypesList.Find(ATypeName, I) then
  begin
    S:=TXSDStdType(StdTypesList.Objects[i]).PascalName;
    Result:=TXSDStdType(StdTypesList.Objects[i]).QuotedStr
  end
  else
    Result:=false;
end;

function GetSimpleType(ATypeName:string):string;
var
  I: Integer;
begin
  if not Assigned(StdTypesList) then
    InitStdTypes;

  if StdTypesList.Find(ATypeName, I) then
    Result:=TXSDStdType(StdTypesList.Objects[i]).PascalName
  else
    Result:='';
end;

finalization
  if Assigned(KeywordsList) then
    FreeAndNil(KeywordsList);
  DoneStdTypes;

end.

