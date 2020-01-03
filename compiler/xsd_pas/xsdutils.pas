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

unit xsdutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function IsSimpleType(ATypeName:string):Boolean;
function GetSimpleType(ATypeName:string):string;
function IsKeyword(const AKeyword: string): boolean;
implementation
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
var
  KeywordsList: TStringList;

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

function IsSimpleType(ATypeName:string):Boolean;
begin
  Result:=
    (ATypeName = 'xs:boolean') or
    (ATypeName = 'xs:date') or
    (ATypeName = 'xs:time') or
    (ATypeName = 'xs:dateTime') or
    (ATypeName = 'xs:base64Binary') or

    (ATypeName = 'xs:ENTITIES') or
    (ATypeName = 'xs:ENTITY') or
    (ATypeName = 'xs:ID') or //	Строка, представляющая идентификационный атрибут (используется только с атрибутами схемы)
    (ATypeName = 'xs:IDREF') or //	Строка, представляющая IDREF атрибут (используется только с атрибутами схемы)
    (ATypeName = 'xs:IDREFS') or //
    (ATypeName = 'xs:language') or //	Строка, содержащая корректный идентификатор языка
    (ATypeName = 'xs:Name') or //	Строка, содержащая корректное XML имя
    (ATypeName = 'xs:NCName') or //
    (ATypeName = 'xs:NMTOKEN') or //	Строка, представляющая NMTOKEN атрибут (используется только с атрибутами схемы)
    (ATypeName = 'xs:NMTOKENS') or //
    (ATypeName = 'xs:normalizedString') or //	Строка, которая не содержит символы перевода строки, переноса каретки или табуляции
    (ATypeName = 'xs:QName') or //
    (ATypeName = 'xs:string') or //	Любая строка
    (ATypeName = 'xs:token') or //	Строка, которая не содержит символы перевода строки, переноса каретки, табуляции, начального и конечного пробелов или множественные пробелы

    (ATypeName = 'xs:byte') or //	8-битное целочисленное значение со знаком
    (ATypeName = 'xs:decimal') or //	Десятичное значение
    (ATypeName = 'xs:int') or //	32-битное целочисленное значение со знаком
    (ATypeName = 'xs:integer') or //	Целочисленное значение
    (ATypeName = 'xs:long') or //	64-битное целочисленное значение со знаком
    (ATypeName = 'xs:negativeInteger') or //	Целочисленное, содержащее только отрицательные значения (..,-2,-1)
    (ATypeName = 'xs:nonNegativeInteger') or //	Целочисленное, содержащее только не-отрицательные значения (0,1,2,..)
    (ATypeName = 'xs:nonPositiveInteger') or //	Целочисленное, содержащее только не-положительные значения (..,-2,-1,0)
    (ATypeName = 'xs:positiveInteger') or //	Целочисленное, содержащее только положительные значения (1,2,..)
    (ATypeName = 'xs:short') or //	16-битное целочисленное значение со знаком
    (ATypeName = 'xs:unsignedLong') or //	64-битное целочисленное значение без знака
    (ATypeName = 'xs:unsignedInt') or //	32-битное целочисленное значение без знака
    (ATypeName = 'xs:unsignedShort') or //	16-битное целочисленное значение без знака
    (ATypeName = 'xs:unsignedByte') //	8-битное целочисленное значение без знака    ;
end;

function GetSimpleType(ATypeName:string):string;
begin
  if (ATypeName = 'xs:boolean') then
    Result:='Boolean'
  else
  if  (ATypeName = 'xs:date') then
    //Result:='TTime'
    Result:='String'
  else
  if (ATypeName = 'xs:time') then
    //Result:='TDate'
    Result:='String'
  else
  if (ATypeName = 'xs:dateTime') then
    //Result:='TDateTime'
    Result:='String'
  else
  if (ATypeName = 'xs:base64Binary') then
    Result:='String'
  else

  if (ATypeName = 'xs:ENTITIES') then
    Result:='String'
  else
  if (ATypeName = 'xs:ENTITY') then
    Result:='String'
  else
  if (ATypeName = 'xs:ID') then                 // Строка, представляющая идентификационный атрибут (используется только с атрибутами схемы)
    Result:='String'
  else
  if (ATypeName = 'xs:IDREF') then              // Строка, представляющая IDREF атрибут (используется только с атрибутами схемы)
    Result:='String'
  else
  if (ATypeName = 'xs:IDREFS') then             //
    Result:='String'
  else
  if (ATypeName = 'xs:language') then           // Строка, содержащая корректный идентификатор языка
    Result:='String'
  else
  if (ATypeName = 'xs:Name') then               // Строка, содержащая корректное XML имя
    Result:='String'
  else
  if (ATypeName = 'xs:NCName') then             //
    Result:='String'
  else
  if (ATypeName = 'xs:NMTOKEN') then            // Строка, представляющая NMTOKEN атрибут (используется только с атрибутами схемы)
    Result:='String'
  else
  if (ATypeName = 'xs:NMTOKENS') then           //
    Result:='String'
  else
  if (ATypeName = 'xs:normalizedString') then   // Строка, которая не содержит символы перевода строки, переноса каретки или табуляции
    Result:='String'
  else
  if (ATypeName = 'xs:QName') then    //
    Result:='String'
  else
  if (ATypeName = 'xs:string') then   // Любая строка
    Result:='String'
  else
  if (ATypeName = 'xs:token') then    // Строка, которая не содержит символы перевода строки, переноса каретки, табуляции, начального и конечного пробелов или множественные пробелы
    Result:='String'
  else

  if (ATypeName = 'xs:byte') then //	8-битное целочисленное значение со знаком
    Result:='Integer'
  else
  if (ATypeName = 'xs:decimal') then //	Десятичное значение
    Result:='Double'
  else
  if (ATypeName = 'xs:int') then//	32-битное целочисленное значение со знаком
    Result:='Integer'
  else
  if (ATypeName = 'xs:integer') then//	Целочисленное значение
    Result:='Integer'
  else
  if (ATypeName = 'xs:long') then//	64-битное целочисленное значение со знаком
    Result:='Integer'
  else
  if (ATypeName = 'xs:negativeInteger') then//	Целочисленное, содержащее только отрицательные значения (..,-2,-1)
    Result:='Integer'
  else
  if (ATypeName = 'xs:nonNegativeInteger') then//	Целочисленное, содержащее только не-отрицательные значения (0,1,2,..)
    Result:='Integer'
  else
  if (ATypeName = 'xs:nonPositiveInteger') then//	Целочисленное, содержащее только не-положительные значения (..,-2,-1,0)
    Result:='Integer'
  else
  if (ATypeName = 'xs:positiveInteger') then//	Целочисленное, содержащее только положительные значения (1,2,..)
    Result:='Integer'
  else
  if (ATypeName = 'xs:short') then//	16-битное целочисленное значение со знаком
    Result:='Integer'
  else
  if (ATypeName = 'xs:unsignedLong') then//	64-битное целочисленное значение без знака
    Result:='Integer'
  else
  if (ATypeName = 'xs:unsignedInt') then//	32-битное целочисленное значение без знака
    Result:='Integer'
  else
  if (ATypeName = 'xs:unsignedShort') then//	16-битное целочисленное значение без знака
    Result:='Integer'
  else
  if (ATypeName = 'xs:unsignedByte') then//	8-битное целочисленное значение без знака    ;
    Result:='Byte'
  else
    Result:='';
end;

finalization
  FreeAndNil(KeywordsList);
end.

