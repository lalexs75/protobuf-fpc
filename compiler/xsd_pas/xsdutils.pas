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
    (ATypeName = 'xs:string') or
    (ATypeName = 'xs:decimal') or
    (ATypeName = 'xs:integer') or
    (ATypeName = 'xs:boolean') or
    (ATypeName = 'xs:date') or
    (ATypeName = 'xs:time') or
    (ATypeName = 'xs:token') or
    (ATypeName = 'xs:dateTime') or
    (ATypeName = 'xs:base64Binary');
end;

function GetSimpleType(ATypeName:string):string;
begin
  if (ATypeName = 'xs:string') or (ATypeName = 'xs:token') then
    Result:='String'
  else
  if (ATypeName = 'xs:decimal') then
    Result:='String'
    //Result:='Double'
  else
  if (ATypeName = 'xs:integer') then
    Result:='Integer'
  else
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
    Result:='';
end;

finalization
  FreeAndNil(KeywordsList);
end.

