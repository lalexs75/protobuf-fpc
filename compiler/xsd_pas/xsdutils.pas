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
implementation

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

end.

