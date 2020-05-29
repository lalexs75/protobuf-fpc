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

unit xsdconsts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

resourcestring
  sNotFoundElementInSchema    = 'Not found element "%s" in schema';
  sNotFoundSchemaInDocument   = 'Not found schema in document';
  sNotFoundIncludeFile        = 'Not found include file name "%s"';

implementation

end.

