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

unit xmlobject_resource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

resourcestring
  sNotFoundPropertyForField     = 'Not found property in %s for data field %s';
  sPropertyNotFound             = 'Not fond property %s.%s(%s)';
  sPropertyNotFound1            = 'Not fond property %s';
  sUknowPropertyType            = 'Uknow property type %s';
  sPropertyIsNotClassType       = 'Property is not class type %s';
  sClassPropertyNotInit         = 'Class property not init %s';
  sUnknowClassProperty          = 'Unknow class property %s.%s';
  sValueExpectedRange           = '%s.%s - Значение атрибута слишком велико (%s - %d)';

implementation

end.

