{ Diadoc interface library for FPC and Lazarus

  Copyright (C) 2018-2026 Lagunov Aleksey alexs75@yandex.ru


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

unit test_crash_whitespace;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, xmlobject, AbstractSerializationObjects;

type

  {  Forward declarations  }
  TPerson = class;

  {  Generic classes for collections  }
  TPersonList = specialize GXMLSerializationObjectList<TPerson>;

  {  TPerson  }
  TPerson = class(TXmlSerializationObject)
  private
    FFirstName:String;
    FLastName:String;
    procedure SetFirstName( AValue:String);
    procedure SetLastName( AValue:String);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
    function RootNodeName:string; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    property FirstName:String read FFirstName write SetFirstName;
    property LastName:String read FLastName write SetLastName;
  end;

implementation

  {  TPerson  }
procedure TPerson.SetFirstName(AValue: String);
begin
  FFirstName:=AValue;
  ModifiedProperty('FirstName');
end;

procedure TPerson.SetLastName(AValue: String);
begin
  FLastName:=AValue;
  ModifiedProperty('LastName');
end;

procedure TPerson.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('FirstName', 'FirstName', [xsaSimpleObject], '', -1, -1);
  P:=RegisterProperty('LastName', 'LastName', [xsaSimpleObject], '', -1, -1);
end;

procedure TPerson.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TPerson.Destroy;
begin
  inherited Destroy;
end;

function TPerson.RootNodeName:string;
begin
  Result:='Person';
end;

constructor TPerson.Create;
begin
  inherited Create;
end;

end.
