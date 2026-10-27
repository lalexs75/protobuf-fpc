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

unit TestCodeGenErrors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, xmlobject, AbstractSerializationObjects;

type

  {  Forward declarations  }
  TAddressType = class;
  TPerson = class;
  TPerson_ContactInfo = class;

  {  Generic classes for collections  }
  TAddressTypeList = specialize GXMLSerializationObjectList<TAddressType>;
  TPersonList = specialize GXMLSerializationObjectList<TPerson>;
  TPerson_ContactInfoList = specialize GXMLSerializationObjectList<TPerson_ContactInfo>;

  {  TAddressType  }
  TAddressType = class(TXmlSerializationObject)
  private
    FStreet:String;
    FCity:String;
    procedure SetStreet( AValue:String);
    procedure SetCity( AValue:String);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    property Street:String read FStreet write SetStreet;
    property City:String read FCity write SetCity;
  end;

  {  TPerson  }
  TPerson = class(TXmlSerializationObject)
  private
    FName:String;
    FAddress:TAddressType;
    FContactInfo:TPerson_ContactInfo;
    procedure SetName( AValue:String);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
    function RootNodeName:string; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    property Name:String read FName write SetName;
    property Address:TAddressType read FAddress;
    property ContactInfo:TPerson_ContactInfo read FContactInfo;
  end;

  {  TPerson_ContactInfo  }
  TPerson_ContactInfo = class(TXmlSerializationObject)
  private
    FPhone:String;
    procedure SetPhone( AValue:String);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    property Phone:String read FPhone write SetPhone;
  end;

implementation

  {  TAddressType  }
procedure TAddressType.SetStreet(AValue: String);
begin
  FStreet:=AValue;
  ModifiedProperty('Street');
end;

procedure TAddressType.SetCity(AValue: String);
begin
  FCity:=AValue;
  ModifiedProperty('City');
end;

procedure TAddressType.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('Street', 'Street', [xsaSimpleObject], '', -1, -1);
  P:=RegisterProperty('City', 'City', [xsaSimpleObject], '', -1, -1);
end;

procedure TAddressType.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TAddressType.Destroy;
begin
  inherited Destroy;
end;

constructor TAddressType.Create;
begin
  inherited Create;
end;

  {  TPerson  }
procedure TPerson.SetName(AValue: String);
begin
  FName:=AValue;
  ModifiedProperty('Name');
end;

procedure TPerson.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('Name', 'Name', [xsaSimpleObject], '', -1, -1);
  P:=RegisterProperty('Address', 'Address', [], '', -1, -1);
  P:=RegisterProperty('ContactInfo', 'ContactInfo', [], '', -1, -1);
end;

procedure TPerson.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FAddress:=TAddressType.Create;
  FContactInfo:=TPerson_ContactInfo.Create;
end;

destructor TPerson.Destroy;
begin
  FAddress.Free;
  FContactInfo.Free;
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

  {  TPerson_ContactInfo  }
procedure TPerson_ContactInfo.SetPhone(AValue: String);
begin
  FPhone:=AValue;
  ModifiedProperty('Phone');
end;

procedure TPerson_ContactInfo.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('Phone', 'Phone', [xsaSimpleObject], '', -1, -1);
end;

procedure TPerson_ContactInfo.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TPerson_ContactInfo.Destroy;
begin
  inherited Destroy;
end;

constructor TPerson_ContactInfo.Create;
begin
  inherited Create;
end;

end.
