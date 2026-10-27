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

unit test_crash_namespace;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, xmlobject, AbstractSerializationObjects;

type

  {  Forward declarations  }
  TInvoice = class;

  {  Generic classes for collections  }
  TInvoiceList = specialize GXMLSerializationObjectList<TInvoice>;

  {  TInvoice  }
  TInvoice = class(TXmlSerializationObject)
  private
    FInvoiceNumber:String;
    FAmount:Double;
    procedure SetInvoiceNumber( AValue:String);
    procedure SetAmount( AValue:Double);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
    function RootNodeName:string; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    property InvoiceNumber:String read FInvoiceNumber write SetInvoiceNumber;
    property Amount:Double read FAmount write SetAmount;
  end;

implementation

  {  TInvoice  }
procedure TInvoice.SetInvoiceNumber(AValue: String);
begin
  FInvoiceNumber:=AValue;
  ModifiedProperty('InvoiceNumber');
end;

procedure TInvoice.SetAmount(AValue: Double);
begin
  FAmount:=AValue;
  ModifiedProperty('Amount');
end;

procedure TInvoice.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('InvoiceNumber', 'InvoiceNumber', [xsaSimpleObject], '', -1, -1);
  P:=RegisterProperty('Amount', 'Amount', [xsaSimpleObject], '', -1, -1);
end;

procedure TInvoice.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TInvoice.Destroy;
begin
  inherited Destroy;
end;

function TInvoice.RootNodeName:string;
begin
  Result:='Invoice';
end;

constructor TInvoice.Create;
begin
  inherited Create;
end;

end.
