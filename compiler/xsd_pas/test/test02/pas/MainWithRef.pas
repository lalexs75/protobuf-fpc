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

unit MainWithRef;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, xmlobject, AbstractSerializationObjects, CommonTypes;

type

  {  Forward declarations  }
  TDocument = class;

  {  Generic classes for collections  }
  TDocumentList = specialize GXMLSerializationObjectList<TDocument>;

  {  TDocument  }
  TDocument = class(TXmlSerializationObject)
  private
    FTitle:String;
    FGlobalID:String;
    Fversion:String;
    procedure SetTitle( AValue:String);
    procedure SetGlobalID( AValue:String);
    procedure Setversion( AValue:String);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
    function RootNodeName:string; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    property Title:String read FTitle write SetTitle;
    property GlobalID:String read FGlobalID write SetGlobalID;
    property version:String read Fversion write Setversion;
  end;

implementation

  {  TDocument  }
procedure TDocument.SetTitle(AValue: String);
begin
  FTitle:=AValue;
  ModifiedProperty('Title');
end;

procedure TDocument.SetGlobalID(AValue: String);
begin
  FGlobalID:=AValue;
  ModifiedProperty('GlobalID');
end;

procedure TDocument.Setversion(AValue: String);
begin
  Fversion:=AValue;
  ModifiedProperty('version');
end;

procedure TDocument.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('Title', 'Title', [xsaSimpleObject], '', -1, -1);
  P:=RegisterProperty('GlobalID', 'GlobalID', [xsaSimpleObject], '', -1, -1);
  P:=RegisterProperty('version', 'version', [], '', -1, -1);
end;

procedure TDocument.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TDocument.Destroy;
begin
  inherited Destroy;
end;

function TDocument.RootNodeName:string;
begin
  Result:='Document';
end;

constructor TDocument.Create;
begin
  inherited Create;
end;

end.
