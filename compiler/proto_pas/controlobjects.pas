{ google protobuf files compiler to FPC class

  Copyright (C) 2018 Lagunov Aleksey alexs@yandex.ru

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

unit ControlObjects;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ProtoParser;

type

  { TImport }

  TImport = class(TProtoObject)
  private
  protected
    procedure InitParserTree;override;
    procedure InternalProcessChildToken(AParser:TProtoParser; AToken:TProtoToken; AWord:string); override;
  public
    procedure GenerateInterfaceSection(AModule:TStrings); override;
  end;

  { TPackage }

  TPackage = class(TProtoObject)
  private
  protected
    procedure InitParserTree;override;
    procedure InternalProcessChildToken(AParser:TProtoParser; AToken:TProtoToken; AWord:string); override;
  public
    procedure GenerateInterfaceSection(AModule:TStrings); override;
  end;


implementation
uses rxstrutils;


{ TPackage }

procedure TPackage.InitParserTree;
var
  T, T1, FStart: TProtoToken;
begin
  //package Diadoc.Api.Proto;
  FStart:=AddToken(stKeyword, nil, 'package', [toHeaderStart, toHeaderEnd]);
    T:=AddToken(stIdentificator, FStart, '', [], 2);
    T1:=AddToken(stSymbol, T, '.', [], 2);
      T1.AddChildToken(T);
  T:=AddToken(stSymbol, T, ';', [], -1);
end;

procedure TPackage.InternalProcessChildToken(AParser: TProtoParser;
  AToken: TProtoToken; AWord: string);
begin
  inherited InternalProcessChildToken(AParser, AToken, AWord);
  case AToken.Code of
    2:Caption:=Caption + AWord;
  end;
end;

procedure TPackage.GenerateInterfaceSection(AModule: TStrings);
begin
  //AModule.Add('package ' + Caption);
end;

{ TImport }

procedure TImport.InitParserTree;
var
  FStart, T, T1: TProtoToken;
begin
  //import "Organization.proto";
  //import public "new.proto";
  FStart:=AddToken(stKeyword, nil, 'import', [toHeaderStart, toHeaderEnd]);
    T1:=AddToken(stKeyword, FStart, 'public', []);
    T:=AddToken(stString, [FStart, T1], '', [], 2);
  T:=AddToken(stSymbol, T, ';', [], -1);
end;

procedure TImport.InternalProcessChildToken(AParser: TProtoParser;
  AToken: TProtoToken; AWord: string);
begin
  inherited InternalProcessChildToken(AParser, AToken, AWord);
  case AToken.Code of
    2:Caption:=ExtractQuotedString(AWord, '"');
  end;
end;

procedure TImport.GenerateInterfaceSection(AModule: TStrings);
begin
  //AModule.Add('import '+Caption+';');
end;

initialization
  RegisterProtoObject(TImport);
  RegisterProtoObject(TPackage);
end.

