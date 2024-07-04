{ google protobuf files compiler to FPC class

  Copyright (C) 2018-2022 Lagunov Aleksey alexs@yandex.ru

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

unit PasCodegen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ProtoParser, EnumObject, ControlObjects, ProtoObjects;

type
  TPascalCodeGenerator = class;

  TCodeGenStatus = procedure (Sender:TPascalCodeGenerator; AObject:TProtoObject; AMessage:string) of object;

  { TPascalCodeGenerator }

  TPascalCodeGenerator = class
  private
    FCopyrightInfoFile: string;
    FEnumList:TStringList;
    FIgnoreMissingUnitName: boolean;
    FIncludeFileFolders: TStringList;
    FOnStatus: TCodeGenStatus;
    FParser:TProtoParser;
    FPasUnitName: string;
    FResultCode:TStringList;
    FResultFileNameLowerCase: Boolean;
    FResultFileNamePrfix: string;
    FShowSourceProtoCode: boolean;
    FTempList:TStringList;
    procedure InternalStatus(AObject:TProtoObject; AMessage:string);
    procedure GenerateUnitHeader;
    procedure GenerateUsesSection;
    procedure GenerateInterfaceSection;
    procedure GenerateImplementationSection;
    procedure GenerateEnumSection;
    procedure GenerateForwardDeclaration;
    //
    procedure ProcessEnums(APS:TProtoParser);
    procedure FixSimpleTypeEnum;
    procedure DoImportFiles;

  public
    constructor Create(AParser:TProtoParser);
    destructor Destroy;override;
    function GeneratePascalCode:string;
    property PasUnitName:string read FPasUnitName write FPasUnitName;
    property IgnoreMissingUnitName:boolean read FIgnoreMissingUnitName write FIgnoreMissingUnitName;
    property OnStatus:TCodeGenStatus read FOnStatus write FOnStatus;
    property IncludeFileFolders:TStringList read FIncludeFileFolders;
    property ShowSourceProtoCode:boolean read FShowSourceProtoCode write FShowSourceProtoCode;
    property CopyrightInfoFile:string read FCopyrightInfoFile write FCopyrightInfoFile;
    property ResultFileNamePrfix:string read FResultFileNamePrfix write FResultFileNamePrfix;
    property ResultFileNameLowerCase:Boolean read FResultFileNameLowerCase write FResultFileNameLowerCase;
  end;

function PascalCodeGen(AParser:TProtoParser):string;

implementation
uses LazFileUtils, LazStringUtils;

function FileToString(const AFileName: string): string;
var
  F: TFileStream;
begin
  if FileExists(AFileName) then
  begin;
    F:=TFileStream.Create(AFileName, fmOpenRead);
    if F.Size>0 then
    begin
      SetLength(Result, F.Size);
      F.ReadBuffer(Result[1], F.Size);
    end
    else
      Result:='';
    F.Free;
  end
  else
    Result:='';
end;

const
  sProtoExt = '.proto';

function DoFormatFileName(AFileName:string):string;
var
  Ext: String;
begin
  Ext:=LowerCase(ExtractFileExt(AFileName));
  if Ext = sProtoExt then
    Delete(AFileName, Length(AFileName) - Length(sProtoExt) + 1, Length(sProtoExt));
  AFileName:=ExtractFileName(AFileName);
  Result:=StringReplace(AFileName, '.', '_', [rfReplaceAll]);
end;

function PascalCodeGen(AParser: TProtoParser): string;
var
  CG: TPascalCodeGenerator;
begin
  CG:=TPascalCodeGenerator.Create(AParser);
  CG.IgnoreMissingUnitName:=true;
  try
    Result:=CG.GeneratePascalCode;
  finally
    CG.Free;
  end;
end;

{ TPascalCodeGenerator }

procedure TPascalCodeGenerator.InternalStatus(AObject: TProtoObject;
  AMessage: string);
begin
  if Assigned(FOnStatus) then
    FOnStatus(Self, AObject, AMessage);
end;

procedure TPascalCodeGenerator.GenerateUnitHeader;
var
  O: TProtoObject;
begin
  if FPasUnitName = '' then
  begin
    for O in FParser.ResultObjects do
      if O is TPackage then
      begin
        FPasUnitName:=DoFormatFileName(O.Caption);
        Break;
      end;
    if FPasUnitName = '' then
      if FIgnoreMissingUnitName then
        Exit
      else
        raise Exception.Create('Not defined output unit file name');
  end;

  if (FCopyrightInfoFile<>'') then
  begin
    if not FileExists(FCopyrightInfoFile) then
      raise Exception.CreateFmt('Not found copyright info file name - %s', [FCopyrightInfoFile]);
    FResultCode.Add(FileToString(FCopyrightInfoFile));
  end;

  FResultCode.Add('unit ' + FResultFileNamePrfix + FPasUnitName+';');
  FResultCode.Add('');
end;

procedure TPascalCodeGenerator.GenerateUsesSection;
var
  O: TProtoObject;
  S: String;
begin
  S:='Classes, SysUtils, types, protobuf_fpc';
  for O in FParser.ResultObjects do
    if O is TImport then
    begin
      if S<>'' then S:=S + ',';
      S:=S + ' ' + FResultFileNamePrfix + DoFormatFileName(O.Caption);
    end;
  if S<>'' then
  begin
    FResultCode.Add('uses ' + S + ';');
    FResultCode.Add('');
  end;
end;

procedure TPascalCodeGenerator.GenerateInterfaceSection;
var
  S1:TStringList;
procedure DoGenerate(P: TProtoObject);
var
  S: String;
begin
  FTempList.Clear;
  S1.Clear;
  P.GenerateInterfaceSection(FTempList);
  if FTempList.Count>0 then
  begin
    FResultCode.Add('  { ' + P.Caption + ' } ');

    if (FShowSourceProtoCode) and (Trim(P.SourceCode)<>'') then
    begin
      S1.Text:=P.SourceCode;
      for S in S1 do
        FResultCode.Add('  //' + S);
    end;

    for S in FTempList do
      FResultCode.Add('  ' + S);
    FResultCode.Add('');
  end;
end;

var
  P: TProtoObject;
  F: TMessageField;
begin
  S1:=TStringList.Create;
  for P in FParser.ResultObjects do
  if not (P is TEnum) then
  begin
    if P is TProtoMessage then
      for F in TProtoMessage(P).Fields do
        if F.FieldType = mftMessageDefinition then
          DoGenerate(F.ProtoObjDef);

    DoGenerate(P);
  end;
  FreeAndNil(S1);
end;

procedure TPascalCodeGenerator.GenerateImplementationSection;
procedure DoGenerate(P: TProtoObject);
var
  S: String;
begin
  FTempList.Clear;
  P.GenerateImplementationSection(FTempList);
  if FTempList.Count>0 then
  begin
    FResultCode.Add('');
    FResultCode.Add('  { ' + P.Caption + ' } ');
    for S in FTempList do
      FResultCode.Add(S);
  end;
end;

var
  P: TProtoObject;
  F: TMessageField;
begin
  for P in FParser.ResultObjects do
  if not (P is TEnum) then
  begin
    if P is TProtoMessage then
      for F in TProtoMessage(P).Fields do
        if F.FieldType = mftMessageDefinition then
          DoGenerate(F.ProtoObjDef);

    DoGenerate(P);
  end;
end;

procedure TPascalCodeGenerator.GenerateEnumSection;
var
  P: TProtoObject;
  S: String;
  S1:TStringList;
  PM: TProtoMessage;
  F: TMessageField;
begin
  S1:=TStringList.Create;
  for P in FParser.ResultObjects do
  begin
    if P is TEnum then
    begin
      FTempList.Clear;
      P.GenerateInterfaceSection(FTempList);
      if FTempList.Count>0 then
      begin
        FResultCode.Add('  { ' + P.Caption + ' } ');

        if (FShowSourceProtoCode) and (Trim(P.SourceCode)<>'') then
        begin
          S1.Text:=P.SourceCode;
          for S in S1 do
            FResultCode.Add('  //' + S);
        end;

        for S in FTempList do
          FResultCode.Add('  ' + S);
        FResultCode.Add('');
      end;
    end
    else
    if P is TProtoMessage then
    begin
      PM:=P as TProtoMessage;
      if PM.Fields.CountEnumDefs > 0 then
      begin
        for F in PM.Fields do
          if (F.FieldType = mftEnumDefinition) and Assigned(F.ProtoObjDef) then
          begin
            FTempList.Clear;
            F.ProtoObjDef.GenerateInterfaceSection(FTempList);

            for S in FTempList do
              FResultCode.Add('  ' + S);
            FResultCode.Add('');
          end;

      end;
    end;
  end;

  S1.Free;
end;

procedure TPascalCodeGenerator.GenerateForwardDeclaration;
var
  P: TProtoObject;
begin
  for P in FParser.ResultObjects do
    if P is TProtoMessage then
    begin
      FResultCode.Add('  T%s = class;',[P.Caption]);
      FResultCode.Add('  T%ss = specialize GSerializationObjectList<T%s>;', [P.Caption, P.Caption]);
    end;
  FResultCode.Add('');
end;

procedure TPascalCodeGenerator.ProcessEnums(APS: TProtoParser);
var
  P: TProtoObject;
  F: TMessageField;
begin
  for P in APS.ResultObjects do
  begin
    if (P is TEnum) and (FEnumList.IndexOf(UpperCase(P.Caption)) < 0) then
      FEnumList.Append(UpperCase(P.Caption))
    else
    if P is TProtoMessage then
    begin
      for F in TProtoMessage(P).Fields do
        if F.FieldType = mftEnumDefinition then
          FEnumList.Add(UpperCase(F.ProtoObjDef.Caption));
    end;
  end;
end;

procedure TPascalCodeGenerator.FixSimpleTypeEnum;
//var
//  FEL:TStringList;

procedure DoProcessMsgType(P: TProtoMessage);
var
  F: TMessageField;
  k: SizeInt;
  S: String;
begin
  //FEL.Clear;
  //for F in P.Fields do
  //  if F.FieldType = mftEnumDefinition then
  //    FEL.Add(UpperCase(F.ProtoObjDef.Caption));

  for F in P.Fields do
  begin
    if F.DataTypeFlag = pdtClass then
    begin

      k:=Pos('.', F.DataType);
      if k > 0 then
        S:=Copy(F.DataType, k+1,Length(F.DataType))
      else
        S:=F.DataType;

      if (FEnumList.IndexOf(UpperCase(S))>-1) {or (FEL.IndexOf(UpperCase(F.DataType))>-1)} then
        F.DataTypeFlag:=pdtEnum
    end
    else
    if (F.FieldType = mftMessageDefinition) and Assigned(F.ProtoObjDef) then
      DoProcessMsgType(F.ProtoObjDef as TProtoMessage);
  end;
end;

var
  P: TProtoObject;
begin
  InternalStatus(nil, 'Process include files');
  FEnumList:=TStringList.Create;
  FEnumList.Sorted:=true;
  //FEL:=TStringList.Create;
  //FEL.Sorted:=true;

  ProcessEnums(FParser);

  DoImportFiles;

  for P in FParser.ResultObjects do
    if P is TProtoMessage then
      DoProcessMsgType(P as TProtoMessage);

  FEnumList.Free;
  //FEL.Free;
end;

procedure TPascalCodeGenerator.DoImportFiles;

procedure DoCompile(S:string);
var
  FP:TProtoParser;
  CF, SD: String;
begin
  CF:=S;
  if (not FileExists(CF)) then
  begin
    CF:='';
    if (FIncludeFileFolders.Count>0) then
    begin
      S:=ExtractFileName(S);
      for SD in FIncludeFileFolders do
        if FileExists(AppendPathDelim(SD) + S) then
        begin
          CF:=AppendPathDelim(SD) + S;
          Break;
        end;
    end;

    if CF = '' then
    begin
      InternalStatus(nil, 'File not found : ' + S);
      Exit;
    end;
  end;
  FP:=TProtoParser.Create(FParser);
  try
    FP.Compile(CF);
    if FP.State <> cmsError then
      ProcessEnums(FP);
  finally
    FP.Free;
  end;
end;

var
  FList:TStringList;
  O: TProtoObject;
  S: String;
begin
  FList:=TStringList.Create;
  for O in FParser.ResultObjects do
    if O is TImport then
      FList.Add(O.Caption);

  try
    for S in FList do
    begin
      InternalStatus(nil, 'Include file ' + S);
      DoCompile(S);
    end;
  finally
    FList.Free;
  end;
end;

constructor TPascalCodeGenerator.Create(AParser: TProtoParser);
begin
  inherited Create;
  FShowSourceProtoCode:=true;
  FIncludeFileFolders:=TStringList.Create;
  FResultCode:=TStringList.Create;
  FTempList:=TStringList.Create;
  FParser:=AParser;
  FResultFileNameLowerCase:=true;
end;

destructor TPascalCodeGenerator.Destroy;
begin
  FResultCode.Free;
  FTempList.Clear;
  FreeAndNil(FIncludeFileFolders);
  inherited Destroy;
end;

function TPascalCodeGenerator.GeneratePascalCode: string;
begin
  FResultCode.Clear;

//  DoImportFiles;
  FixSimpleTypeEnum;

  GenerateUnitHeader;
  FResultCode.Add('interface');
  FResultCode.Add('');
  GenerateUsesSection;
  FResultCode.Add('type');
  FResultCode.Add('');


  GenerateEnumSection;
  GenerateForwardDeclaration;
  GenerateInterfaceSection;




  FResultCode.Add('implementation');

  GenerateImplementationSection;

  FResultCode.Add('end.');

  Result:=FResultCode.Text;
end;

end.

