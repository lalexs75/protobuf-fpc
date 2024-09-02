{ google protobuf files compiler to FPC class

  Copyright (C) 2018-2024 Lagunov Aleksey alexs@yandex.ru

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
program proto_pas;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, ControlObjects, EnumObject, PasCodegen, ProtoObjects,
  LazFileUtils, ProtoParser, CustApp;

resourcestring
  sIncludeFolder = 'Include folder : %s';
  sOutputDirectory = 'Output directory : %s';

type

  { TProtoToPasApplication }

  TProtoToPasApplication = class(TCustomApplication)
  private
    FCodeGen:TPascalCodeGenerator;
    FOutDir:string;
    FFileName:string;

    FResultFileNamePrefix: String;
    FResultFileNameLC: Boolean;

    procedure InitParser;
    procedure ParserStatus(Sender:TProtoParser; ALine:integer; AObject:TProtoObject; AMessage:string);
    procedure CodeGenStatus(Sender:TPascalCodeGenerator; AObject:TProtoObject; AMessage:string);
  protected
    procedure DoRun; override;
    procedure DoCompileFromFile(AFileName:string);
    procedure SaveResultFile(AFileName, AText:string);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
    procedure WriteLogo;
  end;

{ TProtoToPasApplication }

procedure TProtoToPasApplication.InitParser;
var
  IncArr:TStringArray;
  S: String;
  ST: TStringList;
begin
  FFileName:='';
  FCodeGen.IgnoreMissingUnitName:=true;
  IncArr:=GetOptionValues('i','include');
  for S in IncArr do
    FCodeGen.IncludeFileFolders.Add(S);

  FCodeGen.CopyrightInfoFile:=GetOptionValue('c','copyrightinfo');
  FOutDir:=GetOptionValue('o','out');
  FResultFileNamePrefix:=GetOptionValue('p', 'prefix');
  FResultFileNameLC:=HasOption('l','lower');



  ST:=TStringList.Create;
  GetNonOptions('h:o:i:c:p:l', ['help','out', 'include', 'copyrightinfo', 'prefix', 'lower'], ST);
  if (ST.Count>0) then
  begin
    FFileName:=ST[0];
    if FOutDir = '' then
      FOutDir:=ExtractFileDir(FFileName);
  end;
  ST.Free;

{
  -i /home/install/source/diadocsdk-cpp/proto/
  -i /home/install/source/diadocsdk-cpp/proto/Departments/
  -o /usr/local/share/lazarus/components/diadocsdk-fpc/Departments/
  -p diadoc_
  /home/install/source/diadocsdk-cpp/proto/Departments/DepartmentList.proto
}

  WriteLn('Compiling '+FFileName);

  for S in FCodeGen.IncludeFileFolders do
    WriteLn(Format(sIncludeFolder, [S]));

  if FOutDir<>'' then
    writeln(Format(sOutputDirectory, [FOutDir]));

end;

procedure TProtoToPasApplication.ParserStatus(Sender: TProtoParser;
  ALine: integer; AObject: TProtoObject; AMessage: string);
var
  S1: String;
begin
  if Assigned(AObject) then
    S1:=AObject.ClassName
  else
    S1:='';
  WriteLn(Format('(%d) %s : %s', [ALine, S1, AMessage]));
end;

procedure TProtoToPasApplication.CodeGenStatus(Sender: TPascalCodeGenerator;
  AObject: TProtoObject; AMessage: string);
var
  S1: String;
begin
  if Assigned(AObject) then
    S1:=AObject.ClassName
  else
    S1:='';
  WriteLn(Format('%s %s', [S1, AMessage]));
end;

procedure TProtoToPasApplication.DoRun;
var
  ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('hoicpl', ['help','out', 'include', 'copyrightinfo', 'prefix', 'lower']);
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  InitParser;

  // parse parameters
  if HasOption('h', 'help') {or (ParamCount = 0) }or (FFileName = '') then
  begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  DoCompileFromFile(FFileName);

  Terminate;
end;

procedure TProtoToPasApplication.DoCompileFromFile(AFileName: string);
var
  S, S1: String;
begin
  PParser.Compile(AFileName);
  if PParser.State <> cmsError then
  begin
    S1:=ExtractFileNameOnly(AFileName);
    S1:=StringReplace(S1, '-', '_', [rfReplaceAll]);
    FCodeGen.PasUnitName:=S1;
    S1:=ChangeFileExt(FResultFileNamePrefix + S1, '.pas');
    if FResultFileNameLC then
      S1:=LowerCase(S1);
    ParserOptions.ResultFileNamePrefix:=FResultFileNamePrefix;
    ParserOptions.ResultFileNameLowerCase:=FResultFileNameLC;

    S:=FCodeGen.GeneratePascalCode;

    SaveResultFile(S1, S);
  end
  else
    Writeln('Error: '+ PParser.ErrorMessage);
end;

procedure TProtoToPasApplication.SaveResultFile(AFileName, AText: string);
var
  F: TFileStream;
begin
  //AFileName:=ChangeFileExt(AFileName, '.pas');
  if FOutDir <> '' then
  begin
    ForceDirectory(FOutDir);
    AFileName:=AppendPathDelim(FOutDir) + AFileName;
  end;
  F:=TFileStream.Create(AFileName, fmCreate);
  if AText<>'' then
    F.Write(AText[1], Length(AText));
  F.Free;
end;

constructor TProtoToPasApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  FCodeGen:=TPascalCodeGenerator.Create(PParser);
  PParser.OnStatus:=@ParserStatus;
  ParserOptions:=TParserOptions.Create;
end;

destructor TProtoToPasApplication.Destroy;
begin
  FreeAndNil(FCodeGen);
  FreeAndNil(ParserOptions);
  inherited Destroy;
end;

procedure TProtoToPasApplication.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h -o -i --help --out --include <proto_file.proto>');
  writeln('Main options:');
  writeln('-o, --out'#9'out folder');
  writeln('-i, --include'#9'include files folder');
  writeln('Codegeneration options:');
  writeln('-c, --copyrightinfo'#9'copyright info for file header');
  writeln('-p, --prefix'#9'add prefix for result filename (and files in uses sections');
  writeln('-l, --lower'#9'generate file name in lower case');

  writeln('Other options:');
  writeln('-h, --help'#9'show help');
end;

procedure TProtoToPasApplication.WriteLogo;
begin
  writeln('Protobuf files to pascal source code compiller version 2.0 [' + {$I %DATE%}+'] for ' + {$I %FPCTARGETCPU%});
  writeln('Copyright (c) 2018-2024 by Lagunov Aleksey');
end;

var
  Application: TProtoToPasApplication;

{$R *.res}

begin
  Application:=TProtoToPasApplication.Create(nil);
  Application.Title:='Proto compiler';
  Application.WriteLogo;
  Application.Run;
  Application.Free;
end.

