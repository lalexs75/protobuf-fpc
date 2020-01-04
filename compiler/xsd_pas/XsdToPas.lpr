program XsdToPas;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  SysUtils,
  CustApp,
  LazFileUtils,
  XsdElementTypesUnit,
  XsdProcessorUnit,
  XsdPasCodegenUnit;

type

  { TXsdToPasCompiler }

  TXsdToPasCompiler = class(TCustomApplication)
  private
    FOutDir: String;
    FFileName: String;

    FProcessor: TXSDProcessor;
    FCodegen: TXsdPasCodegen;
    FCodeGenDescribeOptions : TCodeGenDescribeOptions;
    procedure InitParser;
    procedure ProcessNode(Sender:TXSDProcessor; ANodeName:string; AMessage:string);
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TXsdToPasCompiler }

procedure TXsdToPasCompiler.InitParser;
var
  ST: TStringList;
begin
  FFileName:='';
(*
  FCodeGen.IgnoreMissingUnitName:=true;
  IncArr:=GetOptionValues('i','include');
  for S in IncArr do
    FCodeGen.IncludeFileFolders.Add(S);

  FCodeGen.CopyrightInfoFile:=GetOptionValue('c','copyrightinfo');
*)
  FOutDir:=GetOptionValue('o','out');


  ST:=TStringList.Create;
  GetNonOptions('h:o:' {i:c:'}, ['help','out'{, 'include', 'copyrightinfo'}], ST);
  if (ST.Count>0) then
  begin
    FFileName:=ST[0];
    if FOutDir = '' then
      FOutDir:=ExtractFileDir(FFileName);
  end;
  ST.Free;

  WriteLn('Compiling '+FFileName);
  (*
  for S in FCodeGen.IncludeFileFolders do
    WriteLn(Format(sIncludeFolder, [S]));

  if FOutDir<>'' then
    writeln(Format(sOutputDirectory, [FOutDir]));
*)

  FCodeGenDescribeOptions:=[];
(*if CheckBox1.Checked then
  FDO:=FDO + [cgdoDescribeClasses];
if CheckBox2.Checked then
  FDO:=FDO + [cgdoDescribeClassProperty];
if CheckBox3.Checked then
  FDO:=FDO + [cgdoDescribeTypes];
*)
end;

procedure TXsdToPasCompiler.ProcessNode(Sender: TXSDProcessor;
  ANodeName: string; AMessage: string);
begin
  WriteLn(ANodeName + ' : '+AMessage);
end;

procedure TXsdToPasCompiler.DoRun;
var
  ErrorMsg, FPasName: String;
  FXSDModule: TXSDModule;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('ho' {'hoic'}, ['help','out'{, 'include', 'copyrightinfo'}]);
  if ErrorMsg<>'' then
  begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  InitParser;
  // parse parameters
  if HasOption('h', 'help') or (FFileName='') then
  begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  { add your program here }

  FProcessor.LoadFromFile(FFileName);
  FXSDModule:=FProcessor.ExecuteProcessor;
  if Assigned(FXSDModule) then
  begin
    FCodegen.XSDModule:=FXSDModule;
    FPasName:=ExtractFileNameOnly(FFileName);
    FCodegen.PasUnitName:=ExtractFileNameOnly(FFileName);
    FCodegen.PasUnitOutputFolder:=FOutDir;
    FCodegen.DescribeOptions:=FCodeGenDescribeOptions;
    FCodegen.GeneratePasCodeToFile('');
  end
  else
    WriteLn('Error on parse XDS module');

  // stop program loop
  Terminate;
end;

constructor TXsdToPasCompiler.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;

  FProcessor:=TXSDProcessor.Create;
  FProcessor.OnProcessNodeEvent:=@ProcessNode;
  FCodegen:=TXsdPasCodegen.Create(nil);
end;

destructor TXsdToPasCompiler.Destroy;
begin
  FProcessor.Free;
  FCodegen.Free;
  inherited Destroy;
end;

procedure TXsdToPasCompiler.WriteHelp;
begin
  //writeln('Usage: ', ExeName, ' -h -o -i --help --out --include <proto_file.proto>');
  writeln('Usage: ', ExeName, ' -h -o --help --out <proto_file.proto>');
  writeln('Main options:');
  writeln('-o, --out'#9'out folder');
//  writeln('-i, --include'#9'include files folder');
  writeln('Codegeneration options:');
//  writeln('-c, --copyrightinfo'#9'copyright info for file header');
  writeln('Other options:');
  writeln('-h, --help'#9'show help');
end;

var
  Application: TXsdToPasCompiler;
begin
  Application:=TXsdToPasCompiler.Create(nil);
  Application.Title:='XSD to PAS';
  Application.Run;
  Application.Free;
end.

