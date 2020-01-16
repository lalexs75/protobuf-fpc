program XsdToPas;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  SysUtils,
  CustApp,
  {$IFNDEF WINDOWS}
  xmliconv,
  {$ELSE}
  xmliconv_windows,
  {$ENDIF}

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
  IncArr: TStringArray;
  S: String;
begin
  FFileName:='';

  //FProcessor.IgnoreMissingUnitName:=true;
  IncArr:=GetOptionValues('i','include');
  for S in IncArr do
    FProcessor.IncludeFolders.Add(S);

  FCodeGen.LicenseHeader:=GetOptionValue('c','copyrightinfo');
  FOutDir:=GetOptionValue('o','out');

  ST:=TStringList.Create;
  GetNonOptions('h:o:c:tsp' , ['help','out'{, 'include'}, 'copyrightinfo', 'describe_types', 'describe_classes', 'describe_class_prop'], ST);
  if (ST.Count>0) then
  begin
    FFileName:=ST[0];
    if FOutDir = '' then
      FOutDir:=ExtractFileDir(FFileName);
  end;
  ST.Free;

  WriteLn('Compiling '+FFileName);

  FCodeGenDescribeOptions:=[];

  if HasOption('t', 'describe_types') then
    FCodeGenDescribeOptions:=FCodeGenDescribeOptions + [cgdoDescribeTypes];
  if HasOption('s', 'describe_classes') then
    FCodeGenDescribeOptions:=FCodeGenDescribeOptions + [cgdoDescribeClasses];
  if HasOption('p', 'describe_class_prop') then
    FCodeGenDescribeOptions:=FCodeGenDescribeOptions + [cgdoDescribeClassProperty];

  if ExtractFileDir(FFileName)<>'' then
    FProcessor.IncludeFolders.Add(ExtractFileDir(FFileName));
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
  ErrorMsg:=CheckOptions('hoctspi', ['help', 'out', 'copyrightinfo', 'describe_types', 'describe_classes', 'describe_class_prop', 'include']);
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
  writeln('Usage: ', ExeName, ' -h -o -i -s -p -t --help --out --describe_types --describe_classes --describe_class_prop --include <proto_file.proto>');
  writeln('Main options:');
  writeln('-o, --out'#9'out folder');
  writeln('-t, --describe_types'#9'Describe types');
  writeln('-s, --describe_classes'#9'Describe classes');
  writeln('-p, --describe_class_prop'#9'Describe class propertys');
  writeln('-i, --include'#9'Describe class propertys');
  writeln('Codegeneration options:');
  writeln('-c, --copyrightinfo'#9'copyright info for file header');
  writeln('Other options:');
  writeln('-h, --help'#9'show help');
end;

var
  Application: TXsdToPasCompiler;

{$R *.res}

begin
  Application:=TXsdToPasCompiler.Create(nil);
  Application.Title:='XSD to PAS';
  Application.Run;
  Application.Free;
end.

