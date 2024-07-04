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

unit ppMainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, EditBtn, Buttons, ActnList, SynEdit, SynHighlighterPas,
  SynHighlighterCpp, SynHighlighterAny, RxIniPropStorage, ProtoParser,
  PasCodegen;

type

  { TProtoParserForm }

  TProtoParserForm = class(TForm)
    actCompile: TAction;
    BitBtn3: TBitBtn;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    DirectoryEdit1: TDirectoryEdit;
    edtFileNamePrefix: TEdit;
    FileNameEdit2: TFileNameEdit;
    ifAdd: TAction;
    ifRemove: TAction;
    ActionList1: TActionList;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    FileNameEdit1: TFileNameEdit;
    ImageList1: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    ListBox1: TListBox;
    Memo1: TMemo;
    PageControl1: TPageControl;
    Panel1: TPanel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RxIniPropStorage1: TRxIniPropStorage;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    SynAnySyn1: TSynAnySyn;
    ResultCode: TSynEdit;
    SourceEditor: TSynEdit;
    SynFreePascalSyn1: TSynFreePascalSyn;
    TabSheet8: TTabSheet;
    TabSheet9: TTabSheet;
    procedure actCompileExecute(Sender: TObject);
    procedure CheckBox2Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ifAddExecute(Sender: TObject);
    procedure ifRemoveExecute(Sender: TObject);
    procedure RadioButton1Change(Sender: TObject);
    procedure RxIniPropStorage1RestoreProperties(Sender: TObject);
  private
    procedure DoCompile(AMemo: TSynEdit);
    procedure DoCompileFromFile(AFileName:string);
    procedure ParserStatus(Sender:TProtoParser; ALine:integer; AObject:TProtoObject; AMessage:string);
    procedure CodeGenStatus(Sender:TPascalCodeGenerator; AObject:TProtoObject; AMessage:string);
    function CodeGen(AParser: TProtoParser): string;
    procedure SaveResultFile(AFileName, AText:string);

    procedure UpdateListBox1Ctrls;
  public
    procedure WriteLog(S:string);
    procedure WriteLog(S:string; AParams:array of const);
  end;

var
  ProtoParserForm: TProtoParserForm;

implementation
uses LazFileUtils;

{$R *.lfm}

{ TProtoParserForm }

procedure TProtoParserForm.actCompileExecute(Sender: TObject);
begin
  if RadioButton1.Checked then
    DoCompileFromFile(FileNameEdit1.FileName)
  else
    DoCompile(SourceEditor);
end;

procedure TProtoParserForm.CheckBox2Change(Sender: TObject);
begin
  //
end;

procedure TProtoParserForm.FormCreate(Sender: TObject);
begin
  PageControl1.ActivePageIndex:=0;
  UpdateListBox1Ctrls;
  RadioButton1Change(nil);
end;

procedure TProtoParserForm.ifAddExecute(Sender: TObject);
begin
  if SelectDirectoryDialog1.Execute then
    ListBox1.Items.Add(SelectDirectoryDialog1.FileName);
  UpdateListBox1Ctrls;
end;

procedure TProtoParserForm.ifRemoveExecute(Sender: TObject);
begin
  if ListBox1.Items.Count>0 then
    ListBox1.Items.Delete(ListBox1.ItemIndex);
  UpdateListBox1Ctrls;
end;

procedure TProtoParserForm.RadioButton1Change(Sender: TObject);
begin
  FileNameEdit1.Enabled:=RadioButton1.Checked;
  SourceEditor.Enabled:=RadioButton2.Checked;
end;

procedure TProtoParserForm.RxIniPropStorage1RestoreProperties(Sender: TObject);
begin
  UpdateListBox1Ctrls;
end;

procedure TProtoParserForm.DoCompile(AMemo: TSynEdit);
begin
  Memo1.Lines.Clear;
  ResultCode.Lines.Clear;

  PParser.OnStatus:=@ParserStatus;
  PParser.CompileStr(AMemo.Lines.Text);
  if PParser.State <> cmsError then
    ResultCode.Lines.Text:=CodeGen(PParser)
  else
  begin
    WriteLog(PParser.ErrorMessage);
    SourceEditor.Enabled:=true;
    SourceEditor.SetFocus;
    SourceEditor.CaretXY:=PParser.ErrorPosition;
  end;
end;

procedure TProtoParserForm.DoCompileFromFile(AFileName: string);
begin
  Memo1.Lines.Clear;
  ResultCode.Lines.Clear;

  SourceEditor.Lines.LoadFromFile(AFileName);
  PParser.OnStatus:=@ParserStatus;
  PParser.Compile(AFileName);
  if PParser.State <> cmsError then
  begin
    ResultCode.Lines.Text:=CodeGen(PParser);
    if CheckBox1.Checked then
      SaveResultFile(ExtractFileNameOnly(AFileName), ResultCode.Lines.Text);
  end
  else
  begin
    WriteLog('Error: '+ PParser.ErrorMessage);
    SourceEditor.Enabled:=true;
    SourceEditor.SetFocus;
    SourceEditor.CaretXY:=PParser.ErrorPosition;
  end;
end;

procedure TProtoParserForm.ParserStatus(Sender: TProtoParser; ALine: integer;
  AObject: TProtoObject; AMessage: string);
var
  S1: String;
begin
  if Assigned(AObject) then
    S1:=AObject.ClassName
  else
    S1:='';
  WriteLog('(%d) %s : %s', [ALine, S1, AMessage]);
end;

procedure TProtoParserForm.CodeGenStatus(Sender: TPascalCodeGenerator;
  AObject: TProtoObject; AMessage: string);
var
  S1: String;
begin
  if Assigned(AObject) then
    S1:=AObject.ClassName
  else
    S1:='';
  WriteLog('%s %s', [S1, AMessage]);
end;

function TProtoParserForm.CodeGen(AParser: TProtoParser): string;
var
  CG: TPascalCodeGenerator;
begin
  CG:=TPascalCodeGenerator.Create(AParser);
  CG.PasUnitName:=ExtractFileNameOnly(FileNameEdit1.FileName);
  CG.IgnoreMissingUnitName:=true;
  CG.IncludeFileFolders.Assign(ListBox1.Items);
  CG.OnStatus:=@CodeGenStatus;
  CG.ResultFileNamePrfix:=edtFileNamePrefix.Text;
  CG.ResultFileNameLowerCase:=CheckBox3.Checked;
  try
    Result:=CG.GeneratePascalCode;
  finally
    CG.Free;
  end;
end;

procedure TProtoParserForm.SaveResultFile(AFileName, AText: string);
var
  F: TFileStream;
begin
  AFileName:=ChangeFileExt(AFileName, '.pas');
  if DirectoryEdit1.Directory <> '' then
    AFileName:=AppendPathDelim(DirectoryEdit1.Directory) + AFileName;
  F:=TFileStream.Create(AFileName, fmCreate);
  if AText<>'' then
    F.Write(AText[1], Length(AText));
  F.Free;
end;

procedure TProtoParserForm.UpdateListBox1Ctrls;
begin
  ifRemove.Enabled:=ListBox1.Items.Count>0;
end;

procedure TProtoParserForm.WriteLog(S: string);
begin
  Memo1.Lines.Add(S);
  Memo1.CaretPos:=Point(0, Memo1.Lines.Count-1);
end;

procedure TProtoParserForm.WriteLog(S: string; AParams: array of const);
begin
  WriteLog(Format(S, AParams));
end;

end.

