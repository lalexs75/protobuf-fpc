{ XSD files compiler to FPC class

  Copyright (C) 2019-2020 Lagunov Aleksey alexs@yandex.ru

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

unit XSDMainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, EditBtn,
  ComCtrls, ExtCtrls, Buttons, Menus, ActnList, XsdElementTypesUnit, SynEdit,
  SynHighlighterPas, SynHighlighterXML, RxIniPropStorage, XsdProcessorUnit;

type

  { TXSDMainForm }

  TXSDMainForm = class(TForm)
    incfAdd: TAction;
    incfDel: TAction;
    ActionList1: TActionList;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Button1: TButton;
    Button2: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    FileNameEdit1: TFileNameEdit;
    FileNameEdit2: TFileNameEdit;
    FileNameEdit3: TFileNameEdit;
    ImageList1: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    ListBox1: TListBox;
    Memo1: TMemo;
    PageControl1: TPageControl;
    Panel1: TPanel;
    RxIniPropStorage1: TRxIniPropStorage;
    SynEdit1: TSynEdit;
    SynEdit2: TSynEdit;
    SynFreePascalSyn1: TSynFreePascalSyn;
    SynXMLSyn1: TSynXMLSyn;
    tabLogs: TTabSheet;
    tabEditor: TTabSheet;
    tabSource: TTabSheet;
    TabSheet3: TTabSheet;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FileNameEdit1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

    procedure ProcessNode(Sender:TXSDProcessor; ANodeName:string; AMessage:string);
    procedure Localize;
  public
    //
  end;

var
  XSDMainForm: TXSDMainForm;

implementation
uses rxlogging, xmliconv, XsdPasCodegenUnit, xsd_gui_consts_unit, LazFileUtils, LazUTF8, rxAppUtils;

{$R *.lfm}

{ TXSDMainForm }

procedure TXSDMainForm.Button1Click(Sender: TObject);
var
  FProcessor: TXSDProcessor;
  FCodegen: TXsdPasCodegen;
  FXSDModule: TXSDModule;
  FDO : TCodeGenDescribeOptions;
begin
  if not FileExistsUTF8(FileNameEdit1.FileName) then
  begin
    ErrorBox(sFileNotFound, [FileNameEdit1.FileName]);
    Exit;
  end;
  SynEdit1.Lines.Clear;
  FProcessor:=TXSDProcessor.Create;

  FDO:=[];
  if CheckBox1.Checked then
    FDO:=FDO + [cgdoDescribeClasses];
  if CheckBox2.Checked then
    FDO:=FDO + [cgdoDescribeClassProperty];
  if CheckBox3.Checked then
    FDO:=FDO + [cgdoDescribeTypes];

  FProcessor.OnProcessNodeEvent:=@ProcessNode;
  FProcessor.IncludeFolders.Assign(ListBox1.Items);

  FProcessor.LoadFromFile(FileNameEdit1.FileName);

  if ExtractFileDir(FileNameEdit1.FileName) <>'' then
    FProcessor.IncludeFolders.Append(ExtractFileDir(FileNameEdit1.FileName));

  FXSDModule:=FProcessor.ExecuteProcessor;

  if Assigned(FXSDModule) then
  begin
    FCodegen:=TXsdPasCodegen.Create(FXSDModule);
    FCodegen.PasUnitName:=ExtractFileNameOnly(FileNameEdit1.FileName);
    FCodegen.DescribeOptions:=FDO;
    if CheckBox4.Checked then
      FCodegen.LicenseHeader:=FileNameEdit3.FileName;

    SynEdit1.Lines.Text:=FCodegen.GeneratePasCode;
    FCodegen.Free;
  end
  else
    ShowMessage(sErrorOnParseXDS);
  FXSDModule.Free;
  FProcessor.Free;
end;

procedure TXSDMainForm.Button2Click(Sender: TObject);
begin
  SynEdit1.Lines.SaveToFile(FileNameEdit2.FileName);
end;

procedure TXSDMainForm.FileNameEdit1Change(Sender: TObject);
begin
  FileNameEdit2.FileName:=ExtractFileNameWithoutExt(FileNameEdit1.FileName)+'.pas';
  if FileExistsUTF8(FileNameEdit1.FileName) then
    SynEdit2.Lines.LoadFromFile(FileNameEdit1.FileName);
end;

procedure TXSDMainForm.FormCreate(Sender: TObject);
begin
  Localize;
  PageControl1.ActivePage:=tabEditor;
end;

procedure TXSDMainForm.ProcessNode(Sender: TXSDProcessor; ANodeName: string;
  AMessage: string);
begin
  Memo1.Lines.Add(ANodeName + ' : '+AMessage);
  RxWriteLog(etDebug, ANodeName + ' : '+AMessage);
end;

procedure TXSDMainForm.Localize;
begin
  Label1.Caption:=sXSDFile;
  Button1.Caption:=sConvert;
end;

end.

