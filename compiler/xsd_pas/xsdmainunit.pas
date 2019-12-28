{ XSD files compiler to FPC class

  Copyright (C) 2019 Lagunov Aleksey alexs@yandex.ru

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, DOM, EditBtn,
  ComCtrls, XsdElementTypesUnit, SynEdit, SynHighlighterPas, RxIniPropStorage,
  XsdProcessorUnit;

type

  { TXSDMainForm }

  TXSDMainForm = class(TForm)
    Button1: TButton;
    FileNameEdit1: TFileNameEdit;
    Label1: TLabel;
    Memo1: TMemo;
    PageControl1: TPageControl;
    RxIniPropStorage1: TRxIniPropStorage;
    SynEdit1: TSynEdit;
    SynFreePascalSyn1: TSynFreePascalSyn;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure Button1Click(Sender: TObject);
  private

    procedure ProcessNode(Sender:TXSDProcessor; ANodeName:string; AMessage:string);
  public
    //procedure WriteLog(S:string);
  end;

var
  XSDMainForm: TXSDMainForm;

implementation
uses rxlogging, XMLRead, xmliconv, XsdPasCodegenUnit, LazFileUtils;

{$R *.lfm}

{ TXSDMainForm }

procedure TXSDMainForm.Button1Click(Sender: TObject);
var
  FProcessor: TXSDProcessor;
  FCodegen: TXsdPasCodegen;
  FXSDModule: TXSDModule;
begin
  SynEdit1.Lines.Clear;
  FProcessor:=TXSDProcessor.Create;
  FProcessor.OnProcessNodeEvent:=@ProcessNode;
  FProcessor.LoadFromFile(FileNameEdit1.FileName);
  FXSDModule:=FProcessor.ExecuteProcessor;

  if Assigned(FXSDModule) then
  begin
    FCodegen:=TXsdPasCodegen.Create(FXSDModule);
    FCodegen.PasUnitName:=ExtractFileNameOnly(FileNameEdit1.FileName);
    SynEdit1.Lines.Text:=FCodegen.GeneratePasCode;
    FCodegen.Free;
  end
  else
    ShowMessage('Error on parse XDS module');
  FXSDModule.Free;
  FProcessor.Free;
end;



procedure TXSDMainForm.ProcessNode(Sender: TXSDProcessor; ANodeName: string;
  AMessage: string);
begin
  Memo1.Lines.Add(ANodeName + ' : '+AMessage);
  RxWriteLog(etDebug, ANodeName + ' : '+AMessage);
end;

end.

