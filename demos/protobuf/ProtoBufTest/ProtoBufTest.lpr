program ProtoBufTest;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, ProtoBufTestCase,
  DIADocTypesUnit, CommonTestTypesUnit;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

