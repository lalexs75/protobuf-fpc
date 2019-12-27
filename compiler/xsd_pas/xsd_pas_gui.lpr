program xsd_pas_gui;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, rxnew, XSDMainUnit, XsdElementTypesUnit, XsdPasCodegenUnit
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TXSDMainForm, XSDMainForm);
  Application.Run;
end.

