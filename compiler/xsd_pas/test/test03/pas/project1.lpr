program project1;

uses test1_xs, test2_xsd, test3_default, test4_custom, test5_main_mixed;

var
  T1: test1_xs.TPerson;
begin
  T1:=test1_xs.TPerson.Create;
  T1.Free;

  T2:=test2_xsd.TPerson.Create;
  T2.Free;

  T3:=test3_default.TPerson.Create;
  T3.Free;

  T4:=test4_custom.TPerson.Create;
  T4.Free;

  T5:=test5_main_mixed.TPerson.Create;
  T5.Free;
end.

