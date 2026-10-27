program project1;

uses test1_xs, test2_xsd, test3_default, test4_custom, test5_main_mixed;

var
  T1: test1_xs.TPerson;
  T2: test2_xsd.TPerson;
  T3: test3_default.TPerson;
  T4: test4_custom.TPerson;
  T5: test5_main_mixed.TPerson;
begin
  T1:=test1_xs.TPerson.Create;
  T1.LoadFromFile('/usr/local/share/lazarus/components/protobuf-fpc/compiler/xsd_pas/test/test03/test_person.xml');
  WriteLn('T1.Name = ', T1.Name);
  WriteLn('T1.Age = ', T1.Age);
  T1.Free;




  T2:=test2_xsd.TPerson.Create;
  T2.LoadFromFile('/usr/local/share/lazarus/components/protobuf-fpc/compiler/xsd_pas/test/test03/test_person.xml');
  WriteLn('T2.Name = ', T2.Name);
  WriteLn('T2.Age = ', T2.Age);
  T2.Free;

  T3:=test3_default.TPerson.Create;
  T3.LoadFromFile('/usr/local/share/lazarus/components/protobuf-fpc/compiler/xsd_pas/test/test03/test_person.xml');
  WriteLn('T3.Name = ', T3.Name);
  WriteLn('T3.Age = ', T3.Age);
  T3.Free;

  T4:=test4_custom.TPerson.Create;
  T4.LoadFromFile('/usr/local/share/lazarus/components/protobuf-fpc/compiler/xsd_pas/test/test03/test_person.xml');
  WriteLn('T4.Name = ', T4.Name);
  WriteLn('T4.Age = ', T4.Age);
  T4.Free;

  T5:=test5_main_mixed.TPerson.Create;
  T5.LoadFromFile('/usr/local/share/lazarus/components/protobuf-fpc/compiler/xsd_pas/test/test03/test_person.xml');
  WriteLn('T5.Name = ', T5.Name);
  WriteLn('T5.Age = ', T5.Age);
  T5.Free;
end.

