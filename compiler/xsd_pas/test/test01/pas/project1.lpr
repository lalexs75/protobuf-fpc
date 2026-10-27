program Project1;
uses
  TestCodeGenErrors;
var
  Person:TPerson;
begin
  Person:=TPerson.Create;
  Person.LoadFromFile('/usr/local/share/lazarus/components/protobuf-fpc/compiler/xsd_pas/test/test01/TestCodeGenErrors.xml');
  WriteLn('Person.Name = ', Person.Name);
  WriteLn('Person.Address.Street = ', Person.Address.Street);
  WriteLn('Person.Address.City = ', Person.Address.City);
  WriteLn('Person.ContactInfo.Phone = ', Person.ContactInfo.Phone);
  Person.SaveToFile('TestCodeGenErrors_new.xml');
  Person.Free;
end.

