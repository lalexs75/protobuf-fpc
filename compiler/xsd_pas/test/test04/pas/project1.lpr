program project1;

uses test_crash_whitespace;

var
  P: TPerson;
begin
  P:=TPerson.Create;
  P.LoadFromFile('/usr/local/share/lazarus/components/protobuf-fpc/compiler/xsd_pas/test/test04/test_crash_whitespace.xml');
  WriteLn('P.FirstName = ', P.FirstName);
  WriteLn('P.LastName = ', P.LastName);
  P.Free;
end.

