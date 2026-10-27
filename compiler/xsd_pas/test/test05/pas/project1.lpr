program project1;

uses test_crash_array;

var
  P: TOrder;
  S: String;
begin
  P:=TOrder.Create;
  P.LoadFromFile('/usr/local/share/lazarus/components/protobuf-fpc/compiler/xsd_pas/test/test05/test_crash_array.xml');
  WriteLn('P.Id = ', P.Id);
  WriteLn('P.Item : ');
  for S in P.Item do
    WriteLn(S);
  P.Free;
end.

