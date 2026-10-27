program project1;

uses MainWithRef;
var
  Document:TDocument;
begin
  Document:=TDocument.Create;
  Document.LoadFromFile('/usr/local/share/lazarus/components/protobuf-fpc/compiler/xsd_pas/test/test02/MainWithRef.xml');
  WriteLn('Document.Title = ',Document.Title);
  WriteLn('Document.GlobalID = ', Document.GlobalID);
  Document.SaveToFile('MainWithRef_new.xml');
  Document.Free;
end.

