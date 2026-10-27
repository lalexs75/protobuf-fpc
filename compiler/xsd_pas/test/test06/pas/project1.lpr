program project1;

uses test_crash_namespace;

var
  Invoice: TInvoice;
begin
  Invoice:=TInvoice.Create;
  Invoice.LoadFromFile('/usr/local/share/lazarus/components/protobuf-fpc/compiler/xsd_pas/test/test06/test_crash_namespace.xml');
  WriteLn('Invoice.InvoiceNumber = ', Invoice.InvoiceNumber);
  WriteLn('Invoice.Amount = ', Invoice.Amount);
  Invoice.SaveToFile('test_crash_namespace_new.xml');
  Invoice.Free;


  Invoice := TInvoice.Create;
  try
    Invoice.DefaultPrefix := 't'; // Все элементы получат префикс 't'
    Invoice.InvoiceNumber := 'INV-001';
    Invoice.Amount := 1500.50;
    Invoice.SaveToFile('new_invoice_01.xml');
  finally
    Invoice.Free;
  end;
end.

