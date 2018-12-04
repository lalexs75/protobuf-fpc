{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit protobuf;

{$warn 5023 off : no warning about unused units}
interface

uses
  protobuf_fpc, protobuf_fpc_types, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('protobuf', @Register);
end.
