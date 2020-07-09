{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit xmlobjects;

{$warn 5023 off : no warning about unused units}
interface

uses
  xmlobject, xmlobject_resource, AbstractSerializationObjects, JSONObjects, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('xmlobjects', @Register);
end.
