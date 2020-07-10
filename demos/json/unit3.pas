unit Unit3;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JSONObjects, AbstractSerializationObjects;

type

  { TEmployee }

  TEmployee = class(TJSONSerializationObject)
  private
    FFirstName: string;
    FLastName: string;
    procedure SetFirstName(AValue: string);
    procedure SetLastName(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property FirstName:string read FFirstName write SetFirstName;
    property LastName:string read FLastName write SetLastName;
  end;
  TEmployeeList = specialize GXMLSerializationObjectList<TEmployee>;


  { TEmployees }

  TEmployees = class(TJSONSerializationObject)
  private
    FEmployees: TEmployeeList;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property Employees:TEmployeeList read FEmployees;
  end;

implementation

{ TEmployee }

procedure TEmployee.SetFirstName(AValue: string);
begin
  if FFirstName=AValue then Exit;
  FFirstName:=AValue;
  ModifiedProperty('FirstName');
end;

procedure TEmployee.SetLastName(AValue: string);
begin
  if FLastName=AValue then Exit;
  FLastName:=AValue;
  ModifiedProperty('LastName');
end;

procedure TEmployee.InternalRegisterPropertys;
begin
  inherited InternalRegisterPropertys;
  RegisterProperty('FirstName', 'firstName', [], '', -1, -1);
  RegisterProperty('LastName', 'lastName', [], '', -1, -1);
end;

procedure TEmployee.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TEmployee.Destroy;
begin
  inherited Destroy;
end;

{ TEmployees }

procedure TEmployees.InternalRegisterPropertys;
begin
  inherited InternalRegisterPropertys;
  RegisterProperty('Employees', 'employees', [], '', -1, -1);
end;

procedure TEmployees.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FEmployees:=TEmployeeList.Create;
end;

destructor TEmployees.Destroy;
begin
  FreeAndNil(FEmployees);
  inherited Destroy;
end;

end.

