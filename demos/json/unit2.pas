unit Unit2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JSONObjects;

type

  { TChildJSONObj }

  TChildJSONObj = class(TJSONSerializationObject)
  private
    FCode: Integer;
    FName: string;
    procedure SetCode(AValue: Integer);
    procedure SetName(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
  public
  published
    property Code:Integer read FCode write SetCode;
    property Name:string read FName write SetName;
  end;

implementation

{ TChildJSONObj }

procedure TChildJSONObj.SetCode(AValue: Integer);
begin
  if FCode=AValue then Exit;
  FCode:=AValue;
  ModifiedProperty('Code');
end;

procedure TChildJSONObj.SetName(AValue: string);
begin
  if FName=AValue then Exit;
  FName:=AValue;
  ModifiedProperty('Name');
end;

procedure TChildJSONObj.InternalRegisterPropertys;
begin
  inherited InternalRegisterPropertys;
  RegisterProperty('Code', 'Code', [], '', -1, -1);
  RegisterProperty('Name', 'Name', [], '', -1, -1);
end;

end.

