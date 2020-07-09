unit Unit2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JSONObjects, AbstractSerializationObjects;

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
  TChildJSONObjList = specialize GXMLSerializationObjectList<TChildJSONObj>;

  { TMainSONObj }

  TMainSONObj = class(TJSONSerializationObject)
  private
    FAdress: string;
    FChild: TChildJSONObj;
    FVersion: Integer;
    procedure SetAdress(AValue: string);
    procedure SetVersion(AValue: Integer);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property Version:Integer read FVersion write SetVersion;
    property Adress:string read FAdress write SetAdress;
    property Child:TChildJSONObj read FChild;
  end;

  { TMainTest }

  TMainTest = class(TJSONSerializationObject)
  private
    FAdress: string;
    FItems: TChildJSONObjList;
    FVersion: Integer;
    procedure SetAdress(AValue: string);
    procedure SetVersion(AValue: Integer);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property Version:Integer read FVersion write SetVersion;
    property Adress:string read FAdress write SetAdress;
    property Items:TChildJSONObjList read FItems;
  end;

implementation

{ TMainTest }

procedure TMainTest.SetAdress(AValue: string);
begin
  if FAdress=AValue then Exit;
  FAdress:=AValue;
  ModifiedProperty('Adress');
end;

procedure TMainTest.SetVersion(AValue: Integer);
begin
  if FVersion=AValue then Exit;
  FVersion:=AValue;
  ModifiedProperty('Version');
end;

procedure TMainTest.InternalRegisterPropertys;
begin
  inherited InternalRegisterPropertys;
  RegisterProperty('Version', 'Version', [], '', -1, -1);
  RegisterProperty('Adress', 'Adress', [], '', -1, -1);
  RegisterProperty('Items', 'Items', [], '', -1, -1);
end;

procedure TMainTest.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FItems:=TChildJSONObjList.Create;
end;

destructor TMainTest.Destroy;
begin
  FreeAndNil(FItems);
  inherited Destroy;
end;

{ TMainSONObj }

procedure TMainSONObj.SetAdress(AValue: string);
begin
  if FAdress=AValue then Exit;
  FAdress:=AValue;
  ModifiedProperty('Adress');
end;

procedure TMainSONObj.SetVersion(AValue: Integer);
begin
  if FVersion=AValue then Exit;
  FVersion:=AValue;
  ModifiedProperty('Version');
end;

procedure TMainSONObj.InternalRegisterPropertys;
begin
  inherited InternalRegisterPropertys;
  RegisterProperty('Version', 'Version', [], '', -1, -1);
  RegisterProperty('Adress', 'Adress', [], '', -1, -1);
  RegisterProperty('Child', 'Child', [], '', -1, -1);
end;

procedure TMainSONObj.InternalInitChilds;
begin
  FChild:=TChildJSONObj.Create;
end;

destructor TMainSONObj.Destroy;
begin
  FreeAndNil(FChild);
  inherited Destroy;
end;

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

