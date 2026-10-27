unit test5_main_mixed;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, xmlobject, AbstractSerializationObjects, common_types;

type

  {  Forward declarations  }
  TPerson = class;

  {  Generic classes for collections  }
  TPersonList = specialize GXMLSerializationObjectList<TPerson>;

  {  TPerson  }
  TPerson = class(TXmlSerializationObject)
  private
    FName:TFullNameType;
    FAge:Int64;
    procedure SetName( AValue:TFullNameType);
    procedure SetAge( AValue:Int64);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
    function RootNodeName:string; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    property Name:TFullNameType read FName write SetName;
    property Age:Int64 read FAge write SetAge;
  end;

implementation

  {  TPerson  }
procedure TPerson.SetName(AValue: TFullNameType);
begin
  CheckStrMaxSize('Name', AValue);
  FName:=AValue;
  ModifiedProperty('Name');
end;

procedure TPerson.SetAge(AValue: Int64);
begin
  FAge:=AValue;
  ModifiedProperty('Age');
end;

procedure TPerson.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('Name', 'Name', [xsaSimpleObject], '', -1, 100);
  P:=RegisterProperty('Age', 'Age', [xsaSimpleObject], '', -1, -1);
end;

procedure TPerson.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TPerson.Destroy;
begin
  inherited Destroy;
end;

function TPerson.RootNodeName:string;
begin
  Result:='Person';
end;

constructor TPerson.Create;
begin
  inherited Create;
end;

end.