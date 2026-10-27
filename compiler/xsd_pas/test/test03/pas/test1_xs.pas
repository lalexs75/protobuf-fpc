unit test1_xs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, xmlobject, AbstractSerializationObjects;

type

  {  Forward declarations  }
  TPerson = class;

  {  Generic classes for collections  }
  TPersonList = specialize GXMLSerializationObjectList<TPerson>;

  {  TPerson  }
  TPerson = class(TXmlSerializationObject)
  private
    FName:String;
    FAge:Int64;
    procedure SetName( AValue:String);
    procedure SetAge( AValue:Int64);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
    function RootNodeName:string; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    property Name:String read FName write SetName;
    property Age:Int64 read FAge write SetAge;
  end;

implementation

  {  TPerson  }
procedure TPerson.SetName(AValue: String);
begin
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
  P:=RegisterProperty('Name', 'Name', [xsaSimpleObject], '', -1, -1);
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