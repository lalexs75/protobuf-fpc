unit DIADocTypesUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, protobuf_fpc;

type
  //message RussianAddress {
  //	optional string ZipCode = 1;	// индекс
  //	required string Region = 2;		// регион (код)
  //	optional string Territory = 3;	// район
  //	optional string City = 4;		// город
  //	optional string Locality = 5;	// населенный пункт
  //	optional string Street = 6;		// улица
  //	optional string Building = 7;	// дом
  //	optional string Block = 8;		// корпус
  //	optional string Apartment = 9;	// квартира
  //}

  { TRussianAddress }

  TRussianAddress = class(TSerializationObject)
  private
    FApartment: string;
    FBlock: string;
    FBuilding: string;
    FCity: string;
    FLocality: string;
    FRegion: string;
    FStreet: string;
    FTerritory: string;
    FZipCode: string;
  protected
    procedure InternalRegisterProperty; override;
  public
  published
    property ZipCode:string read FZipCode write FZipCode;       // = 1; // индекс
    property Region:string read FRegion write FRegion;          // = 2; // регион (код)
    property Territory:string read FTerritory write FTerritory; // = 3; // район
    property City:string read FCity write FCity;                // = 4; // город
    property Locality:string read FLocality write FLocality;    // = 5; // населенный пункт
    property Street:string read FStreet write FStreet;          // = 6; // улица
    property Building:string read FBuilding write FBuilding;    // = 7; // дом
    property Block:string read FBlock write FBlock;             // = 8; // корпус
    property Apartment:string read FApartment write FApartment; // = 9; // квартира
  end;

implementation

{ TRussianAddress }

procedure TRussianAddress.InternalRegisterProperty;
begin
  RegisterProp('ZipCode', 1);    // индекс
  RegisterProp('Region', 2, true);     // регион (код)
  RegisterProp('Territory', 3);  // район
  RegisterProp('City', 4);       // город
  RegisterProp('Locality', 5);   // населенный пункт
  RegisterProp('Street', 6);     // улица
  RegisterProp('Building', 7);   // дом
  RegisterProp('Block', 8);      // корпус
  RegisterProp('Apartment', 9);  // квартира
end;

end.

