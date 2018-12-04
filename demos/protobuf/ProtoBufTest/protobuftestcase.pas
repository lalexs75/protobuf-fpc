unit ProtoBufTestCase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type

  { TProtobufTest }

  TProtobufTest= class(TTestCase)
  protected
    function GetTestStream(ATestName:string):TStream;
    function GetTestForWriteStream(ATestName:string):TStream;
  published
    procedure ZigZagTest;
    procedure SimpleCreate1;
    procedure Test1Read;
    procedure Test2Read;
    procedure Test3Read;
    procedure DiaDocAdressRead;
    procedure SaveCollentions;
    procedure LoadCollentions;
  end;

implementation

uses FileUtil, LazFileUtils, protobuf_fpc, CommonTestTypesUnit, DIADocTypesUnit;


type
  TPeopleData = record
    Code:integer;
    FirstName:string;
    LastName:string;
  end;

const
  PeopleDataArray : array [1..4] of TPeopleData =
    (
      (Code:1;
       FirstName:'Иван';
       LastName:'Иванов'),
      (Code:2;
       FirstName:'Пётр';
       LastName:'Петров'),
      (Code:3;
       FirstName:'Bill';
       LastName:'Gates'),
      (Code:4;
       FirstName:'John';
       LastName:'Smith')
    );

procedure TProtobufTest.SimpleCreate1;
var
  P: TPerson;
begin
  P:=TPerson.Create;
  AssertNotNull(P);
  P.Id:=-123;
  P.Name:='Пример';
  P.EMail:='test@test.dom';
  AssertEquals('Поле ID = -123', P.Id, -123);
  AssertEquals('Поле Name = ''Пример''', P.Name, 'Пример');
  AssertEquals('Поле EMail = ''test@test.dom''', P.EMail, 'test@test.dom');
  P.Free;
end;

procedure TProtobufTest.Test1Read;
var
  S: TStream;
  T: TTest1;
begin
  S:=GetTestStream('test1');
  if Assigned(S) then
  begin
    T:=TTest1.Create;
    T.LoadFromStream(S);
    AssertEquals('Поле ID = 150', T.A, 150);
    T.Free;
    S.Free;
  end;
end;

procedure TProtobufTest.Test2Read;
var
  T: TTest2;
  S: TStream;
begin
  S:=GetTestStream('test2');
  if Assigned(S) then
  begin
    T:=TTest2.Create;
    T.LoadFromStream(S);
    AssertEquals('Поле B = ''testing''', T.B, 'testing');
    T.Free;
    S.Free;
  end;
end;

procedure TProtobufTest.Test3Read;
var
  S: TStream;
  T: TTest3;
begin
  S:=GetTestStream('test3');
  if Assigned(S) then
  begin
    T:=TTest3.Create;
    T.LoadFromStream(S);
    AssertEquals('Поле C.A = 150', T.C.A, 150);
    T.Free;
    S.Free;
  end;
end;

procedure TProtobufTest.DiaDocAdressRead;
var
  S: TStream;
  T: TRussianAddress;
begin
  S:=GetTestStream('GetAdress');
  if Assigned(S) then
  begin
    T:=TRussianAddress.Create;
    T.LoadFromStream(S);
    AssertEquals('T.ZipCode = ', T.ZipCode, '620000');
    AssertEquals('T.Region = ', T.Region, '66');
    AssertEquals('T.Territory = ', T.Territory, '');
    AssertEquals('T.City = ', T.City, 'г. Екатеринбург');
    AssertEquals('T.Locality = ', T.Locality, '');
    AssertEquals('T.Street = ', T.Street, 'ул. Радищева');
    AssertEquals('T.Building = ', T.Building, '28');
    AssertEquals('T.Block = ', T.Block, '');
    AssertEquals('T.Apartment = ', T.Apartment, '');
    T.Free;
    S.Free;
  end;
end;

procedure TProtobufTest.SaveCollentions;
var
  D: TDepartment;
  P: TPeople;
  S: TStream;
  R: TPeopleData;
begin
  D:=TDepartment.Create;
  D.Code:=1;
  D.DepartmentName:='Department # 1';
  for R in PeopleDataArray do
  begin
    P:=D.Peoples.AddItem;
    P.Code:=R.Code;
    P.FirstName:=R.FirstName;
    P.LastName:=R.LastName;
  end;

  S:=GetTestForWriteStream('Collentions_TDepartment');
  D.SaveToStream(S);
  S.Free;
  D.Free;
end;

procedure TProtobufTest.LoadCollentions;
var
  D: TDepartment;
  S: TStream;
  P: TPeople;
  i, J: Integer;
begin
  D:=TDepartment.Create;
  S:=GetTestStream('Collentions_TDepartment');
  D.LoadFromStream(S);
  S.Free;
  J:=D.Peoples.Count;
  J:=High(PeopleDataArray);
  for i:=1 to J do
  begin
    P:=D.Peoples[i-1];
    AssertEquals(PeopleDataArray[i].Code, P.Code);
    AssertEquals(PeopleDataArray[i].FirstName, P.FirstName);
    AssertEquals(PeopleDataArray[i].LastName, P.LastName);
  end;
  AssertEquals(D.Signers.Count, 0);
  D.Free;
end;

function TProtobufTest.GetTestStream(ATestName: string): TStream;
var
  AFileName: String;
begin
  AFileName:=ExtractFileDir(ParamStr(0));
  AFileName:=AppendPathDelim(ExtractFileDir(AFileName)) + AppendPathDelim('data') + ChangeFileExt(ATestName, '.protobuf') ;
  if FileExists(AFileName) then
    Result:=TFileStream.Create(AFileName, fmOpenRead)
  else
    Result:=nil;
end;

function TProtobufTest.GetTestForWriteStream(ATestName: string): TStream;
var
  AFileName: String;
begin
  AFileName:=ExtractFileDir(ParamStr(0));
  AFileName:=AppendPathDelim(ExtractFileDir(AFileName)) + AppendPathDelim('data') + ChangeFileExt(ATestName, '.protobuf') ;
//  if FileExists(AFileName) then
    Result:=TFileStream.Create(AFileName, fmCreate)
{  else
    Result:=nil;}
end;

procedure TProtobufTest.ZigZagTest;
begin
  AssertEquals(0, EncodeValue32(0));
  AssertEquals(1, EncodeValue32(-1));
  AssertEquals(2, EncodeValue32(1));
  AssertEquals(3, EncodeValue32(-2));
  AssertEquals(4, EncodeValue32(2));
  AssertEquals(4294967294, EncodeValue32(2147483647));
  AssertEquals(4294967295, EncodeValue32(-2147483648));

  AssertEquals(0,EncodeValue64(0)); ;
(*  WriteLogFmt(1, '%22d -- %22U = %U', [        -1,            1, EncodeValue64(-1)]);
  WriteLogFmt(1, '%22d -- %22U = %U', [         1,            2, EncodeValue64(1)]);
  WriteLogFmt(1, '%22d -- %22U = %U', [        -2,            3, EncodeValue64(-2)]);
  WriteLogFmt(1, '%22d -- %22U = %U', [         2,            4, EncodeValue64(2)]);
  WriteLogFmt(1, '%22d -- %22U = %U', [ 2147483647,  4294967294, EncodeValue64(2147483647)]);
  WriteLogFmt(1, '%22d -- %22U = %U', [-2147483648,  4294967295, EncodeValue64(-2147483648)]);
  WriteLogFmt(1, '%22d -- %22U = %U', [ 9223372036854775807,  18446744073709551614, EncodeValue64(9223372036854775807)]);
  WriteLogFmt(1, '%22d -- %22U = %U', [-9223372036854775808,  18446744073709551615, EncodeValue64(-9223372036854775808)]);
*)

(*

  WriteLog(0, '');
  WriteLog(0, 'Decode:');
  WriteLogFmt(0, '%15U -- %15d = %d', [         0,            0, DecodeValue32(0) ] );
  WriteLogFmt(0, '%15U -- %15d = %d', [         1,           -1, DecodeValue32(1)]);
  WriteLogFmt(0, '%15U -- %15d = %d', [         2,            1, DecodeValue32(2)]);
  WriteLogFmt(0, '%15U -- %15d = %d', [         3,           -2, DecodeValue32(3)]);
  WriteLogFmt(0, '%15U -- %15d = %d', [         4,            2, DecodeValue32(4)]);
  WriteLogFmt(0, '%15U -- %15d = %d', [4294967294,   2147483647, DecodeValue32(4294967294)]);
  WriteLogFmt(0, '%15U -- %15d = %d', [4294967295,  -2147483648, DecodeValue32(4294967295)]);


  WriteLog(1, 'Encode:');
  WriteLogFmt(1, '%22d -- %22U = %U', [         0,            0, EncodeValue64(0) ] );
  WriteLogFmt(1, '%22d -- %22U = %U', [        -1,            1, EncodeValue64(-1)]);
  WriteLogFmt(1, '%22d -- %22U = %U', [         1,            2, EncodeValue64(1)]);
  WriteLogFmt(1, '%22d -- %22U = %U', [        -2,            3, EncodeValue64(-2)]);
  WriteLogFmt(1, '%22d -- %22U = %U', [         2,            4, EncodeValue64(2)]);
  WriteLogFmt(1, '%22d -- %22U = %U', [ 2147483647,  4294967294, EncodeValue64(2147483647)]);
  WriteLogFmt(1, '%22d -- %22U = %U', [-2147483648,  4294967295, EncodeValue64(-2147483648)]);
  WriteLogFmt(1, '%22d -- %22U = %U', [ 9223372036854775807,  18446744073709551614, EncodeValue64(9223372036854775807)]);
  WriteLogFmt(1, '%22d -- %22U = %U', [-9223372036854775808,  18446744073709551615, EncodeValue64(-9223372036854775808)]);
  WriteLog(1, '');
  WriteLog(1, 'Decode:');
  WriteLogFmt(1, '%22U -- %22d = %d', [         0,            0, DecodeValue64(0) ] );
  WriteLogFmt(1, '%22U -- %22d = %d', [         1,           -1, DecodeValue64(1)]);
  WriteLogFmt(1, '%22U -- %22d = %d', [         2,            1, DecodeValue64(2)]);
  WriteLogFmt(1, '%22U -- %22d = %d', [         3,           -2, DecodeValue64(3)]);
  WriteLogFmt(1, '%22U -- %22d = %d', [         4,            2, DecodeValue64(4)]);
  WriteLogFmt(1, '%22U -- %22d = %d', [4294967294,   2147483647, DecodeValue64(4294967294)]);
  WriteLogFmt(1, '%22U -- %22d = %d', [4294967295,  -2147483648, DecodeValue64(4294967295)]);
  WriteLogFmt(1, '%22U -- %22d = %d', [18446744073709551614,   9223372036854775807, DecodeValue64(18446744073709551614)]);
  WriteLogFmt(1, '%22U -- %22d = %d', [18446744073709551615,  -9223372036854775808, DecodeValue64(18446744073709551615)]);
*)
end;

initialization

  RegisterTest(TProtobufTest);
end.

