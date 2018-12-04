unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Memo2: TMemo;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public
    procedure ClearLogs;
    procedure WriteLog(ALogType:integer; S:string);
    procedure WriteLogFmt(ALogType:integer; S:string; AParams:array of const);
  end;

var
  Form1: TForm1;

implementation
uses protobuf_fpc;

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  ClearLogs;

(*  0	0
  -1	1
  1	2
  -2	3
  2147483647	4294967294
  -2147483648	4294967295*)
  WriteLog(0, 'Encode:');
  WriteLogFmt(0, '%15d -- %15U = %U', [         0,            0, EncodeValue32(0) ] );
  WriteLogFmt(0, '%15d -- %15U = %U', [        -1,            1, EncodeValue32(-1)]);
  WriteLogFmt(0, '%15d -- %15U = %U', [         1,            2, EncodeValue32(1)]);
  WriteLogFmt(0, '%15d -- %15U = %U', [        -2,            3, EncodeValue32(-2)]);
  WriteLogFmt(0, '%15d -- %15U = %U', [         2,            4, EncodeValue32(2)]);
  WriteLogFmt(0, '%15d -- %15U = %U', [ 2147483647,  4294967294, EncodeValue32(2147483647)]);
  WriteLogFmt(0, '%15d -- %15U = %U', [-2147483648,  4294967295, EncodeValue32(-2147483648)]);
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
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ClearLogs;
end;

procedure TForm1.ClearLogs;
begin
  Memo1.Lines.Clear;
  Memo2.Lines.Clear;
end;

procedure TForm1.WriteLog(ALogType: integer; S: string);
begin
  if ALogType = 0 then
    Memo1.Lines.Add(S)
  else
    Memo2.Lines.Add(S);
end;

procedure TForm1.WriteLogFmt(ALogType: integer; S: string;
  AParams: array of const);
begin
  WriteLog(ALogType, Format(S, AParams));
end;

end.

