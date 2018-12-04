unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  LazFileUtils, SynEdit, SynHighlighterAny;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit7: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Memo1: TMemo;
    NameOnShelf: TLabel;
    PageControl1: TPageControl;
    SynAnySyn1: TSynAnySyn;
    SynEdit1: TSynEdit;
    SynEdit2: TSynEdit;
    SynEdit3: TSynEdit;
    SynEdit4: TSynEdit;
    SynEdit5: TSynEdit;
    SynEdit6: TSynEdit;
    SynEdit7: TSynEdit;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    TabSheet6: TTabSheet;
    TabSheet7: TTabSheet;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    procedure SavePeople;
    procedure SaveDepartmet;
  public
    function DataFileName(ANum:Integer; AName:string = ''):string;
  end;

var
  Form1: TForm1;

implementation

uses CommonTestTypesUnit, protobuf_fpc;

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button2Click(Sender: TObject);
var
  T:TSerializationObject;
  F: TFileStream;
  B: TBytes;
  S: String;
  i: Integer;
begin
  T:=nil;
  case PageControl1.ActivePageIndex of
    0:T:=TTest1.Create;
    1:T:=TTest2.Create;
    2:T:=TTest3.Create;
    3:T:=TSignedContent.Create;
    4:T:=TDocumentList.Create;
  end;
  if not Assigned(T) then Exit;


  if not FileExists(DataFileName(PageControl1.ActivePageIndex+1)) then exit;

  F:=TFileStream.Create(DataFileName(PageControl1.ActivePageIndex+1), fmOpenRead);
  T.LoadFromStream(F);
  F.Free;
  case  PageControl1.ActivePageIndex of
    0:Edit1.Text:=TTest1(T).a.ToString;
    1:Edit2.Text:=TTest2(T).b;
    2:Edit3.Text:=TTest3(T).c.a.ToString;
    3:begin
        Edit4.Text:=TSignedContent(T).NameOnShelf;
        B:=TSignedContent(T).Content;
        S:='';
        if Length(B)>0 then
        begin
          for i:=0 to Length(B)-1 do
            S:=S + Char(B[i]);
        end;
        Edit5.Text:=S;
      end;
    4:Memo1.Text:=TDocumentList(T).Lines.Text;
  end;
  T.Free;
end;

procedure TForm1.SavePeople;
var
  P, P1: TPeople;
  S: TFileStream;
begin
  P:=TPeople.Create;
  P.Code:=1;
  P.FirstName:='Иван';
  P.LastName:='Иванов';
  S:=TFileStream.Create(DataFileName(1, 'People'), fmCreate);
  P.SaveToStream(S);
  S.Free;

  P.Code:=Low(Int32);
  P.FirstName:='Иван';
  P.LastName:='Иванов';
  S:=TFileStream.Create(DataFileName(3, 'People'), fmCreate);
  P.SaveToStream(S);
  S.Free;

  P.Code:=-2;
  P.FirstName:='Пётр';
  P.LastName:='Петров';
  S:=TFileStream.Create(DataFileName(2, 'People'), fmCreate);
  P.SaveToStream(S);
  S.Position:=0;

  P1:=TPeople.Create;
  P1.LoadFromStream(S);

  if P1.Code <> P.Code then
    ShowMessage('Код не совпадает');
  if P1.FirstName <> P.FirstName then
    ShowMessage('FirstName не совпадает');
  if P1.LastName <> P.LastName then
    ShowMessage('LastName не совпадает');

  S.Free;
  P1.Free;
  P.Free;
end;

procedure TForm1.SaveDepartmet;
var
  D: TDepartment;
begin
  D:=TDepartment.Create;
  D.Code:=1;
  D.DepartmentName:='Подразделение № 1';
  D.SaveToStream();
  D.Free;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  T:TSerializationObject;
  F: TFileStream;
  B:TBytes;
  S: TCaption;
  I: Integer;
begin
  T:=nil;
  case PageControl1.ActivePageIndex of
    0:begin
        T:=TTest1.Create;
        TTest1(T).a:=StrToInt(Edit1.Text);
      end;
    1:begin
        T:=TTest2.Create;
        TTest2(T).b:=Edit2.Text;
      end;
    2:begin
        T:=TTest3.Create;
        TTest3(T).c.a:=StrToInt(Edit3.Text);
      end;
    3:begin
        T:=TSignedContent.Create;
        TSignedContent(T).NameOnShelf:=Edit4.Text;
        S:=Edit5.Text;
        if Length(S)>0 then
        begin
          SetLength(B, Length(S));
          for I:=1 to Length(S) do
            B[i-1]:=Byte(S[i]);
          TSignedContent(T).Content:=B;
        end;
      end;
    4:begin
        T:=TDocumentList.Create;
        TDocumentList(T).Lines.Text:=Memo1.Text;
        TDocumentList(T).TotalCount:=Memo1.Lines.Count;
      end;
    5:SavePeople;
    6:SaveDepartmet;
  end;
  if not Assigned(T) then Exit;

  F:=TFileStream.Create(DataFileName(PageControl1.ActivePageIndex+1), fmCreate);
  T.SaveToStream(F);
  F.Free;
  T.Free;
end;

function TForm1.DataFileName(ANum: Integer; AName: string): string;
var
  S: String;
begin
  if AName = '' then AName:='test';
  S:=ExtractFileDir(ParamStr(0));
  S:=ExtractFileDir(S);
  Result:=AppendPathDelim(S) + 'data' + PathDelim + Format('%s%d.protobuf', [AName, ANum]);
end;

end.

