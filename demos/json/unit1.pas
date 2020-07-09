unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    procedure WriteLog(S:string);
    procedure WriteLog(S:string; Args: array of const);
  public

  end;

var
  Form1: TForm1;

implementation

uses Unit2;

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  W: TChildJSONObj;
begin
  W:=TChildJSONObj.Create;
  W.Code:=1;
  W.Name:='Привет мир';
  W.SaveToFile('test1.json');
  W.Free;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  W: TChildJSONObj;
begin
  W:=TChildJSONObj.Create;
  W.LoadFromFile('test1.json');
  WriteLog('W.Code = %d', [W.Code]);
  WriteLog('W.Name = %s', [W.Name]);
  W.Free;
end;

procedure TForm1.WriteLog(S: string);
begin
  Memo1.Lines.Add(S);
end;

procedure TForm1.WriteLog(S: string; Args: array of const);
begin
  Memo1.Lines.Add(S, Args);
end;

end.

