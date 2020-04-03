{ google protobuf files compiler to FPC class

  Copyright (C) 2018 Lagunov Aleksey alexs@yandex.ru

  This source is free software; you can redistribute it and/or modify it under the terms of the GNU General Public
  License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later
  version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web at
  <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing to the Free Software Foundation, Inc., 51
  Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}

unit ProtoParser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  CommandDelemiter = ';';

  CharDelimiters = [',',':','}','{',';', '(', ')', '.', '[', ']', '=', '@'];

type
  TCMDState = (cmsNormal, cmsError, cmsEndOfCmd);
  TParserSyntax = (psProto2, psProto3);

  TProtoTokenType = (stNone,
               stIdentificator, stKeyword, stTerminator, stSymbol, stDelimiter,
               stInteger, stFloat, stString);

  TProtoTokenOption = (toHeaderEnd,
                       toHeaderStart,
                       toOptional);

  TProtoTokenOptions = set of TProtoTokenOption;

  TParserPosition = record
    Position:integer;
    X:integer;
    Y:integer;
  end;
  TProtoToken = class;
  TProtoTokenList = class;
  TProtoTokenListEnumerator = class;
  TProtoObjectListEnumerator = class;
  TProtoParser = class;
  TProtoObject = class;

  TProcessComment = procedure (Sender:TProtoParser; AComment:string) of object;
  TParserStatus = procedure (Sender:TProtoParser; ALine:integer; AObject:TProtoObject; AMessage:string) of object;

  { TProtoToken }

  TProtoToken = class
  private
    FChild: TProtoTokenList;
    FCode: integer;
    FOptions: TProtoTokenOptions;
    FToken: string;
    FTokenType: TProtoTokenType;
    FCountRef:integer;
  protected
  public
    constructor Create;
    destructor Destroy;override;
    function Equal(S:string):boolean;
    function ChildByName(S:string):TProtoToken;
    function ChildByType(ATokenType:TProtoTokenType; AWord:string = ''):TProtoToken;
    procedure AddChildToken(AToken:TProtoToken);
    procedure AddChildToken(ATokens:array of TProtoToken);overload;
    property Child:TProtoTokenList read FChild;
    property Options:TProtoTokenOptions read FOptions write FOptions;
    property TokenType:TProtoTokenType read FTokenType;
    property Token:string read FToken;
    property Code:integer read FCode;
  end;

  { TProtoTokenList }

  TProtoTokenList = class
  private
    FList:TFPList;
    function GetCount: integer;
    function GetItem(AIndex: integer): TProtoToken;
    procedure Clear;
  public
    constructor Create;
    destructor Destroy;override;
    function GetEnumerator: TProtoTokenListEnumerator;
    procedure Add(AItem:TProtoToken);
    function IndexOf(AItem:TProtoToken):integer;
    property Item[AIndex:integer]:TProtoToken read GetItem; default;
    property Count:integer read GetCount;
  end;

  { TProtoTokenListEnumerator }

  TProtoTokenListEnumerator = class
  private
    FList: TProtoTokenList;
    FPosition: Integer;
  public
    constructor Create(AList: TProtoTokenList);
    function GetCurrent: TProtoToken;
    function MoveNext: Boolean;
    property Current: TProtoToken read GetCurrent;
  end;

  { TProtoObject }

  TProtoObject = class
  private
    FCaption: string;
    FProtoTokensList : TProtoTokenList;
    FSourceCode: string;
    FStartTokens: TProtoTokenList;
    FKeyWordList:TStringList;
    function FindKeyWord(AWord:string):boolean;
    function DoCheckCommand(AProtoToken:TProtoToken; AParser:TProtoParser):boolean;
    function CheckCommand(AParser: TProtoParser; out StartToken: TProtoToken
      ): boolean;
  protected
    procedure InitParserTree;virtual;
    procedure InternalProcessChildToken(AParser:TProtoParser; AToken:TProtoToken; AWord:string); virtual;

  public
    constructor Create;virtual;
    destructor Destroy;override;
    procedure GenerateInterfaceSection(AModule:TStrings); virtual;
    procedure GenerateImplementationSection(AModule:TStrings); virtual;

    procedure Clear; virtual;
    procedure Assign(ASource:TProtoObject); virtual;
    function AddToken(ATokenType:TProtoTokenType; const Parent:TProtoToken; const AToken:string; AOptions:TProtoTokenOptions; ACode:integer = 0):TProtoToken;
    function AddToken(ATokenType:TProtoTokenType; const Parents:array of TProtoToken; const AToken:string; AOptions:TProtoTokenOptions; ACode:integer = 0):TProtoToken; overload;
    property Caption:string read FCaption write FCaption;
    property SourceCode:string read FSourceCode write FSourceCode;
  end;
  TProtoObjectClass = class of TProtoObject;

  { TProtoObjectList }

  TProtoObjectList = class
  private
    FList:TFPList;
    function GetCount: integer;
    function GetItem(AIndex: integer): TProtoObject;
    procedure Clear;
  public
    constructor Create;
    destructor Destroy;override;
    function GetEnumerator: TProtoObjectListEnumerator;
    procedure Add(AItem:TProtoObject);
    function IndexOf(AItem:TProtoObject):integer;
    property Item[AIndex:integer]:TProtoObject read GetItem; default;
    property Count:integer read GetCount;
  end;

  { TProtoObjectListEnumerator }

  TProtoObjectListEnumerator = class
  private
    FList: TProtoObjectList;
    FPosition: Integer;
  public
    constructor Create(AList: TProtoObjectList);
    function GetCurrent: TProtoObject;
    function MoveNext: Boolean;
    property Current: TProtoObject read GetCurrent;
  end;

  { TSyntax }

  TSyntax = class(TProtoObject)
  private
    FProtoSyntax: TParserSyntax;
  protected
    procedure InitParserTree;override;
    procedure InternalProcessChildToken(AParser:TProtoParser; AToken:TProtoToken; AWord:string); override;
  public
    property ProtoSyntax:TParserSyntax read FProtoSyntax write FProtoSyntax;
  end;

  { TProtoParser }

  TProtoParser = class
  private
    FErrorMessage: string;
    FErrorPosition: TPoint;
    FOnProcessComment: TProcessComment;
    FOnStatus: TParserStatus;
    FParserSyntax: TParserSyntax;
    FState: TCMDState;
    FBuffer:string;
    FPosition: TParserPosition;
    FWordPosition: TParserPosition;
    FProtoObjectList:TProtoObjectList;
    FResultObjectList:TProtoObjectList;
    function GetNextWord2:string;
    function GetNextWord1(AExcludeChar:TSysCharSet):string;
    procedure DoTestLineEnd;
    procedure SetError(const AErrorMessage:string);
    procedure SetState(AValue: TCMDState);
    procedure InternalCompile;
    procedure InternalStatus(AObject:TProtoObject; AMessage:string);
    procedure InternalProcessControl(R: TProtoObject);
  protected
    procedure IncPos(ACountChar:Integer = 1);
    function WordType(Sender:TProtoToken; AWord:string; ACmd:TProtoObject):TProtoTokenType;
    function GetNextWord:string;
    procedure SkipSpace;
    procedure SkipComment1;
    procedure SkipComment2;
    function WaitWord(AWord:string):boolean;
    function GetToWord(AWord:string):string;
    function GetToCommandDelemiter:string;
    function GetToBracket(Bracket:string):string;
  public
    constructor Create(AOwnerParser:TProtoParser);
    destructor Destroy;override;
    procedure Parse(ACmd: TProtoObject; AStartToken: TProtoToken);
    procedure Clear;
    function Eof:boolean;
    property Position:TParserPosition read FPosition write FPosition;
    property WordPosition:TParserPosition read FWordPosition write FWordPosition;
    property State:TCMDState read FState write SetState;
    procedure Compile(AFileName:string); overload;
    procedure Compile(AStream:TStream); overload;
    procedure CompileStr(S:string);
    property ErrorMessage:string read FErrorMessage;
    property ErrorPosition:TPoint read FErrorPosition;
    property OnProcessComment : TProcessComment read FOnProcessComment write FOnProcessComment;
    property ResultObjects:TProtoObjectList read FResultObjectList;
    property OnStatus:TParserStatus read FOnStatus write FOnStatus;
    property ParserSyntax:TParserSyntax read FParserSyntax;
  end;

var
  PParser:TProtoParser = nil;

procedure RegisterProtoObject(AProtoObjectClass:TProtoObjectClass);
implementation
uses LazUTF8, rxstrutils, StrUtils;

resourcestring
  sProtoParserErrorBracket    = 'Unexpected )';
  sProtoParserExpected        = 'Expected : ';
  sProtoParserString          = 'string';
  sProtoParserNumber          = 'number';
  sProtoParserIdentificator   = 'identificator';
  sProtoParserUnknowCommand   = 'Unknow command: %s';

procedure RegisterProtoObject(AProtoObjectClass: TProtoObjectClass);
begin
  if not Assigned(PParser) then
    PParser:=TProtoParser.Create(nil);
  PParser.FProtoObjectList.Add(AProtoObjectClass.Create);
end;

{ TProtoObjectListEnumerator }

constructor TProtoObjectListEnumerator.Create(AList: TProtoObjectList);
begin
  FList := AList;
  FPosition := -1;
end;

function TProtoObjectListEnumerator.GetCurrent: TProtoObject;
begin
  Result := FList[FPosition];
end;

function TProtoObjectListEnumerator.MoveNext: Boolean;
begin
  Inc(FPosition);
  Result := FPosition < FList.Count;
end;

{ TProtoObjectList }

function TProtoObjectList.GetCount: integer;
begin
  Result:=FList.Count;
end;

function TProtoObjectList.GetItem(AIndex: integer): TProtoObject;
begin
  Result:=TProtoObject(FList[AIndex]);
end;

procedure TProtoObjectList.Clear;
var
  I: Integer;
begin
  for I:=0 to FList.Count - 1 do
    TProtoObject(FList[i]).Free;
  FList.Clear;
end;

constructor TProtoObjectList.Create;
begin
  inherited Create;
  FList:=TFPList.Create;
end;

destructor TProtoObjectList.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited Destroy;
end;

function TProtoObjectList.GetEnumerator: TProtoObjectListEnumerator;
begin
  Result := TProtoObjectListEnumerator.Create(Self);
end;

procedure TProtoObjectList.Add(AItem: TProtoObject);
begin
  FList.Add(AItem);
end;

function TProtoObjectList.IndexOf(AItem: TProtoObject): integer;
begin
  Result:=FList.IndexOf(AItem);
end;

{ TProtoObject }

function TProtoObject.FindKeyWord(AWord: string): boolean;
procedure DoFillKeyWords;
var
  i: Integer;
  T: TProtoToken;
begin
  FKeyWordList:=TStringList.Create;
  FKeyWordList.Sorted:=true;

  for T in FProtoTokensList do
    if T.FTokenType = stKeyword then
      if not FKeyWordList.Find(T.Token, i) then
        FKeyWordList.Add(UpperCase(T.Token));
end;

var
  I: Integer;
begin
  if not Assigned(FKeyWordList) then DoFillKeyWords;
  Result:=FKeyWordList.Find(AWord, I);
end;

function TProtoObject.DoCheckCommand(AProtoToken: TProtoToken;
  AParser: TProtoParser): boolean;
var
  Stop: Boolean;
  T, Child: TProtoToken;
  S: String;
  TT: TProtoTokenType;
begin
  Result:=false;

  Stop:=false;
  T:=AProtoToken;
  if not Assigned(T) then
    raise Exception.Create('Не инициализирован ProtoToken - ' + ClassName);

  S:=AParser.GetNextWord;
  if not T.Equal(S) then exit;

  if (toHeaderStart in T.Options) then
  begin
    Result:=true;
    exit;
  end;

  repeat
    S:=AParser.GetNextWord;
    TT:=AParser.WordType(T, S, Self);

    case TT of
      stString,
      stInteger,
      stFloat,
      stIdentificator:Child:=T.ChildByType(TT, S);
      stSymbol,
      stTerminator,
      stKeyword:Child:=T.ChildByName(S);
    else
      Result:=false;
      Stop:=true;
      break;
    end;

    if not Assigned(Child) then
    begin
      Result:=false;
      Stop:=true;
    end
    else
    if (toHeaderEnd in Child.Options) then
    begin
      Result:=true;
      exit;
    end;

    T:=Child;
  until Stop;
end;

function TProtoObject.CheckCommand(AParser: TProtoParser; out StartToken:TProtoToken): boolean;
var
  SavePos: TParserPosition;
begin
  SavePos:=AParser.Position;
  Result:=false;
  for StartToken in FStartTokens do
  begin
    AParser.Position := SavePos;
    if DoCheckCommand(StartToken, AParser) then
      Exit(true);
  end;
end;

procedure TProtoObject.InitParserTree;
begin

end;

procedure TProtoObject.InternalProcessChildToken(AParser: TProtoParser;
  AToken: TProtoToken; AWord: string);
begin
  case AToken.Code of
    1:FCaption:=AWord;
    -1:AParser.FState:=cmsEndOfCmd;
  end;
end;

procedure TProtoObject.GenerateInterfaceSection(AModule: TStrings);
begin

end;

procedure TProtoObject.GenerateImplementationSection(AModule: TStrings);
begin

end;

constructor TProtoObject.Create;
begin
  inherited Create;
  FProtoTokensList:=TProtoTokenList.Create;
  FStartTokens:=TProtoTokenList.Create;
  InitParserTree;
end;

destructor TProtoObject.Destroy;
begin
  FreeAndNil(FStartTokens);
  FProtoTokensList.Clear;
  FreeAndNil(FProtoTokensList);
   if Assigned(FKeyWordList) then
    FreeAndNil(FKeyWordList);
 inherited Destroy;
end;

procedure TProtoObject.Clear;
begin
  FSourceCode:='';
end;

procedure TProtoObject.Assign(ASource: TProtoObject);
begin
  if ASource is TProtoObject then
  begin
    FCaption:=TProtoObject(ASource).Caption;
  end;
end;

function TProtoObject.AddToken(ATokenType: TProtoTokenType;
  const Parent: TProtoToken; const AToken: string;
  AOptions: TProtoTokenOptions; ACode:integer = 0): TProtoToken;
begin
  Result:=TProtoToken.Create;
  FProtoTokensList.Add(Result);

  Result.FTokenType:=ATokenType;
  Result.FToken:=UpperCase(AToken);
  Result.FOptions:=AOptions;
  Result.FCode:=ACode;

  if Assigned(Parent) then
    Parent.Child.Add(Result);

  if toHeaderStart in AOptions then
    FStartTokens.Add(Result);
end;

function TProtoObject.AddToken(ATokenType: TProtoTokenType;
  const Parents: array of TProtoToken; const AToken: string;
  AOptions: TProtoTokenOptions; ACode:integer = 0): TProtoToken;
var
  P: TProtoToken;
begin
  Result:=nil;
  if Length(Parents) = 0 then
    raise Exception.Create('TProtoObject.AddToken : Count parents = 0');

  Result:=AddToken(ATokenType, nil, AToken, AOptions, ACode);
  for P in Parents do
    P.AddChildToken(Result);
end;

{ TProtoTokenList }

function TProtoTokenList.GetCount: integer;
begin
  Result:=FList.Count;
end;

function TProtoTokenList.GetItem(AIndex: integer): TProtoToken;
begin
  Result:=TProtoToken(FList[AIndex]);
end;

procedure TProtoTokenList.Clear;
var
  I: Integer;
begin
  for I:=0 to FList.Count - 1 do
    TProtoToken(FList[i]).Free;
  FList.Clear;
end;

constructor TProtoTokenList.Create;
begin
  inherited Create;
  FList:=TFPList.Create;
end;

destructor TProtoTokenList.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

function TProtoTokenList.GetEnumerator: TProtoTokenListEnumerator;
begin
  Result := TProtoTokenListEnumerator.Create(Self);
end;

procedure TProtoTokenList.Add(AItem: TProtoToken);
begin
  FList.Add(AItem);
end;

function TProtoTokenList.IndexOf(AItem: TProtoToken): integer;
begin
  Result:=FList.IndexOf(AItem);
end;

{ TProtoTokenListEnumerator }

constructor TProtoTokenListEnumerator.Create(AList: TProtoTokenList);
begin
  FList := AList;
  FPosition := -1;
end;

function TProtoTokenListEnumerator.GetCurrent: TProtoToken;
begin
  Result := FList[FPosition];
end;

function TProtoTokenListEnumerator.MoveNext: Boolean;
begin
  Inc(FPosition);
  Result := FPosition < FList.Count;
end;

{ TProtoToken }

constructor TProtoToken.Create;
begin
  inherited Create;
  FChild:=TProtoTokenList.Create;
  FCountRef:=0;
end;

destructor TProtoToken.Destroy;
begin
  FreeAndNil(FChild);
  inherited Destroy;
end;

function TProtoToken.Equal(S: string): boolean;
begin
  Result:=UpperCase(S) = FToken;
end;

function TProtoToken.ChildByName(S: string): TProtoToken;
var
  T: TProtoToken;
begin
  S:=UpperCase(S);
  Result:=nil;
  for T in Child do
    if T.FToken = S then
      Exit(T);
end;

function TProtoToken.ChildByType(ATokenType: TProtoTokenType; AWord: string
  ): TProtoToken;
var
  T: TProtoToken;
begin
  Result:=nil;
  AWord:=UpperCase(AWord);
  for T in Child do
  begin
    if (T.TokenType = ATokenType) then
    begin
      if not Assigned(Result) and (ATokenType <> stKeyword) then Result:=T;
      if (AWord <> '') and (T.FToken = AWord) then
        Exit(T);
    end
  end;
end;

procedure TProtoToken.AddChildToken(AToken: TProtoToken);
var
  S: String;
begin
  if Assigned(AToken) then
  begin
    if (Child.IndexOf(AToken)<0) then
    begin
      Inc(AToken.FCountRef);
      Child.Add(AToken);
    end
    else
    begin
      S:=Format('(%s.%d) - AToken already in Childs (%s.%d)', [FToken, FCode, AToken.FToken, AToken.FCode]);
      raise Exception.Create(S);
    end;
  end
  else
    raise Exception.Create('AToken = nil');
end;

procedure TProtoToken.AddChildToken(ATokens: array of TProtoToken);
var
  T: TProtoToken;
begin
  for T in ATokens do
    AddChildToken(T);
end;

{ TSyntax }

procedure TSyntax.InitParserTree;
var
  FStart, T: TProtoToken;
begin
  FStart:=AddToken(stKeyword, nil, 'syntax', [toHeaderStart, toHeaderEnd]);
  T:=AddToken(stSymbol, FStart, '=', []);
    AddToken(stString, T, '', [], 2);
end;

procedure TSyntax.InternalProcessChildToken(AParser: TProtoParser;
  AToken: TProtoToken; AWord: string);
begin
  inherited InternalProcessChildToken(AParser, AToken, AWord);
  case AToken.Code of
    2:begin
        Caption:=LowerCase(ExtractQuotedString(AWord, '"'));
        if Caption = 'proto3' then
          FProtoSyntax:=psProto3
        else
        if Caption = 'proto2' then
          FProtoSyntax:=psProto2
      end;
  end;
end;

{ TProtoParser }

procedure TProtoParser.Clear;
begin
  FState:=cmsNormal;
  FErrorMessage:='';
  FErrorPosition.X:=0;
  FErrorPosition.Y:=0;
  FPosition.X:=1;
  FPosition.Y:=1;
  FPosition.Position:=1;
  FBuffer:='';
  FResultObjectList.Clear;
  FParserSyntax:=psProto2;
end;

procedure TProtoParser.IncPos(ACountChar: Integer);
var
  i: Integer;
begin
  if ACountChar > 0 then
  begin
    for i:=1 to ACountChar do
      FPosition.Position:=FPosition.Position + UTF8CharacterLength( @FBuffer[FPosition.Position]);
  end
  else
    Inc(FPosition.Position, ACountChar); //Плохое решение - будем думать!
  Inc(FPosition.X, ACountChar);
end;

function TProtoParser.GetNextWord: string;
var
  Stop:boolean;
begin
  Stop:=false;
  SkipSpace;
  repeat
    WordPosition:=Position;
    Result:=GetNextWord1([]);
    if Copy(Result, 1, 2) = '//' then
      SkipComment1
    else
    if Result='/*' then
      SkipComment2
    else
      Stop:=true;
  until Stop or (FPosition.Position > Length(FBuffer));
end;

procedure TProtoParser.SkipSpace;
begin
  while (Position.Position <= Length(FBuffer)) and (FBuffer[Position.Position] < #33) do
  begin
    DoTestLineEnd;
    IncPos;
  end;
end;

procedure TProtoParser.SkipComment1;
var
  P: TParserPosition;
begin
  P:=FPosition;
  while (FPosition.Position < Length(FBuffer)) do
  begin
    if (FBuffer[FPosition.Position]=#13) or (FBuffer[FPosition.Position]=#10) then
    begin
      SkipSpace;
      if Assigned(FOnProcessComment) then
        FOnProcessComment(Self, Copy(FBuffer, P.Position, FPosition.Position - P.Position));
      break;
    end
    else
      IncPos;
  end;
end;

procedure TProtoParser.SkipComment2;
var
  P: TParserPosition;
begin
  P:=Position;
  WaitWord('*/');
  if Assigned(FOnProcessComment) then
    FOnProcessComment(Self, Copy(FBuffer, P.Position, Position.Position - P.Position - Length('*/') ));
  SkipSpace;
end;

function TProtoParser.WaitWord(AWord: string): boolean;
var
  S:string;
begin
  Result:=false;
  AWord:=LowerCase(AWord);
  while not Eof do
  begin
    SkipSpace;
    S:=GetNextWord2;
    if LowerCase(S) = AWord then
      Exit(true);
  end;
end;

function TProtoParser.GetToWord(AWord: string): string;
var
  S: String;
  P: TParserPosition;
begin
  P:=Position;
  Result:='';
  while not Eof do
  begin
    SkipSpace;
    S:=GetNextWord;
    if LowerCase(S)=LowerCase(AWord) then
    begin
      Result:=Copy(FBuffer, P.Position, Position.Position - P.Position);
      Exit;
    end;
  end;
end;

function TProtoParser.GetToCommandDelemiter: string;
var
  CP: Integer;
begin
  CP:=Position.Position;
  Result:=GetToWord(CommandDelemiter);
  if Result = '' then
    Result:=Copy(FBuffer, CP, Length(FBuffer))
end;

function TProtoParser.GetToBracket(Bracket: string): string;
var
  S:string;
  L:integer;
  fStart: Char;
  StartPos: TParserPosition;
begin
  Result:='';
  if Bracket = ')' then fStart:='('
  else
  if Bracket = ']' then
    fStart:='['
  else
    exit;

  L:=0;
  StartPos:=Position;
  while (Position.Position < Length(FBuffer)) do
  begin
    SkipSpace;
    WordPosition:=Position;
    S:=GetNextWord1([]);
    if S = fStart then
      Inc(L)
    else
    if S=Bracket then
    begin
      if L > 0 then
        Dec(L)
      else
      begin
        Result:=Copy(FBuffer, StartPos.Position, WordPosition.Position - StartPos.Position);
        break;
      end;
    end;
  end;
end;

constructor TProtoParser.Create(AOwnerParser: TProtoParser);
var
  R, R1: TProtoObject;
begin
  inherited Create;
  FParserSyntax:=psProto2;
  FProtoObjectList:=TProtoObjectList.Create;
  FResultObjectList:=TProtoObjectList.Create;

  if Assigned(AOwnerParser) then
  begin
    for R in AOwnerParser.FProtoObjectList do
    begin
      R1:=R.NewInstance as TProtoObject;
      R1.Create;
      FProtoObjectList.Add(R1);
    end;
  end;
end;

destructor TProtoParser.Destroy;
begin
  Clear;
  FreeAndNil(FProtoObjectList);
  FreeAndNil(FResultObjectList);
  inherited Destroy;
end;

function TProtoParser.Eof: boolean;
begin
  Result:=(Position.Position > Length(FBuffer));
end;

function TProtoParser.WordType(Sender: TProtoToken; AWord: string;
  ACmd: TProtoObject): TProtoTokenType;
var
  I, C:Integer;
  R:Double;
  P: TProtoToken;
begin
  if IsValidIdent(AWord) then
  begin
    if Assigned(Sender) then
      for i:=0 to Sender.Child.Count-1 do
      begin
        P:=TProtoToken(Sender.Child[i]);
        if P.Token = UpperCase(AWord) then
        begin
          Result:=P.TokenType;
          exit;
        end;
      end;
    if Assigned(ACmd) then
      if ACmd.FindKeyWord(UpperCase(AWord)) then
      begin
        Result:=stKeyword;
        Exit;
      end;
    Result:=stIdentificator
  end
  else
  if ((Length(AWord) = 1) and (AWord[1] in (CharDelimiters + ['=', '<', '>', '+', '-', '^', '[', ']', ':', '\', '*', '@']))) then
    Result:=stSymbol
  else
  if (AWord = ';') then
    Result:=stDelimiter
  else
  if (Length(AWord)>1) and (AWord[1]='''') and (AWord[Length(AWord)]='''') then
    Result:=stString
  else
  if (Length(AWord)>1) and (AWord[1]='"') and (AWord[Length(AWord)]='"') then
    Result:=stString
  else
  begin
    Val(AWord, I, C);
    if C = 0 then
      Result:=stInteger
    else
    begin
      Val(AWord, R, C);
      if C = 0 then
        Result:=stFloat
      else
        Result:=stNone;
    end;
  end
end;

function TProtoParser.GetNextWord2: string;
var
  StartPos: TParserPosition;
begin
  Result:='';
  while (not Eof) and (FBuffer[FPosition.Position] <= ' ') do
  begin
    DoTestLineEnd;
    IncPos;
  end;
  if Eof then exit;

  StartPos:=Position;

  if FBuffer[FPosition.Position]='/' then
  begin
    IncPos;
    if (not Eof) and (FBuffer[FPosition.Position]='*') then
    begin
      Result:='/*';
      IncPos;
      exit;
    end;
  end;

  if FBuffer[FPosition.Position]='*' then
  begin
    IncPos;
    if (not Eof) and (FBuffer[FPosition.Position] = '/') then
    begin
      Result:='*/';
      IncPos;
    end
    else
      Result:='*';
    exit;
  end;

  if (FBuffer[FPosition.Position] = '-') and (FPosition.Position < Length(FBuffer)) and (FBuffer[FPosition.Position + 1] = '-') then
  begin
    Result:='--';
    IncPos;
    IncPos;
  end;

  if (FBuffer[FPosition.Position] in (['''', '"', '`'] + CharDelimiters)) then
    IncPos
  else
  begin
    repeat
      DoTestLineEnd;
      IncPos;
    until (FPosition.Position >= Length(FBuffer)) or (FBuffer[FPosition.Position] < #33) or (not (FBuffer[FPosition.Position] in (['a'..'z','A'..'Z','0'..'9', '_', '$'])));
  end;
  SetLength(Result, FPosition.Position - StartPos.Position);
  Move(FBuffer[StartPos.Position], Result[1], Position.Position - StartPos.Position);
end;

function TProtoParser.GetNextWord1(AExcludeChar: TSysCharSet): string;
procedure DoProcessString(ACh:Char);
begin
  while (Position.Position < Length(FBuffer)) do
  begin
    DoTestLineEnd;
    IncPos;

    if (FPosition.Position < Length(FBuffer)-1) and (FBuffer[FPosition.Position] = ACh) and (FBuffer[FPosition.Position+1] = ACh) then
    begin
      DoTestLineEnd;
      IncPos;
    end
    else
    if (FPosition.Position < Length(FBuffer)) and (FBuffer[FPosition.Position] = ACh) then
      break;
  end;
  DoTestLineEnd;
  IncPos;
end;

var
  StartPos: TParserPosition;
  FNum: Boolean;
  i: Integer;
begin
  Result:='';
  if FPosition.Position > Length(FBuffer) then exit;

  while (FPosition.Position<=Length(FBuffer)) and (FBuffer[FPosition.Position] <= ' ') do
  begin
    DoTestLineEnd;
    IncPos;
  end;
  if Eof then exit;

  StartPos:=Position;

  if FBuffer[FPosition.Position]='/' then
  begin
    IncPos;

    if (FPosition.Position < Length(FBuffer)) and (FBuffer[FPosition.Position]='*') then
    begin
      Result:='/*';
      IncPos;
      exit;
    end;
  end;

  if FBuffer[FPosition.Position]='*' then
  begin
    IncPos;

    if (FPosition.Position < Length(FBuffer)) and (FBuffer[FPosition.Position] = '/') then
    begin
      Result:='*/';
      IncPos;
      exit;
    end
    else
      Result:='*';
  end;

  if (FBuffer[FPosition.Position] = '-') and (FPosition.Position < Length(FBuffer)) and (FBuffer[FPosition.Position + 1] = '-') then
  begin
    Result:='--';
    IncPos;
    IncPos;
  end;

  if Result = '' then
  begin
    if (FBuffer[FPosition.Position]='''') and (AExcludeChar = []) then DoProcessString('''')
    else
    if (FBuffer[FPosition.Position]='"') and (AExcludeChar = [])  then DoProcessString('"')
    else
    if (FBuffer[FPosition.Position]='`') and (AExcludeChar = [])  then DoProcessString('`')
    else
    if FBuffer[FPosition.Position] in CharDelimiters then IncPos
    else
    begin
      repeat
        DoTestLineEnd;
        IncPos;
      until (FPosition.Position >= Length(FBuffer)) or (FBuffer[FPosition.Position] < #33) or (not (FBuffer[FPosition.Position] in (['a'..'z','A'..'Z','0'..'9', '_', '$'] - AExcludeChar)));

      if (FPosition.Position < Length(FBuffer)) and (FBuffer[FPosition.Position] = '.') then
      begin
        //test for float
        FNum:=true;
        for i:=FPosition.Position-1 downto StartPos.Position do
          if not (FBuffer[i] in ['0'..'9']) then
          begin
            FNum:=false;
            Break;
          end;
        if FNum then
          repeat
            DoTestLineEnd;
            IncPos;
          until (FPosition.Position >= Length(FBuffer)) or (FBuffer[FPosition.Position] < #33) or (not (FBuffer[FPosition.Position] in ['0'..'9']));
      end

    end;
    SetLength(Result, FPosition.Position - StartPos.Position);
    Move(FBuffer[StartPos.Position], Result[1], Position.Position - StartPos.Position);
  end;
end;

procedure TProtoParser.DoTestLineEnd;
begin
  if FBuffer[Position.Position] <> LineEnding then exit;
  Inc(FPosition.Y);
  FPosition.X:=0;
end;

procedure TProtoParser.SetError(const AErrorMessage: string);
begin
  FState:=cmsError;
  FErrorMessage:=Format('(%d, %d) %s', [Position.X, Position.Y, AErrorMessage]);
  FErrorPosition.X:=Position.X;
  FErrorPosition.Y:=Position.Y;
end;

procedure TProtoParser.InternalCompile;
function GetNextCommand1(out StartToken: TProtoToken): TProtoObject;
var
  i: Integer;
  SavePos: TParserPosition;
  R: TProtoObject;
  S: String;

begin
  Result:=nil;
  SavePos:=Position;
  for R in FProtoObjectList do
  begin
    Position:=SavePos;
    if R.CheckCommand(Self, StartToken) then
    begin
      Position:=SavePos;
      Result:=R.NewInstance as TProtoObject;
      Result.Create;
      exit;
    end;
  end;
  if not Eof then
  begin
    Position:=SavePos;
    S:=GetNextWord;
    Position:=SavePos;
    FState:=cmsError;
    InternalStatus(R, 'Uknow command - '+S);
  end;
end;
var
  R: TProtoObject;
  StartToken: TProtoToken;
  SP: TParserPosition;
begin
  if FBuffer = '' then Exit;
  while not (Eof or (FState = cmsError)) do
  begin
    FState:=cmsNormal;
    SP:=Position;
    R:=GetNextCommand1(StartToken);

    if Assigned(R) then
    begin
      InternalStatus(R, 'Start');

      FResultObjectList.Add(R);
      Parse(R, StartToken);

      if FState <> cmsError then
        R.SourceCode:=trim(Copy(FBuffer, SP.Position,  Position.Position - SP.Position));

      InternalStatus(R, R.Caption +' - done');
      InternalProcessControl(R);
    end;
  end;
end;

procedure TProtoParser.InternalStatus(AObject: TProtoObject; AMessage: string);
begin
  if Assigned(FOnStatus) then
    FOnStatus(Self, Position.Y, AObject, AMessage);
end;

procedure TProtoParser.InternalProcessControl(R: TProtoObject);

procedure DoChangeSintax(APS:TParserSyntax);
const
  PSN : array [TParserSyntax] of string = ('proto 2', 'proto 3');
begin
  FParserSyntax:=APS;
  InternalStatus(nil, Format('Change syntax to %s', [PSN[APS]]));
end;

begin
  if (FState <> cmsNormal) then exit;
  if (R is TSyntax) then
    DoChangeSintax(TSyntax(R).ProtoSyntax);
end;

procedure TProtoParser.SetState(AValue: TCMDState);
begin
  if FState=AValue then Exit;
  FState:=AValue;
end;

procedure TProtoParser.Parse(ACmd:TProtoObject; AStartToken:TProtoToken);
var
  Stop: Boolean;
  S: String;
  T, Child, CE: TProtoToken;
  TT: TProtoTokenType;
  i: Integer;
begin
  FState:=cmsNormal;
  Stop:=false;
  T:=AStartToken;
  if not Assigned(T) then
    T:=ACmd.FStartTokens[0];
  S:=GetNextWord;
  if not T.Equal(S) then exit;
  ACmd.InternalProcessChildToken(Self, T, S);

  repeat
    S:=GetNextWord;
    TT:=WordType(T, S, ACmd);

    case TT of
      stString,
      stInteger,
      stFloat,
      stIdentificator:Child:=T.ChildByType(TT, S);
      stSymbol,
      stTerminator,
      stKeyword:Child:=T.ChildByName(S);
    else
      Child:=nil;
    end;


    if Assigned(Child) then
      ACmd.InternalProcessChildToken(Self, Child, S)
    else
    begin
      begin
        Stop:=true;
        if T.Child.Count > 0 then
        begin
          FErrorMessage:='';
          FErrorPosition.X:=0;
          FErrorPosition.Y:=0;
          for CE  in T.Child do
          begin
            if (not (toOptional in CE.Options)) or ((S <> CommandDelemiter) and (S <> '')) then
            begin
              if FErrorMessage <> '' then FErrorMessage:=FErrorMessage + ', ';

              if CE.TokenType in [stSymbol, stTerminator, stKeyword] then
                FErrorMessage:=FErrorMessage + CE.FToken
              else
              if CE.TokenType = stString then
                FErrorMessage:=FErrorMessage + sProtoParserString
              else
              if CE.TokenType in [stInteger, stFloat] then
                FErrorMessage:=FErrorMessage + sProtoParserNumber
              else
              if CE.TokenType = stIdentificator then
              begin
                FErrorMessage:=FErrorMessage + sProtoParserIdentificator;
                if CE.Token <> '' then
                  FErrorMessage:=FErrorMessage + ' (' + CE.Token + ')';
              end;
            end;
          end;
          if FErrorMessage <> '' then
            SetError(sProtoParserExpected + FErrorMessage);
        end;
      end;
    end;
    T:=Child;
  until Stop or (FState in [cmsError, cmsEndOfCmd]);
end;

procedure TProtoParser.Compile(AFileName: string);
var
  F: TFileStream;
begin
  F:=TFileStream.Create(AFileName, fmOpenRead);
  Compile(F);
  F.Free;
end;

procedure TProtoParser.Compile(AStream: TStream);
begin
  Clear;
  AStream.Position:=0;
  SetLength(FBuffer, AStream.Size);
  AStream.Read(FBuffer[1], AStream.Size);
  InternalCompile;
end;

procedure TProtoParser.CompileStr(S: string);
begin
  Clear;
  FBuffer:=S;
  InternalCompile;
end;

initialization
  RegisterProtoObject(TSyntax);
end.

