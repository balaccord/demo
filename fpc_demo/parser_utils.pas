unit parser_utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, cef3types, contnrs, RegExpr, cef3intf, cef3lib, cef3own, syncobjs, LCLProc,
  LazFileUtils, EpikTimer, Forms, LCLIntf, sqldb;

type

  { TRegExprExt }

  TRegExprExt = class(TRegExpr)
  public
    function IsMatch(const ReExpr, Str: String): Boolean; {$ifdef SYSTEMINLINE} inline; {$endif} // сокращение
    function RemoveAll(const Str, ReExpr:String):String; // удаляет все регулярные вхождения из строки
    function Dollar1(const Str, ReExpr:String):String; // возвращает $1
  end;

  { TStrHashExt }

  // хеш строковых данных, собранных при парсинге сайта
  TStrHashExt = class(TFPStringHashTable)
    function RemoveAll(const RE: TRegExprExt; const Str, ReExpr:String):TStrHashExt;
    function SetDef(const Key, Val: String):TStrHashExt;
  end;

  { TCuteSQLQuery }

  // где будет список полей при вызове ListToStr:
  // SELECT {flpSelect} ... WHERE {flpWhere}
  // INSERT INTO ... (flpSelect) VALUES {flpInsert}
  TFieldListPosition = (flpSelect, flpInsert, flpWhere);

  // Расширенный функционал для TQuery с удобными обёртками методов
  // Так как методы не перекрыты, то классом можно пользоваться через typecast
  TCuteSQLQuery = class(TSQLQuery)
  private
    procedure SetSelect(const ATableName, SelectList: String; const AFieldNames: array of String);
  public
    function SelectOrInsert(const SelSQL:String; const SelParams: array of String;
      const InsSQL:String; const InsParams: array of String):TCuteSQLQuery;
    function SelOrIns(const ATableName, SelectList:String; const AFieldNames, AParams: array of String):TCuteSQLQuery;
    function SelCute(const ATableName, SelectList:String; const AFieldNames, AParams: array of String):TCuteSQLQuery;
    function InsCute(const ATableName:String; const AFieldNames, AParams: array of String):TCuteSQLQuery;

    class function ToSQL(const FieldNames: array of String; Where: TFieldListPosition): String;
    class function ToSelect(const FieldNames: array of String): String; {$ifdef SYSTEMINLINE} inline; {$endif}
    class function ToInsert(const FieldNames: array of String): String; {$ifdef SYSTEMINLINE} inline; {$endif}
    class function ToWhere(const FieldNames: array of String): String; {$ifdef SYSTEMINLINE} inline; {$endif}

    function OpenCute:TCuteSQLQuery; {$ifdef SYSTEMINLINE} inline; {$endif}
    function CloseCute:TCuteSQLQuery; {$ifdef SYSTEMINLINE} inline; {$endif}
    function ExeCute:TCuteSQLQuery; {$ifdef SYSTEMINLINE} inline; {$endif}

    function GetField(const FieldName:String; out Value: LongInt):TCuteSQLQuery; {$ifdef SYSTEMINLINE} inline; {$endif}
    function GetField(FieldOrder:Integer; out Value: LongInt):TCuteSQLQuery; {$ifdef SYSTEMINLINE} inline; {$endif}
    function GetField(const FieldName:String; out Value: String):TCuteSQLQuery; {$ifdef SYSTEMINLINE} inline; {$endif}
    function GetField(FieldOrder:Integer; out Value: String):TCuteSQLQuery; {$ifdef SYSTEMINLINE} inline; {$endif}

    function IsEmptyCute(out Value: Boolean):TCuteSQLQuery; {$ifdef SYSTEMINLINE} inline; {$endif}
    function IsNotEmptyCute(out Value: Boolean):TCuteSQLQuery; {$ifdef SYSTEMINLINE} inline; {$endif}

    //function SetField(const FieldName:String; Value: LongInt):TCuteSQLQuery;
    //function SetField(FieldOrder:Integer; Value: LongInt):TCuteSQLQuery;
    //function SetField(const FieldName, Value:String):TCuteSQLQuery;
    //function SetField(FieldOrder:Integer; const Value: String):TCuteSQLQuery;

    function SetParam(const FieldName:String; Value: LongInt):TCuteSQLQuery; {$ifdef SYSTEMINLINE} inline; {$endif}
    function SetParam(FieldOrder:Integer; Value: LongInt):TCuteSQLQuery; {$ifdef SYSTEMINLINE} inline; {$endif}
    function SetParam(const FieldName, Value:String):TCuteSQLQuery; {$ifdef SYSTEMINLINE} inline; {$endif}
    function SetParam(FieldOrder:Integer; const Value: String):TCuteSQLQuery; {$ifdef SYSTEMINLINE} inline; {$endif}

    function SetConn(Conn: TSQLConnection):TCuteSQLQuery; {$ifdef SYSTEMINLINE} inline; {$endif}
    function SetSQLText(const aSQL: String):TCuteSQLQuery; {$ifdef SYSTEMINLINE} inline; {$endif}

    function SetAllParams(Info: TStrHashExt; const FieldNames: Array of String):TCuteSQLQuery;
    function SetAllParams(const Values: Array of String):TCuteSQLQuery;

    function StartTransaction: TCuteSQLQuery; {$ifdef SYSTEMINLINE} inline; {$endif}
    function CommitCute: TCuteSQLQuery; {$ifdef SYSTEMINLINE} inline; {$endif}
    function CommitRetainingCute: TCuteSQLQuery; {$ifdef SYSTEMINLINE} inline; {$endif}
    //function CommitSqlite3: TCuteSQLQuery; {$ifdef SYSTEMINLINE} inline; {$endif}
    function Rollback: TCuteSQLQuery; {$ifdef SYSTEMINLINE} inline; {$endif}
  end;

  { TLogger }

  TLogger = class(TComponent)
  private
    FFileName: AnsiString;
  public
    constructor Create(AOwner: TComponent; const AFileName: String); overload;
    //destructor Destroy; override;
    procedure Write(Msg: String);
    procedure Write(Msg: String; const Params: array of const); {$ifdef SYSTEMINLINE} inline; {$endif}
  end;

// ustring -> string
function us2s(const str: ustring): String; {$ifdef SYSTEMINLINE} inline; {$endif}
// чтение из файла в переменную
function ReadFileMy(const filename:String):String;
// создание файла из переменной
procedure WriteFileMy(const filename, s:String);
//* procedure Log(const file_: String; line, severity: Integer; const message: String);
procedure LogPid(const Msg:String);
procedure LogPid(const Fmt:String; const Params:Array of Const);
function DbgCallerName: String;

var
  Logger: TLogger;
  HiresTimer: TEpikTimer;

implementation

// https://arbinada.com/ru/node/1419

// http://forum.lazarus.freepascal.org/index.php?topic=33534.0
// https://www.freepascal.org/docs-html/rtl/lineinfo/getlineinfo.html
// getlineinfo
function DbgCallerName: String;
var
  i: Longint;
  PrevBP: Pointer;
  CallerFrame, CallerAddr, BP: Pointer;
begin
  // get N-2 caller proc name
  BP := get_frame;
  PrevBP := BP - 1;
  i := 0;
  while BP > PrevBP do
  begin
    CallerAddr := get_caller_addr(BP);
    CallerFrame := get_caller_frame(BP);
    if (CallerAddr = nil) or (CallerFrame = nil) then
      break;
    if i = 1 then
    begin
      //Result := BackTraceStrFunc(CallerAddr);
      Result := GetLineInfo(CallerAddr, True);
      break;
    end;
    PrevBP := BP;
    BP := CallerFrame;
    Inc(i);
  end;
end;

procedure LogPid(const Msg:String);
var
  s: String;
  ms: String;
  func: String;
  FCrt: TCriticalSection;
begin
  func := DbgCallerName;
//  GetTickCount;
  ms := HiresTimer.ElapsedStr;
  Delete(ms, 1, Pos(DefaultFormatSettings.DecimalSeparator, ms));
//  ms := Format('', [Frac(HiresTimer.Elapsed * 1000000)]);
  s := Format('[%s.%s pid:%d tid:%d] %s'^M^J^I^I'%s'^M^J, [
    FormatDateTime('yyyy/mm/dd hh:nn:ss', Now), ms,
    GetProcessID, GetThreadID, func, Msg
  ]);

  FCrt := TCriticalSection.Create;
  try
    FCrt.Acquire;
    DbgOutThreadLog(s);
  finally
    FCrt.Release;
  end;
  FreeAndNil(FCrt);
end;

procedure LogPid(const Fmt: String; const Params: array of const);
begin
  LogPid(Format(Fmt, Params));
end;

function ReadFileMy(const filename:String):String;
var
  F:TFileStream;
begin
  try
    F := TFileStream.Create(filename, fmOpenRead, fmShareDenyNone);
    if not Assigned(F) then
      Raise Exception.Create('File not found');
    try
      SetLength(Result, F.Size+1);
      F.Read(Result[Low(Result)], F.Size);
      Result[High(Result)] := #0;
    except
      Result := '';
    end;
  finally
    FreeAndNil(F);
  end;
end;

procedure WriteFileMy(const filename, s:String);
var
  F:TFileStream;
begin
  try
    F := TFileStream.Create(filename, fmCreate, fmShareExclusive);
    if not Assigned(F) then
      Raise Exception.Create('File error');
    F.WriteAnsiString(s);
  finally
    FreeAndNil(F);
  end;
end;

{ TLogger }

procedure TLogger.Write(Msg: String; const Params: array of const); {$ifdef SYSTEMINLINE} inline; {$endif}
begin
  Write(Format(Msg, Params));
end;

constructor TLogger.Create(AOwner: TComponent; const AFileName: String);
begin
  inherited Create(AOwner);
  FFileName := AFileName;
end;

procedure TLogger.Write(Msg: String);
var
  F:TFileStream;
  FCrt: TCriticalSection;
begin
  FCrt := TCriticalSection.Create;
  try
    FCrt.Acquire;
    try
      if FileExistsUTF8(FFileName) then
        F := TFileStream.Create(FFileName, fmOpenReadWrite, fmShareDenyWrite)
      else
        F := TFileStream.Create(FFileName, fmCreate, fmShareDenyWrite);
      try
        F.WriteAnsiString(Format('[%s pid:%d tid:%d] %s'^M^J, [
          FormatDateTime('yyyy/mm/dd hh:nn:ss ', Now),
          GetProcessID, GetThreadID, Msg
        ]));
      finally
        FreeAndNil(F);
      end;
    finally
      FCrt.Release;
    end;
  finally
    FreeAndNil(FCrt);
  end;
end;

procedure _Log(const file_: String; line, severity: Integer; const message: String);
begin
// http://www.freepascal.org/docs-html/prog/progsu41.html#x47-460001.1.41
// Logger({$I %FILE%}, {$I %LINENUM%}, severity, message);
  CefLog(file_, line, severity, message);
end;

function us2s(const str: ustring): String; {$ifdef SYSTEMINLINE} inline; {$endif}
begin
  Result := String(str);
end;

{ TCuteSQLQuery }

function TCuteSQLQuery.OpenCute: TCuteSQLQuery; {$ifdef SYSTEMINLINE} inline; {$endif}
begin
  inherited Open;
  Result := Self;
end;

function TCuteSQLQuery.CloseCute: TCuteSQLQuery; {$ifdef SYSTEMINLINE} inline; {$endif}
begin
  inherited Close;
  Result := Self;
end;

function TCuteSQLQuery.ExeCute: TCuteSQLQuery; {$ifdef SYSTEMINLINE} inline; {$endif}
begin
  inherited ExecSQL;
  Result := Self;
end;

//function TCuteSQLQuery.SetField(const FieldName: String; Value: LongInt): TCuteSQLQuery;
//begin
//  Result := Self;
//  FieldByName(FieldName).AsInteger := Value;
//end;

//function TCuteSQLQuery.SetField(FieldOrder: Integer; Value: LongInt): TCuteSQLQuery;
//begin
//  Result := Self;
//  Fields[FieldOrder].AsInteger := Value;
//end;

//function TCuteSQLQuery.SetField(const FieldName, Value: String): TCuteSQLQuery;
//begin
//  Result := Self;
//  FieldByName(FieldName).AsString := Value;
//end;

//function TCuteSQLQuery.SetField(FieldOrder: Integer; const Value: String): TCuteSQLQuery;
//begin
//  Result := Self;
//  Fields[FieldOrder].AsString := Value;
//end;

function TCuteSQLQuery.GetField(const FieldName: String; out Value: LongInt): TCuteSQLQuery; {$ifdef SYSTEMINLINE} inline; {$endif}
begin
  Result := Self;
  Value := FieldByName(FieldName).AsInteger;
end;

function TCuteSQLQuery.GetField(FieldOrder: Integer; out Value: LongInt): TCuteSQLQuery; {$ifdef SYSTEMINLINE} inline; {$endif}
begin
  Result := Self;
  Value := Fields[FieldOrder].AsInteger;
end;

function TCuteSQLQuery.GetField(const FieldName: String; out Value: String): TCuteSQLQuery; {$ifdef SYSTEMINLINE} inline; {$endif}
begin
  Result := Self;
  Value := FieldByName(FieldName).AsString;
end;

function TCuteSQLQuery.GetField(FieldOrder: Integer; out Value: String): TCuteSQLQuery; {$ifdef SYSTEMINLINE} inline; {$endif}
begin
  Result := Self;
  Value := Fields[FieldOrder].AsString;
end;

function TCuteSQLQuery.IsEmptyCute(out Value: Boolean): TCuteSQLQuery;
begin
  Result := Self;
  Value := IsEmpty;
end;

function TCuteSQLQuery.IsNotEmptyCute(out Value: Boolean): TCuteSQLQuery;
begin
  Result := Self;
  Value := not IsEmpty;
end;

function TCuteSQLQuery.SetParam(const FieldName: String; Value: LongInt): TCuteSQLQuery; {$ifdef SYSTEMINLINE} inline; {$endif}
begin
  Result := Self;
  Params.ParamByName(FieldName).AsInteger := Value;
end;

function TCuteSQLQuery.SetParam(FieldOrder: Integer; Value: LongInt): TCuteSQLQuery; {$ifdef SYSTEMINLINE} inline; {$endif}
begin
  Result := Self;
  Params[FieldOrder].AsInteger := Value;
end;

function TCuteSQLQuery.SetParam(const FieldName, Value: String): TCuteSQLQuery; {$ifdef SYSTEMINLINE} inline; {$endif}
begin
  Result := Self;
  Params.ParamByName(FieldName).AsString := Value;
end;

function TCuteSQLQuery.SetParam(FieldOrder: Integer; const Value: String): TCuteSQLQuery; {$ifdef SYSTEMINLINE} inline; {$endif}
begin
  Result := Self;
  Params[FieldOrder].AsString := Value;
end;

function TCuteSQLQuery.SetConn(Conn: TSQLConnection): TCuteSQLQuery; {$ifdef SYSTEMINLINE} inline; {$endif}
begin
  Result := Self;
  SQLConnection := Conn;
end;

function TCuteSQLQuery.SetSQLText(const aSQL: String): TCuteSQLQuery; {$ifdef SYSTEMINLINE} inline; {$endif}
begin
  Result := Self;
  SQL.Text := aSQL;
end;

function TCuteSQLQuery.SetAllParams(Info: TStrHashExt; const FieldNames: array of String): TCuteSQLQuery;
var
  s: String;
begin
  Result := Self;
  for s in FieldNames do
  begin
    if Info.Find(s) = nil then
      raise Exception.CreateFmt('Не заполнен параметр %s', [s]);
    ParamByName(s).AsString := Info[s];
  end;
end;

function TCuteSQLQuery.SetAllParams(const Values: array of String): TCuteSQLQuery;
var
  i: Integer;
begin
  Result := Self;
  for i := Low(Values) to High(Values) do
    SetParam(i, Values[i]);
end;

function TCuteSQLQuery.StartTransaction: TCuteSQLQuery; {$ifdef SYSTEMINLINE} inline; {$endif}
begin
  Result := Self;
  SQLConnection.Transaction.StartTransaction;
end;

function TCuteSQLQuery.CommitCute: TCuteSQLQuery; {$ifdef SYSTEMINLINE} inline; {$endif}
begin
  Result := Self;
  //LogPid('TCuteSQLQuery.Commit');
  SQLConnection.Transaction.Commit;
end;

function TCuteSQLQuery.CommitRetainingCute: TCuteSQLQuery; {$ifdef SYSTEMINLINE} inline; {$endif}
begin
  Result := Self;
  //LogPid('TCuteSQLQuery.Commit');
  SQLConnection.Transaction.CommitRetaining;
end;

//function TCuteSQLQuery.CommitSqlite3: TCuteSQLQuery;
//var
//  Waiting: Boolean = True;
//  msg: String;
//
//procedure CheckCommit;
//begin
//  try
//    Commit;
//    Waiting := False;
//  except
//    on E:Exception do
//    begin
//      msg := E.Message;
//      if E.Message = 'database is locked' then
//        Sleep(250)
//      else
//        raise;
//    end;
//  end;
//end;
//
//begin
//  Result := Self;
//  while Waiting do
//    CheckCommit;
//end;

function TCuteSQLQuery.Rollback: TCuteSQLQuery; {$ifdef SYSTEMINLINE} inline; {$endif}
begin
  Result := Self;
  SQLConnection.Transaction.Rollback;
end;

function TCuteSQLQuery.SelectOrInsert(const SelSQL: String; const SelParams: array of String;
  const InsSQL: String; const InsParams: array of String): TCuteSQLQuery;
begin
  Result := Self;

  SetSQLText(SelSQL);
  SetAllParams(SelParams);
  Open;
  if not IsEmpty then
    Exit;
  SetSQLText(InsSQL);
  SetAllParams(InsParams);
  ExeCute;
  SetSQLText(SelSQL);
  SetAllParams(SelParams);
  Open;
end;

procedure TCuteSQLQuery.SetSelect(const ATableName, SelectList: String; const AFieldNames: array of String);
begin
  SetSQLText('SELECT '+SelectList+' FROM '+ATableName+' WHERE '+ToWhere(AFieldNames));
  LogPid(SQL.Text);
end;

function TCuteSQLQuery.SelCute(const ATableName, SelectList: String;
  const AFieldNames, AParams: array of String): TCuteSQLQuery;
begin
  Result := Self;

  SetSelect(ATableName, SelectList, AFieldNames);
  SetAllParams(AParams);
  Open;
end;

function TCuteSQLQuery.InsCute(const ATableName: String;
  const AFieldNames, AParams: array of String): TCuteSQLQuery;
begin
  Result := Self;

  SetSQLText('INSERT INTO '+ATableName+' ('+ToSelect(AFieldNames)+') VALUES ('+ToInsert(AFieldNames)+')');
  SetAllParams(AParams);
  ExeCute;
end;

function TCuteSQLQuery.SelOrIns(const ATableName, SelectList: String; const AFieldNames,
  AParams: array of String): TCuteSQLQuery;
begin
  Result := Self;

  SelCute(ATableName, SelectList, AFieldNames, AParams);
  if not IsEmpty then
    Exit;
  InsCute(ATableName, AFieldNames, AParams);
  SelCute(ATableName, SelectList, AFieldNames, AParams);
end;

class function TCuteSQLQuery.ToSQL(const FieldNames: array of String; Where: TFieldListPosition): String;
var
  s: String;
begin
  Result := '';
  for s in FieldNames do
    case Where of
      flpSelect:
        Result += s + ',';
      flpInsert:
        Result += ':' + s + ',';
      flpWhere:
        Result += s + '=:' + s + ' AND ';
    end;
  if Result <> '' then
  case Where of
    flpSelect, flpInsert:
      SetLength(Result, Length(Result)-1);
    flpWhere:
      SetLength(Result, Length(Result)-5);
  end;
end;

class function TCuteSQLQuery.ToSelect(const FieldNames: array of String): String; {$ifdef SYSTEMINLINE} inline; {$endif}
begin
  Result := ToSQL(FieldNames, flpSelect);
end;

class function TCuteSQLQuery.ToInsert(const FieldNames: array of String): String; {$ifdef SYSTEMINLINE} inline; {$endif}
begin
  Result := ToSQL(FieldNames, flpInsert);
end;

class function TCuteSQLQuery.ToWhere(const FieldNames: array of String): String; {$ifdef SYSTEMINLINE} inline; {$endif}
begin
  Result := ToSQL(FieldNames, flpWhere);
end;

{ TRegExprExt }

function TRegExprExt.IsMatch(const ReExpr, Str: String): Boolean; {$ifdef SYSTEMINLINE} inline; {$endif}
begin
  Expression := ReExpr;
  Result := Exec(Str);
end;


function TRegExprExt.RemoveAll(const Str, ReExpr: String): String;
begin
  Result := Str;
  Expression := ReExpr;
  while Exec(Result) do
    System.Delete(Result, MatchPos[0], MatchLen[0]);
end;

function TRegExprExt.Dollar1(const Str, ReExpr: String): String;
begin
  Expression := ReExpr;
  Exec(Str);
  if MatchPos[1] > 0 then
    Result := Match[1]
  else
    Result := '';
end;

{ TStrHashExt }

function TStrHashExt.RemoveAll(const RE: TRegExprExt; const Str, ReExpr: String):TStrHashExt;
var
  node: THTStringNode;
begin
  Result := Self;

  node := THTStringNode(Find(Str));
  if Assigned(node) then
    node.Data := RE.RemoveAll(node.Data, ReExpr);
end;

function TStrHashExt.SetDef(const Key, Val: String): TStrHashExt;
begin
  Result := Self;

  if not Assigned(Find(Key)) then
    Add(Key, Val);
end;

initialization

Logger := TLogger.Create(Application, ChangeFileExt(ParamStr(0), '.log'));
HiresTimer := TEpikTimer.Create(Application);
HiresTimer.Start;

end.

