unit parsers;

{$mode objfpc}{$H+}
{.$IMPLICITEXCEPTIONS OFF}

interface

uses
  Classes, windows, SysUtils, parser_utils, xquery, simplehtmltreeparser, sqlite3conn, RegExpr, URIParser, StdCtrls,
  sqldb, Forms, cef3lcl, cef3types, dateutils;

type

// исключение парсинга
EParseException = class(Exception);
EBrowserException = class(Exception);

IUrlLoader = interface
  function LoadUrl(const Url:String; const Referer:String = ''; Timeout: Integer = 20): Boolean;
  function GetHtml: String;
  function GetChromium: TChromium;
  function GetErrorText: String;
  function GetErrorCode: TCefErrorCode;
  function StopFlag: Boolean;
end;

{ THtmlParser }

THtmlParser = class
strict private
  FTP: TTreeParser;
  FXQE: TXQueryEngine;
  FReferer, FUrl, FHtml: String;
public
  constructor Create(const Html: String = ''; const Url: String = ''; const ContentType: String = 'charset=utf-8');
  destructor Destroy; override;
  function UrlToAbs(const AUrl:String):String; {$ifdef SYSTEMINLINE} inline; {$endif}
  class function UrlToAbs(const ABase, AUrl:String):String; static; {$ifdef SYSTEMINLINE} inline; {$endif}
  function Parse(const Html: String = ''; const Url: String = ''; const ContentType: String = 'charset=utf-8'): TTreeDocument;
  function GetIXQV(const CSS, ErrMsg:String):IXQValue;
  function GetIXQV(IXQV:IXQValue; const CSS, ErrMsg:String):IXQValue;
  function GetIXQV(TreeDoc:TTreeDocument; const CSS, ErrMsg:String):IXQValue;
  property Referer: String read FReferer write FReferer;
  property Url: String read FUrl;
  property Html: String read FHtml;
end;

{ TSiteParser }

// базовый класс для парсеров
TSiteParser = class
strict private
  FInfo: TStrHashExt; //
  //FXQE: TXQueryEngine; //
  //FTP: TTreeParser; //
  FSortedInfoKeys: TStringList;
  FRE: TRegExprExt; //
  q: TCuteSQLQuery;
  FDBClassName: String;
  function GetInfo(Key: String): String;
  procedure SetInfo(Key: String; AValue: String);
  // из-за отсутствия сортировки хеша и безымянных процедур и замыканий требуется такой вот метод
  // для получения сортированного списка ключей, см. SyncSortedInfoKeys
  procedure FillSortedInfoKeys(Item: String; const Key: string; var Continue: Boolean);
strict protected
  // парсинг страницы-списка, создаём парсер, вызываем ParseListPage, уничтожаем парсер
  function ParseListPageInternal(const Url, Html:String):String;
  // На выходе надо отдать адрес следующей страницы. Если строка пустая, тогда конец цикла.
  function ParseListPage(Parser: THtmlParser):String; virtual;
  // парсинг страницы-описания, метод должен быть перекрыт в наследниках!!!
  procedure ParseInfoPage(Parser: THtmlParser); virtual;
  // необязательный метод очистки извлечённых данных, например, дат и адресов
  procedure Cleanup(Parser: THtmlParser); virtual;
protected
  // Парсинг страницы-списка, парсер уже содержит в себе дерево.
  procedure QueueInfoPage(const Referer, Url: String);
  // на многих сайтах есть списки вида "ключ -> значение" и надо найти соответствие
  // ключа на сайте и ключа Info (поля в базе данных)
  procedure ReAssignTuples(const ReExpr, Str: String; Tuples: Array of String);
  // отладочный вывод извлечённых данных
  procedure DumpInfo;
  // запись изменений в базу
  procedure UpdateDB;
  // см FillSortedInfoKeys
  procedure SyncSortedInfoKeys;
  // результат строковый вместо целочисленного до лучших времён, SQLite позволяет такие выкрутасы
  function GetRegionId(const s:String):String;
  // результат строковый вместо целочисленного до лучших времён, SQLite позволяет такие выкрутасы
  function GetCityId(const RegionId, s:String):String;
  // результат строковый вместо целочисленного до лучших времён, SQLite позволяет такие выкрутасы
  function GetDistrictId(const CityId, s:String):String;
  //function IsStored(const Url:String):Boolean;
  // пауза после загрузки страницы, чтоб не забанили, лучше, наверное, делать случайной длительности
  // возвращаемый результат: True - продолжить цинл, False - прервать
  function Pause(SecondsFrom:  Integer = 0; SecondsTo: Integer = 1): Boolean; virtual;
public
  constructor Create(SQLConn: TSQLConnection; const ADBClassName: String; const Url:String = '');
  destructor Destroy; override;
  // основная точка входа - запуск парсинга
  procedure ParseStart(StartUrl:String; Timeout: Integer = 20);
  // простая обёртка для if not condition then raise
  procedure RaiseCheck(Condition: Boolean; const ErrMsg: String);
  // упрощённый синтаксис для запихивания данных в хеш
  function HashIt(Parser: THtmlParser; const Key, CSS, ErrMsg: String): IXQValue;
  function HashIt(Parser: THtmlParser; IXQV:IXQValue; const Key, CSS, ErrMsg: String): IXQValue;
  function HashIt(Parser: THtmlParser; TreeDoc:TTreeDocument; const Key, CSS, ErrMsg: String): IXQValue;
  property Info[Key:String]:String read GetInfo write SetInfo;
  property DBClassName: String read FDBClassName;
  property InfoHash: TStrHashExt read FInfo;
  property RE: TRegExprExt read FRE;
end;

{ TDomofondParser }

TDomofondParser = class(TSiteParser)
strict protected
  function ParseListPage(Parser: THtmlParser):String; override;
  procedure ParseInfoPage(Parser: THtmlParser); override;
  procedure Cleanup(Parser: THtmlParser); override;
protected
  function Pause(SecondsFrom:  Integer = 0; SecondsTo: Integer = 1): Boolean; override;
public
  constructor Create(SQLConn: TSQLConnection; const Url:String = '');
end;

var
  Memo: TMemo;
  UrlLoader: IUrlLoader;
{$hint VERY-VERY DIRTY HACK!!!!!!!}

implementation

//uses Main;

{ THtmlParser }

constructor THtmlParser.Create(const Html: String; const Url: String; const ContentType: String = 'charset=utf-8');
begin
  FXQE := TXQueryEngine.Create;
  FTP := TTreeParser.Create;
  FTP.parsingModel := pmHTML;
  //FTP.trimText := True;
  //FTP.autoDetectHTMLEncoding := False;
  FUrl := Url;
  FHtml := Html;
  FReferer := FUrl;
  FTP.TargetEncoding := CP_UTF8;
  if Html = '' then
    Exit;

  try
    FTP.parseTree(Html, Url, ContentType);
    //if Assigned(FTP.getLastTree) then
    //  FTP.getLastTree.setEncoding(CP_UTF8, False, True);
  except
    on E:Exception do
      raise EParseException.Create(E.Message);
  end;
end;

destructor THtmlParser.Destroy;
begin
  FreeAndNil(FTP);
  FreeAndNil(FXQE);
  inherited;
end;

function THtmlParser.UrlToAbs(const AUrl: String): String; {$ifdef SYSTEMINLINE} inline; {$endif}
begin
  ResolveRelativeURI(FUrl, AUrl, Result);
end;

class function THtmlParser.UrlToAbs(const ABase, AUrl: String): String; {$ifdef SYSTEMINLINE} inline; {$endif}
begin
  ResolveRelativeURI(ABase, AUrl, Result);
end;

function THtmlParser.Parse(const Html: String; const Url: String; const ContentType: String): TTreeDocument;
begin
  FUrl := Url;
  FHtml := Html;
  FReferer := FUrl;
  try
    Result := FTP.parseTree(Html, Url, ContentType);
  except
    on E:Exception do
      raise EParseException.Create(E.Message);
  end;
end;

function THtmlParser.GetIXQV(const CSS, ErrMsg: String): IXQValue;
begin
  Result := FXQE.evaluateCSS3(CSS, FTP.getLastTree);
  if not Assigned(Result) then
    raise EParseException.Create(errmsg);
end;

function THtmlParser.GetIXQV(IXQV: IXQValue; const CSS, ErrMsg: String): IXQValue;
begin
  Result := FXQE.evaluateCSS3(CSS, IXQV);
  if not Assigned(Result) then
    raise EParseException.Create(errmsg);
end;

function THtmlParser.GetIXQV(TreeDoc: TTreeDocument; const CSS, ErrMsg: String): IXQValue;
begin
  Result := FXQE.evaluateCSS3(CSS, TreeDoc);
  if not Assigned(Result) then
    raise EParseException.Create(errmsg);
end;

{ TSiteParser }

procedure TSiteParser.FillSortedInfoKeys(Item: String; const Key: string;
  var Continue: Boolean);
begin
  FSortedInfoKeys.Add(Key);
  Continue := True;
end;

function TSiteParser.GetInfo(Key: String): String;
begin
  Result := FInfo[Key];
end;

procedure TSiteParser.SetInfo(Key: String; AValue: String);
begin
  if Assigned(FInfo.Find(Key)) then
    FInfo.Delete(Key);
  FInfo[Key] := AValue;
end;

function TSiteParser.ParseListPageInternal(const Url, Html: String): String;
var
  Parser: THtmlParser;
begin
  Result := '';
  FInfo.Clear;
  Info['_url'] := Url;
  Memo.Lines.Clear;
  Parser := THtmlParser.Create;
  try
    Parser.Parse(Html, Url);
    Result := ParseListPage(Parser);
  finally
    FreeAndNil(Parser);
  end;
  Pause;
end;

procedure TSiteParser.ParseInfoPage(Parser: THtmlParser);
begin

end;

procedure TSiteParser.QueueInfoPage(const Referer, Url: String);
var
  Parser: THtmlParser;
var
  Html: String;
begin
  Parser := THtmlParser.Create;
  try
    //Html := ReadFileMy('Q:\Games\estate_informer\internettools_test\domofond_page.html');
    UrlLoader.LoadUrl(Url, Referer, 30);
    if UrlLoader.GetErrorCode <> 200 then
      raise EBrowserException.CreateFmt('"%s": ошибка %d, %s', [
        Url, UrlLoader.GetErrorCode, UrlLoader.GetErrorText
      ]);
    Html := UrlLoader.GetHtml;
    Parser.Parse(Html, Info['url']);
    Parser.Referer := Referer;
    ParseInfoPage(Parser);
    Cleanup(Parser);
    DumpInfo;
    UpdateDB;
  finally
    FreeAndNil(Parser);
  end;
  Pause;
end;

procedure TSiteParser.Cleanup(Parser: THtmlParser);
begin

end;

constructor TSiteParser.Create(SQLConn: TSQLConnection;
  const ADBClassName: String; const Url: String);
begin
  FDBClassName := ADBClassName;
  FInfo := TStrHashExt.Create;
  FSortedInfoKeys := TStringList.Create;
  RegExprModifierS := True;
  FRE := TRegExprExt.Create;
  q := TCuteSQLQuery.Create(nil).SetConn(SQLConn);

  FSortedInfoKeys.Sorted := True;

  if Url <> '' then
    ParseStart(Url);
end;

destructor TSiteParser.Destroy;
begin
  FreeAndNil(q);
  FreeAndNil(FRE);
  FreeAndNil(FSortedInfoKeys);
  FreeAndNil(FInfo);
  inherited;
end;

procedure TSiteParser.ParseStart(StartUrl: String; Timeout: Integer = 20);
var
  Html, NextUrl: String;
begin
  //Html := ReadFileMy('Q:\Games\estate_informer\internettools_test\domofond.html');
  repeat
    UrlLoader.LoadUrl(StartUrl, '', 30);
    if UrlLoader.GetErrorCode <> 200 then
      raise EBrowserException.CreateFmt('"%s": ошибка %d, %s', [
        StartUrl, UrlLoader.GetErrorCode, UrlLoader.GetErrorText
      ]);
    Html := UrlLoader.GetHtml;
    NextUrl := ParseListPageInternal(StartUrl, Html);
    if (NextUrl = '') or UrlLoader.StopFlag then
      break;
    StartUrl := THtmlParser.UrlToAbs(StartUrl, NextUrl);
  until False;
end;

procedure TSiteParser.RaiseCheck(Condition: Boolean; const ErrMsg: String);
begin
  if not Condition then
    raise EParseException.Create(ErrMsg);
end;

function TSiteParser.HashIt(Parser: THtmlParser; const Key, CSS, ErrMsg: String): IXQValue;
begin
  Result := Parser.GetIXQV(CSS, ErrMsg);
  if Assigned(FInfo.Find(Key)) then
    FInfo.Delete(Key);
  FInfo[Key] := Result.toString
end;

function TSiteParser.HashIt(Parser: THtmlParser; IXQV: IXQValue; const Key, CSS, ErrMsg: String): IXQValue;
begin
  Result := Parser.GetIXQV(IXQV, CSS, ErrMsg);
  if Assigned(FInfo.Find(Key)) then
    FInfo.Delete(Key);
  FInfo[Key] := Result.toString
end;

function TSiteParser.HashIt(Parser: THtmlParser; TreeDoc: TTreeDocument; const Key, CSS, ErrMsg: String): IXQValue;
begin
  Result := Parser.GetIXQV(TreeDoc, CSS, ErrMsg);
  if Assigned(FInfo.Find(Key)) then
    FInfo.Delete(Key);
  FInfo[Key] := Result.toString
end;

procedure TSiteParser.ReAssignTuples(const ReExpr, Str: String; Tuples: array of String);
var
  k: String;
  i: Integer;
  found: Boolean;
begin
  FRE.Expression := ReExpr;
  FRE.Exec(Str);
  while FRE.MatchPos[0] > 0 do begin
    k := FRE.Match[1];
    found := False;
    for i := 0 to High(Tuples) mod 2 do
    begin
      if Tuples[i*2] <> k then
        continue;
      Info[Tuples[i*2+1]] := FRE.Match[2];
      found := True;
      break;
    end;
    if not found then
      raise Exception.CreateFmt('Неизвестный параметр "%s"', [k]);
    FRE.ExecNext;
  end;
end;

procedure TSiteParser.DumpInfo;
var
  i: Integer;
  s: String;
begin
  SyncSortedInfoKeys;
  //Memo.Lines.Add('==========================================');
  //for i := 0 to FSortedInfoKeys.Count-1 do
  //  Memo.Lines.Add(FSortedInfoKeys[i] + ': "' + Info[FSortedInfoKeys[i]] + '"');
  s := '==========================================';
  for i := 0 to FSortedInfoKeys.Count-1 do
    s += ^M^J + FSortedInfoKeys[i] + ': "' + Info[FSortedInfoKeys[i]] + '"';
  LogPid(s);
end;

procedure TSiteParser.SyncSortedInfoKeys;
begin
  FSortedInfoKeys.Clear;
  FInfo.Iterate(@FillSortedInfoKeys);
end;

function TSiteParser.GetRegionId(const s: String): String;
begin
  q.SelOrIns('regions', 'region_id', ['region_name'], [s])
  .GetField(0, Result)
  .Close;
end;

function TSiteParser.GetCityId(const RegionId, s: String): String;
begin
  q.SelOrIns('cities', 'city_id', ['region_id', 'city_name'], [RegionId, s])
  .GetField(0, Result)
  .Close;
end;

function TSiteParser.GetDistrictId(const CityId, s: String): String;
begin
  q.SelOrIns('districts', 'district_id', ['city_id', 'district_name'], [CityId, s])
  .GetField(0, Result)
  .Close;
end;

function TSiteParser.Pause(SecondsFrom: Integer; SecondsTo: Integer): Boolean;
var
  FStopTime: Int64;
  rnd: Integer;
begin
  rnd := Round(Random * (SecondsTo - SecondsFrom));
  LogPid('Wait: %d', [SecondsFrom + rnd]);
  FStopTime := DateTimeToUnix(Now) + SecondsFrom + rnd;
  repeat
    Sleep(50);
    Application.ProcessMessages;
  until UrlLoader.StopFlag or (DateTimeToUnix(Now) >= FStopTime);
  Result := False;
end;

//function TSiteParser.IsStored(const Url: String): Boolean;
//begin
//  q.SelCute('estates', 'estate_id', ['url'], [Url])
//    .IsNotEmptyCute(Result).
//    CloseCute;
//end;

function TSiteParser.ParseListPage(Parser: THtmlParser): String;
begin
  Result := '';
end;

procedure TSiteParser.UpdateDB;
const
  Fields: Array[0..25] of String = (
    'region_id', 'site_id', 'city_id', 'district_id', 'person', 'street', 'street_no',
    'house_type_id', 'flat_type_id', 'site_pub_id', 'title', 'full_desc', 'creation_time',
    'publication_time', 'url', 'total_sqaure', 'living_square', 'kitchen_square', 'rooms_count',
    'storeys', 'storey', 'balcony', 'has_photos', 'bath_joint', 'other_info', 'debug_data'
  );
var
  estate_id, site_id, Url, Price, PriceNew: String;

procedure Insert;
begin
  q.SelCute('sites', 'site_id', ['site_class'], [DBClassName])
    .GetField(0, site_id)
    .CloseCute;
  Info['site_id'] := site_id;

  q.SetSQLText(Format('INSERT INTO estates (%s) VALUES (%s)', [q.ToSelect(Fields), q.ToInsert(Fields)]))
    .SetAllParams(FInfo, Fields)
    .ExeCute;

  q.SelCute('estates', 'estate_id', ['url'], [Url])
    .GetField(0, estate_id)
    .CloseCute;

  q.InsCute('seen_count',
    ['estate_id', 'seen_count'],
    [estate_id, Info['seen_count']]
  );

  q.InsCute('prices',
    ['estate_id', 'price'],
    [estate_id, PriceNew]
  );

  LogPid('Added url=%s', [Url]);
end;

procedure Update;
begin
  q.GetField(0, Price)
    .GetField(1, estate_id)
    .CloseCute
  ;

  q.InsCute('seen_count',
    ['estate_id', 'seen_count'],
    [estate_id, Info['seen_count']]
  );

  if Price <> PriceNew then
    q.InsCute('prices',
      ['estate_id', 'price'],
      [estate_id, PriceNew]
    );
  if Price > PriceNew then
    LogPid('Цена повысилась с %s до %s', [Price, PriceNew])
  else if Price < PriceNew then
    LogPid('Цена снизилась с %s до %s', [Price, PriceNew])
  else
    LogPid('Цена не изменилась', [Price, PriceNew])
end;

begin
  Url := Info['url'];
  PriceNew := Info['price'];

  try
    q.SetSQLText('SELECT p.price,p.estate_id FROM prices AS p,estates AS e WHERE p.estate_id=e.estate_id AND e.url=:url ORDER BY p.price_time DESC LIMIT 1')
      .SetParam(0, Url)
      .OpenCute;

    if q.IsEmpty then
      Insert
    else
      Update;

    q.CommitCute;
  except
    on E:Exception do
    begin
      q.Rollback;
      Application.MessageBox(PChar(E.Message), 'Ошибка записи', MB_OK + MB_ICONEXCLAMATION);
    end;
  end;
end;

{ TDomofondParser }

constructor TDomofondParser.Create(SQLConn: TSQLConnection; const Url: String);
begin
  inherited Create(SQLConn, 'domofond', Url);
end;

function TDomofondParser.ParseListPage(Parser: THtmlParser): String;
var
  v, lodge, link: IXQValue;
begin
  Result := '';

  Result := Parser.GetIXQV('div.b-pager > a:last-of-type', 'Не найдена ссылка на следующую страницу поиска').toNode['href'];

  HashIt(Parser, '_found_total', 'h4.g-no-margins', 'Не найден счётчик количества публикаций');
  HashIt(Parser, '_current_page', 'ul.e-pages > li.active > a', 'Не найден номер текущей страницы');
  Info['_found_current'] := '???'; // добавить

  if Result = 'javascript:;' then
    Result := '';
  Info['_next_page_url'] := Result;

  DumpInfo;

  v := Parser.GetIXQV('div#listingResults > div.b-results-tile', 'Список квартир пуст или ошибка парсинга');
  RaiseCheck(v.Count > 0, 'Список квартир пуст или ошибка парсинга');
  Memo.Lines.Add(Format('Найдено объявлений: %d', [v.Count]));
  for lodge in v do
  begin
    link := Parser.GetIXQV(lodge, 'a', 'Не найдена ссылка на описание');
    Info['url'] := Parser.UrlToAbs(link.toNode['href']);
    Info['title'] := link.toNode['aria-label'];

    Info['price'] := Parser.GetIXQV(lodge, 'h2[itemprop=price]', 'Не найдена цена').toString;
    Info['full_desc'] := Parser.GetIXQV(lodge, 'div[itemprop=description]', 'Не найдено полное описание ссылки').toNode.innerHTML;
    Info['address'] := Parser.GetIXQV(lodge, 'span[itemprop=address]', 'Не найден адрес').toNode.innerHTML;
    ReAssignTuples(
      '<div.*?>\s*(.+?):\s*<strong>(.+?)</strong>',
      Parser.GetIXQV(lodge, 'div.e-tile-size', 'Не найдена жилплошадь').toNode.innerHTML,
      [
      'Этаж', '_storey',
      'Пл',   'total_sqaure'
      ]
    );
    if Assigned(InfoHash.Find('_storey')) and RE.IsMatch('^\s*(\d+)/(\d+)', Info['_storey']) then
    begin
      Info['storey'] := RE.Match[1];
      Info['storeys'] := RE.Match[2];
      InfoHash.Delete('_storey');
    end;
    InfoHash.RemoveAll(RE, 'price', '\D+');
    InfoHash.RemoveAll(RE, 'total_sqaure', '[^\d\.,]+');

    DumpInfo;

    if UrlLoader.StopFlag then
      break;

    QueueInfoPage(Parser.Url, Info['url']);
    //Result := '';
    //break;
  end;
end;

procedure TDomofondParser.ParseInfoPage(Parser: THtmlParser);
begin
  //MainForm.FLoadUrl := ustring(Info['url']);
  //MainForm.FReferer := ustring(Info['_url']);
  //MainForm.LoadUrl(Parser.Url, Parser.Referer, 60);
  //MainForm.FReferer := '';
  //Exit;

  Info['region'] := 'Волгоградская область';
  Info['city'] := 'Волгоград';
  Info['district'] := 'Краснооктябрьский';

  Info['site_pub_id'] := RE.Dollar1(Parser.Html, '<strong>\s*Номер в каталоге:</strong>\s+(\d+)');
  Info['creation_time'] := RE.Dollar1(Parser.Html, '<strong>\s*Дата публикации объявления:</strong>\s+(\S+)');
  Info['publication_time'] := RE.Dollar1(Parser.Html, '<strong>\s*Дата обновления объявления:</strong>\s+(\S+)');
  Info['living_square'] := RE.Dollar1(Parser.Html, '<strong>\s*Жилая площадь.*?</strong>\s+([\d,\.]+)');
  Info['kitchen_square'] := RE.Dollar1(Parser.Html, '<strong>\s*Площадь кухни.*?</strong>\s+([\d,\.]+)');
  Info['rooms_count'] := RE.Dollar1(Parser.Html, '<strong>\s*Комнаты:</strong>\s+(\d+)');
  Info['full_desc'] := Parser.GetIXQV('div.b-listing-details p.m-listing-description', 'Не найдено полное описание').toNode.innerHTML;
end;

procedure TDomofondParser.Cleanup(Parser: THtmlParser);

procedure CleanupDate(const Field: String);
begin
  if RE.IsMatch('^(\d\d)/(\d\d)/(\d\d\d\d)$', Info[Field]) then
    Info[Field] := Format('00:00:00 %s-%s-%s', [RE.Match[1], RE.Match[2], RE.Match[3]]);
end;

begin
  Info['region_id'] := GetRegionId('Волгоградская область');
  InfoHash.Delete('region');

  Info['city_id'] := GetCityId(Info['region_id'], Info['city']);
  InfoHash.Delete('city');

  Info['district_id'] := GetDistrictId(Info['city_id'], Info['district']);
  InfoHash.Delete('district');

  Info['street'] := Info['address'];
  InfoHash.Delete('address');

  CleanupDate('creation_time');
  CleanupDate('publication_time');

  InfoHash.SetDef('house_type_id', '1')
    .SetDef('flat_type_id', '1')
    .SetDef('creation_time', '00:00:00 01-01-1970')
    .SetDef('publication_time', '00:00:00 01-01-1970')
    .SetDef('person', '')
    .SetDef('street_no', '')
    .SetDef('site_pub_id', '')
    .SetDef('living_square', '0')
    //.SetDef('kitchen_square', '0')
    .SetDef('has_photos', '0')
    .SetDef('bath_joint', '0')
    .SetDef('balcony', '0')
    .SetDef('seen_count', '0')
    .SetDef('other_info', '')
    .SetDef('debug_data', '')
  ;
end;

function TDomofondParser.Pause(SecondsFrom:  Integer = 0; SecondsTo: Integer = 1): Boolean;
begin
  Result := False;
  inherited Pause(15, 25);
end;

//var
//  i: Integer;
//
//finalization
//
//I := 1;

end.

