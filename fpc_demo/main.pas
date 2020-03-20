unit main;

{$i warnings.inc}

{$mode objfpc}{$H+}

interface

{$DEFINE DEBUG}

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, LazFileUtils,
  StdCtrls, cef3lcl, {cef3osr, cef3context, cef3gui, cef3ref, sqldblib,}
  cef3intf, cef3types, cef3lib, gettext, cef3own, sqlite3conn, sqlite3dyn,
  ctypes, sqldb, db, LCLProc, ExtCtrls, ComCtrls, DbCtrls, DBGrids, dateutils,
  Grids, strutils, LCLIntf, MaskEdit, Menus, xquery, simplehtmltreeparser,
  contnrs, URIParser, RegExpr, parsers, parser_utils, chromium_ext, syncobjs,
  LResources, XMLPropStorage, windows, Types, LazUTF8, variants, Buttons;

type

  { TStrObject }

  TStrObject = class
  private
    FCrt: syncobjs.TCriticalSection;
    FS: String;
    procedure SetS(AValue: String);
  public
    constructor Create(const s: String = '');
    destructor Destroy; override;
    property s: String read FS write SetS;
  end;

  { TMainForm }

  TMainForm = class(TForm, IUrlLoader)
    btGo: TButton;
    btBack: TButton;
    btForward: TButton;
    btShow: TButton;
    btClear: TButton;
    btCommentUpdate: TButton;
    cbStoreyNotLast: TCheckBox;
    cbPriceIsNull: TCheckBox;
    cbRatingFrom: TComboBox;
    cbRatingTo: TComboBox;
    crm: TChromium;
    DBGridPhones: TDBGrid;
    DBMemostreet: TDBMemo;
    DBMemo_street_no: TDBMemo;
    dsrcCount: TDataSource;
    DBGridPrices: TDBGrid;
    DBText_count: TDBText;
    dsrcList: TDataSource;
    DBGridList: TDBGrid;
    DBMemo_title: TDBMemo;
    DBMemo_full_desc: TDBMemo;
    DBMemo_url: TDBMemo;
    DBMemo_district_name: TDBMemo;
    dsrcPrices: TDataSource;
    dsrcPhones: TDataSource;
    edJS: TEdit;
    edRoomsFrom: TMaskEdit;
    edRoomsTo: TMaskEdit;
    edStoreyFrom: TMaskEdit;
    edStoreysFrom: TMaskEdit;
    edSquareFrom: TMaskEdit;
    edSquareTo: TMaskEdit;
    edStoreyTo: TMaskEdit;
    edStoreysTo: TMaskEdit;
    gbPrice: TGroupBox;
    gbStorey: TGroupBox;
    gbStoreys: TGroupBox;
    gbSquare: TGroupBox;
    gbRooms: TGroupBox;
    ilFavicons: TImageList;
    ilSortArrows: TImageList;
    Label1: TLabel;
    edPriceFrom: TMaskEdit;
    edPriceTo: TMaskEdit;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    lbComment: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    edAddr: TLabeledEdit;
    MainMenu: TMainMenu;
    GridMenuRating3: TMenuItem;
    GridMenuRating4: TMenuItem;
    GridMenuRating1: TMenuItem;
    GridMenuRating2: TMenuItem;
    GridMenuRating5: TMenuItem;
    MenuItem10: TMenuItem;
    GridMenuPublished: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    mmComment: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    GridMenuRating0: TMenuItem;
    GridMenuHide: TMenuItem;
    mmHtml: TMemo;
    mmUrl: TMemo;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    GridMenu: TPopupMenu;
    Panel5: TPanel;
    qListbalcony: TBooleanField;
    qListbath_joint: TBooleanField;
    qListcity_name: TMemoField;
    qListcomment: TMemoField;
    qListcreation_time: TDateTimeField;
    qListdebug_data: TMemoField;
    qListdistrict_name: TMemoField;
    qListestate_id: TLongintField;
    qListfull_desc: TMemoField;
    qListgeo_lat: TLargeintField;
    qListgeo_lon: TLargeintField;
    qListhidden: TBooleanField;
    qListhomepage: TMemoField;
    qListhouse_cond: TMemoField;
    qListkitchen_square: TLargeintField;
    qListliving_square: TLargeintField;
    qListrating: TSmallintField;
    qListother_info: TMemoField;
    qListperson: TMemoField;
    qListprice: TLargeintField;
    qListprice_time: TStringField;
    qListpublication_time: TDateTimeField;
    qListpublished: TBooleanField;
    qListregion_name: TMemoField;
    qListrooms_count: TSmallintField;
    qListseen_time: TDateTimeField;
    qListsite_class: TMemoField;
    qListsite_desc: TMemoField;
    qListsite_pub_id: TMemoField;
    qListstorey: TSmallintField;
    qListstoreys: TSmallintField;
    qListstore_time: TDateTimeField;
    qListstreet: TMemoField;
    qListstreet_no: TMemoField;
    qListtitle: TMemoField;
    qListtotal_square: TLargeintField;
    qListurl: TMemoField;
    qPhones: TSQLQuery;
    qPhonesphone: TMemoField;
    qPricesprice: TLargeintField;
    qPricesprice_time: TDateTimeField;
    rgComment: TRadioGroup;
    rgPublished: TRadioGroup;
    rgShowDeleted: TRadioGroup;
    qWorker: TSQLQuery;
    Splitter3: TSplitter;
    tbStop: TToolButton;
    TrayMenu: TPopupMenu;
    qList: TSQLQuery;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    SQLite3Conn: TSQLite3Connection;
    qPrices: TSQLQuery;
    qCount: TSQLQuery;
    SQLTran: TSQLTransaction;
    StatusBar1: TStatusBar;
    tbAvitoJS: TToolButton;
    TrayIcon: TTrayIcon;
    tsPage: TTabSheet;
    tsDBList: TTabSheet;
    tglImage: TToggleBox;
    ToolBar1: TToolBar;
    tbAvito: TToolButton;
    tbDomino: TToolButton;
    tbDomofond: TToolButton;
    tsBrowser: TTabSheet;
    tsHtml: TTabSheet;
    XMLPropStorage: TXMLPropStorage;
    procedure btBackClick(Sender: TObject);
    procedure btForwardClick(Sender: TObject);
    procedure btGoClick(Sender: TObject);
    procedure btCommentUpdateClick(Sender: TObject);
    procedure cbRatingFromChange(Sender: TObject);
    procedure cbRatingToChange(Sender: TObject);
    procedure crmKeyEvent(Sender: TObject; const Browser: ICefBrowser;
      const event: PCefKeyEvent; osEvent: TCefEventHandle; out Result: Boolean);
    procedure DBGridListContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure DBGridListDrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure DBGridPhonesPrepareCanvas(sender: TObject; DataCol: Integer;
      Column: TColumn; AState: TGridDrawState);
    procedure DoRefresh(Sender: TObject);
    procedure btClearClick(Sender: TObject);
    procedure crmBeforePopup(Sender: TObject; const browser: ICefBrowser;
      const frame: ICefFrame; const targetUrl, targetFrameName: ustring;
      targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean;
      var popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo;
      var client: ICefClient; var settings: TCefBrowserSettings;
      var noJavascriptAccess: Boolean; out Result: Boolean);
    procedure crmBeforeResourceLoad(Sender: TObject;
      const browser: ICefBrowser; const frame: ICefFrame;
      const request: ICefRequest; const callback: ICefRequestCallback; out
      Result: TCefReturnValue);
    procedure crmLoadEnd(Sender: TObject; const Browser: ICefBrowser;
      const Frame: ICefFrame; httpStatusCode: Integer);
    procedure crmLoadError(Sender: TObject; const Browser: ICefBrowser;
      const Frame: ICefFrame; errorCode: TCefErrorCode; const errorText,
      failedUrl: ustring);
    procedure crmProcessMessageReceived(Sender: TObject;
      const Browser: ICefBrowser; sourceProcess: TCefProcessId;
      const message: ICefProcessMessage; out Result: Boolean);
    procedure DBGridListDblClick(Sender: TObject);
    procedure DBGridListKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure DBGridListPrepareCanvas(sender: TObject; DataCol: Integer;
      Column: TColumn; AState: TGridDrawState);
    procedure DBGridListTitleClick(Column: TColumn);
    procedure DBMemo_urlClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure GridMenuHideClick(Sender: TObject);
    procedure GridMenuPublishedClick(Sender: TObject);
    procedure GridMenuRatingClick(Sender: TObject);
    procedure MainFormClose(Sender: TObject);
    procedure MainFormShow(Sender: TObject);
    procedure MainFormHide(Sender: TObject);
    procedure mmCommentChange(Sender: TObject);
    procedure mmCommentKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure mmUrlEnter(Sender: TObject);
    procedure mmUrlKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure qListAfterScroll(DataSet: TDataSet);
    procedure qListcreation_timeGetText(Sender: TField; var aText: string;
      DisplayText: Boolean);
    procedure qListratingGetText(Sender: TField; var aText: string;
      DisplayText: Boolean);
    procedure SQLite3ConnAfterConnect(Sender: TObject);
    procedure tbAvitoClick(Sender: TObject);
    procedure tbAvitoJSClick(Sender: TObject);
    procedure tglImageChange(Sender: TObject);
    procedure tbDominoClick(Sender: TObject);
    procedure tbDomofondClick(Sender: TObject);
    procedure tbStopClick(Sender: TObject);
    procedure TrayIconClick(Sender: TObject);
    procedure XMLPropStorageRestoreProperties(Sender: TObject);
  private
    FStartTime: Int64;
    FErrorCode: TCefErrorCode;
    FErrorText: String;
    FUrl, FReferer: ustring;
    FDomainBlackList: TFPHashList; // also TPFList or TAvgLvlTree
    //FLoaded: Boolean;
    FHtml: TStrObject;
    FDomofond: TDomofondParser;
    FStop: Boolean;
    FKeyFieldName: String;
    FKeyFieldAscending: Boolean;
    procedure DBGridOnGetText(Sender: TField; var aText: string; DisplayText: Boolean);
    procedure InitDomainBlackList{(const BlackDomains:Array of ShortString)};
    function IsBlackDom(const Url:ustring):Boolean;
    procedure InitChromium;
    procedure RefreshList;
    procedure OpenItemInBrowser;
    procedure MarkFieldAs(const AFieldName: String; AValue: Boolean);
    procedure MarkRecordAsHidden(del: Boolean);
    procedure MarkRecordAsPublished(pub: Boolean);
    procedure SetRating(rating: Integer);
    procedure LoadComment;
    procedure UpdateComment;
  protected
    procedure EvalAvito(const StartUrl:String);
    procedure EvalDomino(const StartUrl:String);
    procedure EvalDomofond(const StartUrl:String);
  public
    function LoadUrl(const Url:String; const Referer:String = ''; Timeout: Integer = 20): Boolean;
    function GetHtml: String;
    function GetChromium: TChromium;
    function GetErrorText: String;
    function GetErrorCode: TCefErrorCode;
    function StopFlag: Boolean;
  end;

  { TCefStringVisitor }

  //класс для получения данных для использования в обработчике сообщений
  //при получении сообщения gethtml_1
  TCefStringVisitor = class(TCefStringVisitorOwn)
  private
    FForm: TMainForm;
    //FIPCServerID: String;
  protected
    procedure Visit(const str: ustring); override;
  public
    constructor Create(const Form: TMainForm); overload;
  end;

  { TCustomRenderProcessHandler }

  // попытка получить данные при получении сообщения gethtml_1
  TCustomRenderProcessHandler = class(TCefRenderProcessHandlerOwn)
  protected
    function OnProcessMessageReceived(const browser: ICefBrowser;
      sourceProcess: TCefProcessId; const message: ICefProcessMessage): Boolean; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TStrObject }

constructor TStrObject.Create(const s: String);
begin
  FCrt := syncobjs.TCriticalSection.Create;
  //LogPid({$I %FILE%}+':'+{$I %LINE%}+' TStrObject.Create');
  SetS(s);
end;

destructor TStrObject.Destroy;
begin
  LogPid({$I %FILE%}+':'+{$I %LINE%}+' TStrObject.Destroy');
  FreeAndNil(FCrt);
  inherited;
end;

procedure TStrObject.SetS(AValue: String);
begin
  //LogPid({$I %FILE%}+':'+{$I %LINE%}+' TStrObject.SetS');
  try
    FCrt.Acquire;
    FS := AValue;
  finally
    FCrt.Release;
  end;
end;

{ TCustomRenderProcessHandler }

function TCustomRenderProcessHandler.OnProcessMessageReceived(
  const browser: ICefBrowser; sourceProcess: TCefProcessId;
  const message: ICefProcessMessage): Boolean;
begin
  case message.Name of
  'gethtml_1':
    begin
      //browser.MainFrame.GetSource(TCefStringVisitor.Create(browser));
      Result := True;
    end;
  else
    Result := inherited;
  end;
end;

{ TCefStringVisitor }

procedure TCefStringVisitor.Visit(const str: ustring);
begin
  //LogPid({$I %FILE%}+':'+{$I %LINE%}+' TCefStringVisitor.Visit');
  //LogPid(Format('FForm = %p', [Pointer(FForm)]));
  //LogPid(Format('FForm.FHtml = %p', [Pointer(FForm.FHtml)]));
  FForm.FHtml.s := us2s(str);
  //LogPid({$I %FILE%}+':'+{$I %LINE%}+' Length(MainForm.FHtml) = ' + IntToStr(Length(MainForm.FHtml.s)));
end;

constructor TCefStringVisitor.Create(const Form: TMainForm);
begin
  inherited Create;
  FForm := Form;
end;

{ TMainForm }

procedure TMainForm.btGoClick(Sender: TObject);
begin
  {(crm as TChromiumExt).}LoadUrl(MainForm.mmUrl.Lines.Text);
end;

procedure TMainForm.btCommentUpdateClick(Sender: TObject);
begin
  UpdateComment;
end;

procedure TMainForm.cbRatingFromChange(Sender: TObject);
var
  OldChange: TNotifyEvent;
begin
  if cbRatingTo.ItemIndex < cbRatingFrom.ItemIndex then
  begin
    OldChange := cbRatingTo.OnChange;
    cbRatingTo.OnChange := nil;
    cbRatingTo.ItemIndex := cbRatingFrom.ItemIndex;
    cbRatingTo.OnChange := OldChange;
  end;
  RefreshList;
end;

procedure TMainForm.cbRatingToChange(Sender: TObject);
var
  OldChange: TNotifyEvent;
begin
  if cbRatingFrom.ItemIndex > cbRatingTo.ItemIndex then
  begin
    OldChange := cbRatingFrom.OnChange;
    cbRatingFrom.OnChange := nil;
    cbRatingFrom.ItemIndex := cbRatingTo.ItemIndex;
    cbRatingFrom.OnChange := OldChange;
  end;
  RefreshList;
end;

procedure TMainForm.UpdateComment;
var
  estate_id: Integer;
begin
  estate_id := qListestate_id.AsInteger;
  TCuteSQLQuery(Pointer(qWorker)) // чтоб даже с отладочной опцией не вылезала ошибка invalid typecast
    .SetSQLText('UPDATE estates SET comment=trim(:comment) WHERE estate_id=:estate_id')
    .SetParam(0, mmComment.Lines.Text)
    .SetParam(1, estate_id)
    .ExeCute.ExeCute
    .CommitRetainingCute;
  lbComment.Caption := 'Комментарий';
  qList.Refresh;
  qList.Locate('estate_id', VarArrayOf([estate_id]), []);
  DBGridList.SetFocus;
  LoadComment;
end;

procedure TMainForm.crmKeyEvent(Sender: TObject; const Browser: ICefBrowser;
  const event: PCefKeyEvent; osEvent: TCefEventHandle; out Result: Boolean);
begin
  if (event^.kind = KEYEVENT_RAWKEYDOWN)
    and (event^.character = WideChar(VK_ESCAPE))
    and (event^.modifiers * [EVENTFLAG_SHIFT_DOWN, EVENTFLAG_CONTROL_DOWN, EVENTFLAG_ALT_DOWN] = [])
  then
  begin
    Result := True;
    PageControl1.ActivePage := tsDBList;
    DBGridList.SetFocus;
  end;
end;

procedure TMainForm.DBGridListContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin
  //if qListmarked.AsBoolean then
  //begin
  //  GridMenuRating0.Caption := 'Снять отметку "интересно"';
  //  GridMenuRating0.Checked := True;
  //end else begin
  //  GridMenuRating0.Caption := 'Пометить как интересное';
  //  GridMenuRating0.Checked := False;
  //end;

  if qListhidden.AsBoolean then
  begin
    GridMenuHide.Caption := 'Снять отметку "скрыто"';
    GridMenuHide.Checked := True;
  end else begin
    GridMenuHide.Caption := 'Скрыть это объявление';
    GridMenuHide.Checked := False;
  end;

  if qListpublished.AsBoolean then
  begin
    GridMenuPublished.Caption := 'Пометить как "неактивно"';
    //GridMenuPublished.Checked := True;
  end else begin
    GridMenuPublished.Caption := 'Пометить как "активно"';
    //GridMenuPublished.Checked := False;
  end;
end;

procedure TMainForm.DBGridPhonesPrepareCanvas(sender: TObject;
  DataCol: Integer; Column: TColumn; AState: TGridDrawState);
var
  MyTextStyle: TTextStyle;
begin
  // Here you choose what column will be affected (the columns of the DBGrid not SQL).
//  if (DataCol = 1) then
  if Column.Field is TMemoField then
  begin
    // The next is not neccesary but you can use it to adjust your text appearance.
    // you can change colors, font, size, as well other parameters.
    MyTextStyle := DBGridList.Canvas.TextStyle;
    MyTextStyle.SingleLine := False;
    MyTextStyle.Wordbreak  := False;
    TDBGrid(Sender).Canvas.TextStyle := MyTextStyle;

    // Here how to show any text:
    // just assign an event procedure to OnGetText of the Field.
    Column.Field.OnGetText := @DBGridOnGetText;
  end;
end;

procedure TMainForm.btBackClick(Sender: TObject);
begin
  //crm.Browser.GoBack;
end;

procedure TMainForm.btForwardClick(Sender: TObject);
begin
  //crm.Browser.GoForward;
end;

procedure TMainForm.crmBeforePopup(Sender: TObject; const browser: ICefBrowser;
  const frame: ICefFrame; const targetUrl, targetFrameName: ustring;
  targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean;
  var popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo;
  var client: ICefClient; var settings: TCefBrowserSettings;
  var noJavascriptAccess: Boolean; out Result: Boolean);
begin
  LoadUrl(us2s(targetUrl));
  Result := True;
end;

procedure TMainForm.crmBeforeResourceLoad(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame;
  const request: ICefRequest; const callback: ICefRequestCallback; out
  Result: TCefReturnValue);
var
  Url: ustring;
begin
  Url := request.GetUrl;
  if IsBlackDom(Url) then
  begin
    //LogPid({$I %FILE%}+':'+{$I %LINE%}+' Blocked: ' + String(Url));
    Result := RV_CANCEL;
    Exit;
  end
  else
    Result := RV_CONTINUE;

  if (FUrl = Url) and (FReferer <> '') then
  begin
    LogPid('SETREFERER: FUrl = "%s", Url = "%s", Referer = "%s"', [FUrl, Url, FReferer]);
    request.SetReferrer(FReferer, REFERRER_POLICY_ALWAYS);
    //map := TCefStringMultimapOwn.Create; // не работает
    //request.GetHeaderMap(map);
    //map.Append('Referer', FReferer);
    //request.SetHeaderMap(map);
  end;
end;

procedure TMainForm.crmLoadEnd(Sender: TObject;
  const Browser: ICefBrowser; const Frame: ICefFrame; httpStatusCode: Integer);
begin
  if Assigned(Frame) and Frame.IsMain and (Browser.Identifier = crm.BrowserId) then
  begin
    //LogPid({$I %FILE%}+':'+{$I %LINE%}+' TMainForm.crmLoadEnd: ' + IntToStr(httpStatusCode));
    FErrorCode := httpStatusCode;
    if httpStatusCode = 200 then
    begin
      crm.Browser.MainFrame.GetSource(TCefStringVisitor.Create(Self));
      mmUrl.Lines.Text := String(crm.Browser.MainFrame.Url);
      mmHtml.Lines.Text := FHtml.s;
    end;
  end;
end;

procedure TMainForm.crmLoadError(Sender: TObject; const Browser: ICefBrowser;
  const Frame: ICefFrame; errorCode: TCefErrorCode; const errorText,
  failedUrl: ustring);
begin
  if FUrl <> failedUrl then
    Exit;
  FErrorCode := errorCode;
  FErrorText := us2s(errorText);
end;

procedure TMainForm.crmProcessMessageReceived(Sender: TObject;
  const Browser: ICefBrowser; sourceProcess: TCefProcessId;
  const message: ICefProcessMessage; out Result: Boolean);
begin
  case message.Name of
  'gethtml':
    begin
      MainForm.mmHtml.Lines.Text := us2s(message.ArgumentList.GetString(0));
      Result := True;
    end;
  end;
end;

procedure TMainForm.DBGridListDblClick(Sender: TObject);
begin
  OpenItemInBrowser;
end;

procedure TMainForm.MarkRecordAsHidden(del: Boolean);
begin
  MarkFieldAs('hidden', del);
end;

procedure TMainForm.MarkRecordAsPublished(pub: Boolean);
begin
  MarkFieldAs('published', pub);
end;

procedure TMainForm.SetRating(rating: Integer);
var
  estate_id, estate_id_next: Integer;
begin
  if (rating = qListrating.AsInteger) or (rating < 0) or (rating > 9) then
    Exit;
  estate_id := qListestate_id.AsInteger;
  qList.Next;
  estate_id_next := qListestate_id.AsInteger;
  qWorker.SQL.Text := 'UPDATE estates SET rating=:rating WHERE estate_id=:estate_id';
  qWorker.Params[0].AsInteger := rating;
  qWorker.Params[1].AsInteger := estate_id;
  qWorker.ExecSQL;
  SQLTran.CommitRetaining;
  RefreshList;
  qList.Locate('estate_id', estate_id_next, []);
  //if qList.Locate('estate_id', estate_id, []) then
  //  qList.Next;
end;

procedure TMainForm.DBGridListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Shift = [] then
  begin
    case Key of
      VK_ADD:
        MarkRecordAsHidden(False);
      VK_SUBTRACT:
        MarkRecordAsHidden(True);
      VK_DIVIDE:
        MarkRecordAsPublished(not qListpublished.AsBoolean);
      VK_0..VK_9:
        SetRating(Key - VK_0);
      VK_NUMPAD0..VK_NUMPAD9:
        SetRating(Key - VK_NUMPAD0);
    end;
  end;
end;

procedure TMainForm.qListratingGetText(Sender: TField; var aText: string;
  DisplayText: Boolean);
begin
  //aText := DupeString('☆', qListrating.AsInteger);
  aText := DupeString('★', qListrating.AsInteger);
  //aText := DupeString('♦', qListrating.AsInteger);
end;

procedure TMainForm.DBGridListPrepareCanvas(sender: TObject; DataCol: Integer;
  Column: TColumn; AState: TGridDrawState);
var
  MyTextStyle: TTextStyle;
begin
// Here you choose what column will be affected (the columns of the DBGrid not SQL).
//if (DataCol = 1) then
  if Column.Field is TMemoField then
  begin
    // The next is not neccesary but you can use it to adjust your text appearance.
    // you can change colors, font, size, as well other parameters.
    MyTextStyle := DBGridList.Canvas.TextStyle;
    MyTextStyle.SingleLine := False;
    MyTextStyle.Wordbreak  := False;
    TDBGrid(Sender).Canvas.TextStyle := MyTextStyle;

    // Here how to show any text:
    // just assign an event procedure to OnGetText of the Field.
    Column.Field.OnGetText := @DBGridOnGetText;
  end;
  if qListhidden.AsBoolean then
  begin
    if not (gdSelected in AState) then
      DBGridList.Canvas.Brush.Color := clRed;
  end;
  if not qListpublished.AsBoolean then
  begin
    DBGridList.Canvas.Font.Italic := True;
    //DBGridList.Canvas.Font.Size := DBGridList.Canvas.Font.Size - 2;
    DBGridList.Canvas.Font.StrikeThrough := True;
    if not (gdSelected in AState) then
      DBGridList.Canvas.Font.Color := clGray;
  end;
  //if qListrating.AsInteger > 0 then
  //begin
  //  //DBGridList.Canvas.Brush.Color := clYellow;
  //  DBGridList.Canvas.Font.Bold := True;
  //  DBGridList.Canvas.Font.Underline := True;
  //end;
  if Column.Field = qListrating then
  begin
    //DBGridList.Canvas.Font.Color := $00AEFF; // BGR
    DBGridList.Canvas.Font.Style := [];
  end;
end;

procedure TMainForm.DBGridListDrawColumnCell(Sender: TObject;
  const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
begin
//
end;

procedure TMainForm.DBGridListTitleClick(Column: TColumn);
begin
  if FKeyFieldName = Column.Field.FieldName then
    FKeyFieldAscending := not FKeyFieldAscending
  else
    FKeyFieldAscending := True;
  FKeyFieldName := Column.Field.FieldName;
  RefreshList;
end;

procedure TMainForm.DBMemo_urlClick(Sender: TObject);
begin
  OpenItemInBrowser;
end;

procedure myOnBeforeCommandLineProcessing(const processType: ustring;
   const commandLine: ICefCommandLine);
begin
//  commandLine.AppendSwitch('enable-system-flash');
  commandLine.AppendSwitchWithValue('remote-debugging-port','9222'); // remote debugger
//  commandLine.AppendSwitchWithValue('proxy-server','http://127.0.0.1:8888'); // fiddler
//  commandLine.AppendSwitchWithValue('proxy-server','http://127.0.0.1:8118'); // privoxy + tor
end;

procedure TMainForm.InitChromium;
var
  Path: String;
  Lang: String = '';
  FallbackLang: String = '';
begin
  InitDomainBlackList; {([
    'api-maps.yandex.ru', // *******

    'ad.mail.ru',
    'ame.avira.com',
    'an.yandex.ru',
    'bid.g.doubleclick.net',
    'cdn.optimizely.com',
    'connect.facebook.net',
    'counter.yadro.ru',
    'crl.microsoft.com',
    'dmp.vihub.ru',
    'googleads.g.doubleclick.net',
    'ipm.avira.com',
    'jsre.r24-tech.com',
    'mc.yandex.ru',
    'pagead2.googlesyndication.com',
    's0.2mdn.net',
    's3.eu-central-1.amazonaws.com',
    'safehub.ru',
    'securepubads.g.doubleclick.net',
    'stats.g.doubleclick.net',
    't.qservz.com',
    'top-fwz1.mail.ru',
    'tpc.googlesyndication.com',
    'vk.com',
    'widget.criteo.com',
    'www.avira.com',
    'www.facebook.com',
    'www.google-analytics.com',
    'www.google.com',
    'www.google.ru',
    'www.googletagmanager.com',
    'www.googletagservices.com',
    'www.gstatic.com',
    'www.tns-counter.ru'
    //'',
  ]);}

  //https://www.avito.ru/volgograd/kvartiry/prodam/ne_posledniy?pmax=2000000&pmin=1000000&district=6&f=549_5697-5698.59_13987b.496_5121b

  CefOnBeforeCommandLineProcessing := @myOnBeforeCommandLineProcessing; // proxy http://forum.lazarus.freepascal.org/index.php?topic=34595.0
  CefRenderProcessHandler := TCustomRenderProcessHandler.Create;
  Path := GetCurrentDirUTF8 + DirectorySeparator;
  CefCachePath := '';// ustring(Path) + 'Cache';
  //Lang := ''; FallbackLang := ''; // disable wrong "not initialized"
  GetLanguageIDs(Lang, FallbackLang);
  CefLocale := UTF8Decode(FallbackLang);
  CefBrowserSubprocessPath := 'subprocess32.exe';
  //CefNoSandbox := False;
  //CefWindowlessRenderingEnabled := True;
  //CefLogFile := 'debug.log';
  CefPersistSessionCookies := True;
  //CefLogSeverity := LOGSEVERITY_DISABLE;

  //crm_new := TChromiumExt.Create(tsBrowser);
  //crm_new.Left := crm.Left;
  //crm_new.Height := crm.Height;
  //crm_new.Top := crm.Top;
  //crm_new.Width := crm.Width;
  //crm_new.Align := crm.Align;
  //crm_new.TabOrder := crm.TabOrder;
  //FreeAndNil(crm);
  //crm := crm_new;
end;

procedure TMainForm.DoRefresh(Sender: TObject);
begin
  RefreshList;
end;

procedure TMainForm.btClearClick(Sender: TObject);

procedure DefCheckBox(cb:TCheckBox; state: Boolean = False);
var
  OnEvent: TNotifyEvent;
begin
  OnEvent := cb.OnChange;
  cb.OnChange := nil;
  cb.Checked := state;
  cb.OnChange := OnEvent;
end;

procedure DefRadio(rg:TRadioGroup; index: Integer = 0);
var
  OnEvent: TNotifyEvent;
begin
  OnEvent := rg.OnSelectionChanged;
  rg.OnSelectionChanged := nil;
  rg.ItemIndex := index;
  rg.OnSelectionChanged := OnEvent;
end;

procedure DefComboBox(cb:TComboBox; index: Integer = 0);
var
  OnEvent: TNotifyEvent;
begin
  OnEvent := cb.OnChange;
  cb.OnChange := nil;
  cb.ItemIndex := index;
  cb.OnChange := OnEvent;
end;

begin
  edPriceFrom.Text := '0';
  edPriceTo.Text := '';
  edStoreyFrom.Text := '';
  edStoreyTo.Text := '';
  edStoreysFrom.Text := '';
  edStoreysTo.Text := '';
  edSquareFrom.Text := '';
  edSquareTo.Text := '';
  edRoomsFrom.Text := '';
  edRoomsTo.Text := '';
  edAddr.Text := '';

  DefCheckBox(cbStoreyNotLast);
  DefCheckBox(cbPriceIsNull);

  DefRadio(rgShowDeleted, 1);
  DefRadio(rgComment);
  DefRadio(rgPublished, 1);

  DefComboBox(cbRatingFrom, 0);
  DefComboBox(cbRatingTo, 9);

  RefreshList;
end;

procedure TMainForm.RefreshList;
var
  cond: array of String;

procedure _AddSortArrow;
var
  i: Integer;
  col: TColumn;
  title: TGridColumnTitle;
begin
  for i := 0 to DBGridList.Columns.Count-1 do
  begin
    col := DBGridList.Columns[i];
    title := col.Title;
    title.ImageLayout := blGlyphLeft;
    title.PrefixOption := poHeaderClick;
    if col.FieldName <> FKeyFieldName then
    begin
      title.Font.Bold := False;
      title.ImageIndex := -1
    end
    else
    begin
      title.Font.Bold := True;
      if FKeyFieldAscending then
        title.ImageIndex := 0 // up arrow
      else
        title.ImageIndex := 1 // down arrow
    end
  end;
end;

procedure _AddCond(const s:String);
begin
  SetLength(cond, Length(cond)+1);
  cond[Length(cond)-1] := s;
end;

function _CondList:String;
var
  i:integer;
begin
  Result := '';
  if Length(cond) > 0 then
    Result := ' WHERE ' + cond[0];
  for i := 1 to Length(cond)-1 do
    Result := Result + ' AND ' + cond[i];
end;

function _OrderBy:String;
begin
  if FKeyFieldAscending then
    Result := ' ORDER BY ' + FKeyFieldName
  else
    Result := ' ORDER BY ' + FKeyFieldName + ' DESC';
  if FKeyFieldName <> 'price' then
    Result += ',price';
end;

procedure _Refresh(const q:TSQLQuery; const sql:string);
var
  S: String;
begin
  S := sql + _CondList + _OrderBy;
  mmHtml.Lines.Text := S;
  q.SQL.Text := S;

  if not cbPriceIsNull.Checked then
  begin
    if edPriceFrom.Text <> '' then
       q.ParamByName('pricefrom').AsLargeInt := StrToInt64(edPriceFrom.Text) * 1000;
    if edPriceTo.Text <> '' then
      q.ParamByName('priceto').AsLargeInt := StrToInt64(edPriceTo.Text) * 1000;
  end;

  if edStoreyFrom.Text <> '' then
     q.ParamByName('storeyfrom').AsLargeInt := StrToInt(edStoreyFrom.Text);
  if edStoreyTo.Text <> '' then
    q.ParamByName('storeyto').AsLargeInt := StrToInt(edStoreyTo.Text);
  if edStoreysFrom.Text <> '' then
     q.ParamByName('storeysfrom').AsLargeInt := StrToInt(edStoreysFrom.Text);
  if edStoreysTo.Text <> '' then
    q.ParamByName('storeysto').AsLargeInt := StrToInt(edStoreysTo.Text);
  if edSquareFrom.Text <> '' then
    q.ParamByName('total_square_from').AsLargeInt := StrToInt(edSquareFrom.Text);
  if edSquareTo.Text <> '' then
    q.ParamByName('total_square_to').AsLargeInt := StrToInt(edSquareTo.Text);
  if edRoomsFrom.Text <> '' then
    q.ParamByName('rooms_count_from').AsLargeInt := StrToInt(edRoomsFrom.Text);
  if edRoomsTo.Text <> '' then
    q.ParamByName('rooms_count_to').AsLargeInt := StrToInt(edRoomsTo.Text);
  if edAddr.Text <> '' then
  begin
    S := LazUTF8.UTF8LowerCase(Trim(edAddr.Text));
    q.ParamByName('addr_part').AsString := S;
    q.ParamByName('full_desc_part').AsString := S;
  end;
  q.ParamByName('rating_from').AsLargeInt := cbRatingFrom.ItemIndex;
  q.ParamByName('rating_to').AsLargeInt := cbRatingTo.ItemIndex;

  q.Close;
  q.Open;
end;

begin
  if cbPriceIsNull.Checked then
  begin
    edPriceFrom.Enabled := False;
    edPriceTo.Enabled := False;
    _AddCond('(price is null)');
  end else begin
    edPriceFrom.Enabled := True;
    edPriceTo.Enabled := True;
    if edPriceFrom.Text <> '' then
      _AddCond('(price >= :pricefrom)');
    if edPriceTo.Text <> '' then
      _AddCond('(price <= :priceto)');
  end;

  if edStoreyFrom.Text <> '' then
    _AddCond('(storey >= :storeyfrom)');
  if edStoreyTo.Text <> '' then
    _AddCond('(storey <= :storeyto)');
  if edStoreysFrom.Text <> '' then
    _AddCond('(storeys >= :storeysfrom)');
  if edStoreysTo.Text <> '' then
    _AddCond('(storeys <= :storeysto)');
  if cbStoreyNotLast.Checked then
    _AddCond('(storey < storeys)');
  if edSquareFrom.Text <> '' then
    _AddCond('(total_square >= :total_square_from)');
  if edSquareTo.Text <> '' then
    _AddCond('(total_square <= :total_square_to)');
  if edRoomsFrom.Text <> '' then
    _AddCond('(rooms_count >= :rooms_count_from)');
  if edRoomsTo.Text <> '' then
    _AddCond('(rooms_count <= :rooms_count_to)');
  if edAddr.Text <> '' then
    _AddCond('((instr(utf8lower(street), :addr_part) > 0) or (instr(utf8lower(full_desc), :full_desc_part) > 0))');

  case rgShowDeleted.ItemIndex of
    1: _AddCond('(hidden = 0)');
    2: _AddCond('(hidden <> 0)');
  end;
  case rgComment.ItemIndex of
    1: _AddCond('(not (comment = '''' or comment is null))');
    2: _AddCond('(comment = '''' or comment is null)');
  end;
  case rgPublished.ItemIndex of
    1: _AddCond('(published <> 0)');
    2: _AddCond('(published = 0)');
  end;

  _AddCond('(rating >= :rating_from)');
  _AddCond('(rating <= :rating_to)');

  SQLite3Conn.Connected := False;
  SQLite3Conn.Connected := True;
  _Refresh(qCount, 'SELECT count(*) FROM price_view');
  _Refresh(qList, 'SELECT * FROM price_view');
  _AddSortArrow;
  qPrices.Active := True;
  qPhones.Active := True;
  LoadComment;
end;

procedure TMainForm.OpenItemInBrowser;
//var
//  MyLink: string;
begin
//  OpenURL(DBMemo_url.Text);
  PageControl1.ActivePage := tsBrowser;
  LoadUrl(DBMemo_url.Text);
//  MyLink := DBMemo_url.Text;
//  ShellExecute(Application.Handle, PChar('open'), PChar(MyLink), nil, nil, SW_SHOW);
end;

procedure TMainForm.MarkFieldAs(const AFieldName: String; AValue: Boolean);
var
  estate_id, estate_id_next: Integer;
begin
  estate_id := qListestate_id.AsInteger;
  qList.Next;
  estate_id_next := qListestate_id.AsInteger;
  qWorker.SQL.Text := 'UPDATE estates SET '+AFieldName+'=:value WHERE estate_id=:estate_id';
  qWorker.Params[1].AsInteger := estate_id;
  if AValue then
    qWorker.Params[0].AsInteger := 1
  else
    qWorker.Params[0].AsInteger := 0;
  qWorker.ExecSQL;
  SQLTran.CommitRetaining;
  RefreshList;
  qList.Locate('estate_id', estate_id_next, []);
  //if not qList.Locate('estate_id', estate_id, []) then
    //qList.Next;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  //LogPid('TMainForm.FormCreate');

  //SQLite3Conn.DatabaseName := ChangeFileExt(ParamStr(0), '.sqlite');
  //XMLPropStorage.FileName := ChangeFileExt(ParamStr(0), '.xml');

  Randomize;

  FDomofond := TDomofondParser.Create(SQLite3Conn);
  FHtml := TStrObject.Create;
  FKeyFieldName := 'price';
  FKeyFieldAscending := True;

  parsers.Memo := mmHtml;
  parsers.UrlLoader := Self;


  PageControl1.ActivePage := tsDBList;
  InitChromium;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FDomofond);
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) and (Shift = []) and (PageControl1.ActivePage = tsBrowser) then
  begin
    Key := 0;
    PageControl1.ActivePage := tsDBList;
  end;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  TrayIcon.Show;
end;

procedure TMainForm.GridMenuHideClick(Sender: TObject);
begin
  MarkRecordAsHidden(not qListhidden.AsBoolean);
end;

procedure TMainForm.GridMenuPublishedClick(Sender: TObject);
begin
  MarkRecordAsPublished(not qListpublished.AsBoolean);
end;

procedure TMainForm.GridMenuRatingClick(Sender: TObject);
begin
  SetRating((Sender as TMenuItem).Tag);
end;

procedure TMainForm.MainFormClose(Sender: TObject);
begin
  //Application.Terminate;
  Close;
end;

procedure TMainForm.MainFormShow(Sender: TObject);
begin
  Show
end;

procedure TMainForm.MainFormHide(Sender: TObject);
begin
  Hide
end;

procedure TMainForm.mmCommentChange(Sender: TObject);
begin
  lbComment.Caption := '* Комментарий';
end;

procedure TMainForm.mmCommentKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  //if (ssCtrl in Shift) and (Key <> VK_CONTROL) then
  //  Key := Key;
  if ((Key = VK_F2) and (Shift = [])) or ((Key = Ord('S')) and (Shift * [ssShift, ssAlt, ssCtrl] = [ssCtrl])) then
    UpdateComment;
end;

procedure TMainForm.mmUrlEnter(Sender: TObject);
begin
  if mmUrl.SelLength = 0 then
    mmUrl.SelectAll;
end;

procedure TMainForm.mmUrlKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = 13) and (Shift = []) then
    {(crm as TChromiumExt).}LoadUrl(MainForm.mmUrl.Lines.Text);
end;

procedure TMainForm.qListAfterScroll(DataSet: TDataSet);
begin
  LoadComment;
end;

procedure TMainForm.LoadComment;
var
  event: TNotifyEvent;
begin
  lbComment.Caption := 'Комментарий';
  event := mmComment.OnChange;
  mmComment.OnChange := nil;
  mmComment.Lines.Text := qListcomment.AsString;
  mmComment.OnChange := event;
end;

procedure TMainForm.qListcreation_timeGetText(Sender: TField;
  var aText: string; DisplayText: Boolean);
begin
  aText := qListcreation_time.AsString;
end;

procedure UTF8xLower(ctx: psqlite3_context; N: cint; V: ppsqlite3_value); cdecl;
var
  S: AnsiString;
begin
  SetString(S, sqlite3_value_text(V[0]), sqlite3_value_bytes(V[0]));
  S := LazUTF8.UTF8LowerCase(S);
  sqlite3_result_text(ctx, PAnsiChar(S), Length(S), sqlite3_destructor_type(SQLITE_TRANSIENT));
  //AnsiContainsText
end;

procedure TMainForm.SQLite3ConnAfterConnect(Sender: TObject);
begin
  SQLite3Conn.ExecuteDirect('PRAGMA foreign_keys = ON; PRAGMA case_sensitive_like = OFF;');
  // register function LOWER() using SQLite3 API (requires sqlite3dyn unit):
  sqlite3_create_function(SQLite3Conn.Handle, 'utf8lower', 1, SQLITE_UTF8 or SQLITE_DETERMINISTIC, nil, @UTF8xLower, nil, nil);
end;

procedure TMainForm.tbAvitoJSClick(Sender: TObject);
//var
//  j: String;
begin
  //j := 'var i = document.querySelector("a.pagination-page.js-pagination-next");';
  //j += 'alert(i.getAttribute("class"));';
  //j += 'j.scrollIntoView()';
  crm.Browser.MainFrame.ExecuteJavaScript(UnicodeString(edJS.Text), 'about:blank', 0);
  //j := DupeString('0123456789', 50);
  //edJS.Text := j;
end;

procedure TMainForm.tglImageChange(Sender: TObject);
begin
{  tglImage.Checked := not tglImage.Checked;
  case tglImage.Checked of
    True: crm.Options.ImageLoading := STATE_ENABLED;
    False:  crm.Options.ImageLoading := STATE_DISABLED;
  end;}
end;

procedure TMainForm.tbAvitoClick(Sender: TObject);
begin
  EvalAvito('https://www.avito.ru/volgograd/kvartiry/prodam/ne_posledniy?pmax=2000000&pmin=1000000&view=gallery&district=6&f=549_5697-5698.59_13987b.496_5121b');
end;

procedure TMainForm.tbDominoClick(Sender: TObject);
begin
  EvalDomino('http://domino-rf.ru/nedvizimost/prodazha/kvartiry/?fSearch=&fReg=3400000000000&fSubReg=3400000100000&cityArea=%D0%9A%D1%80%D0%B0%D1%81%D0%BD%D0%BE%D0%BE%D0%BA%D1%82%D1%8F%D0%B1%D1%80%D1%8C%D1%81%D0%BA%D0%B8%D0%B9+%D1%80-%D0%BD&prop_3_from=2&prop_3_to=&prop_8%5B%5D=2&prop_9%5B%5D=&prop_2_from=30&prop_2_to=&prop_1_from=1250000&prop_1_to=2000000&prop_29_from=&prop_29_to=');
end;

procedure TMainForm.tbDomofondClick(Sender: TObject);
begin
  FStop := True;
  tbStop.Click;
  EvalDomofond('http://www.domofond.ru/prodazha-kvartiry-krasnooktyabrskiy-d57?PriceFrom=1300000&PriceTo=1800000&Rooms=Two%2CThree&FloorFrom=2&NotLastFloor=True&FloorSizeFrom=30');
end;

procedure TMainForm.tbStopClick(Sender: TObject);
begin
  FStop := not FStop;
  tbStop.Down := Fstop;
end;

procedure TMainForm.TrayIconClick(Sender: TObject);
begin
  Show
end;

procedure TMainForm.XMLPropStorageRestoreProperties(Sender: TObject);
begin
  RefreshList;
end;

function TMainForm.LoadUrl(const Url: String; const Referer: String; Timeout: Integer): Boolean;
begin
  LogPid('TMainForm.LoadUrl: Url = "%s", Referer = "%s"', [Url, Referer]);
  //Logger.Write(Url);
  FStartTime := DateTimeToUnix(Now);
  Result := False;
  FErrorCode := ERR_NONE;
  FErrorText := 'Timeout';
  FReferer := ustring(Referer);
  FUrl := ustring(Url);
  FHtml.s := '';
  //FLoaded := False;
  mmHtml.Lines.Text := '';
  //LogPid({$I %FILE%}+':'+{$I %LINE%}+' TMainForm.LoadUrl before call of crm.Url');
  crm.Load(Url);
  repeat
    Sleep(50);
    Application.ProcessMessages;
    //LogPid({$I %FILE%}+':'+{$I %LINE%}+' TMainForm.LoadUrl wait loop');
    if FHtml.s <> '' then
      break;
  until (FHtml.s <> '') or (DateTimeToUnix(Now) - FStartTime >= Timeout);
  //LogPid({$I %FILE%}+':'+{$I %LINE%}+' TMainForm.LoadUrl after call of crm.Url');
  //LogPid({$I %FILE%}+':'+{$I %LINE%}+' Length(MainForm.FHtml) = ' + IntToStr(Length(MainForm.FHtml.s)));

  if (FHtml.s = '') and crm.Browser.IsLoading then
    crm.Browser.StopLoad;
end;

function TMainForm.GetHtml: String;
begin
  Result := FHtml.s;
end;

function TMainForm.GetChromium: TChromium;
begin
  Result := crm;
end;

function TMainForm.GetErrorText: String;
begin
  Result := FErrorText;
end;

function TMainForm.GetErrorCode: TCefErrorCode;
begin
  Result := FErrorCode;
end;

function TMainForm.StopFlag: Boolean;
begin
  Result := FStop;
end;

procedure TMainForm.DBGridOnGetText(Sender: TField; var aText: string;
  DisplayText: Boolean);
begin
  if (DisplayText) then
    aText := AnsiReplaceStr(AnsiReplaceStr(Sender.AsString, #13, ''), #10, '');
end;

procedure TMainForm.InitDomainBlackList{(const BlackDomains: array of ShortString)};
var
  s: ShortString;
  F: Text;
  r: TLResource;
  //rs: String;
begin
  FDomainBlackList := TFPHashList.Create;

  r := LazarusResources.Find('informer_filter');
  if r=nil then
    raise Exception.Create('resource informer_filter is missing');
  //rs := r.Value;

  AssignFile(F, ChangeFileExt(ParamStr(0), '_filter.txt'));
  try
    Reset(F);
    //for s in BlackDomains do
      //FDomainBlackList.Add(s, FDomainBlackList);
    while not Eof(F) do
    begin
      ReadLn(F, s);
      if (s <> '') and (s[1] <> '#') then
        FDomainBlackList.Add(s, FDomainBlackList);
    end;
    CloseFile(F);
  finally
  end;
end;

function TMainForm.IsBlackDom(const Url: ustring): Boolean;
begin
  Result := Assigned(FDomainBlackList.Find(ParseURI(ShortString(Url)).Host));
end;

procedure TMainForm.EvalAvito(const StartUrl: String);

procedure parseHTML(const tp: TTreeParser; const html: String);
begin
  tp.clearTrees;
//  tp.readComments := True;
//  tp.readProcessingInstructions := True;
  tp.parsingModel := pmHTML;
//  tp.trimText := True;
  tp.autoDetectHTMLEncoding := False;
  tp.TargetEncoding := CP_UTF8;
  tp.parseTree(html, ''{StartUrl}, 'charset=utf-8');
  if Assigned(tp.getLastTree) then
    tp.getLastTree.setEncoding(CP_UTF8, False, True);
end;

{var
  xqe: TXQueryEngine;
  xqv: IXQValue;
  tp: TTreeParser;
  s, s1, s2: String;
  //cp1, cp2, l1, l2: Integer;
  b: Boolean;}
begin
(*
  {(crm as TChromiumExt).}LoadUrl(StartUrl, '', 30);
  if not FErrorCode = 200 then
    Exit;
  xqe := TXQueryEngine.Create;
  tp := TTreeParser.Create;
  try
//    xqe.ParsingOptions.AllowExtendedStrings:=CheckBoxVarsInStrs.Checked;
//    if CheckBoxObjects.Checked then xqe.ParsingOptions.AllowPropertyDotNotation:=xqpdnAllowFullDotNotation
//    else xqe.ParsingOptions.AllowPropertyDotNotation:=xqpdnDisallowDotNotation;
//    xqe.ParsingOptions.AllowJSON:=CheckBoxJSON.Checked;

//    if (sender as tbutton).tag = 1 then xqe.parseXQuery3(mmComment.lines.Text)
//    else xqe.parseXPath3(mmComment.Lines.text);

//    xqe.parseCSS3('div[class^="item item_table"], div[class^="item item_gallery"], div.vip_item');
    //s := '';

    s1 := ReadFileMy('Q:\Games\estate_informer\internettools_test\avito1.html');
    //cp1 := StringCodePage(s1);
    //l1 := Length(s1);
//    s2 := mmHtml.Lines.Text; //FHtml;
    s2 := mmHtml.Lines.GetText;
    WriteFileMy('Q:\Games\estate_informer\internettools_test\avito2.html', s2);
    //cp2 := StringCodePage(s2);
    //l2 := Length(s2);
    b := s2 = s1;

    parseHTML(tp, s1);
    for xqv in xqe.evaluateCSS3('div.header', tp.getLastTree) do
    begin
      s := xqv.toString;
      s := xqv.toNode.outerXML();
      s := xqv.toXQuery;
    end;

    parseHTML(tp, s2);
    for xqv in xqe.evaluateCSS3('div.header', tp.getLastTree) do
    begin
      s := xqv.toString;
      s := xqv.toNode.outerXML();
      s := xqv.toXQuery;
    end;
  finally
    FreeAndNil(tp);
    FreeAndNil(xqe);
  end;
*)
end;

procedure TMainForm.EvalDomino(const StartUrl: String);
begin
  (crm as TChromiumExt).LoadUrl_1(StartUrl);
end;

procedure TMainForm.EvalDomofond(const StartUrl: String);
begin
  FDomofond.ParseStart(StartUrl);
  //LoadUrl(StartUrl);
end;

initialization

{$I informer_filter.lrs}

end.

