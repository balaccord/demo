unit chromium_ext;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, cef3lcl, cef3types, cef3intf, cef3own, cef3ref, contnrs, dateutils, {StdCtrls,}
  Forms, URIParser, parser_utils;

type

  TChromiumExt = class;

  { TCefStringVisitor }

  //класс для получения данных для использования в обработчике сообщений
  //при получении сообщения gethtml_1
  TCefStringVisitor = class(TCefStringVisitorOwn)
  private
    FBrowser: ICefBrowser;
    FChromium: TChromiumExt;
//    FSourceProcess: TCefProcessId;
  protected
    procedure Visit(const str: ustring); override;
  public
    constructor Create(const browser: ICefBrowser; const Chromium: TChromiumExt); overload;
  end;

  { TCustomRenderProcessHandler }

  // попытка получить данные при получении сообщения gethtml_1
  TCustomRenderProcessHandler = class(TCefRenderProcessHandlerOwn)
  protected
    function OnProcessMessageReceived(const browser: ICefBrowser;
      sourceProcess: TCefProcessId; const message: ICefProcessMessage): Boolean; override;
  end;

  { TChromiumExt }

  TChromiumExt = class(TChromium)
  private
    FStartTime: Int64;
    FErrorCode: TCefErrorCode;
    FErrorText: String;
    FUrl: ustring;
    FLoadUrl, FReferer: ustring;
    FDomainBlackList: TFPHashList;

    FHtml: String;
    procedure TrueLog(Msg: ustring); // реальная запись в файл
  protected
    procedure DoBeforePopup(Sender: TObject; const abrowser: ICefBrowser;
      const frame: ICefFrame; const targetUrl, targetFrameName: ustring;
      targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean;
      var popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo;
      var client: ICefClient; var settings: TCefBrowserSettings;
      var noJavascriptAccess: Boolean; out Result: Boolean);
    procedure DoProcessMessageReceived(Sender: TObject;
      const ABrowser: ICefBrowser; sourceProcess: TCefProcessId;
      const message: ICefProcessMessage; out Result: Boolean);
    procedure DoBeforeResourceLoad(Sender: TObject;
      const abrowser: ICefBrowser; const frame: ICefFrame;
      const request: ICefRequest; const callback: ICefRequestCallback; out
      Result: TCefReturnValue);
    procedure DoLoadEnd(Sender: TObject;
      const ABrowser: ICefBrowser; const Frame: ICefFrame; httpStatusCode: Integer);
    procedure DoLoadError(Sender: TObject; const ABrowser: ICefBrowser;
      const Frame: ICefFrame; errorCode: TCefErrorCode; const errorText,
      failedUrl: ustring);
  public
    constructor Create(TheOwner: TComponent); override;
    //destructor Destroy; override;
    procedure Log(AMessage: ustring); // сообщение главному браузеру, что надо записать что-то в лог
    procedure Log(Msg: String; const Params: Array of const);
    function LoadUrl_1(const url:String; timeout: Integer = 20): Boolean;
    procedure InitDomainBlackList(const BlackDomains:Array of ShortString);
    function IsBlackDom(const Url:ustring):Boolean;
    property Html: String read FHtml;
  end;


implementation

{ TCustomRenderProcessHandler }

function TCustomRenderProcessHandler.OnProcessMessageReceived(
  const browser: ICefBrowser; sourceProcess: TCefProcessId;
  const message: ICefProcessMessage): Boolean;
begin
  case message.Name of
  'gethtml_1':
    begin
      //browser.MainFrame.GetSource(TCefStringVisitor.Create(browser, nil));
      Result := True;
    end;
  else
    Result := inherited;
  end;
end;

{ TCefStringVisitor }

procedure TCefStringVisitor.Visit(const str: ustring);
{var
  msg: ICefProcessMessage;}
begin
{  msg := TCefProcessMessageRef.New('gethtml');
  msg.ArgumentList.SetString(0, str);
  FBrowser.SendProcessMessage(PID_BROWSER, msg);}
{.$hint *** do it!!! ***}
  //FChromium.FHtml := us2s(MainForm.crm.Browser.MainFrame.Url);
  FChromium.FHtml := us2s(str);
end;

constructor TCefStringVisitor.Create(const browser: ICefBrowser;  const Chromium: TChromiumExt);
begin
  inherited Create;
  FBrowser := browser;
  FChromium := Chromium;
end;

{ TChromiumExt }

procedure TChromiumExt.DoLoadEnd(Sender: TObject;
  const ABrowser: ICefBrowser; const Frame: ICefFrame; httpStatusCode: Integer);
//var
//  res:boolean;
begin
  if Assigned(Frame) and Frame.IsMain and (ABrowser.Identifier = BrowserId) then
  begin
    FErrorCode := httpStatusCode;
    if httpStatusCode = 200 then
    begin
      ABrowser.MainFrame.GetSource(TCefStringVisitor.Create(ABrowser, Self));
      //lbUrl.Caption := 'Ok';
      //res := crm.Browser.SendProcessMessage(PID_RENDERER, TCefProcessMessageRef.New('gethtml'));
      //crm.Browser.MainFrame.GetSourceProc(@StringVisitor); // ok
    end else begin
      FErrorCode := httpStatusCode;
    end;
  end;
end;

procedure TChromiumExt.DoLoadError(Sender: TObject; const ABrowser: ICefBrowser;
  const Frame: ICefFrame; errorCode: TCefErrorCode; const errorText,
  failedUrl: ustring);
begin
  if FUrl <> failedUrl then
    Exit;
  FErrorCode := errorCode;
  FErrorText := us2s(errorText);
end;

function TChromiumExt.IsBlackDom(const Url: ustring): Boolean;
begin
  Result := Assigned(FDomainBlackList.Find(ParseURI(ShortString(Url)).Host));
end;

procedure TChromiumExt.InitDomainBlackList(const BlackDomains: array of ShortString);
var
  s: ShortString;
begin
  FDomainBlackList := TFPHashList.Create;
  for s in BlackDomains do
    FDomainBlackList.Add(s, FDomainBlackList);
end;

procedure TChromiumExt.DoBeforePopup(Sender: TObject; const abrowser: ICefBrowser;
  const frame: ICefFrame; const targetUrl, targetFrameName: ustring;
  targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean;
  var popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo;
  var client: ICefClient; var settings: TCefBrowserSettings;
  var noJavascriptAccess: Boolean; out Result: Boolean);
begin
  LoadUrl_1(us2s(targetUrl));
  Result := True;
end;

function TChromiumExt.LoadUrl_1(const url: String; timeout: Integer): Boolean;
begin
  FStartTime := DateTimeToUnix(Now);
  Result := False;
  FErrorCode := ERR_NONE;
  FErrorText := 'Timeout';
  FUrl := ustring(url);
  //FHtml := '';
  //FLoaded := False;
  FHtml := '';
  Load(url);
  while (FHtml = '') and (DateTimeToUnix(Now) - FStartTime < timeout) do
  begin
    Sleep(50);
    Application.ProcessMessages;
  end;
  if FHtml = '' then
    Browser.StopLoad;
end;

procedure TChromiumExt.DoBeforeResourceLoad(Sender: TObject;
  const abrowser: ICefBrowser; const frame: ICefFrame;
  const request: ICefRequest; const callback: ICefRequestCallback; out
  Result: TCefReturnValue);
var
  //map: ICefStringMultimap;
  url: ustring;
begin
  Result := RV_CONTINUE;
  url := request.Url;

  if (FLoadUrl = request.Url) and (FReferer <> '') then
  begin
    request.SetReferrer(FReferer, REFERRER_POLICY_ALWAYS);
    //map := TCefStringMultimapOwn.Create;
    //request.GetHeaderMap(map);
    //map.Append('Referer', FReferer);
    //request.SetHeaderMap(map);
  end;
  if IsBlackDom(url) then
    Result := RV_CANCEL;
end;

procedure TChromiumExt.DoProcessMessageReceived(Sender: TObject;
  const ABrowser: ICefBrowser; sourceProcess: TCefProcessId;
  const message: ICefProcessMessage; out Result: Boolean);
begin
  Result := True;
  case message.Name of
  'gethtml':
    FHtml := us2s(message.ArgumentList.GetString(0));
  'log':
    TrueLog(message.ArgumentList.GetString(0));
  else
    Result := False;
  end;
end;

constructor TChromiumExt.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  OnBeforePopup := @DoBeforePopup;
  OnBeforeResourceLoad := @DoBeforeResourceLoad;
  OnLoadEnd := @DoLoadEnd;
  OnLoadError := @DoLoadError;
  OnProcessMessageReceived := @DoProcessMessageReceived;
end;

procedure TChromiumExt.TrueLog(Msg: ustring);
begin
  Logger.Write('%s(%d): %s', [ClassName, Browser.Identifier, String(Msg)]);
end;

procedure TChromiumExt.Log(AMessage: ustring);
var
  msg: ICefProcessMessage;
begin
  msg := TCefProcessMessageRef.New('log');
  msg.ArgumentList.SetString(0, AMessage);
  Browser.SendProcessMessage(PID_BROWSER, msg);
end;

procedure TChromiumExt.Log(Msg: String; const Params: array of const);
begin
  Log(ustring(Format(Msg, Params)));
end;

end.

