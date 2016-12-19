{ *******************************************************************************
  DW.CORE.TDWClientConnection is based in THttpAppSrv from
  Project Overbyte ICS http://www.overbyte.be/frame_index.html

  Delphi Web is
  Developped by Delcio Sbeghen @ SRP Sistemas
  delcio @ eavnet.com.br (remove spaces)
  under MIT Licence
}

unit DW.CORE.DWClientConnection;

interface

uses
  Classes, System.SysUtils, OverbyteIcsHttpSrv, DW.CORE.DWApplication,
  DWUrlHandlerBase;

type

  TDWClientConnection     = class;
  TDWHttpHandlerProcedure = procedure(aClient: TDWClientConnection; aParams: TStrings;
    var Flags: THttpGetFlag) of object;

  // This class represent one Cliente Connection(Socket) with Server
  // This is changed for each new request
  TDWClientConnection = class(THttpConnection)
  private
    FSessionCookieName: string;
    // header of response
    FReplyHeader: string;
    FParamList: TStringList;
    procedure SetSessionCookieName(const Value: string);
    procedure SetReplyHeader(const Value: string);
  protected
    // List of active DWApplication Threads
    FDWApplicationList: TDWApplicationList;
    // DWApplication for this client connection
    FDWApplication: TDWAppThread;

    FOnDestroying: TNotifyEvent;
    FDWHTTPServer: TObject;
    function GetHostName: String;

  public
    PostedData: PAnsiChar;  // Will hold dynamically allocated buffer
    PostedDataLen: Integer; // Keep track of received byte count.
    destructor Destroy; override;
    // set the current DWApplication for this connection
    // return False if no DWApplication found
    // ITS A CORE FUNCTION, DO NOT CALL THIS OUTSIDE THE DWHttpServer.InitDWApplication
    function SetDWApplication: Boolean;
    // start new DWApplication(New Session Connection occured)
    // ITS A CORE FUNCTION, DO NOT CALL THIS OUTSIDE THE DWHttpServer.InitDWApplication
    procedure StartNewDWApplication(aCookieparam: string);
    // Set DWApplicationList to permit find DWApplication for this connection
    procedure SetDWApplicationList(aAppList: TDWApplicationList);
    procedure BeforeGetHandler(Proc: TDWHttpHandlerProcedure; var OK: Boolean); virtual;
    procedure BeforeObjGetHandler(SObj: TDWUrlHandlerBase; var OK: Boolean); virtual;
    procedure BeforePostHandler(Proc: TDWHttpHandlerProcedure; var OK: Boolean); virtual;
    procedure BeforeObjPostHandler(SObj: TDWUrlHandlerBase; var OK: Boolean); virtual;
    procedure NoGetHandler(var OK: Boolean); virtual;
    // Return DWApplication associated with this connection
    function DWApplication: TDWAppThread;
    // Return one TstringList with Request Header Params and post body params
    function ParamList: TStringList;
    procedure Answer500(aErrorMsg: string);
    property HostName: String read GetHostName;
    property OnDestroying: TNotifyEvent read FOnDestroying write FOnDestroying;
    property AppServer: TObject read FDWHTTPServer write FDWHTTPServer;
    // Name of Cookie to Save Session on Browser
    property SessionCookieName: string read FSessionCookieName write SetSessionCookieName;
    // header of response
    property ReplyHeader: string read FReplyHeader write SetReplyHeader;
  end;

implementation

uses DWUtils, OverbyteIcsWSocket, DWTypes;

{ TDWClientConnection }

procedure TDWClientConnection.Answer500(aErrorMsg: string);
var
  Body: String;
begin
  Body := '500 Internal Error';
  if aErrorMsg <> '' then
    Body := Body + #13#10 + #13#10 + aErrorMsg;
  SendHeader(FVersion + ' 500 Internal Error' + #13#10 + 'Content-Type: text/plain' + #13#10 +
    'Content-Length: ' + IntToStr(Length(Body)) + #13#10 + GetKeepAliveHdrLines + #13#10);
  FAnswerStatus := 500;
  if FSendType = httpSendHead then
    Send(nil, 0)
  else
    SendStr(Body);
end;

procedure TDWClientConnection.BeforeGetHandler(Proc: TDWHttpHandlerProcedure; var OK: Boolean);
begin

end;

procedure TDWClientConnection.BeforeObjGetHandler(SObj: TDWUrlHandlerBase; var OK: Boolean);
begin

end;

procedure TDWClientConnection.BeforeObjPostHandler(SObj: TDWUrlHandlerBase; var OK: Boolean);
begin

end;

procedure TDWClientConnection.BeforePostHandler(Proc: TDWHttpHandlerProcedure; var OK: Boolean);
begin

end;

destructor TDWClientConnection.Destroy;
begin
  if Assigned(FOnDestroying) then
    FOnDestroying(Self);

  if Assigned(PostedData) then
    begin
      FreeMem(PostedData);
      PostedData    := nil;
      PostedDataLen := 0;
    end;
  if Assigned(FParamList) then
    FParamList.Free;
  inherited Destroy;
end;

function TDWClientConnection.DWApplication: TDWAppThread;
begin
  Result := FDWApplication;
end;

function TDWClientConnection.GetHostName: String;
begin
  Result := AnsiToUnicode(WSocketResolveIp(AnsiString(PeerAddr)));
end;

procedure TDWClientConnection.NoGetHandler(var OK: Boolean);
begin

end;

function TDWClientConnection.ParamList: TStringList;
begin
  if not Assigned(FParamList) then
    begin
      FParamList                    := TStringList.Create;
      FParamList.Delimiter          := '&';
      FParamList.NameValueSeparator := '=';
      FParamList.QuoteChar          := '"';
    end;
  FParamList.Clear;
  if (Params <> '') and (AnsiString(PostedData) <> '') then
    FParamList.DelimitedText := Params + '&' + PostedData
  else if Params <> '' then
    FParamList.DelimitedText := Params
  else if (AnsiString(PostedData) <> '') then
    FParamList.DelimitedText := AnsiString(PostedData);
  Result                     := FParamList;
end;

function TDWClientConnection.SetDWApplication: Boolean;
var
  LList: Tlist;
  I: Integer;
  CookieValue: string;
begin
  Result := False;
  GetCookieValue(FRequestCookies, FSessionCookieName, CookieValue);
  LList := FDWApplicationList.LockList;
  try
    for I := 0 to LList.Count - 1 do
      begin
        if CookieValue = TDWAppThread(LList.Items[I]).DWApp.DWAppID then
          begin
            Self.FDWApplication := TDWAppThread(LList.Items[I]);
            Result              := True;
            Break;
          end;
      end;
  finally
    FDWApplicationList.UnlockList;
  end;
end;

procedure TDWClientConnection.SetDWApplicationList(aAppList: TDWApplicationList);
begin
  FDWApplicationList := aAppList;
end;

procedure TDWClientConnection.SetReplyHeader(const Value: string);
begin
  FReplyHeader := Value;
end;

procedure TDWClientConnection.SetSessionCookieName(const Value: string);
begin
  FSessionCookieName := Value;
end;

procedure TDWClientConnection.StartNewDWApplication(aCookieparam: string);
var
  TheSessionID: String;
  Year, Month, Day: Word;
  Hour, Min, Sec, MSec: Word;
  Today: TDateTime;
  LAppID: string;
  I: Integer;
begin
  Today := Now;
  DecodeDate(Today, Year, Month, Day);
  DecodeTime(Today, Hour, Min, Sec, MSec);
  TheSessionID := Format('^%s^%s^%04d%02d%02d %02d%02d%02d.%03d^',
    [UpperCase(Trim(aCookieparam)), IntToHex(FDWApplicationList.Count, 8), Year, Month, Day, Hour,
    Min, Sec, MSec]);
  LAppID         := Base64Encode(TheSessionID);
  FDWApplication := TDWAppThread.Create(LAppID, DWServer.MainForm);
  { TODO 1 -oDELCIO -cIMPLEMENT : Session Timeout Property in DWServer }
  FDWApplication.DWApp.SetSessionTimeOut(30);
  FDWApplicationList.Add(FDWApplication);
  ReplyHeader := NO_CACHE + MakeCookie(FSessionCookieName, LAppID, 0, '/');
  FDWApplication.DWApp.AddGetAlowedPath('/dwlib/', afBeginBy);
  { TODO 1 -oDELCIO -cIMPLEMENT : !!!!!!!!!! Syncronize this or ERRROR }
  for I := 0 to DWServer.AllowedPaths.Count - 1 do
    begin
      FDWApplication.DWApp.AddGetAlowedPath(DWServer.AllowedPaths[I], afBeginBy);
    end;
  FDWApplication.Start;
end;

end.
