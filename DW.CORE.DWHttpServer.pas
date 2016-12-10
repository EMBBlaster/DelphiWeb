{ *******************************************************************************
  DW.CORE.TDWHTTPServer is based in THttpAppSrv from
  Project Overbyte ICS http://www.overbyte.be/frame_index.html

  Delphi Web is
  Developped by Delcio Sbeghen @ SRP Sistemas
  delcio @ eavnet.com.br (remove spaces)
  under MIT Licence
}

unit DW.CORE.DWHttpServer;

interface

uses Classes, System.SysUtils, Vcl.ExtCtrls, OverbyteIcsHttpSrv, OverbyteIcsWSocketS,
  DW.CORE.DWClientConnection, DW.CORE.DWApplication;

type

  TDWHTTPServer = class(TCustomSslHttpServer)
  private
    [unsafe]
    FDWAppThreadList: TDWApplicationList;
    FHasAllocateHWnd: Boolean;
    // timer for Timeout Sessions
    FSessionTimer: TTimer;
    FDWAppCookieParam: string;
    procedure ConfigClientConnection(Client: TDWClientConnection);
    // Executed Before Process Request
    procedure DoBeforeProcessRequest(Client: TObject);
    procedure InitDWApplication(Client: TDWClientConnection);
    procedure SessionTimerHandler(Sender: TObject);
    procedure DoGetDocument(Sender: TObject; var Flags: THttpGetFlag);
    procedure DoPostDocument(Sender: TObject; var Flags: THttpGetFlag);
    procedure DoReceivePostedData(Sender: TObject; ErrCode: WORD);
    procedure DoDWAppCallBackExecute(Sender: TObject; var Flags: THttpGetFlag);
  protected
    procedure WSocketServerClientConnect(Sender: TObject; Client: TWSocketClient;
      Error: WORD); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // used to compound DWApplicationCookie
    procedure SetDWAppCookieParam(const Value: string);
  end;

implementation

type
  THackClient = Class(THttpConnection);

  { TDWHTTPServer }

procedure TDWHTTPServer.ConfigClientConnection(Client: TDWClientConnection);
begin
  THackClient(Client).FServer   := Self;
  Client.LineMode               := TRUE;
  Client.LineEdit               := FALSE;
  Client.LineEnd                := AnsiChar(#10);
  Client.DocDir                 := Self.DocDir;
  Client.TemplateDir            := Self.TemplateDir;
  Client.DefaultDoc             := Self.DefaultDoc;
  Client.OnGetDocument          := DoGetDocument;
  Client.OnHeadDocument         := TriggerHeadDocument;
  Client.OnPostDocument         := DoPostDocument;
  Client.OnOptionsDocument      := TriggerOptionsDocument; { V8.08 }
  Client.OnPutDocument          := TriggerPutDocument;     { V8.08 }
  Client.OnDeleteDocument       := TriggerDeleteDocument;  { V8.08 }
  Client.OnTraceDocument        := TriggerTraceDocument;   { V8.08 }
  Client.OnPatchDocument        := TriggerPatchDocument;   { V8.08 }
  Client.OnConnectDocument      := TriggerConnectDocument; { V8.08 }
  Client.OnPostedData           := DoReceivePostedData;
  Client.OnHttpRequestDone      := TriggerHttpRequestDone;
  Client.OnBeforeProcessRequest := DoBeforeProcessRequest;
  Client.OnFilterDirEntry       := TriggerFilterDirEntry;
  Client.MaxRequestsKeepAlive   := Self.MaxRequestsKeepAlive;
  Client.OnBeforeAnswer         := TriggerBeforeAnswer;         { V7.19 }
  Client.OnAfterAnswer          := TriggerAfterAnswer;          { V7.19 }
  Client.OnContentEncode        := TriggerContentEncode;        { V7.20 }
  Client.OnContEncoded          := TriggerContEncoded;          { V7.20 }
  Client.OnUnknownRequestMethod := TriggerUnknownRequestMethod; { V2.29 }
  Client.OnMimeContentType      := TriggerMimeContentType;      { V7.41 }
  //
  Client.SessionCookieName := 'DelphiWeb' + Port;
  Client.SetDWApplicationList(FDWAppThreadList);
end;

constructor TDWHTTPServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // At time of writing, the ancestor class do not call AllocateHWnd, so
  // we must do it. Just chech Window Handle to avoid allocating twice...
  if FHandle = 0 then
    begin
      FHasAllocateHWnd := TRUE;
      AllocateHWnd;
    end;
  FDWAppThreadList         := TDWApplicationList.Create;
  FClientClass             := TDWClientConnection;
  FSessionTimer            := TTimer.Create(nil);
  FSessionTimer.Enabled    := FALSE;
  FSessionTimer.OnTimer    := SessionTimerHandler;
  FSslEnable               := FALSE; // V8.02
  FWSocketServer.SslEnable := FALSE; // V8.02
end;

procedure TDWHTTPServer.SessionTimerHandler(Sender: TObject);
begin
  if Assigned(FDWAppThreadList) then
    FDWAppThreadList.RemoveAged;
end;

procedure TDWHTTPServer.SetDWAppCookieParam(const Value: string);
begin
  FDWAppCookieParam := Value;
end;

procedure TDWHTTPServer.DoDWAppCallBackExecute(Sender: TObject; var Flags: THttpGetFlag);
begin
  (* if Flags in [hgWillSendMySelf, hg404, hg403, hg401, hgAcceptData,   { V7.03 don't ignore Flags }
    hgSendDirList] then *)
  // Exit ;
  with Sender as TDWClientConnection do
    begin
      Flags := hgWillSendMySelf;
      DWApplication.ProcessRequest(Sender, Flags);
    end;
end;

destructor TDWHTTPServer.Destroy;
begin
  FDWAppThreadList.Free;
  inherited;
end;

procedure TDWHTTPServer.DoBeforeProcessRequest(Client: TObject);
begin
  // Set or create DWApplication for this ClientConnection
  InitDWApplication(TDWClientConnection(Client));

  if Assigned(FOnBeforeProcessRequest) then
    FOnBeforeProcessRequest(Self, Client);
end;

procedure TDWHTTPServer.InitDWApplication(Client: TDWClientConnection);
begin
  // verify if exists one application for this client connection
  // and set if exists, else if no exists, create a new DWApplication
  if Not Client.SetDWApplication then
    Client.StartNewDWApplication(FDWAppCookieParam);
end;

procedure TDWHTTPServer.WSocketServerClientConnect(Sender: TObject; Client: TWSocketClient;
  Error: WORD);
begin
  ConfigClientConnection(TDWClientConnection(Client));
  if Assigned(FOnClientConnect) then
    FOnClientConnect(Self, Client, Error);
end;

procedure TDWHTTPServer.DoGetDocument(Sender: TObject; var Flags: THttpGetFlag);
begin
  // OutputDebugString(PChar('HTTP_GET  ' + (Sender as THttpAppSrvConnection).Path));
  inherited TriggerGetDocument(Sender, Flags);
  if Flags in [hgWillSendMySelf, hg404, hg403, hg401, hgAcceptData, { V7.03 don't ignore Flags }
  hgSendDirList] then
    Exit;

  with Sender as TDWClientConnection do
    begin
      Flags := hgWillSendMySelf;
      DWApplication.ProcessRequest(Sender, Flags);
    end;

  // Reject anything else
  // Flags := hg404;
end;

procedure TDWHTTPServer.DoPostDocument(Sender: TObject; var Flags: THttpGetFlag);
var
  LClient: TDWClientConnection;
begin
  inherited TriggerPostDocument(Sender, Flags);
  LClient := Sender as TDWClientConnection;
  // if no posted data or all data received
  if LClient.PostedDataLen = LClient.RequestContentLength then
    // send Post to DWApplication
    DoDWAppCallBackExecute(Sender, Flags)
  else
    begin
      ReallocMem(LClient.PostedData, LClient.RequestContentLength + 1);
      LClient.PostedDataLen := 0;
      LClient.LineMode      := FALSE;
      Flags                 := hgAcceptData;
    end;
end;

procedure TDWHTTPServer.DoReceivePostedData(Sender: TObject; ErrCode: WORD);
var
  Len: Integer;
  Remains: Integer;
  Junk: array [0 .. 255] of char;
  ClientCnx: TDWClientConnection;
  Dummy: THttpGetFlag;
begin
  ClientCnx := Sender as TDWClientConnection;

  { How much data do we have to receive ? }
  Remains := ClientCnx.RequestContentLength - ClientCnx.PostedDataLen;
  if Remains <= 0 then
    begin
      { We got all our data. Junk anything else ! }
      Len := ClientCnx.Receive(@Junk, SizeOf(Junk) - 1);
      if Len >= 0 then
        Junk[Len] := #0;
      Exit;
    end;
  { Receive as much data as we need to receive. But warning: we may }
  { receive much less data. Data will be split into several packets we }
  { have to assemble in our buffer. }
  Len := ClientCnx.Receive(ClientCnx.PostedData + ClientCnx.PostedDataLen, Remains);
  { Sometimes, winsock doesn't wants to givve any data... }
  if Len <= 0 then
    Exit;

  { Add received length to our count }
  Inc(ClientCnx.PostedDataLen, Len);
  { Add a nul terminating byte (handy to handle data as a string) }
  ClientCnx.PostedData[ClientCnx.PostedDataLen] := #0;
  { Display receive data so far }
  // Display('Data: ''' + StrPas(ClientCnx.PostedData) + '''');

  { When we received the whole thing, we can process it }
  if ClientCnx.PostedDataLen = ClientCnx.RequestContentLength then
    begin
      { First we must tell the component that we've got all the data }
      ClientCnx.PostedDataReceived;
      // Execute the request
      try
        DoDWAppCallBackExecute(ClientCnx, Dummy);
      except
        on e: Exception do
          begin
            THackClient(ClientCnx).Answer404;
            raise Exception.Create('Error in DoReceivePostedData: ' + e.Message);
          end;
      end;
    end;
end;

end.
