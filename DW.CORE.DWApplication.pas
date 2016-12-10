{ *******************************************************************************
  DW.CORE.DWApplication contains code from
  Project Overbyte ICS http://www.overbyte.be/frame_index.html

  Delphi Web is
  Developped by Delcio Sbeghen @ SRP Sistemas
  delcio @ eavnet.com.br (remove spaces)
  under MIT Licence
}

unit DW.CORE.DWApplication;

interface

uses
  Classes, System.StrUtils, Winapi.Windows, System.SysUtils, System.SyncObjs,
  DW.CORE.DWHandlers, OverbyteIcsHttpSrv, System.Masks, DWTypes, DWCallbacks,
  DW.XHR.CallBackResp, DWForm;

type
  // list of DWApplication actives
  TDWApplicationList = Class(TThreadList)

  public
    procedure RemoveAged;
    function Count: Integer;

  End;

  // Represents one request in  TDWApplication.FRequestQueue;
  TDWAppRequet = class
    Client: TObject;
    Flags: PHttpGetFlag;
  end;

  { TDWApplication provides TApplication-like functionality in a VCL application,
    however it is one Thread created for each user session, and is responsible for all
    processing of that user and for isolating one user from another. }

  TDWApplication = class(TThread)
  private
    FGetHandler: TDWHttpHandlerList;
    FGetAllowedPath: TDWHttpAllowedPath;
    FPostHandler: TDWHttpHandlerList;
    // For TimeOut Control
    FLastAccess: TDateTime;
    FRequestCount: Integer;
    FSessionTimeOut: Integer;
    FOnDWApplicationTerminate: TNotifyEvent;
    FOnSessionClose: TNotifyEvent;
    // ID Of Application
    FAppID: string;
    // Queue of requests to be processed
    FRequestQueue: TThreadList;
    // UserSession Data
    FUserSessionData: TObject;
    FMsg_WM_FINISH: UINT;
    // list of post callback handlers
    FCallbacks: TDWCallBacks;
    FCallbackResp: TDWXhrCallbackResp;
    FIsCallBack: Boolean;
    FMainFormClass: TDWFormClass;
    FMainForm: TDWForm;
    FForms: TList;
    // Return true if Session is Timed Out
    function IsTimedOut: Boolean;
    procedure SetOnDWApplicationTerminate(const Value: TNotifyEvent);
    procedure SetOnSessionClose(const Value: TNotifyEvent);
    function GetDispatchCallBacks(aClient: TObject; var Flags: PHttpGetFlag): Boolean;
    function GetDispatchForms(aClient: TObject; var Flags: PHttpGetFlag): Boolean;
    procedure ExecuteRequest(aClient: TObject; Flags: PHttpGetFlag);
    function GetDispatchVirtualDocument(aClient: TObject; var Flags: PHttpGetFlag): Boolean;
    function GetDispatchNormalDocument(aClient: TObject; var Flags: PHttpGetFlag): Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(aAppId: string; aMainForm: TDWFormClass); reintroduce;
    destructor Destroy; override;
    function DWAppID: string;
    procedure AddGetAlowedPath(aPath: string; aFlags: TDWHttpAllowedFlag);
    procedure SetSessionTimeOut(aMinutes: Integer);
    // Return the Class of Main Web Form
    function MainFormClass: TDWFormClass;
    procedure AddForm(aForm: TDWForm);
    procedure RemoveForm(aForm: TDWForm);
    property Forms: TList read FForms;
    function RegisterCallBack(aControl: TObject; AType: TDWAsyncEventType;
      ACallbackProcedure: TDWCallbackProcedure): string;
    procedure UnregisterCallBack(const AName: String);
    // Contains the XHR Response for CallBacks
    function CallBackResp: TDWXhrCallbackResp;
    // Occurs when UserSession is Destroyed and before DWApplication Terminate
    property OnSessionClose: TNotifyEvent read FOnSessionClose write SetOnSessionClose;
    // Occurs when DWApplicatios is Terminated
    property OnDWApplicationTerminate: TNotifyEvent read FOnDWApplicationTerminate
      write SetOnDWApplicationTerminate;
    procedure ProcessRequest(aClientConnection: TObject; var Flags: THttpGetFlag);
    property UserSessionData: TObject read FUserSessionData;
    property CallBacks: TDWCallBacks read FCallbacks;
    // Return True if is processing an Callback
    property IsCallback: Boolean read FIsCallBack;
    // main web form for this WDApplication
    property MainForm: TDWForm read FMainForm write FMainForm;
  end;

implementation

uses DW.CORE.DWClientConnection, DWUrlHandlerForms,
  DW.VCL.CustomForm, OverbyteIcsMD5, DWUrlHandlerBase, DWUtils,
  OverbyteIcsWndControl, DW.CORE.UserSession;

type
  THackUrlHandler = class(TDWUrlHandlerBase);
  THackClient     = class(TDWClientConnection);

  { TDWApplicationList }

function TDWApplicationList.Count: Integer;
begin
  Result := LockList.Count;
  UnlockList;
end;

procedure TDWApplicationList.RemoveAged;
var
  I: Integer;
  lList: TList;
begin
  lList := LockList;
  try
    for I := 0 to lList.Count - 1 do
      begin
        if TDWApplication(lList.Items[I]).IsTimedOut then
          TDWApplication(lList.Items[I]).Terminate;
      end;
  finally
    UnlockList;
  end;
end;

{ TDWApplication }

procedure TDWApplication.AddForm(aForm: TDWForm);
begin
  if FForms.IndexOf(aForm) < 0 then
    FForms.Add(aForm);
end;

procedure TDWApplication.AddGetAlowedPath(aPath: string; aFlags: TDWHttpAllowedFlag);
(* var
  LItem:THttpAllowedElement;
  begin
  LItem:= THttpAllowedElement.Create;
  LItem.Path:= aPath;
  LItem.Flags:= aFlags;
  FGetAllowedPath.AddObject( LItem); *)
var
  Item: THttpAllowedElement;
  Index: Integer;
begin
  Index := FGetAllowedPath.IndexOf({$IFDEF POSIX}aPath{$ELSE}UpperCase(aPath){$ENDIF});
  if Index >= 0 then
    begin
      // Update the element if the path already exists
      Item       := THttpAllowedElement(FGetAllowedPath.Objects[Index]);
      Item.Flags := aFlags;
    end
  else
    begin
      // Create a new element if path doesn't exist yet
      Item       := THttpAllowedElement.Create;
      Item.Path  := {$IFDEF POSIX}Path{$ELSE}UpperCase(aPath){$ENDIF};
      Item.Flags := aFlags;
      FGetAllowedPath.AddObject(Item.Path, Item);
    end;
end;

constructor TDWApplication.Create(aAppId: string; aMainForm: TDWFormClass);
begin
  inherited Create(False);
  FRequestQueue   := TThreadList.Create;
  FAppID          := aAppId;
  FForms          := TList.Create;
  FMainFormClass  := aMainForm;
  FCallbacks      := TDWCallBacks.Create(Self);
  FCallbackResp   := TDWXhrCallbackResp.Create(Self);
  FGetHandler     := TDWHttpHandlerList.Create;
  FGetAllowedPath := TDWHttpAllowedPath.Create;
  FPostHandler    := TDWHttpHandlerList.Create;
  FRequestCount   := 0;
  FIsCallBack     := False;
  //Self.Start;
end;

destructor TDWApplication.Destroy;
begin
  FGetHandler.Free;
  FGetAllowedPath.Free;
  FPostHandler.Free;
  FRequestQueue.Free;
  FCallbackResp.Free;
  FCallbacks.Free;
  FForms.Free;
  inherited;
end;

function TDWApplication.RegisterCallBack(aControl: TObject; AType: TDWAsyncEventType;
  ACallbackProcedure: TDWCallbackProcedure): string;
begin
  Result := FCallbacks.RegisterCallBack(aControl, AType, ACallbackProcedure);
end;

procedure TDWApplication.RemoveForm(aForm: TDWForm);
var
  Lindex: Integer;
begin
  Lindex := FForms.IndexOf(aForm);
  if Lindex > -1 then
    FForms.Delete(Lindex);
end;

procedure TDWApplication.UnregisterCallBack(const AName: String);
begin
  FCallbacks.UnregisterCallBack(AName);
end;

function TDWApplication.CallBackResp: TDWXhrCallbackResp;
begin
  Result := FCallbackResp;
end;

function TDWApplication.DWAppID: string;
begin
  Result := FAppID;
end;

procedure TDWApplication.Execute;
var
  LRequest: TDWAppRequet;
  LQueue: TList;
begin
  while not Terminated do
    begin
      try
        try
          LQueue := FRequestQueue.LockList;
          try
            if LQueue.Count > 0 then
              begin
                LRequest := LQueue.Items[0];
                if Assigned(LRequest) and (Assigned(LRequest.Client)) then
                  begin
                    ExecuteRequest(LRequest.Client, LRequest.Flags);
                  end;
                LQueue.Delete(0);
              end;
          finally
            FRequestQueue.UnlockList;
          end;
        except
          raise Exception.Create('Shit, you have to finish this! "TDWApplication.Execute"');
        end;
      finally
        Sleep(10);
      end;
    end;
end;

procedure TDWApplication.ExecuteRequest(aClient: TObject; Flags: PHttpGetFlag);
begin
  // because no call inherited TriggerGetDocument FOnGetDocument is called here
  // if Assigned(FOnGetDocument) then
  // FOnGetDocument(Self, Sender, Flags);

  // if Flags in [hgWillSendMySelf, hg404, hg403, hg401, hgAcceptData, { V7.03 don't ignore Flags }
  // hgSendDirList] then
  // Exit;

  // dispatch callbacks
  if aClient = nil then
    Exit;
  if GetDispatchCallBacks(aClient, Flags) then
    Exit;

  // Handle Main Form or any session created form
  if aClient = nil then
    Exit;
  if (GetDispatchForms(aClient, Flags)) then
    Exit;

  // Handle all virtual documents. Returns TRUE if document handled.
  if aClient = nil then
    Exit;
  if (GetDispatchVirtualDocument(aClient, Flags)) then
    Exit;

  // Handle all normal (static) documents. Returns TRUE if document handled.
  if aClient = nil then
    Exit;
  if GetDispatchNormalDocument(aClient, Flags) then
    Exit;

  // Reject anything else
  if aClient = nil then
    Exit;
  if TDWClientConnection(aClient).RequestMethod = THttpMethod.httpMethodPost then
    THackClient(aClient).Answer404 // if request is POST, Flags is Dummy and not processed after
  else                             // else change flag to process response after
    Flags^ := hg404;
end;

function TDWApplication.GetDispatchCallBacks(aClient: TObject; var Flags: PHttpGetFlag): Boolean;
var
  LClient: TDWClientConnection;
  LCallBackName: string;
  I, C: Integer;
  LCallbackIndex: Integer;
  LCallBack: TDWCallback;
begin
  Result := False;
  //
  LClient := TDWClientConnection(aClient);
  if LClient = nil then
    Exit;
  LCallBackName := LClient.ParamList.Values['callback'];
  if (Self = nil) or (LCallBackName = '') then
    Exit;
  // Find form
  for I := 0 to Self.Forms.Count - 1 do
    begin
      // if url match of Form Name
      if AnsiEndsText(TDWCustomForm(Self.Forms[I]).Name, LClient.Path) then
        begin
          for C := 0 to DWApplication.CallBacks.Count - 1 do
            begin
              LCallbackIndex := DWApplication.CallBacks.FindCallback(LCallBackName);
              if LCallbackIndex <> -1 then
                begin
                  FIsCallBack := True;
                  try
                    FCallbackResp.Clear;
                    LCallBack := DWApplication.CallBacks.Objects[LCallbackIndex] as TDWCallback;
                    TDWCustomForm(Self.Forms.Items[I]).ExecuteCallBack(LClient.ParamList,
                      LCallBack);
                    LClient.AnswerString(Flags^, '', 'text/xml', NO_CACHE, FCallbackResp.Render);
                  finally
                    FIsCallBack := False;
                  end;
                  // LClient.PutStringInSendBuffer(FCallbackResp.Render);
                  // LClient.SendStream;
                  Result := True;
                  Break;
                end;
            end;
        end;
    end;
end;

function TDWApplication.GetDispatchForms(aClient: TObject; var Flags: PHttpGetFlag): Boolean;
var
  LUserSession: TDWUserSession;
  HandlerForms: TUrlHandlerForms;
  OK: Boolean;
  I: Integer;
  LClient: TDWClientConnection;
begin
  Result  := False;
  LClient := TDWClientConnection(aClient);
  if LClient = nil then
    Exit;
  // if Session Data is not created
  if FUserSessionData = nil then
    begin
      // create session Data
      LUserSession     := TDWUserSession.Create(nil);
      FUserSessionData := LUserSession;
      // redirect to DWURLHandlerForms for render MainForm
      Result := True;
    end
  else
    begin
      // get the session
      LUserSession := FUserSessionData as TDWUserSession;
      // check if url correspond with one FormName of Session instanced forms
      for I := 0 to Self.Forms.Count - 1 do
        begin
          // if url match of Form Name
          if AnsiEndsText(TDWCustomForm(Self.Forms[I]).Name, LClient.Path) then
            begin
              LClient.ReplyHeader := NO_CACHE;
              // redirect to DWURLHandlerForms for render Form
              Result := True;
              Break;
            end;
        end;
      if (Not Result) and ((LClient.Path = '/') or (LClient.Path = ('/' + 'index.html'))) then
        begin
          LClient.ReplyHeader := NO_CACHE;
          // redirect to DWURLHandlerForms for render MainForm
          Result := True;
        end;
    end;

  if Result then
    begin
      // set User Session params
      FLastAccess := Now;
      AtomicIncrement(FRequestCount, 1);
      LUserSession.LoginChallenge := StrMD5(IntToHex(DWGetTickCount, 8));
      // Create the TUrlHandlerForms to dispath form
      HandlerForms := TUrlHandlerForms.Create(nil);
      try
        THackUrlHandler(HandlerForms).FClient        := LClient;
        THackUrlHandler(HandlerForms).FFlags         := hgWillSendMySelf;
        THackUrlHandler(HandlerForms).FMsg_WM_FINISH := FMsg_WM_FINISH;
        THackUrlHandler(HandlerForms).FWndHandle     := LClient.Server.Handle;
        THackUrlHandler(HandlerForms).FMethod        := TDWHttpMethod.httpMethodGet;
        LClient.OnDestroying := THackUrlHandler(HandlerForms).ClientDestroying;
        OK := True;
        LClient.BeforeObjGetHandler(HandlerForms, OK);
        if OK then
          begin
            HandlerForms.Execute;
            Flags^ := THackUrlHandler(HandlerForms).FFlags;
          end
        else
          begin
            Flags^ := THackUrlHandler(HandlerForms).FFlags;
            FreeAndNil(HandlerForms);
          end;
      except
        on E: Exception do
          begin
            FreeAndNil(HandlerForms);
            // if Assigned(FOnVirtualExceptionEvent) then { V7.05 }
            // FOnVirtualExceptionEvent(Self, E, httpMethodGet, ClientCnx.Path);
          end;
      end;
    end;
end;

function TDWApplication.GetDispatchVirtualDocument(aClient: TObject;
  var Flags: PHttpGetFlag): Boolean;
var
  I: Integer;
  PathBuf: String;
  Status: Boolean;
  Proc: TMethod;
  OK: Boolean;
  Disp: THttpDispatchElement;
  SObj: TDWUrlHandlerBase;
  LClient: TDWClientConnection;
begin
  LClient := TDWClientConnection(aClient);
  if LClient = nil then
    Exit;
  for I := 0 to FGetHandler.Count - 1 do
    begin
      PathBuf := FGetHandler.Strings[I];

      if MatchesMask(LClient.Path, PathBuf) then
        Status := True
      else
        Status := (CompareText(PathBuf, LClient.Path) = 0);

      if Status then
        begin
          Result := True;
          Disp   := FGetHandler.Disp[I];
          Flags^ := Disp.Flags;
          OK     := True;
          if Disp.Proc <> nil then
            begin
              Proc.Code := Disp.Proc;
              Proc.Data := LClient;
              LClient.BeforeGetHandler(TDWHttpHandlerProcedure(Proc), OK);
              if OK and (Proc.Code <> nil) then
                TDWHttpHandlerProcedure(Proc)(Flags^);
            end
          else if Disp.SObjClass <> nil then
            begin
              SObj := Disp.SObjClass.Create(nil);
              try
                THackUrlHandler(SObj).FClient        := LClient;
                THackUrlHandler(SObj).FFlags         := Disp.Flags;
                THackUrlHandler(SObj).FMsg_WM_FINISH := FMsg_WM_FINISH;
                THackUrlHandler(SObj).FWndHandle     := LClient.Server.Handle;
                THackUrlHandler(SObj).FMethod        := httpMethodGet;
                LClient.OnDestroying                 := THackUrlHandler(SObj).ClientDestroying;
                LClient.BeforeObjGetHandler(SObj, OK);
                if OK then
                  begin
                    SObj.Execute;
                    Flags^ := THackUrlHandler(SObj).FFlags;
                  end
                else
                  begin
                    Flags^ := THackUrlHandler(SObj).FFlags;
                    FreeAndNil(SObj);
                  end;
              except
                on E: Exception do
                  begin
                    FreeAndNil(SObj);
                    // if Assigned(FOnVirtualExceptionEvent) then { V7.05 }
                    // FOnVirtualExceptionEvent(Self, E, httpMethodGet, LClient.Path);
                  end;
              end;
            end;
          Exit;
        end;
    end;
  Result := False;
  if (Assigned(LClient)) then
    LClient.NoGetHandler(OK);
end;

function TDWApplication.GetDispatchNormalDocument(aClient: TObject;
  var Flags: PHttpGetFlag): Boolean;
var
  I: Integer;
  Elem: THttpAllowedElement;
  LClient: TDWClientConnection;
begin
  LClient := aClient as TDWClientConnection;
  for I   := 0 to FGetAllowedPath.Count - 1 do
    begin
      Elem := FGetAllowedPath.Elem[I];
      case Elem.Flags of
        afBeginBy:
          begin
            if AnsiStartsText(Elem.Path, LClient.Path) then
              begin
                Flags^ := hgWillSendMySelf;
                LClient.SendDocument;
                Result := True;
                Exit;
              end;
          end;
        afExactMatch:
          begin
            if CompareText(Elem.Path, LClient.Path) = 0 then
              begin
                Flags^ := hgWillSendMySelf;
                LClient.SendDocument;
                Result := True;
                Exit;
              end;
          end;
        afDirList:
          begin
            if CompareText(Elem.Path, LClient.Path) = 0 then
              begin
                Flags^           := hgSendDirList;
                LClient.Document := LClient.Path;
                LClient.SendDocument;

                // LClient.AnswerPage(Flags^, '', '', LClient.Path, nil, []);
                Result := True;
                Exit;
              end;
          end;
      end;
    end;
  Result := False;
end;

function TDWApplication.IsTimedOut: Boolean;
begin
  Result := FLastAccess + FSessionTimeOut < Now;
end;

function TDWApplication.MainFormClass: TDWFormClass;
begin
  Result := FMainFormClass;
end;

procedure TDWApplication.ProcessRequest(aClientConnection: TObject; var Flags: THttpGetFlag);
var
  LRequest: TDWAppRequet;
begin
  LRequest        := TDWAppRequet.Create;
  LRequest.Client := aClientConnection;
  LRequest.Flags  := @Flags;
  FRequestQueue.Add(LRequest);
end;

procedure TDWApplication.SetOnDWApplicationTerminate(const Value: TNotifyEvent);
begin
  FOnDWApplicationTerminate := Value;
end;

procedure TDWApplication.SetOnSessionClose(const Value: TNotifyEvent);
begin
  FOnSessionClose := Value;
end;

procedure TDWApplication.SetSessionTimeOut(aMinutes: Integer);
begin
  FSessionTimeOut := aMinutes;
end;

end.
