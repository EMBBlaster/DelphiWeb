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
  Classes, System.StrUtils, dialogs, Controls, Winapi.Windows, System.SysUtils,
  System.DateUtils, System.Win.ComObj, DW.CORE.DWHandlers, OverbyteIcsHttpSrv,
  System.Masks, DWTypes, DWCallbacks, DW.XHR.CallBackResp, DWForm,
  DW.VCL.CustomForm,
  DWUrlHandlerBase, DWUrlHandlerRest, DW.CORE.Cache, JclDebug, DW.VCL.Buttons,
  System.SyncObjs;

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
    however it is created for each user session, under an user thread,
    and is responsible for isolating one user from another. }
  TDWApplication = class(TComponent)
  private
    FGetHandler: TDWHttpHandlerList;
    FGetAllowedPath: TDWHttpAllowedPath;
    // FPostHandler: TDWHttpHandlerList;
    // For TimeOut Control
    FLastAccess: TDateTime;
    FRequestCount: Integer;
    FSessionTimeOut: Integer;
    FOnDWApplicationTerminate: TNotifyEvent;
    FOnSessionClose: TNotifyEvent;
    // ID Of Application
    FAppID: string;
    // UserSession Data
    FUserSessionData: TObject;
    // list of post callback handlers
    FCallbacks: TDWCallBacks;
    FCallbackResp: TDWXhrCallbackResp;
    FMainFormClass: TDWFormClass;
    FMainForm: TDWForm;
    FForms: TList;
    FActiveForm: TDWCustomForm;
    // contais an list of Objects to be released(free on next thread loop)
    // this is needed for simulate VCL Form.Release;
    FReleaseList: TList;
    // Return true if Session is Timed Out
    function IsTimedOut: Boolean;
    procedure SetOnDWApplicationTerminate(const Value: TNotifyEvent);
    procedure SetOnSessionClose(const Value: TNotifyEvent);
    procedure SetActiveForm(const Value: TDWCustomForm);
  protected
    FIsCallBack: Boolean;
    FCacheList: TDWCacheList;
  public
    constructor Create(aAppId: string; aMainForm: TDWFormClass); overload;
    destructor Destroy; override;
    // add object to release list,
    // FreleaseList is an list of Objects to be released(free on next thread loop)
    // this is needed for simulate VCL Form.Release;
    procedure ReleaseObject(aObject: TObject);
    function DWAppID: string;
    procedure AddGetAlowedPath(aPath: string; aFlags: TDWHttpAllowedFlag);
    procedure SetSessionTimeOut(aMinutes: Integer);
    // Return the Class of Main Web Form
    function MainFormClass: TDWFormClass;
    procedure AddForm(aForm: TDWCustomForm);
    procedure RemoveForm(aForm: TDWCustomForm);
    property Forms: TList read FForms;
    function RegisterCallBack(aControl: TObject; AType: TDWAsyncEventType;
      ACallbackProcedure: TDWCallbackProcedure): string; overload;
    function RegisterCallBack(aControl: TObject; aName: string;
      ACallbackProcedure: TDWCallbackProcedure): string; overload;
    procedure UnregisterCallBack(const aName: String);
    // register content handler for process GET's from specific url
    function RegisterGetHandler(aControl: TComponent; aUrlPath: string; aProcedure: Pointer;
      aHandler: TDWUrlHandlerClass): string;
    // register content handler for process GET's from specific url
    function RegisterRestCallBack(aControl: TControl; aUrlPath: string;
      aProcedure: TDWRestCallBackFunction): string;

    // Show message dialog in browser
    procedure ShowMessage(aMsg: string; AType: TDWRegionBack = bsrbInfo);
    // Show Confirmation Message in Browser
    procedure ShowConfirm(aMsg: string; aCallBackName: string; aTitle: string;
      BtnOkText: string = 'OK'; BtnCancelText: string = 'CANCEL');
    // Show Confirmation Message in Browser
    procedure ShowPrompt(aMsg: string; aCallbackProc: TDWAsyncEventProc; aTitle: string;
      aDefaultText: string);
    // Show an Alert Message
    procedure ShowAlert(aMsg: string);
    // terminate the DWpplication and session
    procedure Terminate;
    // Send File to Browser
    procedure SendFile(aFilePath: string);
    // Send Stream to Browser
    procedure SendStream(aStream: TStream);
    // Contains the XHR Response for CallBacks
    function CallBackResp: TDWXhrCallbackResp;
    // Occurs when UserSession is Destroyed and before DWApplication Terminate
    property OnSessionClose: TNotifyEvent read FOnSessionClose write SetOnSessionClose;
    // Occurs when DWApplicatios is Terminated
    property OnDWApplicationTerminate: TNotifyEvent read FOnDWApplicationTerminate
      write SetOnDWApplicationTerminate;
    property UserSessionData: TObject read FUserSessionData;
    property CallBacks: TDWCallBacks read FCallbacks;
    // Return True if is processing an Callback
    property IsCallback: Boolean read FIsCallBack;
    // main web form for this WDApplication
    property MainForm: TDWForm read FMainForm write FMainForm;
    property ActiveForm: TDWCustomForm read FActiveForm write SetActiveForm;
  end;

  { TDWAppThread is created for each user session, and is responsible for all
    processing of that user and for isolating one user from another. }
  TDWAppThread = class(TJclDebugThread)
  private
    FWait: TEvent;
    // Represents de Application Object for this thread
    FDWApp: TDWApplication;
    // Queue of requests to be processed
    FRequestQueue: TThreadList;
    { TODO 1 -oDELCIO -cIMPLEMENT : FMsg_WM_FINISH }
    FMsg_WM_FINISH: UINT;
    function GetDispatchCallBacks(aClient: TObject; var Flags: PHttpGetFlag): Boolean;
    function GetDispatchForms(aClient: TObject; var Flags: PHttpGetFlag): Boolean;
    procedure ExecuteRequest(aClient: TObject; Flags: PHttpGetFlag);
    function GetDispatchVirtualDocument(aClient: TObject; var Flags: PHttpGetFlag): Boolean;
    function GetDispatchNormalDocument(aClient: TObject; var Flags: PHttpGetFlag): Boolean;
    procedure ProcessException(e: Exception);
  protected
    procedure Execute; override;
    // for release waitfor and permit Terminate thread
    procedure TerminatedSet; override;
  public
    constructor Create(aAppId: string; aMainForm: TDWFormClass); reintroduce;
    destructor Destroy; override;
    // Return the DWApplication Object for this thread
    // !!!! This is not thread-safe if called from other thread
    function DWApp: TDWApplication;
    procedure ProcessRequest(aClientConnection: TObject; var Flags: THttpGetFlag);
  end;

implementation

uses DW.CORE.DWClientConnection, DWUrlHandlerForms, OverbyteIcsMD5, DWUtils,
  DW.VCL.Interfaces,
  OverbyteIcsWndControl, DW.CORE.UserSession, DW.VCL.dialogs, DW.CORE.Server,
  DW.VCL.Input, DW.VCL.Region, DWGlobal;

type
  THackUrlHandler = class(TDWUrlHandlerBase);
  THackClient     = class(TDWClientConnection);
  THackServer     = class(TDWServer);

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
        if TDWAppThread(lList.Items[I]).FDWApp.IsTimedOut then
          TDWAppThread(lList.Items[I]).Terminate;
      end;
  finally
    UnlockList;
  end;
end;

{ TDWApplication }
constructor TDWApplication.Create(aAppId: string; aMainForm: TDWFormClass);
begin
  inherited Create(nil);
  FAppID                 := aAppId;
  FMainFormClass         := aMainForm;
  FForms                 := TList.Create;
  FCallbacks             := TDWCallBacks.Create(Self);
  FCallbackResp          := TDWXhrCallbackResp.Create(Self);
  FGetHandler            := TDWHttpHandlerList.Create;
  FGetHandler.Duplicates := dupIgnore;
  FGetAllowedPath        := TDWHttpAllowedPath.Create;
  // FPostHandler    := TDWHttpHandlerList.Create;
  FRequestCount          := 0;
  FIsCallBack            := False;
  FCacheList             := TDWCacheList.Create;
  FCacheList.OwnsObjects := True;
  FCacheList.Duplicates  := dupIgnore;
  FReleaseList           := TList.Create;

end;

destructor TDWApplication.Destroy;
var
  I: Integer;
begin
  FGetHandler.Free;
  FGetAllowedPath.Free;
  // FPostHandler.Free;
  FCallbackResp.Free;
  FCallbacks.Free;
  FForms.Free;
  FCacheList.Free;
  // process pending Objects to be released
  for I := 0 to FReleaseList.Count - 1 do
    TObject(FReleaseList[I]).Free;
  FReleaseList.Free;
  inherited;
end;

function TDWApplication.DWAppID: string;
begin
  Result := FAppID;
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

procedure TDWApplication.SetSessionTimeOut(aMinutes: Integer);
begin
  FSessionTimeOut := aMinutes;
end;

function TDWApplication.IsTimedOut: Boolean;
begin
  Result := IncMinute(FLastAccess, FSessionTimeOut) < Now;
end;

function TDWApplication.MainFormClass: TDWFormClass;
begin
  Result := FMainFormClass;
end;

procedure TDWApplication.AddForm(aForm: TDWCustomForm);
begin
  if FForms.IndexOf(aForm) < 0 then
    FForms.Add(aForm);
end;

procedure TDWApplication.RemoveForm(aForm: TDWCustomForm);
var
  Lindex: Integer;
begin
  Lindex := FForms.IndexOf(aForm);
  if Lindex > -1 then
    FForms.Delete(Lindex);
end;

function TDWApplication.RegisterCallBack(aControl: TObject; AType: TDWAsyncEventType;
  ACallbackProcedure: TDWCallbackProcedure): string;
begin
  Result := FCallbacks.RegisterCallBack(aControl, AType, ACallbackProcedure);
end;

function TDWApplication.RegisterCallBack(aControl: TObject; aName: string;
  ACallbackProcedure: TDWCallbackProcedure): string;
begin
  Result := FCallbacks.RegisterCallBack(aControl, aName, ACallbackProcedure);
end;

function TDWApplication.RegisterGetHandler(aControl: TComponent; aUrlPath: string;
  aProcedure: Pointer; aHandler: TDWUrlHandlerClass): string;
var
  aDispatch: THttpDispatchElement;
  L_IControl: IDWCOntrol;
begin
  Result := '';
  with aDispatch.Create do
    begin
      if (aControl <> nil) and (Supports(aControl, IDWCOntrol, L_IControl)) and (L_IControl <> nil)
      then
        Path := L_IControl.Form.Name + '.' + aControl.Name + '.' + aUrlPath
      else if (aControl <> nil) then
        Path := 'NIL.' + aControl.Name + '.' + aUrlPath
      else
        Path    := 'NIL.NIL.' + aUrlPath;
      Flags     := THttpGetFlag.hgWillSendMySelf;
      Proc      := aProcedure;
      SObjClass := aHandler;
    end;
  FGetHandler.AddObject(aUrlPath, aDispatch);
  Result := aDispatch.Path;
end;

function TDWApplication.RegisterRestCallBack(aControl: TControl; aUrlPath: string;
  aProcedure: TDWRestCallBackFunction): string;
begin
  RegisterGetHandler(aControl, aUrlPath, @aProcedure, TDWRestHandlerRest);
end;

procedure TDWApplication.ReleaseObject(aObject: TObject);
begin
  FReleaseList.Add(aObject);
end;

procedure TDWApplication.UnregisterCallBack(const aName: String);
begin
  FCallbacks.UnregisterCallBack(aName);
end;

procedure TDWApplication.SendFile(aFilePath: string);
begin
  { TODO 1 -oDELCIO -cIMPLEMENT : Send file to browser }
  raise Exception.Create('Need to implement TDWApplication.SendFile');
end;

procedure TDWApplication.SendStream(aStream: TStream);
begin
  { TODO 1 -oDELCIO -cIMPLEMENT : Send stream to browser }
  raise Exception.Create('Need to implement TDWApplication.SendStream');
end;

procedure TDWApplication.ShowAlert(aMsg: string);
begin
  with TDWAlert.Create(aMsg, bsasWarning) do
    begin
      //
    end;
end;

procedure TDWApplication.ShowConfirm(aMsg, aCallBackName, aTitle, BtnOkText: string;
  BtnCancelText: string);
begin
  { TODO 1 -oDELCIO -cIMPLEMENT : Show Confirmation Message }
  raise Exception.Create('Need to implement TDWApplication.ShowConfirm');
end;

procedure TDWApplication.ShowMessage(aMsg: string; AType: TDWRegionBack = bsrbInfo);
begin
  if AType <> bsrbInfo then
    raise Exception.Create
      ('Need to implement TDWApplication.ShowMessage for aType diff of bsrbInfo');
  with TDWDialog.Create(ActiveForm, 'Message:', aMsg) do
    begin
      Parent := ActiveForm;
    end;
end;

procedure TDWApplication.ShowPrompt(aMsg: string; aCallbackProc: TDWAsyncEventProc;
  aTitle, aDefaultText: string);
var
  LInput: TDWInput;
  LDialog: TDWDialog;
begin
  LDialog        := TDWDialog.Create(aTitle, aMsg, aCallbackProc);
  LDialog.Parent := ActiveForm;
  LInput         := TDWInput.Create(LDialog);
  LInput.Parent  := LDialog.GetBody;
  LInput.Text    := aDefaultText;
end;

procedure TDWApplication.Terminate;
begin
  raise Exception.Create('Need to Implement Terminate');
end;

function TDWApplication.CallBackResp: TDWXhrCallbackResp;
begin
  Result := FCallbackResp;
end;

procedure TDWApplication.SetOnSessionClose(const Value: TNotifyEvent);
begin
  FOnSessionClose := Value;
end;

procedure TDWApplication.SetOnDWApplicationTerminate(const Value: TNotifyEvent);
begin
  FOnDWApplicationTerminate := Value;
end;

procedure TDWApplication.SetActiveForm(const Value: TDWCustomForm);
begin
  FActiveForm := Value;
end;

{ TDWAppThread }
constructor TDWAppThread.Create(aAppId: string; aMainForm: TDWFormClass);
begin
  inherited Create(True, aAppId);
  FWait         := TEvent.Create(nil, True, False, '');
  FDWApp        := TDWApplication.Create(aAppId, aMainForm);
  FRequestQueue := TThreadList.Create;
  // FAppID          := aAppId;
  // FForms          := TList.Create;
  // FMainFormClass  := aMainForm;
  // FCallbacks      := TDWCallBacks.Create(Self);
  // FCallbackResp   := TDWXhrCallbackResp.Create(Self);
  // FGetHandler     := TDWHttpHandlerList.Create;
  // FGetAllowedPath := TDWHttpAllowedPath.Create;
  // FPostHandler    := TDWHttpHandlerList.Create;
  // FRequestCount   := 0;
  // FIsCallBack     := False;
  // Self.Start;
  THackServer(DWServer).DoNewSession(Self);
  // Suspended:=False;
end;

destructor TDWAppThread.Destroy;
begin
  THackServer(DWServer).DoDWAppTerminate(Self);
  // FGetHandler.Free;
  // FGetAllowedPath.Free;
  // FPostHandler.Free;
  FRequestQueue.Free;
  // FCallbackResp.Free;
  // FCallbacks.Free;
  // FForms.Free;
  FDWApp.Free;
  FWait.Free;
  inherited;
end;

function TDWAppThread.DWApp: TDWApplication;
begin
  Result := FDWApp;
end;

procedure TDWAppThread.Execute;
var
  LRequest: TDWAppRequet;
  LQueue: TList;
  R: Integer;
  QueueListLocked: Boolean;
  Cominitiated: Boolean;
begin
  { TODO 1 -oDELCIO -cIMPLEMENT : COM INITIALIZATION }
  (* if DWServer.ComInitilization then
    begin
    CoInitialize;
    Cominitiated:=True;
    end
    else
    Cominitiated:=False; *)
  try
    { TODO 1 -oDELCIO -cIMPROVE : SEE OmniThreadLibrary }

    repeat
      try
        // process release list for free objects called Release
        for R := FDWApp.FReleaseList.Count - 1 downto 0 do
          begin
            try
              if TObject(FDWApp.FReleaseList[R]).InheritsFrom(TDWCustomForm) then
                FDWApp.RemoveForm(TDWCustomForm(FDWApp.FReleaseList[R]));
              TObject(FDWApp.FReleaseList[R]).Free;
            finally
              FDWApp.FReleaseList.Delete(R);
            end;
          end;
        // procedd Queue of requests
        LQueue          := FRequestQueue.LockList;
        QueueListLocked := True;
        try
          if LQueue.Count > 0 then
            begin
              LRequest := LQueue.Items[0];
              try
                if Assigned(LRequest) and (Assigned(LRequest.Client)) then
                  begin
                    ExecuteRequest(LRequest.Client, LRequest.Flags);
                    // nil request variable
                    LRequest := nil;
                  end;
              finally
                // remove work from queue and Free request
                LQueue.Delete(0);
              end;
            end
          else // if no more work in queue
            begin
              FRequestQueue.UnlockList;
              QueueListLocked := False;
              // wait for next queue work
              FWait.WaitFor(INFINITE);
              FWait.ResetEvent;
            end;
        finally
          if QueueListLocked then
            FRequestQueue.UnlockList;
        end;
      except
        on e: Exception do // on error
          begin
            // try send 500 internal Error
            try
              if (LRequest <> nil) and (LRequest.Client <> nil) then
                TDWClientConnection(LRequest.Client).Answer500(e.Message);
            except
            end;
            // handle exception an log stack trace
            HandleException;
          end;
        // ProcessException(e);
      end;
    until Terminated;
  finally
    { if Cominitiated then
      ComUnitialize }
  end;
end;

procedure TDWAppThread.ExecuteRequest(aClient: TObject; Flags: PHttpGetFlag);
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

  THackClient(aClient).Answer404;
  { if TDWClientConnection(aClient).RequestMethod = THttpMethod.httpMethodPost then
    THackClient(aClient).Answer404 // if request is POST, Flags is Dummy and not processed after
    else                             // else change flag to process response after


    Flags^ := hg404; }
end;

function TDWAppThread.GetDispatchCallBacks(aClient: TObject; var Flags: PHttpGetFlag): Boolean;
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
  for I := 0 to Self.FDWApp.Forms.Count - 1 do
    begin
      // if url match of Form Name
      if AnsiEndsText(TDWCustomForm(Self.FDWApp.Forms[I]).Name, LClient.Path) then
        begin
          for C := 0 to FDWApp.CallBacks.Count - 1 do
            begin
              LCallbackIndex := Self.FDWApp.CallBacks.FindCallback(LCallBackName);
              if LCallbackIndex <> -1 then
                begin
                  FDWApp.FIsCallBack := True;
                  try
                    FDWApp.FCallbackResp.Clear;
                    LCallBack := Self.FDWApp.CallBacks.Objects[LCallbackIndex] as TDWCallback;
                    TDWCustomForm(Self.FDWApp.Forms.Items[I]).ExecuteCallBack(LClient.ParamList,
                      LCallBack);
                    LClient.AnswerString(Flags^, '', 'text/xml', NO_CACHE,
                      FDWApp.FCallbackResp.Render);
                  finally
                    FDWApp.FIsCallBack := False;
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

function TDWAppThread.GetDispatchForms(aClient: TObject; var Flags: PHttpGetFlag): Boolean;
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
  if FDWApp.FUserSessionData = nil then
    begin
      // create session Data
      if gUserSessionClass = nil then
        raise Exception.Create('UserSessionClass need to be set.');
      LUserSession            := gUserSessionClass.Create(FDWApp);
      FDWApp.FUserSessionData := LUserSession;
      // redirect to DWURLHandlerForms for render MainForm
      Result := True;
    end
  else
    begin
      // get the session
      LUserSession := FDWApp.FUserSessionData as TDWUserSession;
      // check if url correspond with one FormName of Session instanced forms
      for I := 0 to Self.FDWApp.Forms.Count - 1 do
        begin
          // if url match of Form Name
          if AnsiEndsText(TDWCustomForm(Self.FDWApp.Forms[I]).Name, LClient.Path) then
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
      FDWApp.FLastAccess := Now;
      AtomicIncrement(FDWApp.FRequestCount, 1);
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
        on e: Exception do
          begin
            FreeAndNil(HandlerForms);
            // if Assigned(FOnVirtualExceptionEvent) then { V7.05 }
            // FOnVirtualExceptionEvent(Self, E, httpMethodGet, ClientCnx.Path);
          end;
      end;
    end;
end;

function TDWAppThread.GetDispatchVirtualDocument(aClient: TObject; var Flags: PHttpGetFlag)
  : Boolean;
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
  Result  := False;
  LClient := TDWClientConnection(aClient);
  if LClient = nil then
    Exit;
  for I := 0 to FDWApp.FGetHandler.Count - 1 do
    begin
      PathBuf := FDWApp.FGetHandler.Strings[I];

      if MatchesMask(LClient.Path, PathBuf) then
        Status := True
      else
        Status := (CompareText(PathBuf, LClient.Path) = 0);

      if Status then
        begin
          Result := True;
          Disp   := FDWApp.FGetHandler.Disp[I];
          Flags^ := Disp.Flags;
          OK     := True;
          // if have an Handler Class
          if Disp.SObjClass <> nil then
            begin // execute handler
              LClient.ReplyHeader := NO_CACHE;
              SObj                := Disp.SObjClass.Create(nil);
              try
                THackUrlHandler(SObj).FClient        := LClient;
                THackUrlHandler(SObj).FFlags         := Disp.Flags;
                THackUrlHandler(SObj).FMsg_WM_FINISH := FMsg_WM_FINISH;
                THackUrlHandler(SObj).FWndHandle     := LClient.Server.Handle;
                THackUrlHandler(SObj).FMethod        := httpMethodGet;
                THackUrlHandler(SObj).FProcedure     := Disp.Proc;
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
                on e: Exception do
                  begin
                    FreeAndNil(SObj);
                    // if Assigned(FOnVirtualExceptionEvent) then { V7.05 }
                    // FOnVirtualExceptionEvent(Self, E, httpMethodGet, LClient.Path);
                  end;
              end;
            end
          else if Disp.Proc <> nil then
            begin // else if have an handler procedure
              LClient.ReplyHeader := NO_CACHE;
              Proc.Code           := Disp.Proc;
              Proc.Data           := LClient;
              LClient.BeforeGetHandler(TDWHttpHandlerProcedure(Proc), OK);
              if OK and (Proc.Code <> nil) then
                TDWHttpHandlerProcedure(Proc)(LClient, LClient.ParamList, Flags^);
            end;
          Exit;
        end;
    end;
  if (Assigned(LClient)) then
    LClient.NoGetHandler(OK);
end;

function TDWAppThread.GetDispatchNormalDocument(aClient: TObject; var Flags: PHttpGetFlag): Boolean;
var
  I: Integer;
  Elem: THttpAllowedElement;
  LClient: TDWClientConnection;
begin
  LClient := aClient as TDWClientConnection;
  for I   := 0 to FDWApp.FGetAllowedPath.Count - 1 do
    begin
      Elem := FDWApp.FGetAllowedPath.Elem[I];
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

procedure TDWAppThread.ProcessException(e: Exception);
begin
  THackServer(DWServer).ProcessException(e);
end;

procedure TDWAppThread.ProcessRequest(aClientConnection: TObject; var Flags: THttpGetFlag);
var
  LRequest: TDWAppRequet;
begin
  LRequest        := TDWAppRequet.Create;
  LRequest.Client := aClientConnection;
  LRequest.Flags  := @Flags;
  FRequestQueue.Add(LRequest);
  FWait.SetEvent;
end;

procedure TDWAppThread.TerminatedSet;
begin
  inherited;
  // Signal event to wake up the thread
  FWait.SetEvent;
end;

end.
