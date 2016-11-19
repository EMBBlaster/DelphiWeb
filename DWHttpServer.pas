unit DWHttpServer;

interface

uses Classes, System.SysUtils, System.StrUtils, System.Masks, OverbyteIcsHttpAppServer,
OverbyteIcsHttpSrv, OverbyteIcsUtils, OverbyteIcsMD5;

type
  THackUrlHandler = class(TUrlHandler);

  TDWHttpServer = class(THttpAppSrv)
  protected
    function GetDispatchVirtualDocument(ClientCnx: THttpAppSrvConnection; var Flags: THttpGetFlag)
      : Boolean; reintroduce;
    function GetDispatchForms(ClientCnx: THttpAppSrvConnection; var Flags: THttpGetFlag)
      : Boolean;
    function PostDispatchCallback(ClientCnx : THttpAppSrvConnection;
                                             var Flags : THttpGetFlag;
                                             ExecFlag  : Boolean): Boolean;
    procedure TriggerGetDocument(Sender: TObject; var Flags: THttpGetFlag); override;
    procedure TriggerPostDocument(Sender: TObject; var Flags : THttpGetFlag); override;
  end;

implementation
  uses DWUserSessionUnit, DWUrlHandlerBase, DW.Server, DWUrlHandlerForms,
  DW.VCL.CustomForm, DWCallbacks;

function Server(aSelf:TDWHttpServer):TDWServer;
begin
  Result:= aSelf.Owner as TDWServer;
end;


{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }
function TDWHttpServer.GetDispatchForms(ClientCnx: THttpAppSrvConnection;
  var Flags: THttpGetFlag): Boolean;
var
  UserSession:TDWUserSession;
  HandlerForms: TUrlHandlerForms;
  OK:Boolean;
  I: Integer;
begin
  Result:=False;
  //if Session is not created
  if not ClientCnx.ValidateSession then
    begin
      //create session
      UserSession := TDWUserSession.Create(nil);
      UserSession.DataModule:= Server(Self).UserDataModule.Create(UserSession);
      UserSession.AssignName; // Angus
      TDWServerClient(ClientCnx).ReplyHeader := NO_CACHE + ClientCnx.CreateSession('', 0, UserSession);
      //redirect to DWURLHandlerForms for render MainForm
      Result:=True;
    end
  else
    begin
      //get the session
      UserSession:= ClientCnx.WSession.SessionData as TDWUserSession;
      //check if url correspond with one FormName of Session instanced forms
      for I := 0 to UserSession.Forms.Count -1 do
        begin
          //if url match of Form Name
          if AnsiEndsText(TDWCustomForm(UserSession.Forms[I]).Name, ClientCnx.Path) then
            begin
              TDWServerClient(ClientCnx).ReplyHeader := NO_CACHE;
              //redirect to DWURLHandlerForms for render Form
              Result:=True;
              Break;
            end;
        end;
      if (Not Result)
      and ((ClientCnx.Path = '/')
      or (ClientCnx.Path = ('/' + Server(self).HttpServer.DefaultDoc))) then
        begin
          TDWServerClient(ClientCnx).ReplyHeader := NO_CACHE;
          //redirect to DWURLHandlerForms for render MainForm
          Result:=True;
        end;
    end;

  if Result then
    begin
      //set User Session params
      UserSession.LastRequest    := Now;
      UserSession.RequestCount   := UserSession.RequestCount + 1;
      UserSession.LoginChallenge := StrMD5(IntToHex(IcsGetTickCount, 8));
      // Create the TUrlHandlerForms to dispath form
      HandlerForms := TUrlHandlerForms.Create(Self);
      try
        THackUrlHandler(HandlerForms).FClient           := ClientCnx;
        THackUrlHandler(HandlerForms).FFlags            := hgWillSendMySelf;
        THackUrlHandler(HandlerForms).FMsg_WM_FINISH    := FMsg_WM_FINISH;
        THackUrlHandler(HandlerForms).FWndHandle        := FHandle;
        THackUrlHandler(HandlerForms).FMethod           := httpMethodGet;
        ClientCnx.OnDestroying := THackUrlHandler(HandlerForms).ClientDestroying;
        OK:=True;
        ClientCnx.BeforeObjGetHandler(HandlerForms, OK);
        if OK then
          begin
            HandlerForms.Execute;
            Flags := THackUrlHandler(HandlerForms).FFlags;
          end
        else
          begin
            Flags := THackUrlHandler(HandlerForms).FFlags;
            FreeAndNil(HandlerForms);
          end;
      except
        on E: Exception do
          begin
            FreeAndNil(HandlerForms);
            if Assigned(FOnVirtualExceptionEvent) then { V7.05 }
              FOnVirtualExceptionEvent(Self, E, httpMethodGet, ClientCnx.Path);
          end;
      end;
    end;
end;

function TDWHttpServer.GetDispatchVirtualDocument(ClientCnx: THttpAppSrvConnection;
  var Flags: THttpGetFlag): Boolean;
var
  I: Integer;
  PathBuf: String;
  Status: Boolean;
  Proc: TMethod;
  OK: Boolean;
  Disp: THttpDispatchElement;
  SObj: TUrlHandler;
begin
  for I := 0 to FGetHandler.Count - 1 do
    begin
      PathBuf := FGetHandler.Strings[I];

      if MatchesMask(ClientCnx.Path, PathBuf) then
        Status := True
      else
        Status := (CompareText(PathBuf, ClientCnx.Path) = 0);

      if Status then
        begin
          Result := TRUE;
          Disp   := FGetHandler.Disp[I];
          Flags  := Disp.Flags;
          OK     := TRUE;
          if Disp.Proc <> nil then
            begin
              Proc.Code := Disp.Proc;
              Proc.Data := ClientCnx;
              ClientCnx.BeforeGetHandler(TMyHttpHandler(Proc), OK);
              if OK and (Proc.Code <> nil) then
                TMyHttpHandler(Proc)(Flags);
            end
          else if Disp.SObjClass <> nil then
            begin
              SObj := Disp.SObjClass.Create(Self);
              try
                THackUrlHandler(SObj).FClient           := ClientCnx;
                THackUrlHandler(SObj).FFlags            := Disp.Flags;
                THackUrlHandler(SObj).FMsg_WM_FINISH    := FMsg_WM_FINISH;
                THackUrlHandler(SObj).FWndHandle        := FHandle;
                THackUrlHandler(SObj).FMethod           := httpMethodGet;
                ClientCnx.OnDestroying := THackUrlHandler(SObj).ClientDestroying;
                ClientCnx.BeforeObjGetHandler(SObj, OK);
                if OK then
                  begin
                    SObj.Execute;
                    Flags := THackUrlHandler(SObj).FFlags;
                  end
                else
                  begin
                    Flags := THackUrlHandler(SObj).FFlags;
                    FreeAndNil(SObj);
                  end;
              except
                on E: Exception do
                  begin
                    FreeAndNil(SObj);
                    if Assigned(FOnVirtualExceptionEvent) then { V7.05 }
                      FOnVirtualExceptionEvent(Self, E, httpMethodGet, ClientCnx.Path);
                  end;
              end;
            end;
          Exit;
        end;
    end;
  Result := FALSE;
  ClientCnx.NoGetHandler(Result);
end;

function TDWHttpServer.PostDispatchCallback(ClientCnx: THttpAppSrvConnection;
  var Flags: THttpGetFlag; ExecFlag: Boolean): Boolean;
var
  UserSession:TDWUserSession;
  I:Integer;
  LCallBackName:string;
  LCallBackIndex:Integer;
begin
  Result:=False;

  //Get CallBack Name
  LCallBackName:= ClientCnx.RequestHeader.Values['callback'];
  if LCallBackName = '' then Exit;

  //get the session
  UserSession:= ClientCnx.WSession.SessionData as TDWUserSession;
  //check if Callback is registered
  LCallBackIndex := UserSession.CallBacks.FindCallback(LCallBackName);
  //if found
  if LCallBackIndex >= 0 then
    begin
      try
        UserSession.ActiveForm.ExecuteCallBack(ClientCnx.RequestHeader, TDWCallBack(UserSession.CallBacks.Objects[I]));
        Result:=True;
      except on e:exception do
         raise Exception.Create('Error on process Callback:' +e.Message);
      end;
    end;
end;

procedure TDWHttpServer.TriggerGetDocument(Sender: TObject; var Flags: THttpGetFlag);

begin
  // because no call inherited TriggerGetDocument FOnGetDocument is called here
  if Assigned(FOnGetDocument) then
    FOnGetDocument(Self, Sender, Flags);

  if Flags in [hgWillSendMySelf, hg404, hg403, hg401, hgAcceptData, { V7.03 don't ignore Flags }
  hgSendDirList] then
    Exit;

  // Handle Main Form or any session created form
  if (GetDispatchForms(Sender as THttpAppSrvConnection, Flags)) then
    Exit;


  // Handle all virtual documents. Returns TRUE if document handled.
  if (GetDispatchVirtualDocument(Sender as THttpAppSrvConnection, Flags)) then
    Exit;

  // Handle all normal (static) documents. Returns TRUE if document handled.
  if GetDispatchNormalDocument(Sender as THttpConnection, Flags) then
    Exit;

  // Reject anything else
  Flags := hg404;
end;

procedure TDWHttpServer.TriggerPostDocument(Sender: TObject;
  var Flags: THttpGetFlag);
begin
  // because no call inherited TriggerPosDocument FOnPostDocument is called here
  if Assigned(FOnPostDocument) then
    FOnPostDocument(Self, Sender, Flags);

  // Handle all CallBacks. Returns TRUE if callback is handled.
  if PostDispatchCallback(Sender as THttpAppSrvConnection, Flags, FALSE) then
      Exit;

  // Handle all virtual documents. Returns TRUE if document handled.
  if PostDispatchVirtualDocument(Sender as THttpAppSrvConnection, Flags, FALSE) then
      Exit;

  // Reject anything else
  Flags := hg404;

end;

end.
