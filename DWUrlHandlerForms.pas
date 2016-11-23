unit DWUrlHandlerForms;

interface

uses
  {$IFDEF MSWINDOWS}
    Windows,
  {$ENDIF}
    dialogs,
    Classes, SysUtils,
    System.StrUtils,
    OverbyteIcsHttpSrv,
    OverbyteIcsHttpAppServer,
    DWUrlHandlerBase, DWUtils;

type

    TUrlHandlerForms = class(TDWUrlHandlerBase)
    public
        procedure Execute; override;
    end;

implementation
  uses DW.CORE.Server, DWUserSessionUnit, DWForm, DW.CORE.DWClientConnection;



procedure TUrlHandlerForms.Execute;
var
  UserSession:TDWUserSession;
  FakeVar:string;
  GetUrl:string;
  I: Integer;
  LForm, auxForm:TDWForm;
  Location:string;
  Status:string;
begin
    Status:='';

    GetUrl:= (Client as TDWClientConnection).Path;

    UserSession:= TDWUserSession((Client as TDWClientConnection).DWApplication.UserSessionData);

    if UserSession = nil then
        Exit;
    if UserSession.MainForm = nil then //if MainForm not exists
      begin // Create and redirect to it
        UserSession.MainForm:= DWServer.MainForm.Create(nil);
        UserSession.AddForm(UserSession.MainForm);
        UserSession.MainForm.UserSession:= UserSession;
        TDWClientConnection(Client).DocStream:= UserSession.MainForm.Render;
        Location:= '/' + UserSession.MainForm.Name;
      end
    else
      begin
        //check if url represent one created form
        LForm:= nil;
        for I := 0 to UserSession.Forms.Count -1 do
          begin
            auxForm:= TDWForm(UserSession.Forms[I]);

            if AnsiEndsText(auxForm.Name, GetUrl) then
              begin
                LForm:= auxForm;
                Break;
              end;
          end;
        if LForm <> nil then  //if form found
          begin
            TDWClientConnection(Client).DocStream:= LForm.Render;  //render form
            Location:= '/' + LForm.Name;
          end
        else
          begin
            TDWClientConnection(Client).DocStream:= UserSession.MainForm.Render; //render main form
            Location:= '/' + UserSession.MainForm.Name;
          end;
      end;
    if Location <> GetUrl  then
      begin
        Status := '302 moved';
        TDWClientConnection(Client).ReplyHeader:=
            TDWClientConnection(Client).ReplyHeader + 'Location: ' + Location;
      end;

    AnswerStream(Status,'', TDWClientConnection(Client).ReplyHeader);
    Finish;
end;

end.
