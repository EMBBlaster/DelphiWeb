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
  uses DW.Server, DWUserSessionUnit, DWForm;



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

    GetUrl:= Client.Path;

    UserSession:= CheckSession(FakeVar);

    if UserSession = nil then
        Exit;
    if UserSession.MainForm = nil then //if MainForm not exists
      begin // Create and redirect to it
        UserSession.MainForm:= DWServer.MainForm.Create(UserSession);
        UserSession.AddForm(UserSession.MainForm);
        UserSession.MainForm.UserSession:= UserSession;
        Client.DocStream:= UserSession.MainForm.Render;
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
            Client.DocStream:= LForm.Render;  //render form
            Location:= '/' + LForm.Name;
          end
        else
          begin
            Client.DocStream:= UserSession.MainForm.Render; //render main form
            Location:= '/' + UserSession.MainForm.Name;
          end;
      end;
    if Location <> GetUrl  then
      begin
        Status := '302 moved';
        TDWServerClient(Client).ReplyHeader:=
            TDWServerClient(Client).ReplyHeader + 'Location: ' + Location;
      end;

    AnswerStream(Status,'', TDWServerClient(Client).ReplyHeader);
    Finish;
end;

end.
