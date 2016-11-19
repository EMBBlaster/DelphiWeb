unit DWUrlHandlerHome;

interface

uses
  {$IFDEF MSWINDOWS}
    Windows,
  {$ENDIF}
    Classes, SysUtils,
    OverbyteIcsHttpSrv,
    OverbyteIcsHttpAppServer,
    DWUrlHandlerBase;

type
    TUrlHandlerDefaultDoc = class(TDWUrlHandlerBase)
    public
        procedure Execute; override;
    end;

    TUrlHandlerHomePageHtml = class(TDWUrlHandlerBase)
    public
        procedure Execute; override;
    end;

implementation
  uses DWMainServer, DWUserSessionUnit, DWForm;


procedure TUrlHandlerDefaultDoc.Execute;
var
  Headers:string;
begin
    if CheckSession(Headers) = nil then
        Exit;
    raise Exception.Create('Aqui');
    //Relocate(UrlHomePage);
end;

procedure TUrlHandlerHomePageHtml.Execute;
var
  UserSession:TDWUserSession;
  Headers:string;
begin
    UserSession:= CheckSession(Headers);
    if UserSession = nil then
        Exit;
    if UserSession.MainForm = nil then
      UserSession.MainForm:= TDWForm(FrmDwServer.MainForm.Create(UserSession));


    UserSession.MainForm.UserSession:= UserSession;
    Client.DocStream:= UserSession.MainForm.Render;
    AnswerStream('','',Headers);
    Finish;



end;


end.
