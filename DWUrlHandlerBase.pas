unit DWUrlHandlerBase;

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Classes, SysUtils,
  DWUserSessionUnit,
  OverbyteIcsMD5,
  OverbyteIcsUtils,
  DWTypes,
  OverbyteIcsHttpSrv;

type
  TDWUrlHandlerBase = class(TComponent)
  private

  protected
    FClient          : TObject;
        FFlags           : THttpGetFlag;
        FMsg_WM_FINISH   : UINT;
        FWndHandle       : HWND;
        FMethod          : TDWHttpMethod;
   procedure ClientDestroying(Sender : TObject);

 public
   procedure Execute; virtual;
   procedure AnswerStream(const Status   : String;
                               const ContType : String;
                               const Header   : String);
    procedure Finish;

 published
   property Client : TObject     read  FClient;

  end;

const
  CRLF = #13#10;

implementation
   uses DWUtils, DW.CORE.DWClientConnection;




{ TDWUrlHandlerBase }

procedure TDWUrlHandlerBase.AnswerStream(const Status, ContType,
  Header: String);
begin
  if Assigned(Client) then
        TDWClientConnection(Client).AnswerStream(FFlags, Status, ContType, Header);
end;

procedure TDWUrlHandlerBase.Execute;
begin
// Nothing to do here, just to allow overriden method
end;

procedure TDWUrlHandlerBase.Finish;
begin
    // We need to destroy the server object, but we can't do it safely from
    // one of his methods. Delaying the detroy until all queued events are
    // processed is better. This is why we use an intermediate message.
    if (FWndHandle <> 0) and (FMsg_WM_FINISH > 0) then
        PostMessage(FWndHandle, FMsg_WM_FINISH, 0, LPARAM(Self));
end;

procedure TDWUrlHandlerBase.ClientDestroying(Sender : TObject);
begin
    if FClient = Sender then
        FClient := nil;
end;

end.
