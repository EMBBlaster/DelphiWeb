unit DWUrlHandlerBase;

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Classes, SysUtils,
  OverbyteIcsMD5,
  OverbyteIcsUtils,
  DWTypes,
  OverbyteIcsHttpSrv;

type
  TDWUrlHandlerClass = class of TDWUrlHandlerBase;

  TDWUrlHandlerBase = class(TComponent)
  private

  protected
    FClient: TObject;
    FFlags: THttpGetFlag;
    FMsg_WM_FINISH: UINT;
    FWndHandle: HWND;
    FMethod: TDWHttpMethod;
    FProcedure: Pointer;
    procedure ClientDestroying(Sender: TObject);
  public
    procedure Execute; virtual;
    procedure AnswerStream(const Status: String; const ContType: String; const Header: String);
    procedure AnswerString(const Status: String; const ContType: String; const Header: String;
      Body: string);
    procedure Finish;

  published
    property Client: TObject read FClient;

  end;

const
  CRLF = #13#10;

implementation

uses DWUtils, DW.CORE.DWClientConnection;

{ TDWUrlHandlerBase }

procedure TDWUrlHandlerBase.AnswerStream(const Status, ContType, Header: String);
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

procedure TDWUrlHandlerBase.AnswerString(const Status, ContType, Header: String; Body: string);
begin
  if Assigned(Client) then
    TDWClientConnection(Client).AnswerString(FFlags, Status, ContType, Header, Body);
end;

procedure TDWUrlHandlerBase.ClientDestroying(Sender: TObject);
begin
  if FClient = Sender then
    FClient := nil;
end;

end.
