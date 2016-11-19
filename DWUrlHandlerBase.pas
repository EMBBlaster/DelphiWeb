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
  OverbyteIcsHttpAppServer;

type
  TDWUrlHandlerBase = class(TUrlHandler)
  protected
    function CheckSession(out Headers:string): TDWUserSession;
    function GetSessionData: TDWUserSession;
    procedure Relocate(const Location: String);
    property SessionData: TDWUserSession read GetSessionData;
  end;

const
  CRLF = #13#10;

implementation
   uses DWUtils;

function TDWUrlHandlerBase.CheckSession(out Headers:string): TDWUserSession;
begin
  if not ValidateSession then
    begin
      // Inc(GSessionDataCount);
      Result := TDWUserSession.Create(nil);
      Result.DataModule:= DWServer.UserDataModule.Create(Result);
      // MySessionData.Name := 'MySessionData' + IntToStr(GSessionDataCount);
      Result.AssignName; // Angus
      Headers := NO_CACHE + CreateSession('', 0, Result);
    end
  else
    begin
      Result := SessionData;
      Headers       := NO_CACHE;
    end;

  Result.LastRequest    := Now;
  Result.RequestCount   := Result.RequestCount + 1;
  Result.LoginChallenge := StrMD5(IntToHex(IcsGetTickCount, 8));
end;

function TDWUrlHandlerBase.GetSessionData: TDWUserSession;
begin
  if Assigned(WSession) then
    Result := WSession.SessionData as TDWUserSession
  else
    Result := nil;
end;

procedure TDWUrlHandlerBase.Relocate(const Location: String);
begin
  AnswerPage('302 moved', 'Location: ' + Location + CRLF + NO_CACHE, 'Moved.html', nil,
    ['LOCATION', Location]);
end;

end.
