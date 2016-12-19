unit DWUrlHandlerRest;

interface

uses
  Classes, SysUtils, StrUtils, DWUrlHandlerBase;

type
  TDWRestHandlerRest = class(TDWUrlHandlerBase)
  protected
    //
  public
    procedure Execute; override;
  end;

  TDWRestCallBackFunction = procedure(aParams: TStrings; var aReply: string) of object;

implementation

uses
  DW.CORE.DWClientConnection;

procedure TDWRestHandlerRest.Execute;
var
  Status: string;
  LReply: string;
begin
  Status := '';
  LReply := '';

  if FProcedure <> nil then
    begin
      TDWRestCallBackFunction(FProcedure^)(TDWClientConnection(FClient).ParamList, LReply);
    end;
  if LReply <> '' then
    begin
      AnswerString(Status, '', TDWClientConnection(Client).ReplyHeader, LReply);
    end;
  Finish;
end;

end.
