unit DWUserSessionUnit;

interface

uses
  Classes, OverbyteIcsWebSession, DWForm, DWCallbacks;

type

  TDWUserSession = class(TWebSessionData)
  private
    FMainForm: TDWForm;
    FDataModule: TDataModule;
    FOnCreate: TNotifyEvent;
    FForms:TList;
    FCallbacks: TDWCallBacks;
    FActiveForm: TDWForm;
    procedure SetMainForm(const Value: TDWForm);
    procedure SetDataModule(const Value: TDataModule);
    procedure SetOnCreate(const Value: TNotifyEvent);
    procedure SetActiveForm(const Value: TDWForm);
  protected
    FUserCode: String;
    FLogonTime: TDateTime;
    FLastRequest: TDateTime; // Last request time stamp
    FRequestCount: Integer;  // Count the requests
    FIP: String;             // Client IP Adress (beware of proxies)
    FLoginChallenge: String; // Used for secure login
    FConfigPort: String;     // Used for configuration process
    FConfigTempDir: String;  // Used for configuration process
    FConfigHasLogo: Boolean; // Used for configuration process
    FTempVar: Integer;       // Currently used for anti-spam
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddForm(aForm:TDWForm);
    procedure RemoveForm(aForm:TDWForm);
    property Forms:TList read FForms;
    property CallBacks:TDWCallBacks read FCallbacks;
    procedure RegisterCallBack (const AName: String;  ACallbackProcedure: TDWCallBackProc);
    procedure UnregisterCallBack (const AName: String);
    property ActiveForm:TDWForm read FActiveForm write SetActiveForm;
  published
    property UserCode: String read FUserCode write FUserCode;
    property LogonTime: TDateTime read FLogonTime write FLogonTime;
    property RequestCount: Integer read FRequestCount write FRequestCount;
    property LastRequest: TDateTime read FLastRequest write FLastRequest;
    property IP: String read FIP write FIP;
    property LoginChallenge: String read FLoginChallenge write FLoginChallenge;
    property ConfigPort: String read FConfigPort write FConfigPort;
    property ConfigTempDir: String read FConfigTempDir write FConfigTempDir;
    property ConfigHasLogo: Boolean read FConfigHasLogo write FConfigHasLogo;
    property TempVar: Integer read FTempVar write FTempVar;
    property MainForm:TDWForm read FMainForm write SetMainForm;
    property DataModule:TDataModule read FDataModule write SetDataModule;
    property OnCreate:TNotifyEvent read FOnCreate write SetOnCreate;
  end;

  // var
  // GSessionDataCount : Integer;

implementation

{ TAppSrvSessionData }

procedure TDWUserSession.AddForm(aForm: TDWForm);
begin
  if FForms.IndexOf(aForm) < 0 then
    FForms.Add(aForm);
end;

constructor TDWUserSession.Create(AOwner: TComponent);
begin
  inherited;
  FForms:= TList.Create;
  FCallbacks:= TDWCallBacks.Create(Self);
  FTempVar := -1;
  if Assigned(FOnCreate) then
    FOnCreate(Self);
end;

destructor TDWUserSession.Destroy;
var
  I: Integer;
begin
 { TODO 1 -oDELCIO -cVERIFY : Chech Memory leak}
 { for I := 0 to FForms.Count -1 do
    begin
      if Assigned(FForms[I]) then
        TDWForm(FForms[I]).Free;
    end;
  FForms.Free;
  FMainForm.Free; }
  inherited;
end;

procedure TDWUserSession.RegisterCallBack(const AName: String;
  ACallbackProcedure: TDWCallBackProc);
begin
   FCallbacks.RegisterCallBack(AName, ACallbackProcedure);
end;

procedure TDWUserSession.RemoveForm(aForm: TDWForm);
var
  Lindex:Integer;
begin
  Lindex:= FForms.IndexOf(aForm);
  if Lindex > -1  then
    FForms.Delete(Lindex);
end;

procedure TDWUserSession.SetActiveForm(const Value: TDWForm);
begin
  FActiveForm := Value;
end;

procedure TDWUserSession.SetDataModule(const Value: TDataModule);
begin
  FDataModule := Value;
end;

procedure TDWUserSession.SetMainForm(const Value: TDWForm);
begin
  FMainForm := Value;
end;

procedure TDWUserSession.SetOnCreate(const Value: TNotifyEvent);
begin
  FOnCreate:= Value;
end;

procedure TDWUserSession.UnregisterCallBack(const AName: String);
begin
  FCallbacks.UnregisterCallBack(AName);
end;

initialization

RegisterClass(TDWUserSession);

end.
