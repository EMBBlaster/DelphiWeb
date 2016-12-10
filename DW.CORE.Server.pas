unit DW.CORE.Server;

interface

uses Classes, Forms, System.SysUtils, OverbyteIcsHttpAppServer, DW.CORE.DWHttpServer,
  OverbyteIcsFtpSrvT, DWForm;

type
  (* TDWServerClient = class(THttpAppSrvConnection)
    private
    FResponseHeader: string;
    procedure SetResponseHeader(const Value: string);

    protected

    public
    CStartTick: longword ;
    CLastRead: int64 ;
    CLastWrite: int64 ;
    constructor Create(AOwner: TComponent); override;
    //header of response
    property ReplyHeader:string read FResponseHeader write SetResponseHeader;
    end; *)

  TDatamoduleClass = class of TDataModule;

  TDWServer = class(TComponent)
  private
    FHttpSrv: TDWHttpServer;
    FMainForm: TDWFormClass;
    FLibDir: string;
    FUrlBase: string;
    FRefreshCacheParam: string;
    FUserDataModule: TDatamoduleClass;
    FCookieParam: string;
    procedure SetDocDir(const Value: string);
    procedure SetPort(const Value: String);
    procedure SetTemplateDir(const Value: string);
    function GetDocDir: string;
    function GetPort: String;
    function GetTemplateDir: string;
    procedure SetMainForm(const Value: TDWFormClass);
    procedure SetLibDir(const Value: string);
    procedure SetUrlBase(const Value: string);
    function GetUrlBase: string;
    procedure HttpAppSrvBeforeProcessRequest(Sender, Client: TObject);
    procedure SetUserDataModule(const Value: TDatamoduleClass);
    procedure SetCookieParam(const Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    function AppPath: string;
    class function GetDWServer: TDWServer;
  published
    property Port: String read GetPort write SetPort;
    property DocDir: string read GetDocDir write SetDocDir;
    property TemplateDir: string read GetTemplateDir write SetTemplateDir;
    property LibDir: string read FLibDir write SetLibDir;
    property MainForm: TDWFormClass read FMainForm write SetMainForm;
    property UrlBase: string read GetUrlBase write SetUrlBase;
    property RefreshCacheParam: string read FRefreshCacheParam;
    property UserDataModule: TDatamoduleClass read FUserDataModule write SetUserDataModule;
    property HttpServer: TDWHttpServer read FHttpSrv;
    property CookieParam: string read FCookieParam write SetCookieParam;
  end;

implementation

uses DWUrlHandlerForms;

var
  gDWServer: TDWServer;

  { TDWServer }

function TDWServer.AppPath: string;
begin
  Result := ExtractFilePath(Application.ExeName);
end;

constructor TDWServer.Create(AOwner: TComponent);
begin
  inherited;
  FHttpSrv := TDWHttpServer.Create(Self);
  with FHttpSrv do
    begin
      // ClientClass:= TDWServerClient;
      Port        := '80';
      DocDir      := AppPath + 'wwwroot';
      TemplateDir := AppPath + 'wwwroot\templates';
      ForceDirectories(AppPath + 'wwwroot');
      ForceDirectories(AppPath + 'wwwroot\templates');
      OnBeforeProcessRequest := HttpAppSrvBeforeProcessRequest;
    end;
  gDWServer := Self;
  FLibDir   := AppPath + 'wwwroot\dwlib';
  FUrlBase  := '';
end;

destructor TDWServer.Destroy;
begin
  FHttpSrv.Stop;
  FHttpSrv.Free;
  inherited;
end;

class function TDWServer.GetDWServer: TDWServer;
begin
  Result := gDWServer;
end;

function TDWServer.GetDocDir: string;
begin
  Result := FHttpSrv.DocDir;
end;

function TDWServer.GetPort: String;
begin
  Result := FHttpSrv.Port;
end;

function TDWServer.GetTemplateDir: string;
begin
  Result := FHttpSrv.TemplateDir;
end;

function TDWServer.GetUrlBase: string;
begin
  if FUrlBase <> '' then
    Result := FUrlBase
  else
    begin
      if FHttpSrv.Addr <> '' then
        begin
          Result := 'http://' + FHttpSrv.Addr + ':' + FHttpSrv.Port;
        end;
    end;
end;

procedure TDWServer.SetCookieParam(const Value: string);
begin
  if (FCookieParam <> Value) then
    begin
      FCookieParam := Value;
      FHttpSrv.SetDWAppCookieParam(Value);
    end;
end;

procedure TDWServer.SetDocDir(const Value: string);
begin
  if FHttpSrv.DocDir <> Value then
    FHttpSrv.DocDir := Value;
end;

procedure TDWServer.SetLibDir(const Value: string);
begin
  FLibDir := Value;
end;

procedure TDWServer.SetMainForm(const Value: TDWFormClass);
begin
  FMainForm := Value;
end;

procedure TDWServer.SetPort(const Value: String);
begin
  if FHttpSrv.Port <> Value then
    FHttpSrv.Port := Value;
end;

procedure TDWServer.SetTemplateDir(const Value: string);
begin
  if FHttpSrv.TemplateDir <> Value then
    FHttpSrv.TemplateDir := Value;
end;

procedure TDWServer.SetUrlBase(const Value: string);
begin
  FUrlBase := Value;
end;

procedure TDWServer.SetUserDataModule(const Value: TDatamoduleClass);
begin
  FUserDataModule := Value;
end;

procedure TDWServer.Start;
begin
  FRefreshCacheParam := FormatDateTime('yyyymmddhhnnsszzz', now);
  FHttpSrv.Start;
end;

procedure TDWServer.Stop;
begin
  FHttpSrv.Stop;
end;

procedure TDWServer.HttpAppSrvBeforeProcessRequest(Sender, Client: TObject);
// var
// RemoteClient: TDWServerClient;
begin
  // RemoteClient := TDWServerClient(Client) ;
  // RemoteClient.CStartTick := IcsGetTickCountX;
  // RemoteClient.CLastWrite := RemoteClient.WriteCount;
end;

{ TAppHttpConnection }

(* constructor TDWServerClient.Create(AOwner: TComponent);
  begin
  inherited Create(AOwner);
  { keep alive means connection may be used for multiple requests so we must track how much
  data is sent before and after each request }
  CLastRead := 0 ;
  CLastWrite := 0 ;
  FResponseHeader:='';
  end;

  procedure TDWServerClient.SetResponseHeader(const Value: string);
  begin
  FResponseHeader := Value;
  end; *)

end.
