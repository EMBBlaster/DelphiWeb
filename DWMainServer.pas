unit DWMainServer;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, OverbyteIcsWndControl,
  OverbyteIcsHttpSrv, OverbyteIcsHttpAppServer, DWForm, DWUserSessionUnit, OverbyteIcsFtpSrvT;

type
 TAppHttpConnection = class(THttpAppSrvConnection)
  protected
  public
    CStartTick: longword ;
    CLastRead: int64 ;
    CLastWrite: int64 ;
    constructor Create(AOwner: TComponent); override;
  end ;

  TFrmDwServer = class(TForm)
    HttpAppSrv: THttpAppSrv;
    procedure FormShow(Sender: TObject);
    procedure HttpAppSrvClientConnect(Sender, Client: TObject; Error: Word);
    procedure HttpAppSrvBeforeProcessRequest(Sender, Client: TObject);
  private
    FMainForm: TCustomFormClass;
    procedure DoAddUrlHandlers;
    procedure SetMainForm(const Value: TCustomFormClass);
  public
    function AppPath:string;
    property MainForm:TCustomFormClass read FMainForm write SetMainForm;

  end;

var
  FrmDwServer: TFrmDwServer;

implementation

uses  DWUrlHandlerHome, uForm1;

{$R *.dfm}

function TFrmDwServer.AppPath: string;
begin
  Result:= ExtractFilePath(Application.ExeName);
end;

procedure TFrmDwServer.DoAddUrlHandlers;
begin
    // Add all dynamic webpage handlers
    HttpAppSrv.AddGetHandler('/', TUrlHandlerHomePageHtml);
end;

procedure TFrmDwServer.FormShow(Sender: TObject);
begin
  MainForm:= TForm1;
  with HttpAppSrv do
    begin
      ClientClass:=  TAppHttpConnection;
      Port:= '80';
      DocDir:= AppPath + 'wwwroot';
      TemplateDir:= DocDir + '\templates';
      DefaultDoc:= 'index.html';
      AddGetAllowedPath('/', afBeginBy);
      ForceDirectories(DocDir);
      ForceDirectories(TemplateDir);
      DoAddUrlHandlers;
      Start;
    end;
end;

procedure TFrmDwServer.HttpAppSrvBeforeProcessRequest(Sender, Client: TObject);
var
    RemoteClient: TAppHttpConnection;
begin
    RemoteClient := TAppHttpConnection(Client) ;
    RemoteClient.CStartTick := IcsGetTickCountX ;
    RemoteClient.CLastWrite := RemoteClient.WriteCount ;
end;

procedure TFrmDwServer.HttpAppSrvClientConnect(Sender, Client: TObject;
  Error: Word);
var
    ClientCnx : THttpAppSrvConnection;
begin
    ClientCnx                := Client as THttpAppSrvConnection;
    ClientCnx.WSessionCookie := 'OverbyteIcsWebAppServer' + HttpAppSrv.Port;

end;


procedure TFrmDwServer.SetMainForm(const Value: TCustomFormClass);
begin
  FMainForm := Value;
end;


{ TAppHttpConnection }

constructor TAppHttpConnection.Create(AOwner: TComponent);
begin
inherited Create(AOwner);
  { keep alive means connection may be used for multiple requests so we must track how much
    data is sent before and after each request }
    CLastRead := 0 ;
    CLastWrite := 0 ;
end;

end.
