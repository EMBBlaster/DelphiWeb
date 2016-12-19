unit uServerMainForm;

interface

uses
  VCL.Forms, System.SysUtils, DW.CORE.Server, System.Classes;
type
  TFrmServerMain = class(TForm)
    Server: TDWServer;
    procedure FormShow(Sender: TObject);
  private
{ private declarations }
  public
    { public declarations }
  end;

{ !!! Do Not Declare Global Variables !!! }

implementation
  uses
    uWebMainForm, uDWSessionData;

{$R *.dfm}

procedure TFrmServerMain.FormShow(Sender: TObject);
begin
  Server.MainForm:= TWebMainForm;
  Server.DocDir      := Server.AppPath + 'wwwroot\';
  Server.LibDir      := Server.AppPath + 'wwwroot\dwlib\';
  Server.TemplateDir := Server.AppPath + 'wwwroot\templates\';
  ForceDirectories(Server.AppPath + 'wwwroot');
  ForceDirectories(Server.AppPath + 'wwwroot\templates');
  Server.Start;
end;
end.