program DelphiWeb;

uses
  Vcl.Forms,
  uServerMainForm in 'uServerMainForm.pas' {FrmServerMain},
  uWebMainForm in 'uWebMainForm.pas' {WebMainForm: TDWForm},
  uDWSessionData in 'uDWSessionData.pas' {DWSessionData: TDWUserSession};

{$R *.res}

var
  FrmServerMain:TFrmServerMain;
begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmServerMain, FrmServerMain);
  Application.Run;
end.