unit DWUrlHandlerForms;

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Classes, SysUtils,
  System.StrUtils,
  OverbyteIcsHttpSrv,
  OverbyteIcsHttpAppServer,
  DWUrlHandlerBase, DWUtils;

type

  TUrlHandlerForms = class(TDWUrlHandlerBase)
  public
    procedure Execute; override;
  end;

implementation

uses DW.CORE.Server, DWForm, DW.CORE.DWClientConnection, DW.CORE.DWApplication;

procedure TUrlHandlerForms.Execute;
var
  LDWApplication: TDWApplication;
  GetUrl: string;
  I: Integer;
  LForm, auxForm: TDWForm;
  Location: string;
  Status: string;
begin

  Status := '';

  GetUrl := (Client as TDWClientConnection).Path;

  LDWApplication := DWApplication;

  if LDWApplication = nil then
    Exit;

  { TODO 1 -oDELCIO -cIMPLEMENT :  Change to render DWApplication.Active Form (by DWForm.Show) }
  if LDWApplication.MainForm = nil then // if MainForm not exists
    begin                               // Create and redirect to it
      LDWApplication.MainForm := LDWApplication.MainFormClass.Create(DWApplication);
      LDWApplication.MainForm.Show; // LDWApplication.ActiveForm:= LDWApplication.MainForm;
      // LDWApplication.AddForm(LDWApplication.MainForm);
      TDWClientConnection(Client).DocStream := LDWApplication.MainForm.Render;
      Location                              := '/' + LDWApplication.MainForm.Name;
    end
  else
    begin
      // check if url represent one created form
      LForm := nil;
      for I := 0 to LDWApplication.Forms.Count - 1 do
        begin
          auxForm := TDWForm(LDWApplication.Forms[I]);

          if AnsiEndsText(auxForm.Name, GetUrl) then
            begin
              LForm := auxForm;
              Break;
            end;
        end;
      if LForm <> nil then // if form found
        begin
          LForm.Show; // LDWApplication.ActiveForm:= LForm;
          TDWClientConnection(Client).DocStream := LForm.Render; // render form
          Location                              := '/' + LForm.Name;
        end
      else
        begin
          LDWApplication.MainForm.Show; // LDWApplication.ActiveForm:= LDWApplication.MainForm;
          TDWClientConnection(Client).DocStream := LDWApplication.MainForm.Render;
          // render main form
          Location := '/' + LDWApplication.MainForm.Name;
        end;
    end;
  if Location <> GetUrl then
    begin
      Status                                  := '302 moved';
      TDWClientConnection(Client).ReplyHeader := TDWClientConnection(Client).ReplyHeader +
        'Location: ' + Location;
    end;

  AnswerStream(Status, '', TDWClientConnection(Client).ReplyHeader);

  Finish;
end;

end.
