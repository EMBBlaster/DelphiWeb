unit DWRegister;

interface

uses
  Classes, Winapi.Windows, SysUtils, Dialogs, DesignEditors, DesignIntf, ToolsAPI;

type

  TDWFormModule = class(TCustomModule)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure ValidateComponent(Component: TComponent); override;
  end;

procedure register;

implementation

uses DW.VCL.Control, DW.VCL.Labels, DW.CORE.Server, DW.VCL.Edit,
  DW.DESIGN.FormWizard, DW.DESIGN.AppWizard, DWForm, DW.VCL.Container,
  DW.HTML.Page, DW.CORE.UserSession, DMForm, DW.VCL.DataModule, DW.VCL.Input,
  DW.VCL.Region, DW.VCL.Buttons, DW.VCL.Modal, DW.VCL.Accordion,
  DW.DESIGN.AccordionEditor;

procedure register;
begin
  RegisterComponents('DelphiWeb', [TDWServer]);
  RegisterComponents('DelphiWeb', [TDWLabel]);
  RegisterComponents('DelphiWeb', [TDWEdit]);
  //Bootstrap Inputs
  RegisterComponents('DelphiWeb', [TDWInput]);
  RegisterComponents('DelphiWeb', [TDWMemo]);
  RegisterComponents('DelphiWeb', [TDWCheckBox]);
  RegisterComponents('DelphiWeb', [TDWRadioButton]);
  RegisterComponents('DelphiWeb', [TDWSelect]);
  RegisterComponents('DelphiWeb', [TDWRadioGroup]);
  //others
  RegisterComponents('DelphiWeb', [TDWButton]);
  RegisterComponents('DelphiWeb', [TDWModal]);
  RegisterComponents('DelphiWeb', [TDWAccordion]);
  RegisterComponentEditor(TDWAccordion, TDWAccordionEditor);
  //Bootstrap Regions
  RegisterComponents('DelphiWeb', [TDWRegion]);


  //DelphiWeb Wizards
  RegisterCustomModule(TDWForm, TCustomModule);
  RegisterCustomModule(TDWUserSession, TDataModuleCustomModule);
  RegisterCustomModule(TDWDatamodule, TDataModuleCustomModule);
  RegisterPackageWizard(TDWAppWizard.Create);
  RegisterPackageWizard(TDWFormWizard.Create);
end;

{ TDWFormModule }

procedure TDWFormModule.ExecuteVerb(Index: Integer);
var
  NewName: string;
begin
  if Index = 0 then
    begin
      NewName := Root.Name;
      if InputQuery('Panel Module Editor', 'New panel name:', NewName) then
        Root.Name := NewName;
    end;

end;

function TDWFormModule.GetVerb(Index: Integer): string;
begin
  if Index = 0 then
    Result := 'Rename...';
end;

function TDWFormModule.GetVerbCount: Integer;
begin
  Result := 1;
end;

procedure TDWFormModule.ValidateComponent(Component: TComponent);
begin
  if (not Component.InheritsFrom(TDWControl)) and (not Component.InheritsFrom(TDWContainer)) then
    raise Exception.Create('The DWForm can host only DWControls');
end;

end.
