unit DWRegister;

interface

uses
  Classes, Winapi.Windows, SysUtils, Dialogs, DesignEditors, DesignIntf,
  ToolsAPI, DMForm;

type

  TDWFormModule = class(TCustomModule)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure ValidateComponent(Component: TComponent); override;
    function ValidateComponentClass(ComponentClass: TComponentClass): Boolean; override;
  end;

  TDWDatamoduleModule = class(TDataModuleCustomModule) // TDataModuleDesignerCustomModule)
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure ValidateComponent(Component: TComponent); override;
  end;

procedure register;

implementation

uses DW.VCL.Control, DW.VCL.Labels, DW.CORE.Server, DW.VCL.Edit,
  DW.DESIGN.FormWizard, DW.DESIGN.AppWizard, DWForm, DW.VCL.Container,
  DW.HTML.Page, DW.CORE.UserSession, DW.VCL.DataModule, DW.VCL.Input,
  DW.VCL.Region, DW.VCL.Buttons, DW.VCL.Modal, DW.VCL.Accordion,
  DW.DESIGN.AccordionEditor, DW.VCL.TabControl, DW.DESIGN.TabControlEditor,
  DW.VCL.JQGrid, DW.VCL.FluidForm, DW.VCL.Table, DW.VCL.Iframe, DW.VCL.Validator,
  DW.VCL.InputMoney, DW.VCL.Select2, DW.VCL.Image, DW.DESIGN.FrameWizard,
  DW.VCL.Frame, DW.VCL.InputForm, DW.DESIGN.DataModuleWizard;

procedure register;
begin
  RegisterComponents('DelphiWeb', [TDWServer]);
  RegisterComponents('DelphiWeb', [TDWLabel]);
  RegisterComponents('DelphiWeb', [TDWEdit]);
  // Bootstrap Inputs
  RegisterComponents('DelphiWeb', [TDWInput]);
  RegisterComponents('DelphiWeb', [TDWMemo]);
  RegisterComponents('DelphiWeb', [TDWCheckBox]);
  RegisterComponents('DelphiWeb', [TDWRadioButton]);
  RegisterComponents('DelphiWeb', [TDWSelect]);
  RegisterComponents('DelphiWeb', [TDWRadioGroup]);
  RegisterComponents('DelphiWeb', [TDWInputMoney]);
  RegisterComponents('DelphiWeb', [TDWSelect2]);
  RegisterComponents('DelphiWeb', [TDWInputGroup]);
  // others
  RegisterComponents('DelphiWeb', [TDWButton]);
  RegisterComponents('DelphiWeb', [TDWModal]);
  RegisterComponents('DelphiWeb', [TDWAccordion]);
  RegisterComponentEditor(TDWAccordion, TDWAccordionEditor);
  RegisterComponents('DelphiWeb', [TDWTabControl]);
  RegisterComponentEditor(TDWTabControl, TDWTabControlEditor);
  RegisterComponents('DelphiWeb', [TDWJQGrid]);
  RegisterComponents('DelphiWeb', [TDWTable]);
  RegisterComponents('DelphiWeb', [TDWValidator]);
  RegisterComponents('DelphiWeb', [TDWImage]);

  // Bootstrap Regions
  RegisterComponents('DelphiWeb', [TDWRegion]);
  RegisterComponents('DelphiWeb', [TDWFluidForm]);
  RegisterComponents('DelphiWeb', [TDWIFrame]);

  // DelphiWeb Wizards
  RegisterCustomModule(TDWForm, TDWFormModule);
  RegisterCustomModule(TDWFrame, TDWFormModule);
  RegisterCustomModule(TDWUserSession, TDataModuleCustomModule);
  // UnlistPublishedProperty(TDWUserSession, 'OldCreateOrder');
  // UnlistPublishedProperty(TDWUserSession, 'OnCreate');
  // UnlistPublishedProperty(TDWUserSession, 'OnDestroy');
  RegisterCustomModule(TDWDatamodule, TDataModuleCustomModule);
  RegisterPackageWizard(TDWAppWizard.Create);
  RegisterPackageWizard(TDWFormWizard.Create);
  RegisterPackageWizard(TDWFrameWizard.Create);
  RegisterPackageWizard(TDWDataModuleWizard.Create);
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

function TDWFormModule.ValidateComponentClass(ComponentClass: TComponentClass): Boolean;
begin
  if ComponentClass.InheritsFrom(TDWContainer) or ComponentClass.InheritsFrom(TDWControl) then
    Result := True
  else
    Result := False;
end;

{ TDWDatamodule }

procedure TDWDatamoduleModule.ExecuteVerb(Index: Integer);
begin
  inherited;

end;

function TDWDatamoduleModule.GetVerb(Index: Integer): string;
begin
  Result := inherited;
end;

function TDWDatamoduleModule.GetVerbCount: Integer;
begin
  Result := inherited;
end;

procedure TDWDatamoduleModule.ValidateComponent(Component: TComponent);
begin
  inherited;
end;

end.
