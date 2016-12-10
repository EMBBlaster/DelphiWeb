unit DW.DESIGN.AppWizard;

interface

uses Classes, System.SysUtils, Vcl.Dialogs, ToolsAPI, ExpertsModules;

type
  TDWAppWizard = class(TInterfacedObject, IOTAWizard, IOTANotifier, IOTARepositoryWizard,
    IOTAProjectWizard)
  public
    // IOTANotifier
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;
    // IOTAWizard
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
    procedure Execute;
    // IOTARepositoryWizard
    function GetAuthor: string;
    function GetComment: string;
    function GetPage: string;
    function GetGlyph: Cardinal;
  end;

  TDWAppCreator = class(TInterfacedObject, IOTACreator, IOTAProjectCreator, IOTAProjectCreator50,
    IOTAProjectCreator80)
  private
    FOwner: IOTAProjectGroup;
  public
    constructor Create(const AOwner: IOTAProjectGroup);
    // IOTACreator
    function GetCreatorType: string;
    function GetExisting: Boolean;
    function GetFileSystem: string;
    function GetOwner: IOTAModule;
    function GetUnnamed: Boolean;
    // IOTAProjectCreator
    function GetFileName: string;
    function GetOptionFileName: string;
    function GetShowSource: Boolean;
    procedure NewDefaultModule;
    function NewOptionSource(const ProjectName: string): IOTAFile;
    procedure NewProjectResource(const Project: IOTAProject);
    function NewProjectSource(const ProjectName: string): IOTAFile;
    // IOTAProjectCreator50
    procedure NewDefaultProjectModule(const Project: IOTAProject);
    // IOTAProjectCreator80
    function GetProjectPersonality: string;
  end;

  TDWAppSource = class(TInterfacedObject, IOTAFile)
  private
    FProjectName: string;
  public
    constructor Create(const ProjectName: string);
    // IOTAFile
    function GetSource: string;
    function GetAge: TDateTime;
  end;

implementation

uses DW.DESIGN.FormWizard, DW.DESIGN.MainForm, DW.DESIGN.UserSessionWizard;

{ TDWAppWizard }

procedure TDWAppWizard.AfterSave;
begin

end;

procedure TDWAppWizard.BeforeSave;
begin

end;

procedure TDWAppWizard.Destroyed;
begin

end;

function GetActiveProjectGroup(const ModuleServices: IOTAModuleServices): IOTAProjectGroup;
var
  I: Integer;
begin
  Result := nil;
  for I  := 0 to ModuleServices.ModuleCount - 1 do
    if Supports(ModuleServices.Modules[I], IOTAProjectGroup, Result) then
      Break;
end;

procedure TDWAppWizard.Execute;
var
  // WizardForm: TzWizardForm;
  ModuleServices: IOTAModuleServices;
  ProjectGroup: IOTAProjectGroup;
begin
  // ShowMessage('TDWAppWizard.Execute');
  // WizardForm := TzWizardForm.Create(Application);
  try
    // if WizardForm.ShowModal <> mrOk then
    // Exit;
    ModuleServices := BorlandIDEServices as IOTAModuleServices;
    ProjectGroup   := GetActiveProjectGroup(ModuleServices);
    // if ProjectGroup <> nil then
    // ShowMessage(ProjectGroup.FileName);
    ModuleServices.CreateModule(TDWAppCreator.Create(ProjectGroup));

  finally
    // WizardForm.Free;
  end;
end;

function TDWAppWizard.GetAuthor: string;
begin
  Result := 'Delcio Sbeghen SRP Sistemas';
end;

function TDWAppWizard.GetComment: string;
begin
  Result := 'Creates a new DelphiWeb Application';
end;

function TDWAppWizard.GetGlyph: Cardinal;
begin
  Result := 0;
end;

function TDWAppWizard.GetIDString: string;
begin
  Result := 'DW.DWApp';
end;

function TDWAppWizard.GetName: string;
begin
  Result := 'DelphiWeb Application';
end;

function TDWAppWizard.GetPage: string;
begin
  Result := 'Delphi Web';
end;

function TDWAppWizard.GetState: TWizardState;
begin
  Result := [];
  // ShowMessage('TDWAppWizard.GetState');
end;

procedure TDWAppWizard.Modified;
begin
  // ShowMessage('TDWAppWizard.Modified');
end;

{ TDWAppCreator }

constructor TDWAppCreator.Create(const AOwner: IOTAProjectGroup);
begin
  inherited Create;
  FOwner := AOwner;
  // ShowMessage('TDWAppCreator.Create');
end;

function TDWAppCreator.GetCreatorType: string;
begin
  Result := sApplication;
  // ShowMessage('TDWAppCreator.GetCreatorType');
end;

function TDWAppCreator.GetExisting: Boolean;
begin
  Result := False;
  // ShowMessage('TDWAppCreator.GetExisting');
end;

function TDWAppCreator.GetFileName: string;
begin
  Result := GetCurrentDir + '\DelphiWeb.dpr';
  // ShowMessage('TDWAppCreator.GetFileName');
end;

function TDWAppCreator.GetFileSystem: string;
begin
  Result := '';
  // ShowMessage('TDWAppCreator.GetFileSystem');
end;

function TDWAppCreator.GetOptionFileName: string;
begin
  Result := '';
  // ShowMessage('TDWAppCreator.GetOptionFileName');
end;

function TDWAppCreator.GetOwner: IOTAModule;
begin
  Result := FOwner;
  // ShowMessage('TDWAppCreator.GetOwner');
end;

function TDWAppCreator.GetProjectPersonality: string;
begin
  Result := sDelphiPersonality;
  // ShowMessage('TDWAppCreator.GetProjectPersonality');
end;

function TDWAppCreator.GetShowSource: Boolean;
begin
  Result := False;
  // ShowMessage('TDWAppCreator.GetShowSource');
end;

function TDWAppCreator.GetUnnamed: Boolean;
begin
  Result := True;
  // ShowMessage('TDWAppCreator.GetUnnamed');
end;

procedure TDWAppCreator.NewDefaultModule;
begin
  // ShowMessage('TDWAppCreator.NewDefaultModule');
end;

procedure TDWAppCreator.NewDefaultProjectModule(const Project: IOTAProject);
var
  ModuleServices: IOTAModuleServices;
begin
  // ShowMessage('TDWAppCreator.NewDefaultProjectModule');
  ModuleServices := BorlandIDEServices as IOTAModuleServices;
  // Create the Server Main Form
  ModuleServices.CreateModule(TDWMainFormModuleCreator.Create(Project));
  // Created the Web main Form
  ModuleServices.CreateModule(TDWFormModuleCreator.Create(Project, True));
  // Cteate the UserSession DataModule
  ModuleServices.CreateModule(TDWUserSessionModuleCreator.Create(Project));
end;

function TDWAppCreator.NewOptionSource(const ProjectName: string): IOTAFile;
begin
  Result := nil;
  // ShowMessage('TDWAppCreator.NewOptionSource');
end;

procedure TDWAppCreator.NewProjectResource(const Project: IOTAProject);
begin
  // ShowMessage('TDWAppCreator.NewProjectResource');
end;

function TDWAppCreator.NewProjectSource(const ProjectName: string): IOTAFile;
begin
  // ShowMessage('TDWAppCreator.NewProjectSource');
  Result := TDWAppSource.Create(ProjectName);
  // ShowMessage(Result.Source);
end;

{ TDWAppSource }

constructor TDWAppSource.Create(const ProjectName: string);
begin
  inherited Create;
  FProjectName := ProjectName;
  // ShowMessage('TDWAppSource.Create');
end;

function TDWAppSource.GetAge: TDateTime;
begin
  // ShowMessage('TDWAppSource.GetAge');
  Result := -1;
end;

function TDWAppSource.GetSource: string;
begin
  Result := 'program ' + FProjectName + ';' + CrLf2 +

    'uses' + CrLf + '  Vcl.Forms;' + CrLf2 +

    ' {$R *.res}' + CrLf2 +

    'var' + CrLf + '  FrmServerMain:TFrmServerMain;' + CrLf + 'begin' + CrLf +
    '  Application.Initialize;' + CrLf + '  Application.MainFormOnTaskbar := True;' + CrLf +
    '  Application.CreateForm(TFrmServerMain, FrmServerMain);' + CrLf + '  Application.Run;' +
    CrLf + 'end.';
  // ShowMessage('TDWAppSource.GetSource');
end;

end.
