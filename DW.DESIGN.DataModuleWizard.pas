unit DW.DESIGN.DataModuleWizard;

interface

uses Classes, Winapi.Windows, Vcl.Dialogs, DesignIntf, ToolsAPI, TypInfo;

type

  TDWDataModuleWizard = class(TInterfacedObject, IOTAWizard, IOTANotifier, IOTARepositoryWizard,
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

  TDWDataModuleModuleCreator = class(TInterfacedObject, IOTACreator, IOTAModuleCreator)
  private
    FOwner: IOTAModule;
    FHaveNames: Boolean;
    FImplFileName: string;
    FFrameName: string;
    procedure GetNewModuleAndClassName(out AFormName, AImplFileName: string);
    function GetModuleAndClassNamePrefix: String;
  public
    constructor Create(AOwner: IOTAModule); overload;
    constructor Create; overload;
    // IOTACreator
    function GetCreatorType: string;
    function GetExisting: Boolean;
    function GetFileSystem: string;
    function GetOwner: IOTAModule;
    function GetUnnamed: Boolean;
    // IOTAModuleCreator
    function GetAncestorName: string;
    function GetImplFileName: string;
    function GetIntfFileName: string;
    function GetFormName: string;
    function GetMainForm: Boolean;
    function GetShowForm: Boolean;
    function GetShowSource: Boolean;
    function NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
    function NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
    function NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
    procedure FormCreated(const FormEditor: IOTAFormEditor);

  end;

  TDWDatamoduleModuleSource = class(TInterfacedObject, IOTAFile)
  private
    FModuleIdent: string;
    FFormIdent: string;
    FAncestorIdent: string;
  public
    constructor Create(const ModuleIdent, FormIdent, AncestorIdent: string);
    // IOTAFile
    function GetSource: string;
    function GetAge: TDateTime;
  end;

const
  CrLf2 = #13#10#13#10;
  CrLf  = #13#10;

implementation

uses
  SysUtils, System.Win.ComObj, DWForm, DW.Vcl.Labels;

procedure DebugMsg(const Msg: String);
begin
  // ShowMessage(Msg);
end;

procedure TDWDataModuleWizard.AfterSave;
begin
  DebugMsg('AfterSave');
end;

procedure TDWDataModuleWizard.BeforeSave;
begin
  DebugMsg('BeforeSave');
end;

procedure TDWDataModuleWizard.Destroyed;
begin
  DebugMsg('TDWFormModuleCreator.Destroyed');
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

procedure TDWDataModuleWizard.Execute;
var
  ModuleServices: IOTAModuleServices;
  ProjectGroup: IOTAProjectGroup;
begin

  DebugMsg('TDWFrameWizard.Execute');

  ModuleServices := BorlandIDEServices as IOTAModuleServices;

  ModuleServices.CreateModule(TDWDataModuleModuleCreator.Create);

end;

constructor TDWDataModuleModuleCreator.Create;
begin
  Create(nil);
end;

constructor TDWDataModuleModuleCreator.Create(AOwner: IOTAModule);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TDWDataModuleModuleCreator.FormCreated(const FormEditor: IOTAFormEditor);
var
  Form: IOTAComponent;
  PropInfo: PPropInfo;
  TypeInfo: PTypeInfo;
  NativeFormEditor: INTAFormEditor;
  aHelloWorld: TDWLabel;
begin
  (* DebugMsg('FormCreated');
    if FIsMainForm then
    begin
    Form := FormEditor.GetRootComponent;
    // Add OnShow Event
    if Supports(FormEditor, INTAFormEditor, NativeFormEditor) then
    begin
    if NativeFormEditor.FormDesigner <> nil then
    begin
    aHelloWorld := TDWLabel(NativeFormEditor.FormDesigner.CreateComponent(TDWLabel,
    NativeFormEditor.FormDesigner.GetRoot, 0, 0, 0, 0));
    aHelloWorld.Caption := 'Hello World<br>DelphiWeb';
    end;
    end;
    end; *)
  (*
    var
    aHelloWorld   : IOTAComponent;
    begin
    DebugMsg('FormCreated');
    if FIsMainForm then
    begin
    with FormEditor.CreateComponent(FormEditor.GetRootComponent, 'TDWLabel', 0, 0, 200, 50) do
    begin
    SetPropByName('Caption', 'Hello World<br>DelphiWeb');
    end;
    end; *)
end;

function TDWDataModuleModuleCreator.GetAncestorName: string;
begin
  Result := 'DWDataModule';
  DebugMsg('GetAncestorName: ' + Result);
end;

function TDWDataModuleWizard.GetAuthor: string;
begin
  Result := 'Delcio Sbeghen SRP Sistemas';
  DebugMsg('GetAuthor: ' + Result);
end;

function TDWDataModuleWizard.GetComment: string;
begin
  Result := 'Creates a new DWDataModule.';
  DebugMsg('GetComment: ' + Result);
end;

function TDWDataModuleModuleCreator.GetCreatorType: string;
begin
  Result := sForm;
  DebugMsg('GetCreatorType');
end;

function TDWDataModuleModuleCreator.GetExisting: Boolean;
begin
  Result := False;
  DebugMsg('GetExisting');
end;

function TDWDataModuleModuleCreator.GetFileSystem: string;
begin
  Result := '';
  DebugMsg('GetFileSystem: ' + Result);
end;

function TDWDataModuleModuleCreator.GetFormName: string;
var
  LImplFileName: string;
begin
  GetNewModuleAndClassName(Result, LImplFileName);
end;

procedure TDWDataModuleModuleCreator.GetNewModuleAndClassName(out AFormName: string;
  out AImplFileName: string);
var
  LUnitIdent: string;
  LClassName: string;
  LFileName: string;
begin
  if not FHaveNames then
    begin
      FHaveNames := True;
      LClassName := GetModuleAndClassNamePrefix;
      if LClassName <> '' then
        begin
          (BorlandIDEServices as IOTAModuleServices).GetNewModuleAndClassName(LClassName + 'Unit',
            // Do not localize
            LUnitIdent, LClassName, LFileName);
          FImplFileName := LFileName;
          FFrameName    := LClassName;
        end;
    end;
  AFormName     := FFrameName;
  AImplFileName := FImplFileName;
end;

// define a form and unitname prefix
function TDWDataModuleModuleCreator.GetModuleAndClassNamePrefix: String;
begin
  Result := '';
end;

function TDWDataModuleWizard.GetGlyph: Cardinal;
begin
  Result := 0;
end;

function TDWDataModuleWizard.GetIDString: string;
begin
  Result := 'DW.DWDataModule';
end;

function TDWDataModuleModuleCreator.GetImplFileName: string;
begin
  Result := '';
end;

function TDWDataModuleModuleCreator.GetIntfFileName: string;
begin
  Result := '';
end;

function TDWDataModuleModuleCreator.GetMainForm: Boolean;
begin
  Result := False;
end;

function TDWDataModuleWizard.GetName: string;
begin
  Result := 'DelphiWeb DataModule';
end;

function TDWDataModuleModuleCreator.GetOwner: IOTAModule;
begin
  if FOwner <> nil then
    Result := FOwner
  else
    Result := GetActiveProject;
end;

function TDWDataModuleWizard.GetPage: string;
begin
  Result := 'Delphi Web';
  DebugMsg('GetPage: ' + Result);
end;

function TDWDataModuleModuleCreator.GetShowForm: Boolean;
begin
  Result := True;
  DebugMsg('GetShowForm');
end;

function TDWDataModuleModuleCreator.GetShowSource: Boolean;
begin
  Result := True;
  DebugMsg('GetShowSource');
end;

function TDWDataModuleWizard.GetState: TWizardState;
begin
  Result := [];
  DebugMsg('GetState');
end;

function TDWDataModuleModuleCreator.GetUnnamed: Boolean;
begin
  Result := True;
  DebugMsg('GetUnnamed');
end;

procedure TDWDataModuleWizard.Modified;
begin
  DebugMsg('Modified');
end;

function TDWDataModuleModuleCreator.NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := nil;
  DebugMsg('NewFormFile');
end;

function TDWDataModuleModuleCreator.NewImplSource(const ModuleIdent, FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  Result := TDWDatamoduleModuleSource.Create(ModuleIdent, FormIdent, AncestorIdent);
  DebugMsg('NewImplSource');
end;

function TDWDataModuleModuleCreator.NewIntfSource(const ModuleIdent, FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  Result := nil;
  DebugMsg('NewIntfSource');
end;

constructor TDWDatamoduleModuleSource.Create(Const ModuleIdent, FormIdent, AncestorIdent: string);
begin
  inherited Create;
  FModuleIdent   := ModuleIdent;
  FFormIdent     := FormIdent;     // Copy(FormIdent, 2, MaxInt);
  FAncestorIdent := AncestorIdent; // Copy(AncestorIdent, 2,MaxInt);
  DebugMsg('TDWFormModuleSource.Create: ' + FModuleIdent + ' ,' + FFormIdent + ' ,' +
    FAncestorIdent);
end;

function TDWDatamoduleModuleSource.GetAge: TDateTime;
begin
  Result := -1;
  DebugMsg('GetAge: ' + DateToStr(Result));
end;

function TDWDatamoduleModuleSource.GetSource: string;

const
  cSource = 'unit %0:s;' + CrLf2 +

    'interface' + CrLf2 +

    'uses' + CrLf + '  DW.VCL.DataModule;' + CrLf +

    'type' + CrLf +

    '  T%1:s = class(T%2:s)' + CrLf + '  private' + CrLf + '    { private declarations }' + CrLf +
    '  public' + CrLf + '    { public declarations }' + CrLf + '  end;' + CrLf2 +

    '{ !!! Do Not Declare Global Variables !!! }' + CrLf2 +

    'implementation' + CrLf2 +

    '{$R *.dfm}' + CrLf2 +

    'end.';
begin
  Result := Format(cSource, [FModuleIdent, FFormIdent, FAncestorIdent]);
end;

end.
