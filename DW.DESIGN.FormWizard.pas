unit DW.DESIGN.FormWizard;

interface

uses Classes, Winapi.Windows, Vcl.Dialogs, DesignIntf, ToolsAPI, TypInfo;

type

  TDWFormWizard = class(TInterfacedObject, IOTAWizard, IOTANotifier, IOTARepositoryWizard,
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

  TDWFormModuleCreator = class(TInterfacedObject, IOTACreator, IOTAModuleCreator)
  private
    FOwner: IOTAModule;
    FHaveNames: Boolean;
    FImplFileName: string;
    FFormName: string;
    FIsMainForm: Boolean;
    procedure GetNewModuleAndClassName(out AFormName, AImplFileName: string);
    function GetModuleAndClassNamePrefix: String;
  public
    constructor Create(AOwner: IOTAModule; IsMainForm: Boolean); overload;
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

  TDWFormModuleSource = class(TInterfacedObject, IOTAFile)
  private
    FModuleIdent: string;
    FFormIdent: string;
    FAncestorIdent: string;
    FDefaultForm: Boolean;
  public
    constructor Create(const ModuleIdent, FormIdent, AncestorIdent: string; AsDefaultForm: Boolean);
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

procedure TDWFormWizard.AfterSave;
begin
  DebugMsg('AfterSave');
end;

procedure TDWFormWizard.BeforeSave;
begin
  DebugMsg('BeforeSave');
end;

procedure TDWFormWizard.Destroyed;
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

procedure TDWFormWizard.Execute;
var
  ModuleServices: IOTAModuleServices;
  ProjectGroup: IOTAProjectGroup;
begin

  DebugMsg('TDWFormWizard.Execute');

  ModuleServices := BorlandIDEServices as IOTAModuleServices;

  ModuleServices.CreateModule(TDWFormModuleCreator.Create);

end;

constructor TDWFormModuleCreator.Create(AOwner: IOTAModule);
begin
  Create(AOwner, False);
end;

constructor TDWFormModuleCreator.Create;
begin
  Create(nil, False);
end;

constructor TDWFormModuleCreator.Create(AOwner: IOTAModule; IsMainForm: Boolean);
begin
  inherited Create;
  FOwner      := AOwner;
  FIsMainForm := IsMainForm;
end;

procedure TDWFormModuleCreator.FormCreated(const FormEditor: IOTAFormEditor);
var
  Form: IOTAComponent;
  PropInfo: PPropInfo;
  TypeInfo: PTypeInfo;
  NativeFormEditor: INTAFormEditor;
  aHelloWorld: TDWLabel;
begin
  DebugMsg('FormCreated');
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
              aHelloWorld.RawText := True;
              aHelloWorld.Caption := 'Hello World<br>DelphiWeb';
            end;
        end;
    end;
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

function TDWFormModuleCreator.GetAncestorName: string;
begin
  Result := 'DWForm';
  DebugMsg('GetAncestorName: ' + Result);
end;

function TDWFormWizard.GetAuthor: string;
begin
  Result := 'Delcio Sbeghen SRP Sistemas';
  DebugMsg('GetAuthor: ' + Result);
end;

function TDWFormWizard.GetComment: string;
begin
  Result := 'Creates a new DWForm.';
  DebugMsg('GetComment: ' + Result);
end;

function TDWFormModuleCreator.GetCreatorType: string;
begin
  Result := sForm;
  DebugMsg('GetCreatorType');
end;

function TDWFormModuleCreator.GetExisting: Boolean;
begin
  Result := False;
  DebugMsg('GetExisting');
end;

function TDWFormModuleCreator.GetFileSystem: string;
begin
  Result := '';
  DebugMsg('GetFileSystem: ' + Result);
end;

function TDWFormModuleCreator.GetFormName: string;
var
  LImplFileName: string;
begin
  if FIsMainForm then
    begin
      Result := 'WebMainForm';
    end
  else
    GetNewModuleAndClassName(Result, LImplFileName);
end;

procedure TDWFormModuleCreator.GetNewModuleAndClassName(out AFormName: string;
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
          FFormName     := LClassName;
        end;
    end;
  AFormName     := FFormName;
  AImplFileName := FImplFileName;
end;

// define a form and unitname prefix
function TDWFormModuleCreator.GetModuleAndClassNamePrefix: String;
begin
  Result := '';
end;

function TDWFormWizard.GetGlyph: Cardinal;
begin
  Result := 0;
end;

function TDWFormWizard.GetIDString: string;
begin
  Result := 'DW.DWForm';
end;

function TDWFormModuleCreator.GetImplFileName: string;
begin
  if FIsMainForm then
    Result := GetCurrentDir + '\uWebMainForm.pas'
  else
    Result := '';
end;

function TDWFormModuleCreator.GetIntfFileName: string;
begin
  Result := '';
end;

function TDWFormModuleCreator.GetMainForm: Boolean;
begin
  Result := False;
end;

function TDWFormWizard.GetName: string;
begin
  Result := 'DelphiWeb Form';
end;

function TDWFormModuleCreator.GetOwner: IOTAModule;
begin
  if FOwner <> nil then
    Result := FOwner
  else
    Result := GetActiveProject;
end;

function TDWFormWizard.GetPage: string;
begin
  Result := 'Delphi Web';
  DebugMsg('GetPage: ' + Result);
end;

function TDWFormModuleCreator.GetShowForm: Boolean;
begin
  Result := True;
  DebugMsg('GetShowForm');
end;

function TDWFormModuleCreator.GetShowSource: Boolean;
begin
  Result := True;
  DebugMsg('GetShowSource');
end;

function TDWFormWizard.GetState: TWizardState;
begin
  Result := [];
  DebugMsg('GetState');
end;

function TDWFormModuleCreator.GetUnnamed: Boolean;
begin
  Result := True;
  DebugMsg('GetUnnamed');
end;

procedure TDWFormWizard.Modified;
begin
  DebugMsg('Modified');
end;

function TDWFormModuleCreator.NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := nil;
  DebugMsg('NewFormFile');
end;

function TDWFormModuleCreator.NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string)
  : IOTAFile;
begin
  Result := TDWFormModuleSource.Create(ModuleIdent, FormIdent, AncestorIdent, FIsMainForm);
  DebugMsg('NewImplSource');
end;

function TDWFormModuleCreator.NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string)
  : IOTAFile;
begin
  Result := nil;
  DebugMsg('NewIntfSource');
end;

constructor TDWFormModuleSource.Create(Const ModuleIdent, FormIdent, AncestorIdent: string;
  AsDefaultForm: Boolean);
begin
  inherited Create;
  FModuleIdent   := ModuleIdent;
  FFormIdent     := FormIdent;     // Copy(FormIdent, 2, MaxInt);
  FAncestorIdent := AncestorIdent; // Copy(AncestorIdent, 2,MaxInt);
  FDefaultForm   := AsDefaultForm;
  DebugMsg('TDWFormModuleSource.Create: ' + FModuleIdent + ' ,' + FFormIdent + ' ,' +
    FAncestorIdent);
end;

function TDWFormModuleSource.GetAge: TDateTime;
begin
  Result := -1;
  DebugMsg('GetAge: ' + DateToStr(Result));
end;

function TDWFormModuleSource.GetSource: string;
const
  cHeader = '{ DelphiWeb is an VCL internet Framework for  Delphi' + CrLf +
    '| https://github.com/DrHank/DelphiWeb' + CrLf + '| Developped by Delcio Sbeghen @SRP Sistemas'
    + CrLf + '| Under MIT Licence: https://opensource.org/licenses/MIT' + CrLf + '|' + CrLf +
    '| Credits:' + CrLf + '|   * The Server Core is based in OverByte ICS: http://www.overbyte.be/'
    + CrLf + '|   * The Json Library is based in JsonDataObjects:  https://github.com/ahausladen/JsonDataObjects'
    + CrLf + '|and changes by IWBootstrap Framework' + CrLf +
    '|   * The DW VCL is based on Bootstrap Framework: https://github.com/kattunga/IWBootstrapFramework'
    + CrLf + '}' + CrLf2;

const
  cSource = 'unit %0:s;' + CrLf2 +

    'interface' + CrLf2 +

    'uses' + CrLf + '  DWForm;' + CrLf +

    'type' + CrLf +

    '  T%1:s = class(T%2:s)' + CrLf + '  private' + CrLf + '    { private declarations }' + CrLf +
    '  public' + CrLf + '    { public declarations }' + CrLf + '  end;' + CrLf2 +

    '{ !!! Do Not Declare Global Variables !!! }' + CrLf2 +

    'implementation' + CrLf2 +

    '{$R *.dfm}' + CrLf2 +

    'end.';
begin
  if FDefaultForm then
    Result := Format(cHeader + cSource, [FModuleIdent, FFormIdent, FAncestorIdent])
  else
    Result := Format(cSource, [FModuleIdent, FFormIdent, FAncestorIdent]);
end;

end.
