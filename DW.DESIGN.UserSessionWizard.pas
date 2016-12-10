unit DW.DESIGN.UserSessionWizard;

interface

uses Classes, Winapi.Windows, Vcl.Dialogs, DesignIntf, ToolsAPI, TypInfo;

type

  TDWUserSessionModuleCreator = class(TInterfacedObject, IOTACreator, IOTAModuleCreator)
  private
    FOwner: IOTAModule;
    FImplFileName: string;
    FFormName: string;
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

  TDWUserSessionModuleSource = class(TInterfacedObject, IOTAFile)
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
  SysUtils, System.Win.ComObj, DWForm, DW.Vcl.Labels, DW.CORE.UserSession;

procedure DebugMsg(const Msg: String);
begin
  // ShowMessage(Msg);
end;

constructor TDWUserSessionModuleCreator.Create;
begin
  Create(nil);
end;

constructor TDWUserSessionModuleCreator.Create(AOwner: IOTAModule);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TDWUserSessionModuleCreator.FormCreated(const FormEditor: IOTAFormEditor);
begin
end;

function TDWUserSessionModuleCreator.GetAncestorName: string;
begin
  Result := 'DWUserSession';
end;

function TDWUserSessionModuleCreator.GetCreatorType: string;
begin
  Result := sForm;
end;

function TDWUserSessionModuleCreator.GetExisting: Boolean;
begin
  Result := False;
end;

function TDWUserSessionModuleCreator.GetFileSystem: string;
begin
  Result := '';
end;

function TDWUserSessionModuleCreator.GetFormName: string;
begin
  Result := 'DWSessionData';
end;

function TDWUserSessionModuleCreator.GetImplFileName: string;
begin
  Result := GetCurrentDir + '\uDWSessionData.pas';
end;

function TDWUserSessionModuleCreator.GetIntfFileName: string;
begin
  Result := '';
end;

function TDWUserSessionModuleCreator.GetMainForm: Boolean;
begin
  Result := False;
end;

function TDWUserSessionModuleCreator.GetOwner: IOTAModule;
begin
  if FOwner <> nil then
    Result := FOwner
  else
    Result := GetActiveProject;
end;

function TDWUserSessionModuleCreator.GetShowForm: Boolean;
begin
  Result := True;
end;

function TDWUserSessionModuleCreator.GetShowSource: Boolean;
begin
  Result := True;
end;

function TDWUserSessionModuleCreator.GetUnnamed: Boolean;
begin
  Result := True;
end;

function TDWUserSessionModuleCreator.NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

function TDWUserSessionModuleCreator.NewImplSource(const ModuleIdent, FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  Result := TDWUserSessionModuleSource.Create(ModuleIdent, FormIdent, AncestorIdent);
end;

function TDWUserSessionModuleCreator.NewIntfSource(const ModuleIdent, FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

constructor TDWUserSessionModuleSource.Create(Const ModuleIdent, FormIdent, AncestorIdent: string);
begin
  inherited Create;
  FModuleIdent   := ModuleIdent;
  FFormIdent     := FormIdent;     // Copy(FormIdent, 2, MaxInt);
  FAncestorIdent := AncestorIdent; // Copy(AncestorIdent, 2,MaxInt);
end;

function TDWUserSessionModuleSource.GetAge: TDateTime;
begin
  Result := -1;
end;

function TDWUserSessionModuleSource.GetSource: string;
const
  cSource = 'unit %0:s;' + CrLf2 +

    'interface' + CrLf2 +

    'uses' + CrLf + '  DW.CORE.UserSession;' + CrLf +

    'type' + CrLf +

    '  T%1:s = class(T%2:s)' + CrLf + '  private' + CrLf + '    { private declarations }' + CrLf +
    '  public' + CrLf + '    { public declarations }' + CrLf + '  end;' + CrLf2 +

    '{ !!! Do Not Declare Global Variables !!! }' + CrLf2 +

    'implementation' + CrLf2 +

  // '{'#9'%CLASSGROUP ''Vcl.Controls.TControl''}' + CrLf2 +

    '{$R *.dfm}' + CrLf2 +

    'end.';
begin
  Result := Format(cSource, [FModuleIdent, FFormIdent, FAncestorIdent]);
end;

end.
