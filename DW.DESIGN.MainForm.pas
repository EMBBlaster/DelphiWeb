unit DW.DESIGN.MainForm;

interface

uses Classes, SysUtils, Windows, Vcl.Dialogs, DesignIntf, ToolsAPI, TypInfo;

type

  TDWMainFormModuleCreator = class(TInterfacedObject, IOTACreator, IOTAModuleCreator)
  private
    FOwner: IOTAModule;
    FHaveNames: Boolean;
    FImplFileName: string;
    FFormName: string;
  public
    constructor Create(AOwner: IOTAModule); overload;
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

  TDWmainFormModuleSource = class(TInterfacedObject, IOTAFile)
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
  System.Win.ComObj, DWForm, DW.CORE.Server;

procedure DebugMsg(const Msg: String);
begin
  // ShowMessage(Msg);
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

constructor TDWMainFormModuleCreator.Create(AOwner: IOTAModule);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TDWMainFormModuleCreator.FormCreated(const FormEditor: IOTAFormEditor);
var
  aServer: IOTAComponent;
  Form: IOTAComponent;
  Method: TMethod;
  PropInfo: PPropInfo;
  TypeInfo: PTypeInfo;
  NativeFormEditor: INTAFormEditor;
  Server: TComponent;
begin
  DebugMsg('FormCreated');
  Form := FormEditor.GetRootComponent;
  // Add OnShow Event
  if Supports(FormEditor, INTAFormEditor, NativeFormEditor) then
    begin
      if NativeFormEditor.FormDesigner <> nil then
        begin
          TypeInfo := PTypeInfo(NativeFormEditor.FormDesigner.GetRoot.ClassInfo);
          PropInfo := GetPropInfo(TypeInfo, 'OnShow');
          // Method:=  NativeFormEditor.FormDesigner.GetMethods('FormShow');
          Method := NativeFormEditor.FormDesigner.CreateMethod('FormShow',
            GetTypeData(PropInfo^.PropType^));
          // if Assigned(Method) then
          SetMethodProp(NativeFormEditor.FormDesigner.GetRoot, PropInfo, Method);
          { TODO 1 -oDELCIO -cIMPLEMENT :  write OnShow Form Code }
          // Write Method Code, see chapter 21 of PDF or 11 of online book:
          // http://www.davidghoyle.co.uk/WordPress/wp-content/uploads/2016/03/The-Delphi-IDE-Open-Tools-API-Version-1.1.pdf
          // other literature:
          // http://www.delphipraxis.net/189813-sourcecode-fuer-zur-designtime-erzeugten-methode-per-designer-createmethod.html
          // http://www.delphimaster.ru/cgi-bin/forum.pl?id=1234748058&n=12
          // http://docwiki.embarcadero.com/Libraries/Seattle/en/System.Rtti.TRttiType.GetMethods
          // http://stackoverflow.com/questions/27969212/how-to-find-index-of-a-method-in-an-interface*)
          Server := NativeFormEditor.FormDesigner.CreateComponent(TDWServer,
            NativeFormEditor.FormDesigner.GetRoot, 0, 0, 0, 0);
          Server.Name := 'Server';
        end;
    end;
end;

function TDWMainFormModuleCreator.GetAncestorName: string;
begin
  Result := 'Form';
end;

function TDWMainFormModuleCreator.GetCreatorType: string;
begin
  Result := sForm;
end;

function TDWMainFormModuleCreator.GetExisting: Boolean;
begin
  Result := False;
end;

function TDWMainFormModuleCreator.GetFileSystem: string;
begin
  Result := '';
end;

function TDWMainFormModuleCreator.GetFormName: string;
begin
  Result := 'FrmServerMain';
end;

function TDWMainFormModuleCreator.GetImplFileName: string;
begin
  Result := GetCurrentDir + '\uServerMainForm.pas';
  DebugMsg('GetImplFileName: ' + Result);
end;

function TDWMainFormModuleCreator.GetIntfFileName: string;
begin
  Result := '';
end;

function TDWMainFormModuleCreator.GetMainForm: Boolean;
begin
  Result := True;
  DebugMsg('GetMainForm');
end;

function TDWMainFormModuleCreator.GetOwner: IOTAModule;
begin
  if FOwner <> nil then
    Result := FOwner
  else
    Result := GetActiveProject;
end;

function TDWMainFormModuleCreator.GetShowForm: Boolean;
begin
  Result := True;
  DebugMsg('GetShowForm');
end;

function TDWMainFormModuleCreator.GetShowSource: Boolean;
begin
  Result := True;
  DebugMsg('GetShowSource');
end;

function TDWMainFormModuleCreator.GetUnnamed: Boolean;
begin
  Result := True;
  DebugMsg('GetUnnamed');
end;

function TDWMainFormModuleCreator.NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := nil;
  DebugMsg('NewFormFile');
end;

function TDWMainFormModuleCreator.NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string)
  : IOTAFile;
begin
  Result := TDWmainFormModuleSource.Create(ModuleIdent, FormIdent, AncestorIdent);
  DebugMsg('NewImplSource');
end;

function TDWMainFormModuleCreator.NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string)
  : IOTAFile;
begin
  Result := nil;
  DebugMsg('NewIntfSource');
end;

constructor TDWmainFormModuleSource.Create(Const ModuleIdent, FormIdent, AncestorIdent: string);
begin
  inherited Create;
  FModuleIdent   := ModuleIdent;
  FFormIdent     := FormIdent;     // Copy(FormIdent, 2, MaxInt);
  FAncestorIdent := AncestorIdent; // Copy(AncestorIdent, 2,MaxInt);
  DebugMsg('TDWFormModuleSource.Create: ' + FModuleIdent + ' ,' + FFormIdent + ' ,' +
    FAncestorIdent);
end;

function TDWmainFormModuleSource.GetAge: TDateTime;
begin
  Result := -1;
  DebugMsg('GetAge: ' + DateToStr(Result));
end;

function TDWmainFormModuleSource.GetSource: string;
const
  cSource = 'unit %0:s;' + CrLf2 +

    'interface' + CrLf2 +

    'uses' + CrLf + '  VCL.Forms, System.SysUtils, DW.CORE.Server;' + CrLf +

    'type' + CrLf +

    '  T%1:s = class(T%2:s)' + CrLf + '    procedure FormShow(Sender: TObject);' + CrLf +
    '  private' + CrLf + '{ private declarations }' + CrLf + '  public' + CrLf +
    '    { public declarations }' + CrLf + '  end;' + CrLf2 +

    '{ !!! Do Not Declare Global Variables !!! }' + CrLf2 +

    'implementation' + CrLf + '  uses' + CrLf + '    uWebMainForm, uDWSessionData;' + CrLf2 +

    '{$R *.dfm}' + CrLf2 +

    'procedure T%1:s.FormShow(Sender: TObject);' + CrLf + 'begin' + CrLf +
    '  Server.MainForm:= TWebMainForm;' + CrLf + '  Server.UserDataModule:= TDWSessionData;' + CrLf
    + '  Server.DocDir      := Server.AppPath + ''wwwroot'';' + CrLf +
    '  Server.TemplateDir := Server.AppPath + ''wwwroot\templates'';' + CrLf +
    '  ForceDirectories(Server.AppPath + ''wwwroot'');' + CrLf +
    '  ForceDirectories(Server.AppPath + ''wwwroot\templates'');' + CrLf + '  Server.Start;' + CrLf
    + 'end;' + CrLf +

    'end.';
begin
  Result := Format(cSource, [FModuleIdent, FFormIdent, FAncestorIdent]);
  DebugMsg('GetSource: ' + Result);
end;

end.
