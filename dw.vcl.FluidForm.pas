unit dw.vcl.FluidForm;

interface

uses
  System.Classes, dw.vcl.InputForm, DWTypes, DWElementTag;

type

  TDWFormEncType = (iwbsfeDefault, iwbsfeMultipart, iwbsfeText);

  TDWFluidForm = class(TDWCustomInputForm)
  private
    FEncType: TDWFormEncType;
    FOnSubmit: TDWInputFormSubmitEvent;
    procedure DoSubmit(aParams: TStrings);
  protected
    procedure InternalRenderCss(var ACss: string); override;
    procedure InternalRenderScript(const AHTMLName: string; AScript: TStringList); override;
    function RenderHTML: TDWElementTag; override;
    function RenderAsync: TDWElementXHTMLTag; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetRoleString: string; override;
  published
    property EncType: TDWFormEncType read FEncType write FEncType default iwbsfeDefault;
    property OnSubmit: TDWInputFormSubmitEvent read FOnSubmit write FOnSubmit;
  end;

implementation

uses
  System.SysUtils, dw.vcl.Common, DWUtils;

{ TIWBSFluidForm }

constructor TDWFluidForm.Create(AOwner: TComponent);
begin
  inherited;
  FEncType := iwbsfeDefault;
end;

destructor TDWFluidForm.Destroy;
begin

  inherited;
end;

procedure TDWFluidForm.DoSubmit(aParams: TStrings);
begin

  if Assigned(FOnSubmit) then
    FOnSubmit(aParams);
  { TODO 1 -oDELCIO -cIMPLEMENT : Need to implement TIWBSFluidForm Submit event }
  // aReply.SendRedirect(aApplication.SessionInternalUrlBase);
end;

function TDWFluidForm.GetRoleString: string;
begin
  Result := 'form';
end;

procedure TDWFluidForm.InternalRenderCss(var ACss: string);
begin
  TDWBSCommon.AddCssClass(ACss, 'iwbs-form-fluid');
end;

procedure TDWFluidForm.InternalRenderScript(const AHTMLName: string; AScript: TStringList);
begin
  inherited;
  if ValidationEnabled and Hasvalidator then
    AScript.Add('$("#' + HTMLName + '").validator(''validate'');');
end;

function TDWFluidForm.RenderAsync: TDWElementXHTMLTag;
begin
  Result := inherited;
  if ValidationEnabled and Hasvalidator then
    DWApplication.CallBackResp.AddScriptToExecute
      ('$("#' + HTMLName + '").validator(''validate'');', False);
end;

function TDWFluidForm.RenderHTML: TDWElementTag;
var
  LParentForm: TDWCustomInputForm;
begin
  LParentForm := DWFindParentInputForm(Parent);
  if LParentForm <> nil then
    raise Exception.Create('forms can not be nested, you try to put ' + Name + ' inside ' +
      LParentForm.Name);

  Result := inherited;

  if ValidationEnabled and Hasvalidator then
    Result.AddStringParam('data-toggle', 'validator');

  if Assigned(FOnSubmit) then
    begin
      raise Exception.Create
        ('TIWBSFluidForm Submit event not implemented yet. Contribute with this.');
      { TODO 1 -oDELCIO -cIMPLEMENT : Need to implement TIWBSFluidForm Submit event }
      (* Result.AddStringParam('method', 'post');
        if FEncType = iwbsfeMultipart then
        Result.AddStringParam('enctype', 'multipart/form-data')
        else if FEncType = iwbsfeText then
        Result.AddStringParam('enctype', 'text/plain');
        Result.AddStringParam('action', DWApplication.RegisterCallBack( HTMLName+'.FormSubmit', DoSubmit, (FEncType = iwbsfeMultipart)));
      *)
    end
  else
    Result.AddStringParam('onSubmit', 'return FormDefaultSubmit();');
end;

end.
