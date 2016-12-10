unit DW.VCL.InputForm;

interface
   uses
     Classes, System.SysUtils, Vcl.Controls, DW.VCL.CustomRegion, DWElementTag, DWTypes,
     DW.VCL.Common, DW.CORE.DWClientConnection;

   type
   TDWFormOptions = class(TPersistent)
  private
    FCaptionsSize: TDWGridOptions;
    FInputsSize: TDWGridOptions;
  protected
    procedure SetCaptionsSize(const Value: TDWGridOptions);
    procedure SetInputsSize(const Value: TDWGridOptions);
  public
    constructor Create(AOwner: TControl);
    destructor Destroy; override;
    function GetOffsetClassString: string;
    procedure Assign(Source: TPersistent); override;
  published
    property CaptionsSize: TDWGridOptions read FCaptionsSize write SetCaptionsSize;
    property InputsSize: TDWGridOptions read FInputsSize write SetInputsSize;
  end;


  TDWCustomInputForm = class(TDWCustomRegion)
  private
    FValidationEnabled: Boolean;
  protected
    procedure SetValidationEnabled(const Value: Boolean);
    function Hasvalidator:Boolean;
    function RenderHTML: TDWElementTag; override;
    procedure InternalRenderScript(Const AHTMLName: string; AScript: TStringList); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    //For enable/disable Validations on this form
    property ValidationEnabled:Boolean read FValidationEnabled write SetValidationEnabled default True;
  end;

  TDWInputForm = class(TDWCustomInputForm)
  private
    FEncType: TDWFormEncType;
    FFormType: TDWFormType;
    FFormOptions: TDWFormOptions;
    FOnSubmit: TDWInputFormSubmitEvent;
    procedure DoSubmit(aParams: TStringList);
    procedure SetEncType(const Value: TDWFormEncType);
    procedure SetFormType(const Value: TDWFormType);
  protected
    procedure InternalRenderCss(var ACss: string); override;
    function RenderHTML: TDWElementTag; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetRoleString: string; override;
  published
    property BSFormType: TDWFormType read FFormType write SetFormType default bsftVertical;
    property BSFormOptions: TDWFormOptions read FFormOptions write FFormOptions;
    property EncType: TDWFormEncType read FEncType write SetEncType default bsfeDefault;
    property OnSubmit: TDWInputFormSubmitEvent read FOnSubmit write FOnSubmit;
  end;

   TDWInputGroup = class(TDWCustomRegion)
  private
    FCaption: string;
    FRelativeSize: TDWRelativeSize;
    procedure SetCaption(const Value: string);
    procedure SetRelativeSize(const Value: TDWRelativeSize);
  protected
    procedure InternalRenderCss(var ACss: string); override;
  public
    constructor Create(AOwner: TComponent); override;
    function RenderHTML: TDWElementTag; override;
  published
    property Caption: string read FCaption write SetCaption;
    property BSRelativeSize: TDWRelativeSize read FRelativeSize write SetRelativeSize default bsrzDefault;
  end;

   TDWFormControl = class(TDWCustomRegion)
  private
    FCaption: string;
    procedure SetCaption(const Value: string);
  public
    function RenderHTML: TDWElementTag; override;
  published
    property Caption: string read FCaption write SetCaption;
  end;



implementation
 uses
   DWUtils, DW.VCL.NavBar;
{ TDWCustomInputForm }

constructor TDWCustomInputForm.Create(AOwner: TComponent);
begin
  inherited;
  FTagName := 'form';
  FValidationEnabled:=True;
end;

function TDWCustomInputForm.Hasvalidator: Boolean;
begin
  raise Exception.Create('Need to implement TDWCustomInputForm.Hasvalidator');
end;

procedure TDWCustomInputForm.InternalRenderScript(const AHTMLName: string;
  AScript: TStringList);
begin
  inherited;
end;

function TDWCustomInputForm.RenderHTML: TDWElementTag;
begin
  Result:= inherited;
end;

procedure TDWCustomInputForm.SetValidationEnabled(const Value: Boolean);
begin
  FValidationEnabled:= Value;
end;

{ TDWFormOptions }

procedure TDWFormOptions.Assign(Source: TPersistent);
begin
  if Source is TDWFormOptions then
    begin
      CaptionsSize.Assign(TDWFormOptions(Source).CaptionsSize);
      InputsSize.Assign(TDWFormOptions(Source).InputsSize);
    end
  else
    inherited;
end;

constructor TDWFormOptions.Create(AOwner: TControl);
begin
  FCaptionsSize := TDWGridOptions.Create(AOwner);
  FInputsSize := TDWGridOptions.Create(AOwner);
end;

destructor TDWFormOptions.Destroy;
begin
  FreeAndNil(FCaptionsSize);
  FreeAndNil(FInputsSize);
  inherited;
end;

function TDWFormOptions.GetOffsetClassString: string;
begin
  Result := FInputsSize.GetClassString(
    FCaptionsSize.GridXSOffset+FCaptionsSize.GridXSSpan,
    FCaptionsSize.GridSMOffset+FCaptionsSize.GridSMSpan,
    FCaptionsSize.GridMDOffset+FCaptionsSize.GridMDSpan,
    FCaptionsSize.GridLGOffset+FCaptionsSize.GridLGSpan);
end;

procedure TDWFormOptions.SetCaptionsSize(const Value: TDWGridOptions);
begin
  FCaptionsSize.Assign(Value);
end;

procedure TDWFormOptions.SetInputsSize(const Value: TDWGridOptions);
begin
  FInputsSize.Assign(Value);
end;

{ TDWInputForm }

constructor TDWInputForm.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TDWInputForm.Destroy;
begin

  inherited;
end;

procedure TDWInputForm.DoSubmit(aParams: TStringList);
begin
  if Assigned(FOnSubmit) then
    FOnSubmit(aParams);
  raise Exception.Create('Need to implement SendRedirect in TDWInputForm.DoSubmit');
  //aReply.SendRedirect(aApplication.SessionInternalUrlBase);
end;

function TDWInputForm.GetRoleString: string;
begin

end;

procedure TDWInputForm.InternalRenderCss(var ACss: string);
begin
  if FFormType = bsftInline then
    TDWBSCommon.AddCssClass(ACss, 'form-inline')
  else if FFormType = bsftHorizontal then
    TDWBSCommon.AddCssClass(ACss, 'form-horizontal');
  if (Parent is TDWNavBarBase) then
    TDWBSCommon.AddCssClass(ACss, 'navbar-form');
  inherited;
end;

function TDWInputForm.RenderHTML: TDWElementTag;
var
  LParentForm: TDWInputForm;
begin
  LParentForm := DWFindParentInputForm(Parent);
  if LParentForm <> nil then
    raise Exception.Create('forms can not be nested, you try to put '+Name+' inside '+LParentForm.Name);

  Result := inherited;

  if Assigned(FOnSubmit) then
    begin
      Result.AddStringParam('method', 'post');
      if FEncType = bsfeMultipart then
        Result.AddStringParam('enctype', 'multipart/form-data')
      else if FEncType = bsfeText then
        Result.AddStringParam('enctype', 'text/plain');
      Result.AddStringParam('action',
          DWApplication.RegisterCallBack(Self, ae_submit, DoSubmit));
    end
  else
    { TODO 1 -oDELCIO -cIMPLEMENT : FormDefaultSubmit() }
    Result.AddStringParam('onSubmit', 'return FormDefaultSubmit();');
end;

procedure TDWInputForm.SetEncType(const Value: TDWFormEncType);
begin
  FEncType := Value;
  Invalidate;
end;

procedure TDWInputForm.SetFormType(const Value: TDWFormType);
begin
  FFormType := Value;
  Invalidate;
end;

{ TDWInputGroup }

constructor TDWInputGroup.Create(AOwner: TComponent);
begin
  inherited;
  FRelativeSize := bsrzDefault;
end;

procedure TDWInputGroup.InternalRenderCss(var ACss: string);
begin
  TDWBSCommon.AddCssClass(ACss, 'input-group');
  if FRelativeSize <> bsrzDefault then
    TDWBSCommon.AddCssClass(ACss, 'input-group-'+aDWRelativeSize[FRelativeSize]);
  inherited;
end;

function TDWInputGroup.RenderHTML: TDWElementTag;
begin
  Result := inherited;
  Result := DWCreateInputFormGroup(Self, Parent, Result, FCaption, HTMLName);
end;

procedure TDWInputGroup.SetCaption(const Value: string);
begin
  FCaption := Value;
  Invalidate;
end;

procedure TDWInputGroup.SetRelativeSize(const Value: TDWRelativeSize);
begin
  FRelativeSize := Value;
  Invalidate;
end;

{ TDWFormControl }

function TDWFormControl.RenderHTML: TDWElementTag;
begin
  Result := Inherited;
  Result := DWCreateInputFormGroup(Self, Parent, Result, FCaption, HTMLName);
end;

procedure TDWFormControl.SetCaption(const Value: string);
begin
  FCaption := Value;
  Invalidate;
end;

end.
