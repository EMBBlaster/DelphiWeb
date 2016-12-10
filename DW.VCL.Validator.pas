unit DW.VCL.Validator;

interface

uses Classes, DWElementTag, DW.VCL.InputForm;

type

  TDWValRuleType = (tbsvalRequired, tbsvalEmail, tbsvalUrl, tbsvalNumber, tbsvalTel, tbsvalDate);
  TDWValidation = class;

  TIWBSValidationRuleClass = class of TDWValidationRule;
  TDWValidationRule = class(TPersistent)
    private
      FValidation:TDWValidation;
    protected
    procedure RenderRuleTag(aHTMLTag:TDWElementTag); virtual;
    public
     property Validation: TDWValidation read FValidation;
  end;

  TDWValidationHack = class(TDWValidationRule)
    // This Hack are necessary to initialize TIWBSValidation Class
    // is necessary decarate all property off all TIWBSValidationRule descendants
    // on this class.
    { TODO 1 : View how to solve Access Violation on create TIWBSValidation in DesignMode and remove this Hack }
  private
    FMax: Integer;
    FMin: Integer;
    FValidateRange: Boolean;
    procedure SetMax(const Value: Integer);
    procedure SetMin(const Value: Integer);
    procedure SetValidateRange(const Value: Boolean);
  published
    property Min: Integer read FMin write SetMin;
    property Max: Integer read FMax write SetMax;
     property ValidateRange:Boolean read FValidateRange write SetValidateRange default False;
  end;

  TDWValidationRuleNumber = class(TDWValidationRule)
  private
    FMax: Integer;
    FMin: Integer;
    FValidateRange: Boolean;
    procedure SetMax(const Value: Integer);
    procedure SetMin(const Value: Integer);
    procedure SetValidateRange(const Value: Boolean);
  protected
    procedure RenderRuleTag(aHTMLTag:TDWElementTag); override;
    public
      constructor Create;
  published
    property Min: Integer read FMin write SetMin;
    property Max: Integer read FMax write SetMax;
    property ValidateRange:Boolean read FValidateRange write SetValidateRange default False;
  end;

  TDWValidation = class(TCollectionItem)
  private
    FRule: TDWValidationRule;
    FRuleType: TDWValRuleType;
    FRuleClass: TIWBSValidationRuleClass;
    FErrorMsg: string;
    FShowErrorMsg: Boolean;
    procedure SetRule(const Value: TDWValidationRule);
    procedure SetRuleType(const Value: TDWValRuleType);
    function GetRule: TDWValidationRule;
    function GetRuleClass: TIWBSValidationRuleClass;
    procedure SetErrorMsg(const Value: string);
    procedure SetShowErrorMsg(const Value: Boolean);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
  published
    property RuleType: TDWValRuleType read FRuleType write SetRuleType;
    property Rule: TDWValidationRule read GetRule write SetRule;
    property ErrorMsg:string read FErrorMsg write SetErrorMsg;
    property ShowErrorMsg:Boolean read FShowErrorMsg write SetShowErrorMsg default False;
  end;

  TDWValidator   = class;
  TIWBSValidationClass = class of TDWValidation;

  TDWValidations = class(TCollection)
  private
    FComp: TDWValidator;
    function GetValidation(Index: Integer): TDWValidation;
    procedure SetValidation(Index: Integer; const Value: TDWValidation);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(Comp: TDWValidator; ItemClass: TIWBSValidationClass);
    function Add: TDWValidation;
    property Comp: TDWValidator read FComp;
    property Items[Index: Integer]: TDWValidation read GetValidation write SetValidation; default;

  end;

  TDWValidator = class(TComponent)
  private
   // FControl: TIWBSCustomInput;
    FValidations: TDWValidations;
    procedure SetValidations(const Value: TDWValidations);

   // procedure SetControl(const Value: TIWBSCustomInput);
  protected
    //function GetDisplayName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure RenderValidation(aHTMLTag:TDWElementTag);
     procedure Validate(aForm: TDWCustomInputForm = nil);
     //procedure Update(aForm: TIWBSCustomInputForm = nil);
  published
    //property Control: TIWBSCustomInput read FControl write SetControl;
    property Validations: TDWValidations read FValidations write SetValidations;
  end;

 (* TIWBSValidator          = class;
  TIWBSValidadorCompClass = class of TIWBSValidadorComp;

  TIWBSValidadorComps = class(TCollection)
  private
    FValidator: TIWBSValidator;
    function GetValComp(Index: Integer): TIWBSValidadorComp;
    procedure SetValComp(Index: Integer; const Value: TIWBSValidadorComp);
  protected
    function GetOwner: TPersistent; override;

  public
    constructor Create(Validator: TIWBSValidator; ItemClass: TIWBSValidadorCompClass);
    function Add: TIWBSValidadorComp;
    property Validator: TIWBSValidator read FValidator;
    property Items[Index: Integer]: TIWBSValidadorComp read GetValComp write SetValComp; default;
  end;

  TIWBSValidator = class(TComponent)
  private
    FFocusOnError: Boolean;
    FCustomValidations: string;
    FFeedbackErrorIcon: string;
    FFeedbackSucessIcon: string;
    FPaternErrorMsg: string;
    FRequiredErrorMsg: string;
    FAllErrorMsg: string;
    FMatchErrorMsg: string;
    FComps: TIWBSValidadorComps;
    procedure SetFocusOnError(const Value: Boolean);
    procedure SetCustomValidations(const Value: string);
    procedure SetFeedbackErrorIcon(const Value: string);
    procedure SetFeedbackSucessIcon(const Value: string);
    procedure SetAllErrorMsg(const Value: string);
    procedure SetMatchErrorMsg(const Value: string);
    procedure SetPaternErrorMsg(const Value: string);
    procedure SetRequiredErrorMsg(const Value: string);
    procedure SetComps(const Value: TIWBSValidadorComps);
    function CreateComps: TIWBSValidadorComps;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Validate(aForm: IIWBSContainer = nil);
    procedure RenderValidation(aInput:TIWBSCustomInput; aHTMLTag:TIWHTMLTag);
  published
    property FocusOnError: Boolean read FFocusOnError write SetFocusOnError;
    property FeedbackSucessIcon: string read FFeedbackSucessIcon write SetFeedbackSucessIcon;
    property FeedbackErrorIcon: string read FFeedbackErrorIcon write SetFeedbackErrorIcon;
    property CustomValidations: string read FCustomValidations write SetCustomValidations;
    property PaternErrorMsg: string read FPaternErrorMsg write SetPaternErrorMsg;
    property RequiredErrorMsg: string read FRequiredErrorMsg write SetRequiredErrorMsg;
    property MatchErrorMsg: string read FMatchErrorMsg write SetMatchErrorMsg;
    property AllErrorMsg: string read FAllErrorMsg write SetAllErrorMsg;
    property Comps: TIWBSValidadorComps read FComps write SetComps;
  end; *)

implementation

uses
  System.SysUtils, System.TypInfo, Winapi.Windows, DWUtils, DW.VCL.CustomForm,
  DW.VCL.Frame, DWGlobal;

{ TIWBSValidator }

(*constructor TIWBSValidator.Create(AOwner: TComponent);
begin
  inherited;
  FComps              := CreateComps;
  FocusOnError        := True;
  FCustomValidations  := '';
  FFeedbackErrorIcon  := 'glyphicon-remove';
  FFeedbackSucessIcon := 'glyphicon-ok';
  FPaternErrorMsg     := 'The value does not match the accepted pattern.';
  FRequiredErrorMsg   := 'Fill this field.';
  FAllErrorMsg        := 'This field is not filled correctly';
  FMatchErrorMsg      := 'Whoops, these don''t match';


end;

function TIWBSValidator.CreateComps: TIWBSValidadorComps;
begin
  Result := TIWBSValidadorComps.Create(Self, TIWBSValidadorComp);
end;

procedure TIWBSValidator.RenderValidation(aInput:TIWBSCustomInput; aHTMLTag:TIWHTMLTag);
var
  I: Integer;
begin
  for I := 0 to FComps.Count -1 do
    begin
      if FComps[I].FControl = aInput then
        begin
          FComps[I].RenderValidationsForControl(aHTMLTag);
          Break;
        end;
    end;
end;

procedure TIWBSValidator.SetAllErrorMsg(const Value: string);
begin
  FAllErrorMsg := Value;
end;

procedure TIWBSValidator.SetComps(const Value: TIWBSValidadorComps);
begin
  FComps.Assign(Value);
end;

procedure TIWBSValidator.SetCustomValidations(const Value: string);
begin
  FCustomValidations := Value;
end;

procedure TIWBSValidator.SetFeedbackErrorIcon(const Value: string);
begin
  FFeedbackErrorIcon := Value;
end;

procedure TIWBSValidator.SetFeedbackSucessIcon(const Value: string);
begin
  FFeedbackSucessIcon := Value;
end;

procedure TIWBSValidator.SetFocusOnError(const Value: Boolean);
begin
  FFocusOnError := Value;
end;

procedure TIWBSValidator.SetMatchErrorMsg(const Value: string);
begin
  FMatchErrorMsg := Value;
end;

procedure TIWBSValidator.SetPaternErrorMsg(const Value: string);
begin
  FPaternErrorMsg := Value;
end;

procedure TIWBSValidator.SetRequiredErrorMsg(const Value: string);
begin
  FRequiredErrorMsg := Value;
end; *)

procedure TDWValidator.Validate(aForm: TDWCustomInputForm = nil);
var
  i:Integer;
  LValidator:TDWValidator;
begin
   if (csDesigning in ComponentState) or (csLoading in ComponentState) then
     Exit;
  if Assigned(aForm) then
    begin
      DWApplication.CallBackResp.AddScriptToExecute('$("#'+ aForm.HTMLName +'").validator(''validate'');', False);
    end
  else if(Owner is TDWCustomForm)
  or ((Owner is TDWFrame)) then
    begin
      for i := 0 to Owner.ComponentCount -1 do
        begin
          if (Owner.Components[I] is TDWCustomInputForm) then
            if TDWCustomInputForm(Owner.Components[I]).ValidationEnabled then
              DWApplication.CallBackResp.AddScriptToExecute(
                  '$("#'+ TDWCustomInputForm(Owner.Components[I]).HTMLName +'").validator(''validate'');'
                  , False);
        end;
    end
  else
    raise Exception.Create('IWBSValidator need to be Ownned for TIWForm or TFrame');
end;

{ TIWBSValidadorComp }

constructor TDWValidator.Create(AOwner: TComponent);
begin
  FValidations := TDWValidations.Create(Self, TDWValidation);
  inherited;
end;

(*function TIWBSValidadorComp.GetDisplayName: string;
begin
  if Assigned(FControl) then
    Result:= FControl.Name
  else
    inherited;
end; *)

procedure TDWValidator.RenderValidation(aHTMLTag:TDWElementTag);
var
 I:Integer;
begin
  if (csDesigning in ComponentState) or (csLoading in ComponentState) then
     Exit;
  for I := 0 to FValidations.Count -1 do
    begin
      FValidations[I].Rule.RenderRuleTag(aHTMLTag);
    end;
end;

(*procedure TIWBSValidadorComp.SetControl(const Value: TIWBSCustomInput);
begin
  FControl := Value;
end;   *)

procedure TDWValidator.SetValidations(const Value: TDWValidations);
begin
  FValidations.Assign(Value);
end;

(*procedure TIWBSValidator.Update(aForm: TIWBSCustomInputForm);
var
  i:Integer;
  LValidator:TIWBSValidator;
begin
  if Assigned(aForm) then
    begin
       IWBSExecuteAsyncJScript('$("#'+ aForm.HTMLName +'").validator(''update'');');
    end
  else if(Owner is TIWForm)
  or ((Owner is TFrame)) then
    begin
      for i := 0 to Owner.ComponentCount -1 do
        begin
          if (Owner.Components[I] is TIWBSCustomInputForm) then
            if TIWBSCustomInputForm(Owner.Components[I]).ValidationEnabled then
               IWBSExecuteAsyncJScript('$("#'+ TIWBSCustomInputForm(Owner.Components[I]).HTMLName +'").validator(''update'');');
        end;
    end
  else
    raise Exception.Create('IWBSValidator need to be Ownned for TIWForm or TFrame');

end;*)

{ TIWBSValidadorComps }

(*function TIWBSValidadorComps.Add: TIWBSValidadorComp;
begin
  Result := TIWBSValidadorComp(inherited Add);
end;

constructor TIWBSValidadorComps.Create(Validator: TIWBSValidator;
  ItemClass: TIWBSValidadorCompClass);
begin
  inherited Create(ItemClass);
  FValidator := Validator;
end;

function TIWBSValidadorComps.GetOwner: TPersistent;
begin
  Result := FValidator;
end;

function TIWBSValidadorComps.GetValComp(Index: Integer): TIWBSValidadorComp;
begin
  Result := TIWBSValidadorComp(inherited Items[Index]);
end;

procedure TIWBSValidadorComps.SetValComp(Index: Integer; const Value: TIWBSValidadorComp);
begin
  Items[Index].Assign(Value);
end;  *)

{ TIWBSValidations }

function TDWValidations.Add: TDWValidation;
begin
  Result := TDWValidation(inherited Add);
end;

constructor TDWValidations.Create(Comp: TDWValidator; ItemClass: TIWBSValidationClass);
begin
  inherited Create(ItemClass);
  FComp := Comp;
end;

function TDWValidations.GetOwner: TPersistent;
begin
  Result := FComp;
end;

function TDWValidations.GetValidation(Index: Integer): TDWValidation;
begin
  Result := TDWValidation(inherited Items[Index]);
end;

procedure TDWValidations.SetValidation(Index: Integer; const Value: TDWValidation);
begin
  Items[Index].Assign(Value);
end;

{ TIWBSValidation }

constructor TDWValidation.Create(Collection: TCollection);
begin
  inherited;
  FRuleType  := tbsvalRequired;
  FRuleClass := TDWValidationHack;
  FRule      := TDWValidationHack.Create;
  FRule.FValidation:=Self;
  FErrorMsg:= '';
  FShowErrorMsg:= False;
end;

function TDWValidation.GetDisplayName: string;
begin
  case FRuleType of
    tbsvalRequired: Result:= 'Required';
    tbsvalEmail:  Result:= 'Email';
    tbsvalUrl:  Result:= 'URL';
    tbsvalNumber:  Result:= 'Number';
    tbsvalTel:  Result:= 'Fone';
    tbsvalDate:  Result:= 'Date';
  else
    inherited;
  end;
end;

function TDWValidation.GetRule: TDWValidationRule;
begin
  if FRule <> nil then
    if FRule.ClassType <> FRuleClass then
      FreeAndNil(FRule);
  if FRule = nil then
    FRule := FRuleClass.Create;
  Result  := (FRule as FRuleClass);
end;

function TDWValidation.GetRuleClass: TIWBSValidationRuleClass;
begin
  case FRuleType of
    tbsvalNumber:
      Result := TDWValidationRuleNumber;
  else
    Result := TDWValidationRule;
  end;
end;

procedure TDWValidation.SetErrorMsg(const Value: string);
begin
  FErrorMsg := Value;
end;

procedure TDWValidation.SetRule(const Value: TDWValidationRule);
begin
  if Value = nil then
    FRule := nil
  else { if Value.ClassType = FRuleClass then }
    begin
      FRule.Assign(Value);
      FRuleClass := TIWBSValidationRuleClass(Value.ClassType);
      FRule.FValidation:=Self;
    end;
end;

procedure TDWValidation.SetRuleType(const Value: TDWValRuleType);
begin
  if FRuleType <> Value then
    begin
      FRuleType  := Value;
      FRuleClass := GetRuleClass;
      if FRule = nil then
        FRule := FRuleClass.Create
      else if FRule.ClassType <> FRuleClass then
        begin
          FreeAndNil(FRule);
          FRule := FRuleClass.Create;
          try
            FindRootDesigner(Self).Notification(Self, opRemove);
            FindRootDesigner(Self).Notification(Self, opInsert);
            // FindRootDesigner(Self).Modified;
          except
            // To avoid DesignTime Errors
          end;
        end;
    end;
end;

procedure TDWValidation.SetShowErrorMsg(const Value: Boolean);
begin
  FShowErrorMsg := Value;
end;

{ TIWBSValidationRuleNumber }

constructor TDWValidationRuleNumber.Create;
begin
  inherited Create;
  FValidateRange:=False;
end;

procedure TDWValidationRuleNumber.RenderRuleTag(aHTMLTag: TDWElementTag);
begin
  if aHTMLTag.Params.Values['type'] <> 'number' then
    aHTMLTag.AddStringParam('type', 'number');
  if FValidateRange then
    begin
      if aHTMLTag.Params.Values['min'] <> IntToStr(FMin) then
        aHTMLTag.AddStringParam('min', IntToStr(FMin));
      if aHTMLTag.Params.Values['max'] <> IntToStr(FMax) then
        aHTMLTag.AddStringParam('max', IntToStr(FMax));
    end;
end;

procedure TDWValidationRuleNumber.SetMax(const Value: Integer);
begin
  FMax := Value;
end;

procedure TDWValidationRuleNumber.SetMin(const Value: Integer);
begin
  FMin := Value;
end;

procedure TDWValidationRuleNumber.SetValidateRange(const Value: Boolean);
begin
  FValidateRange := Value;
end;

{ TIWBSValidationHack }

procedure TDWValidationHack.SetMax(const Value: Integer);
begin
  FMax:= Value;
end;

procedure TDWValidationHack.SetMin(const Value: Integer);
begin
  FMin:=Value;
end;

procedure TDWValidationHack.SetValidateRange(const Value: Boolean);
begin
  FValidateRange:= Value;
end;

{ TIWBSValidationRule }

procedure TDWValidationRule.RenderRuleTag(aHTMLTag: TDWElementTag);
begin
  case FValidation.RuleType of
    tbsvalRequired: begin
                      if aHTMLTag.Params.IndexOfName('required') = -1 then
                        aHTMLTag.Add('required');
                      if FValidation.ShowErrorMsg then
                        if FValidation.ErrorMsg = '' then
                          aHTMLTag.AddStringParam('data-required-error', 'Please fill this field.')
                        else
                         aHTMLTag.AddStringParam('data-required-error', FValidation.ErrorMsg);
                    end;
    tbsvalEmail:    begin
                      if aHTMLTag.Params.Values['type'] <> 'email' then
                        aHTMLTag.AddStringParam('type', 'email');
                      if FValidation.ShowErrorMsg then
                        if FValidation.ErrorMsg = '' then
                          aHTMLTag.AddStringParam('data-error', 'This email is not valid.')
                        else
                         aHTMLTag.AddStringParam('data-error', FValidation.ErrorMsg);
                    end;
    tbsvalUrl:      begin
                      if aHTMLTag.Params.Values['type'] <> 'url' then
                        aHTMLTag.AddStringParam('type', 'url');
                      if FValidation.ShowErrorMsg then
                        if FValidation.ErrorMsg = '' then
                          aHTMLTag.AddStringParam('data-error', 'This url is not valid.')
                        else
                         aHTMLTag.AddStringParam('data-error', FValidation.ErrorMsg);
                    end;
    //tbsvalNumber: ;
    tbsvalTel:      begin
                      if aHTMLTag.Params.Values['type'] <> 'tel' then
                        aHTMLTag.AddStringParam('type', 'tel');
                      if FValidation.ShowErrorMsg then
                        if FValidation.ErrorMsg = '' then
                          aHTMLTag.AddStringParam('data-error', 'This fone is not valid.')
                        else
                         aHTMLTag.AddStringParam('data-error', FValidation.ErrorMsg);
                    end;
    tbsvalDate:     begin
                      if aHTMLTag.Params.Values['type'] <> 'date' then
                        aHTMLTag.AddStringParam('type', 'date');
                      if FValidation.ShowErrorMsg then
                        if FValidation.ErrorMsg = '' then
                          aHTMLTag.AddStringParam('data-error', 'This date is not valid.')
                        else
                         aHTMLTag.AddStringParam('data-error', FValidation.ErrorMsg);
                    end;
  end;
end;

initialization
 IWBSAddGlobalLinkFile('/<dwlibpath>/validator/validator.js');


end.
