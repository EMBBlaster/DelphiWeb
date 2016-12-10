unit DW.VCL.CustomInput;

interface

uses
  Classes, SysUtils, StrUtils, Controls, db, DW.VCL.DBControl, DWTypes,
  DW.VCL.Validator, DWElementTag;

type
  TDWCustomInput = class(TDWCustomDbControl)
  private
    FAutoEditable: Boolean;
    FAutoFocus: boolean;
    FDbEditable: boolean;
    FCaption: string;
    FInputType: TDWInputType;
    FReadOnly: Boolean;
    FRequired: Boolean;
    FValidator: TDWValidator;
    FNonEditableAsLabel: Boolean;
    procedure EditingChanged;
    function GetAsDateTime: TDateTime;
    function GetAsDouble: Double;
    function GetAsVariant: Variant;
    procedure SetAsDateTime(const Value: TDateTime);
    procedure SetAsDouble(const Value: Double);
    procedure SetValidator(const Value: TDWValidator);
    procedure SetNonEditableAsLabel(const Value: Boolean);
  protected
    FIsStatic: boolean;
    FSupportReadOnly: boolean;
    FText: TCaption;

    FOldText: string;

    function getText: TCaption; virtual;

    procedure CheckData; override;
    procedure SetCaption(const AValue: string);
    procedure SetReadOnly(const AValue:boolean);
    procedure SetRequired(const AValue:boolean);
    procedure SetValue(const AValue: string); override;

    function get_ShouldRenderTabOrder: boolean;virtual;

    procedure GetInputControlNames(ANames: TStringList);
    function IsForThisControl(AName: string): boolean;

    procedure InternalRenderHTML(var AHTMLTag: TDWElementTag); override;
    procedure InternalSetValue(const ASubmitValue: string; var ATextValue: string; var ASetFieldValue: boolean); virtual;

    function IsReadOnly: boolean; override;
    function IsDisabled: boolean; override;


    procedure InternalRenderCss(var ACss: string);  override;

    property ReadOnly: boolean read FReadOnly write SetReadOnly;
    property InputType: TDWInputType read FInputType write FInputType;
  public
    constructor Create(AOwner: TComponent); override;
     procedure SetText(const AValue: TCaption); virtual;
    procedure Invalidate; override;
    property Required: Boolean read FRequired write SetRequired default False;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsDouble: Double read GetAsDouble write SetAsDouble;
    property AsVariant: Variant read GetAsVariant;
  published
    property AutoEditable: Boolean read FAutoEditable write FAutoEditable default True;
    property AutoFocus: boolean read FAutoFocus write FAutoFocus default False;
    property Caption: string read FCaption write SetCaption;
    property Editable default True;
    property Enabled default True;
    { TODO 1 -oDELCIO -cIMPLEMENT :  ExtraTagParams}
    //property ExtraTagParams;
    { TODO 1 -oDELCIO -cIMPLEMENT :  FriendlyName}
    //property FriendlyName;
    property NonEditableAsLabel:Boolean read FNonEditableAsLabel write SetNonEditableAsLabel default False;
    property ScriptEvents;
    property ScriptInsideTag default False;
    { TODO 1 -oDELCIO -cIMPLEMENT :  SubmitOnAsyncEvent}
    //property SubmitOnAsyncEvent default True;
    property Text: TCaption read GetText write SetText;
    property Validator:TDWValidator read FValidator write SetValidator;
  end;

  TDWCustomTextInput = class(TDWCustomInput)
  private
    FPlaceHolder: string;
    FTextAlignment: TDWTextAlignment;
    FTextCase: TDWTextCase;
  protected
    procedure InternalRenderAsync(const AHTMLName: string); override;
    procedure InternalRenderCss(var ACss: string);  override;
  published
    constructor Create(AOwner: TComponent); override;
    property BSTextAlignment: TDWTextAlignment read FTextAlignment write FTextAlignment default bstaDefault;
    property BSTextCase: TDWTextCase read FTextCase write FTextCase default bstcDefault;
    property MaxLength default 0;
    property PlaceHolder: string read FPlaceHolder write FPlaceHolder;
    property ReadOnly default False;
  end;

  TDWCustomSelectInput = class(TDWCustomInput)
  private
    FItems: TStringList;
    FItemsHaveValues: boolean;
    procedure SetItems(AValue: TStringList);
    procedure SetItemsHaveValues(AValue: boolean);
  protected
    FItemIndex: integer;

    procedure InternalRenderCss(var ACss: string); override;
    procedure InternalSetValue(const ASubmitValue: string; var ATextValue: string; var ASetFieldValue: boolean); override;
    function FindValue(const AValue: string): integer;
    procedure Loaded; override;
    procedure OnItemsChange(ASender : TObject); virtual;
    procedure SetItemIndex(AValue: integer); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetText(const AValue: TCaption); override;
  published
    property ItemIndex: integer read FItemIndex write SetItemIndex default -1;
    property Items: TStringList read FItems write SetItems;
    property ItemsHaveValues: boolean read FItemsHaveValues write SetItemsHaveValues default False;
  end;

implementation

uses
  System.Variants, DWUtils, DW.VCL.InputForm, DW.VCL.Common;

var
  LFormatSettings: TFormatSettings;

{$region 'TIWBSCustomInput'}
constructor TDWCustomInput.Create(AOwner: TComponent);
begin
  inherited;
  FValidator:= nil;
  FAutoEditable := True;
  FAutoFocus := False;
  FCaption := '';
  FInputType := bsitText;
  FNonEditableAsLabel := False;
  FReadOnly := False;
  FRequired := False;

  //FNeedsFormTag := True;
  FIsStatic := False;
  FSupportReadOnly := False;

  ScriptInsideTag := False;
  Height := 25;
  Width := 121;
end;

procedure TDWCustomInput.Invalidate;
begin
  inherited;
  if (Validator <> nil) and  IsDesignMode then //to force update validator for this field whith no focus on first error field
    //IWBSExecuteAsyncJScript('$("#' + HTMLName + '").trigger(''change.bs.validator'');', False,False);
   // IWBSExecuteAsyncJScript('$(''form'').validator(''validate'')',False, false);
   DWApplication.CallBackResp.AddScriptToExecute('var Validt = $("form").data(''bs.validator''); ' +
                           'if (Validt) {' +
                          'Validt.validateInput($("#'+ HTMLName + '"));};', False);
end;

function TDWCustomInput.GetAsDateTime: TDateTime;
begin
  if FInputType in [bsitDateTimeLocal, bsitDate, bsitMonth, bsitTime] then
    if FText = '' then
      Result := 0
    else
      Result := StrToDateTime(ReplaceStr(FText,'T',' '), LFormatSettings)
  else
    raise Exception.Create('Invalid InputType');
end;

procedure TDWCustomInput.SetAsDateTime(const Value: TDateTime);
begin
  if Value = 0 then
    FText := ''
  else if FInputType = bsitDateTimeLocal then
    FText := FormatDateTime('yyyy-mm-dd"T"hh:nn', Value)
  else if FInputType = bsitDate then
    FText := FormatDateTime('yyyy-mm-dd', Value)
  else if FInputType = bsitTime then
    FText := FormatDateTime('hh:nn', Value)
  else
    FText := DateTimeToStr(Value, LFormatSettings);
end;

function TDWCustomInput.GetAsDouble: Double;
begin
  if FInputType = bsitNumber then
    if FText = '' then
      Result := 0
    else
      Result := StrToFloat(FText, LFormatSettings)
  else
    raise Exception.Create('Invalid InputType');
end;

procedure TDWCustomInput.SetAsDouble(const Value: Double);
begin
  if FInputType = bsitNumber then
    FText := FloatToStr(Value, LFormatSettings)
  else
    raise Exception.Create('Invalid InputType');
end;

function TDWCustomInput.GetAsVariant: Variant;
begin
  if FInputType = bsitNumber then
    Result := GetAsDouble
  else if FInputType in [bsitDateTimeLocal, bsitDate, bsitMonth, bsitTime] then
    Result := GetAsDateTime
  else
    Result := FText;
end;

procedure TDWCustomInput.GetInputControlNames(ANames: TStringList);
begin
  ANames.Text := HTMLName+InputSuffix;
end;

function TDWCustomInput.IsForThisControl(AName: string): boolean;
begin
  Result := SameText(HTMLName+InputSuffix, AName);
end;

function TDWCustomInput.GetText: TCaption;
begin
  Result := FText;
end;

procedure TDWCustomInput.SetText(const AValue: TCaption);
begin
  if AValue = '' then
    FText := AValue
  else if FInputType in [bsitDateTimeLocal, bsitDate, bsitTime] then
    SetAsDateTime(StrToDateTime(AValue, LFormatSettings))
  else if FInputType in [bsitNumber] then
    SetAsDouble(StrToFloat(AValue, LFormatSettings))
  else
    FText := AValue;
  invalidate;
end;

procedure TDWCustomInput.CheckData;
var
  LField: TField;
begin
  if DataSource <> nil then
    begin
      if CheckDataSource(DataSource, DataField, LField) then
        begin
          if AutoEditable then
            FDbEditable := (DataSource.Dataset.State in dsEditModes)  and (LField.CanModify) ;
          if Assigned(LField.OnGetText) then
            Text := LField.Text
          else if (FInputType = bsitNumber) and (LField.DataType in [ftFloat, ftCurrency, ftBCD, ftFMTBCD, ftExtended]) then
            Text := FloatToStr(LField.AsExtended, LFormatSettings)

          // aca agregar todos los tipos fecha que hay
          else if (FInputType = bsitDate) and (LField.DataType in [ftDate, ftTime, ftDateTime, ftTimeStamp, ftOraTimeStamp]) then
            Text := FormatDateTime('yyyy-mm-dd',LField.AsDateTime)
          else if (FInputType = bsitDateTimeLocal) and (LField.DataType in [ftDate, ftTime, ftDateTime, ftTimeStamp, ftOraTimeStamp]) then
            Text := FormatDateTime('yyyy-mm-dd"T"hh:nn',LField.AsDateTime)
          else
            Text := LField.AsString;
        end
      else
        begin
          Text := '';
          if AutoEditable then
            FDbEditable := True;
        end;
    end
  else
    FDbEditable := true;
end;

procedure TDWCustomInput.SetValidator(const Value: TDWValidator);
var
  LInputForm:TDWCustomInputForm;
begin
  if FValidator <> Value then
    begin
      FValidator := Value;
        if not IsDesignMode then  //AV in DesignMode
          begin
            LInputForm:= DWFindParentInputForm(Self);
            if LInputForm <> nil then
              if LInputForm.ValidationEnabled then
                DWApplication.CallBackResp.AddScriptToExecute('$("#' + LInputForm.HTMLName + '").validator(''update'');',False);
          end;
    end;
end;
procedure TDWCustomInput.SetValue(const AValue: string);
var
  LField: TField;
  LText: string;
  LSave: boolean;
begin
  { TODO 1 -oDELCIO -cVERIFY : what is it? }
  {if RequiresUpdateNotification(Parent) then
    UpdateNotifiedInterface(Parent).NotifyUpdate(Self,AValue);}
  LSave := True;
  InternalSetValue(AValue, LText, LSave);
  if (FOldText <> LText) or (FText <> LText) then begin
    FOldText := LText;
    FText := LText;
    try
      if CheckDataSource(DataSource, DataField, LField) and LSave then
        if (DataSource.DataSet.State in dsEditModes) and LField.CanModify then
          begin
            if Assigned(LField.OnSetText) then
              LField.Text := LText
            else
              if FInputType = bsitNumber then
                LField.AsFloat := StrToFloat(LText, LFormatSettings)
              else if FInputType = bsitDate then
                LField.AsDateTime := StrToDate(LText, LFormatSettings)
              else if FInputType = bsitDateTimeLocal then  // agregar todos los tipos fecha que hay
                LField.AsDateTime := StrToDateTime(ReplaceStr(LText,'T',' '), LFormatSettings)
              else
                LField.AsString := LText;
          end
        else
          raise Exception.Create('DataSource is Not inEdit Mode: ' + DataSource.Name + ', control: ' + Name);
    finally
      Invalidate;
      CheckData;
    end;
  end;
end;

procedure TDWCustomInput.EditingChanged;
begin
  Invalidate;
end;

function TDWCustomInput.get_ShouldRenderTabOrder: boolean;
begin
  result := Editable or (NonEditableAsLabel = false);
end;

procedure TDWCustomInput.InternalSetValue(const ASubmitValue: string; var ATextValue: string; var ASetFieldValue: boolean);
begin
  ATextValue := ASubmitValue;
end;

function TDWCustomInput.IsReadOnly: boolean;
begin
  Result := FSupportReadOnly and (FReadOnly or not FDbEditable);
end;

function TDWCustomInput.IsDisabled: boolean;
begin
  Result := not (Enabled and Editable and (FDbEditable or FSupportReadOnly));
end;

procedure TDWCustomInput.InternalRenderCss(var ACss: string);
begin
  inherited;

end;

procedure TDWCustomInput.InternalRenderHTML(var AHTMLTag: TDWElementTag);
begin
  inherited;
  FOldText := FText;
end;

procedure TDWCustomInput.SetCaption(const AValue: string);
begin
  FCaption := AValue;
  Invalidate;
end;

procedure TDWCustomInput.SetNonEditableAsLabel(const Value: Boolean);
begin
  FNonEditableAsLabel := Value;
end;

procedure TDWCustomInput.SetReadOnly(const AValue:boolean);
begin
  if FReadOnly <> AValue then
  begin
    FReadOnly := AValue;
    Invalidate;
  end;
end;

procedure TDWCustomInput.SetRequired(const AValue:boolean);
begin
  if FRequired <> AValue then begin
    FRequired := AValue;
    Invalidate;
  end;
end;
{$endregion}

{$region 'TIWBSCustomTextInput'}
constructor TDWCustomTextInput.Create(AOwner: TComponent);
begin
  inherited;
  FSupportReadOnly := True;
  FTextAlignment := bstaDefault;
  FTextCase := bstcDefault;
end;

procedure TDWCustomTextInput.InternalRenderAsync(const AHTMLName: string);
begin
  inherited;
  if FIsStatic then
    TDWBSCommon.SetAsyncHtml(AHTMLName, FText, FOldText)
  else
    TDWBSCommon.SetAsyncText(AHTMLName, FText, FOldText);
end;

procedure TDWCustomTextInput.InternalRenderCss(var ACss: string);
begin
  inherited;
  FIsStatic := not Editable and NonEditableAsLabel;
  if FIsStatic then
    TDWBSCommon.AddCssClass(ACss, 'form-control-static')
  else
    TDWBSCommon.AddCssClass(ACss, 'form-control');
  if FTextAlignment <> bstaDefault then
    TDWBSCommon.AddCssClass(ACss, aDWTextAlignment[FTextAlignment]);
  if FTextCase <> bstcDefault then
    TDWBSCommon.AddCssClass(ACss, aDWTextCase[FTextCase]);
end;
{$endregion}

{$region 'TIWBSCustomSelectInput'}
constructor TDWCustomSelectInput.Create(AOwner: TComponent);
begin
  inherited;
  FItemIndex := -1;
  FItems := TStringList.Create;
  FItems.OnChange := OnItemsChange;
  FItemsHaveValues := False;
  FSupportReadOnly := False;
end;

destructor TDWCustomSelectInput.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

procedure TDWCustomSelectInput.OnItemsChange(ASender : TObject);
begin
  AsyncRefreshControl;
end;

procedure TDWCustomSelectInput.Loaded;
begin
  SetItemIndex(FItemIndex);
end;

procedure TDWCustomSelectInput.SetItemIndex(AValue: integer);
begin
  if csReading in ComponentState then
    FItemIndex := AValue
  else
    begin
      if (AValue >= -1) and (AValue < FItems.Count) then
        begin
          FItemIndex := AValue;
          if FItemIndex >= 0 then
            if FItemsHaveValues then
              FText := FItems.ValueFromIndex[AValue]
            else
              FText := FItems[AValue]
          else
            FText := '';
        end
      else
        begin
          FItemIndex := -1;
          FText := ''
        end;
      Invalidate;
    end;
end;

procedure TDWCustomSelectInput.SetItems(AValue: TStringList);
begin
  FItems.Assign(AValue);
end;

procedure TDWCustomSelectInput.SetItemsHaveValues(AValue: boolean);
begin
  FItemsHaveValues := AValue;
  Invalidate;
end;

function TDWCustomSelectInput.FindValue(const AValue: string): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to FItems.Count-1 do
    if AnsiSameStr(IfThen(FItemsHaveValues, FItems.ValueFromIndex[i], FItems[i]), AValue) then begin
      Result := i;
      Break;
    end;
end;

procedure TDWCustomSelectInput.SetText(const AValue: TCaption);
begin
  inherited;
  FItemIndex := FindValue(FText);
end;

procedure TDWCustomSelectInput.InternalRenderCss(var ACss: string);
begin
  TDWBSCommon.AddCssClass(ACss, 'form-control');
  inherited;
end;

procedure TDWCustomSelectInput.InternalSetValue(const ASubmitValue: string; var ATextValue: string; var ASetFieldValue: boolean);
var
  i: integer;
begin
  if TryStrToInt(ASubmitValue, i) and (i >= 0) and (i < Items.Count) then
    begin
      if ItemsHaveValues then
        ATextValue := Items.ValueFromIndex[i]
      else
        ATextValue := Items[i];
      FItemIndex := i;
    end
  else
    begin
      ATextValue := '';
      FItemIndex := -1;
    end;
end;
{$endregion}

initialization
  LFormatSettings := TFormatSettings.Create('en-US'); // locale de us
  LFormatSettings.DateSeparator := '-';
  LFormatSettings.LongDateFormat := 'yyyy-mm-dd';
  LFormatSettings.ShortDateFormat := LFormatSettings.LongDateFormat;
  LFormatSettings.LongTimeFormat := 'hh:nn:ss';
  LFormatSettings.ShortTimeFormat := LFormatSettings.LongTimeFormat;

end.
