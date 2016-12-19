unit DW.VCL.Input;

interface

uses
  SysUtils, Classes, db, StrUtils, Controls, DW.VCL.CustomInput, DWElementTag,
  DWTypes;

type

  TDWInput = class(TDWCustomTextInput)
  private
    FMask: string;
    FAutoComplete: Boolean;
    FMaskSave: Boolean;
    procedure SetMask(const Value: string);
    procedure SetAutoComplete(const Value: Boolean);
    procedure SetMaskSave(const Value: Boolean);
  protected
    procedure InternalRenderHTML(var AHTMLTag: TDWElementTag); override;
    procedure InternalSetValue(const ASubmitValue: string; var ATextValue: string;
      var ASetFieldValue: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property InputType default bsitText;
    property Mask: string read FMask write SetMask;
    property MaskSave: Boolean read FMaskSave write SetMaskSave default False;
    // FAutocomplete for websites it is good, but for applications it not intended
    property AutoComplete: Boolean read FAutoComplete write SetAutoComplete default False;
  end;

  TDWMemo = class(TDWCustomTextInput)
  private
    FLines: TStringList;
    FResizeDirection: TDWResizeDirection;
    FRows: integer;
    FVertScrollBar: Boolean;
    procedure OnLinesChange(ASender: TObject);
    procedure SetLines(const AValue: TStringList);
  protected
    procedure InternalSetValue(const ASubmitValue: string; var ATextValue: string;
      var ASetFieldValue: Boolean); override;
    procedure InternalRenderHTML(var AHTMLTag: TDWElementTag); override;
    procedure InternalRenderStyle(AStyle: TStringList); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetText(const AValue: TCaption); override;
  published
    property Lines: TStringList read FLines write SetLines;
    property ResizeDirection: TDWResizeDirection read FResizeDirection write FResizeDirection
      default bsrdDefault;
    property Rows: integer read FRows write FRows default 5;
    property VertScrollBar: Boolean read FVertScrollBar write FVertScrollBar default True;
  end;

  TDWCheckBox = class(TDWCustomInput)
  private
    FChecked: Boolean;
    FValueChecked: string;
    FValueUnchecked: string;
  protected
    procedure InternalSetValue(const ASubmitValue: string; var ATextValue: string;
      var ASetFieldValue: Boolean); override;
    procedure InternalRenderAsync(const AHTMLName: string); override;
    procedure InternalRenderHTML(var AHTMLTag: TDWElementTag); override;
    procedure SetChecked(AValue: Boolean);
    procedure SetName(const AValue: TComponentName); override;
  public
    procedure SetText(const AValue: TCaption); override;
  published
    constructor Create(AOwner: TComponent); override;
    property Checked: Boolean read FChecked write SetChecked default False;
    property ValueChecked: string read FValueChecked write FValueChecked;
    property ValueUnchecked: string read FValueUnchecked write FValueUnchecked;
  end;

  TDWRadioButton = class(TDWCustomInput)
  private
    FChecked: Boolean;
    FGroup: string;
    FSaveUnchecked: Boolean;
    FValueChecked: string;
    FValueUnchecked: string;
  protected
    procedure InternalSetValue(const ASubmitValue: string; var ATextValue: string;
      var ASetFieldValue: Boolean); override;
    procedure InternalRenderAsync(const AHTMLName: string); override;
    procedure InternalRenderHTML(var AHTMLTag: TDWElementTag); override;
    procedure SetChecked(AValue: Boolean);
    procedure SetName(const AValue: TComponentName); override;
    function InputSuffix: string; override;
  public
    procedure SetText(const AValue: TCaption); override;
  published
    constructor Create(AOwner: TComponent); override;
    property Checked: Boolean read FChecked write SetChecked default False;
    property SaveUnchecked: Boolean read FSaveUnchecked write FSaveUnchecked default True;
    property Group: string read FGroup write FGroup;
    property ValueChecked: string read FValueChecked write FValueChecked;
    property ValueUnchecked: string read FValueUnchecked write FValueUnchecked;
  end;

  TDWSelect = class(TDWCustomSelectInput)
  private
    FItemsSelected: array of Boolean;
    FMultiSelect: Boolean;
    FSize: integer;
    procedure ResetItemsSelected;
    procedure SetSize(AValue: integer);
  protected
    procedure InternalSetValue(const ASubmitValue: string; var ATextValue: string;
      var ASetFieldValue: Boolean); override;
    procedure InternalRenderAsync(const AHTMLName: string); override;
    procedure InternalRenderHTML(var AHTMLTag: TDWElementTag); override;
    procedure OnItemsChange(ASender: TObject); override;
    procedure SetItemIndex(AValue: integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetText(const AValue: TCaption); override;
  published
    property MultiSelect: Boolean read FMultiSelect write FMultiSelect default False;
    property Size: integer read FSize write SetSize default 1;
  end;

  TDWRadioGroup = class(TDWCustomSelectInput)
  protected
    procedure InternalRenderAsync(const AHTMLName: string); override;
    procedure InternalRenderHTML(var AHTMLTag: TDWElementTag); override;
    function InputSelector: string; override;
    function InputSuffix: string; override;
  end;

implementation

uses
  DWUtils, DWMarkupLinguageTag, DW.VCL.Common, DWGlobal, DW.VCL.InputForm;

var
  LFormatSettings: TFormatSettings;

{$REGION 'TIWBSInput'}

constructor TDWInput.Create(AOwner: TComponent);
begin
  inherited;
  FAutoComplete := False;
  FMask         := '';
end;

procedure TDWInput.InternalRenderHTML(var AHTMLTag: TDWElementTag);
var
  MaskTag: TDWElementTag;
  FakeAutocomp: TDWElementTag;
begin
  inherited;
  if FIsStatic then
    begin
      AHTMLTag := TDWElementTag.CreateHTMLTag('p', nil, ctdwFalse);
      try
        AHTMLTag.AddClassParam(ActiveCss);
        AHTMLTag.AddStringParam('id', HTMLName);
        AHTMLTag.AddStringParam('style', ActiveStyle);
        AHTMLTag.Contents.AddText(TDWBSCommon.TextToHTML(FText));
      except
        FreeAndNil(AHTMLTag);
        raise;
      end;
    end
  else
    begin
      AHTMLTag := TDWElementTag.CreateHTMLTag('input');
      try
        AHTMLTag.AddClassParam(ActiveCss);
        AHTMLTag.AddStringParam('id', HTMLName);
        AHTMLTag.AddStringParam('name', HTMLName);
        AHTMLTag.AddStringParam('type', aDWInputType[InputType]);
        if ShowHint and (Hint <> '') then
          AHTMLTag.AddStringParam('title', Hint);
        if AutoFocus then
          AHTMLTag.Add('autofocus');
        if IsReadOnly then
          AHTMLTag.Add('readonly');
        if IsDisabled then
          AHTMLTag.Add('disabled');
        if MaxLength > 0 then
          AHTMLTag.AddIntegerParam('maxlength', MaxLength);
        AHTMLTag.AddStringParam('value', TDWBSCommon.TextToHTML(FText));
        if Required then
          AHTMLTag.Add('required');
        if PlaceHolder <> '' then
          AHTMLTag.AddStringParam('placeholder', TDWBSCommon.TextToHTML(PlaceHolder));
        if TabIndex <> 0 then
          AHTMLTag.AddStringParam('tabindex', IntToStr(TabIndex));
        AHTMLTag.AddStringParam('autocomplete', IfThen(FAutoComplete, 'on', 'off'));
        if (Validator <> nil) and (not(csDesigning in ComponentState)) then
          Validator.RenderValidation(AHTMLTag);
        AHTMLTag.AddStringParam('style', ActiveStyle);
      except
        FreeAndNil(AHTMLTag);
        raise;
      end;
    end;
  if FMask <> '' then
    begin
      MaskTag := TDWElementTag.CreateHTMLTag('script');
      MaskTag.Contents.AddText('$("#' + HTMLName + '").mask("' + FMask + '",{placeholder:" "});');
      AHTMLTag.Contents.AddElemetAsObject(MaskTag);
    end;

  if not(Parent is TDWInputGroup) and (InputType <> bsitHidden) then
    begin
      AHTMLTag := DWCreateInputFormGroup(Self, Parent, AHTMLTag, Caption, HTMLName);
      // browsers not respect autocomplete "off" in password inputs,
      // to solve this, put another input password hidden,
      // it elude browser with password change form
      if (InputType = bsitPassword) and (not FAutoComplete) then
        begin
          FakeAutocomp := TDWElementTag.CreateHTMLTag('input');
          FakeAutocomp.AddStringParam('style',
            'visibility: hidden; height:0; margin:0; border:0; padding:0;display:block;');
          FakeAutocomp.AddStringParam('type', 'password');
          if Caption <> '' then
            AHTMLTag.Contents.Insert(1, FakeAutocomp)
          else
            AHTMLTag.Contents.Insert(0, FakeAutocomp);
        end;
    end;
end;

procedure TDWInput.InternalSetValue(const ASubmitValue: string; var ATextValue: string;
  var ASetFieldValue: Boolean);
Var
  I: integer;
  LenValue: integer;
begin
  if (FMask = '') or FMaskSave then
    ATextValue := ASubmitValue
  else
    begin
      ATextValue := '';
      LenValue   := Length(ASubmitValue);
      For I      := 1 to LenValue do
        begin
          if (CharIsAlphaNum(ASubmitValue[I])) or (Pos(ASubmitValue[I], FMask) = 0) then
            ATextValue := ATextValue + ASubmitValue[I];
        end;
    end;
end;

procedure TDWInput.SetAutoComplete(const Value: Boolean);
begin
  FAutoComplete := Value;
end;

procedure TDWInput.SetMask(const Value: string);
begin
  if FMask <> Value then
    begin
      FMask := Value;
    end;
  AsyncRefreshControl;
end;

procedure TDWInput.SetMaskSave(const Value: Boolean);
begin
  FMaskSave := Value;
end;

{$ENDREGION}
{$REGION 'TIWBSMemo'}

constructor TDWMemo.Create(AOwner: TComponent);
begin
  inherited;
  FLines           := TStringList.Create;
  FLines.OnChange  := OnLinesChange;
  FResizeDirection := bsrdDefault;
  FRows            := 5;
  FVertScrollBar   := True;
  Height           := 101;
  Width            := 121;
end;

destructor TDWMemo.Destroy;
begin
  FreeAndNil(FLines);
  inherited;
end;

procedure TDWMemo.OnLinesChange(ASender: TObject);
begin
  FText := FLines.Text;
  Invalidate;
  if Script.Count > 0 then
    AsyncRefreshControl;
end;

procedure TDWMemo.SetLines(const AValue: TStringList);
begin
  FLines.Assign(AValue);
end;

procedure TDWMemo.SetText(const AValue: TCaption);
begin
  inherited;
  FLines.Text := FText;
  FText       := FLines.Text; // this autoadjust linebreaks
end;

procedure TDWMemo.InternalSetValue(const ASubmitValue: string; var ATextValue: string;
  var ASetFieldValue: Boolean);
begin
  FLines.Text := ASubmitValue;
  ATextValue  := FLines.Text;
end;

procedure TDWMemo.InternalRenderHTML(var AHTMLTag: TDWElementTag);
begin
  inherited;
  AHTMLTag := TDWElementTag.CreateHTMLTag('textarea');
  try
    AHTMLTag.AddStringParam('id', HTMLName);
    AHTMLTag.AddClassParam(ActiveCss);
    AHTMLTag.AddStringParam('name', HTMLName);
    if ShowHint and (Hint <> '') then
      AHTMLTag.AddStringParam('title', Hint);
    if AutoFocus then
      AHTMLTag.Add('autofocus');
    if IsReadOnly then
      AHTMLTag.Add('readonly');
    if IsDisabled then
      AHTMLTag.Add('disabled');
    if MaxLength > 0 then
      AHTMLTag.AddIntegerParam('maxlength', MaxLength);
    if Required then
      AHTMLTag.Add('required');
    if PlaceHolder <> '' then
      AHTMLTag.AddStringParam('placeholder', TDWBSCommon.TextToHTML(PlaceHolder));
    AHTMLTag.AddIntegerParam('rows', FRows);
    if TabIndex <> 0 then
      AHTMLTag.AddStringParam('tabindex', IntToStr(TabIndex));
    if (Validator <> nil) and (not(csDesigning in ComponentState)) then
      Validator.RenderValidation(AHTMLTag);
    AHTMLTag.AddStringParam('style', ActiveStyle);
    AHTMLTag.Contents.AddText(TDWBSCommon.TextToHTML(FText, False, False));
  except
    FreeAndNil(AHTMLTag);
    raise;
  end;

  if not(Parent is TDWInputGroup) then
    AHTMLTag := DWCreateInputFormGroup(Self, Parent, AHTMLTag, Caption, HTMLName);
end;

procedure TDWMemo.InternalRenderStyle(AStyle: TStringList);
begin
  if not FVertScrollBar then
    AStyle.Values['overflow'] := 'hidden';
  if FResizeDirection <> bsrdDefault then
    AStyle.Values['resize'] := aDWResizeDirection[FResizeDirection];
end;
{$ENDREGION}
{$REGION 'TIWBSCheckBox'}

constructor TDWCheckBox.Create(AOwner: TComponent);
begin
  inherited;
  FChecked        := False;
  FValueChecked   := 'true';
  FValueUnchecked := 'false';
  FText           := FValueUnchecked;
end;

procedure TDWCheckBox.SetName(const AValue: TComponentName);
begin
  if Caption = Name then
    Caption := AValue;
  inherited;
end;

procedure TDWCheckBox.SetChecked(AValue: Boolean);
begin
  FChecked := AValue;
  if AValue then
    FText := FValueChecked
  else
    FText := FValueUnchecked;
  Invalidate;
end;

procedure TDWCheckBox.SetText(const AValue: TCaption);
begin
  inherited;
  FChecked := FText = FValueChecked;
end;

procedure TDWCheckBox.InternalSetValue(const ASubmitValue: string; var ATextValue: string;
  var ASetFieldValue: Boolean);
begin
  FChecked := ASubmitValue = 'on';
  if FChecked then
    ATextValue := FValueChecked
  else
    ATextValue := FValueUnchecked;
end;

procedure TDWCheckBox.InternalRenderAsync(const AHTMLName: string);
begin
  inherited;
  if FText <> FOldText then
    begin
      DWApplication.CallBackResp.AddScriptToExecuteFirst('$("#' + HTMLName + '").prop("checked", ' +
        iif(Checked, 'true', 'false') + ');', False);
      FOldText := FText;
    end;
end;

procedure TDWCheckBox.InternalRenderHTML(var AHTMLTag: TDWElementTag);
begin
  inherited;
  AHTMLTag := TDWElementTag.CreateHTMLTag('input');
  try
    AHTMLTag.AddStringParam('id', HTMLName);
    AHTMLTag.AddStringParam('name', HTMLName);
    AHTMLTag.AddClassParam(ActiveCss);
    AHTMLTag.AddStringParam('type', 'checkbox');
    if IsDisabled then
      AHTMLTag.Add('disabled');
    if Checked then
      AHTMLTag.Add('checked');
    if TabIndex <> 0 then
      AHTMLTag.AddStringParam('tabindex', IntToStr(TabIndex));
    if (Validator <> nil) and (not(csDesigning in ComponentState)) then
      Validator.RenderValidation(AHTMLTag);
    AHTMLTag.AddStringParam('style', ActiveStyle);
  except
    FreeAndNil(AHTMLTag);
    raise;
  end;

  if Parent is TDWInputGroup then
    AHTMLTag := DWCreateInputGroupAddOn(AHTMLTag, HTMLName, 'addon')
  else
    AHTMLTag := DWCreateCheckBoxFormGroup(Parent, AHTMLTag, 'checkbox', Caption, Hint, HTMLName,
      ShowHint);
end;
{$ENDREGION}
{$REGION 'TIWBSRadioButton'}

constructor TDWRadioButton.Create(AOwner: TComponent);
begin
  inherited;
  FChecked        := False;
  FGroup          := 'group';
  FSaveUnchecked  := True;
  FValueChecked   := 'true';
  FValueUnchecked := 'false';
  FText           := FValueUnchecked;
end;

function TDWRadioButton.InputSuffix: string;
begin
  Result := '_INPUT';
end;

procedure TDWRadioButton.SetName(const AValue: TComponentName);
begin
  if Caption = Name then
    Caption := AValue;
  inherited;
end;

procedure TDWRadioButton.SetChecked(AValue: Boolean);
begin
  FChecked := AValue;
  if AValue then
    FText := FValueChecked
  else
    FText := FValueUnchecked;
  Invalidate;
end;

procedure TDWRadioButton.SetText(const AValue: TCaption);
begin
  inherited;
  FChecked := FText = FValueChecked;
end;

procedure TDWRadioButton.InternalSetValue(const ASubmitValue: string; var ATextValue: string;
  var ASetFieldValue: Boolean);
begin
  if ASubmitValue = 'on' then
    ATextValue := FValueChecked
  else
    begin
      ATextValue     := FValueUnchecked;
      ASetFieldValue := FSaveUnchecked;
    end;
end;

procedure TDWRadioButton.InternalRenderAsync(const AHTMLName: string);
begin
  inherited;
  if FText <> FOldText then
    begin
      DWApplication.CallBackResp.AddScriptToExecuteFirst('$("#' + HTMLName + InputSuffix +
        '").prop("checked", ' + iif(Checked, 'true', 'false') + ');', False);
      FOldText := FText;
    end;
end;

procedure TDWRadioButton.InternalRenderHTML(var AHTMLTag: TDWElementTag);
begin
  inherited;
  AHTMLTag := TDWElementTag.CreateHTMLTag('input');
  try
    AHTMLTag.AddStringParam('id', HTMLName + InputSuffix);
    AHTMLTag.AddStringParam('name', FGroup);
    AHTMLTag.AddClassParam(ActiveCss);
    AHTMLTag.AddStringParam('type', 'radio');
    if IsDisabled then
      AHTMLTag.Add('disabled');
    if FChecked then
      AHTMLTag.Add('checked');
    AHTMLTag.AddStringParam('onclick', 'radioButtonClick(event, ''' + FGroup + ''',''' + HTMLName +
      InputSuffix + ''');');
    AHTMLTag.AddStringParam('value', 'on');
    if TabIndex <> 0 then
      AHTMLTag.AddStringParam('tabindex', IntToStr(TabIndex));
    if (Validator <> nil) and (not(csDesigning in ComponentState)) then
      Validator.RenderValidation(AHTMLTag);
    AHTMLTag.AddStringParam('style', ActiveStyle);
  except
    FreeAndNil(AHTMLTag);
    raise;
  end;

  if Parent is TDWInputGroup then
    AHTMLTag := DWCreateInputGroupAddOn(AHTMLTag, HTMLName, 'addon')
  else
    AHTMLTag := DWCreateCheckBoxFormGroup(Parent, AHTMLTag, 'radio', Caption, Hint, HTMLName,
      ShowHint);
end;
{$ENDREGION}
{$REGION 'TIWBSSelect'}

constructor TDWSelect.Create(AOwner: TComponent);
begin
  inherited;
  FMultiSelect := False;
  FSize        := 1;
end;

procedure TDWSelect.OnItemsChange(ASender: TObject);
begin
  inherited;
  SetLength(FItemsSelected, Items.Count);
  ResetItemsSelected;
  AsyncRefreshControl;
end;

procedure TDWSelect.SetItemIndex(AValue: integer);
begin
  if not FMultiSelect and (AValue < 0) and (Items.Count > 0) then
    AValue := 0;
  inherited;
  ResetItemsSelected;
end;

procedure TDWSelect.ResetItemsSelected;
var
  I: integer;
begin
  for I               := 0 to Length(FItemsSelected) - 1 do
    FItemsSelected[I] := False;
  if (FItemIndex >= 0) and (FItemIndex < Length(FItemsSelected)) then
    FItemsSelected[FItemIndex] := True;
end;

procedure TDWSelect.SetSize(AValue: integer);
begin
  FSize := AValue;
  Invalidate;
end;

procedure TDWSelect.SetText(const AValue: TCaption);
var
  LSelectedVal: TStringList;
  I, j: integer;
begin
  FText := AValue;
  if FMultiSelect and AnsiContainsStr(FText, ',') then
    begin
      ResetItemsSelected;
      LSelectedVal := TStringList.Create;
      try
        LSelectedVal.StrictDelimiter := True;
        LSelectedVal.CommaText       := FText;
        for I                        := 0 to LSelectedVal.Count - 1 do
          for j                      := 0 to Items.Count - 1 do
            if AnsiSameStr(IfThen(ItemsHaveValues, Items.ValueFromIndex[j], Items[j]),
              LSelectedVal[I]) then
              FItemsSelected[j] := True;
      finally
        LSelectedVal.Free;
      end;
    end
  else
    begin
      FItemIndex := FindValue(FText);
      if not FMultiSelect and (FItemIndex < 0) and (Items.Count > 0) then
        begin
          FItemIndex := 0;
          FText := IfThen(ItemsHaveValues, Items.ValueFromIndex[FItemIndex], Items[FItemIndex]);
        end;
      ResetItemsSelected;
    end;
  Invalidate;
end;

procedure TDWSelect.InternalSetValue(const ASubmitValue: string; var ATextValue: string;
  var ASetFieldValue: Boolean);
var
  LSelectedIdx, LSelectedVal: TStringList;
  I, v: integer;
begin
  if FMultiSelect and AnsiContainsStr(ASubmitValue, ',') then
    begin
      FItemIndex := -1;
      ResetItemsSelected;
      LSelectedIdx := TStringList.Create;
      LSelectedVal := TStringList.Create;
      try
        LSelectedIdx.CommaText := ASubmitValue;
        for I                  := 0 to LSelectedIdx.Count - 1 do
          if TryStrToInt(LSelectedIdx[I], v) and (v >= 0) and (v < Items.Count) then
            begin
              if I = 0 then
                FItemIndex := v
              else if ItemsHaveValues then
                LSelectedVal.Add(Items.ValueFromIndex[v])
              else
                LSelectedVal.Add(Items[v]);
              FItemsSelected[v] := True;
            end;
        LSelectedVal.StrictDelimiter := True;
        ATextValue                   := LSelectedVal.CommaText;
      finally
        LSelectedIdx.Free;
        LSelectedVal.Free;
      end;
    end
  else
    begin
      inherited InternalSetValue(ASubmitValue, ATextValue, ASetFieldValue);
      ResetItemsSelected;
    end;
end;

procedure TDWSelect.InternalRenderAsync(const AHTMLName: string);
var
  LSelectedIdx: string;
  I: integer;
begin
  inherited;
  if (FText <> FOldText) then
    begin
      LSelectedIdx := '';
      if FMultiSelect then
        begin
          for I := 0 to Length(FItemsSelected) - 1 do
            if FItemsSelected[I] then
              begin
                if LSelectedIdx <> '' then
                  LSelectedIdx := LSelectedIdx + ',';
                LSelectedIdx   := LSelectedIdx + IntToStr(I);
              end;
        end
      else if FItemIndex >= 0 then
        LSelectedIdx := IntToStr(FItemIndex);
      DWApplication.CallBackResp.AddScriptToExecuteFirst('$("#' + AHTMLName + '").val([' +
        LSelectedIdx + ']);', False);
      FOldText := FText;
    end;
end;

procedure TDWSelect.InternalRenderHTML(var AHTMLTag: TDWElementTag);
var
  I: integer;
begin
  inherited;
  AHTMLTag := TDWElementTag.CreateHTMLTag('select');
  try
    AHTMLTag.AddStringParam('id', HTMLName);
    AHTMLTag.AddClassParam(ActiveCss);
    AHTMLTag.AddStringParam('name', HTMLName);
    if FSize > 0 then
      AHTMLTag.AddIntegerParam('size', FSize)
    else
      AHTMLTag.AddIntegerParam('size', Items.Count);
    if FMultiSelect then
      AHTMLTag.Add('multiple');
    if IsDisabled then
      AHTMLTag.Add('disabled');
    if AutoFocus then
      AHTMLTag.Add('autofocus');
    if (Validator <> nil) and (not(csDesigning in ComponentState)) then
      Validator.RenderValidation(AHTMLTag);
    if TabIndex <> 0 then
      AHTMLTag.AddStringParam('tabindex', IntToStr(TabIndex));
    for I := 0 to Items.Count - 1 do
      begin
        with AHTMLTag.Contents.AddElement('option') do
          begin
            AddStringParam('value', IntToStr(I));
            if FItemsSelected[I] then
              Add('selected');
            Contents.AddText(TDWBSCommon.TextToHTML(iif(ItemsHaveValues, Items.Names[I],
              Items[I])));
          end;
      end;
  except
    FreeAndNil(AHTMLTag);
    raise;
  end;

  if not(Parent is TDWInputGroup) then
    AHTMLTag := DWCreateInputFormGroup(Self, Parent, AHTMLTag, Caption, HTMLName);
end;
{$ENDREGION}
{$REGION 'TIWBSRadioGroup'}

function TDWRadioGroup.InputSelector: string;
begin
  Result := ' input';
end;

function TDWRadioGroup.InputSuffix: string;
begin
  Result := '_INPUT';
end;

procedure TDWRadioGroup.InternalRenderAsync(const AHTMLName: string);
begin
  inherited;
  if (FText <> FOldText) then
    begin
      if FItemIndex >= 0 then
        DWApplication.CallBackResp.AddScriptToExecuteFirst('$("#' + AHTMLName + '_INPUT_' +
          IntToStr(FItemIndex) + '").prop("checked", true);', False)
      else
        DWApplication.CallBackResp.AddScriptToExecuteFirst
          ('$("#' + AHTMLName + ' input").prop("checked", false);', False);
      FOldText := FText;
    end;
end;

procedure TDWRadioGroup.InternalRenderHTML(var AHTMLTag: TDWElementTag);
var
  I: integer;
begin
  inherited;
  AHTMLTag := TDWElementTag.CreateHTMLTag('div');
  try
    AHTMLTag.AddStringParam('id', HTMLName);
    AHTMLTag.AddClassParam('radio');
    if TabIndex <> 0 then
      AHTMLTag.AddStringParam('tabindex', IntToStr(TabIndex));
    if (Validator <> nil) and (not(csDesigning in ComponentState)) then
      Validator.RenderValidation(AHTMLTag);

    AHTMLTag.AddStringParam('style', ActiveStyle);
    for I := 0 to Items.Count - 1 do
      begin
        with AHTMLTag.Contents.AddElement('label') do
          begin
            with Contents.AddElement('input') do
              begin
                AddStringParam('type', 'radio');
                Add(iif(FItemIndex = I, 'checked', ''));
                AddStringParam('name', HTMLName + InputSuffix);
                AddStringParam('id', HTMLName + InputSuffix + '_' + IntToStr(I));
                AddStringParam('value', IntToStr(I));
                if IsDisabled then
                  Add('disabled');
              end;
            Contents.AddText(TDWBSCommon.TextToHTML(iif(ItemsHaveValues, Items.Names[I],
              Items[I])));
          end;
        AHTMLTag.Contents.AddText('<br>');
      end;
  except
    FreeAndNil(AHTMLTag);
    raise;
  end;

  if Parent is TDWInputGroup then
    AHTMLTag := DWCreateInputGroupAddOn(AHTMLTag, HTMLName, 'addon')
  else
    AHTMLTag := DWCreateInputFormGroup(Self, Parent, AHTMLTag, Caption, HTMLName);
end;
{$ENDREGION}

initialization

IWBSAddGlobalLinkFile('/<dwlibpath>/maskedinput/jquery.maskedinput.min.js');
LFormatSettings                 := TFormatSettings.Create('en-US'); // locale de us
LFormatSettings.DateSeparator   := '-';
LFormatSettings.LongDateFormat  := 'yyyy-mm-dd';
LFormatSettings.ShortDateFormat := LFormatSettings.LongDateFormat;
LFormatSettings.LongTimeFormat  := 'hh:nn:ss';
LFormatSettings.ShortTimeFormat := LFormatSettings.LongTimeFormat;

end.
