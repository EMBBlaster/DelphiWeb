unit DW.VCL.InputMoney;

// Created by Delcio 06/10/2016 22:44:56

interface

uses Classes, DW.VCL.CustomInput, DWElementTag, DW.VCL.InputForm;

type
  TDWInputMoney = class(TDWCustomTextInput)
  private
    FAutoComplete: Boolean;
    FSymbol: string;
    FThousandsSeparator: string;
    FDecimalSeparator: string;
    FAllowZero: Boolean;
    FPrecision: Byte;
    FAllowNegative: Boolean;
    procedure SetAutoComplete(const Value: Boolean);
    procedure SetDecimalSeparator(const Value: string);
    procedure SetSymbol(const Value: string);
    procedure SetThousandsSeparator(const Value: string);
    procedure SetAllowZero(const Value: Boolean);
    procedure SetPrecision(const Value: Byte);
    procedure SetAllowNegative(const Value: Boolean);
    function GetAsCurrency: Currency;
    procedure SetAsCurrency(const Value: Currency);
  protected
    procedure InternalRenderAsync(const AHTMLName: string); override;
    procedure InternalRenderHTML(var AHTMLTag: TDWElementTag); override;
    procedure InternalSetValue(const ASubmitValue: string; var ATextValue: string;
      var ASetFieldValue: Boolean); override;
    procedure InternalRenderScript(const AHTMLName: string; AScript: TStringList); override;

  public
    constructor Create(AOwner: TComponent); override;
  published
    property AsCurrency: Currency read GetAsCurrency write SetAsCurrency;
    // FAutocomplete for websites it is good, but for applications it not intended
    property AutoComplete: Boolean read FAutoComplete write SetAutoComplete default False;
    // The currençy symbol
    property Symbol: string read FSymbol write SetSymbol;
    // the Decimal Separator
    property DecimalSeparator: string read FDecimalSeparator write SetDecimalSeparator;
    // the thousand separator
    property ThousandsSeparator: string read FThousandsSeparator write SetThousandsSeparator;
    // the number of digits after decimal separator
    property Precision: Byte read FPrecision write SetPrecision;
    // if true show $ 0,00 instead of blank
    property AllowZero: Boolean read FAllowZero write SetAllowZero default True;
    // if true allow negative values
    property AllowNegative: Boolean read FAllowNegative write SetAllowNegative default False;
  end;

implementation

uses
  System.SysUtils, System.StrUtils, DWUtils, DW.VCL.Common, DWGlobal, DWTypes;

var
  LFormatSettings: TFormatSettings;

  { TIWBSInputMoney }

constructor TDWInputMoney.Create(AOwner: TComponent);
begin
  inherited;
  FAutoComplete       := False;
  FSymbol             := 'R$';
  FDecimalSeparator   := ',';
  FThousandsSeparator := '.';
  FAllowZero          := True;
  FPrecision          := 2;
  FAllowNegative      := False;
end;

function TDWInputMoney.GetAsCurrency: Currency;
begin
  if FText = '' then
    Result := 0
  else
    Result := StrToCurr(FText, LFormatSettings)
end;

procedure TDWInputMoney.InternalRenderAsync(const AHTMLName: string);
var
  LValue: string;
begin
  inherited;
  LValue := StringReplace(FText, FormatSettings.DecimalSeparator, '.',
    [rfReplaceAll, rfIgnoreCase]);
  if LValue <> '' then
    DWApplication.CallBackResp.AddScriptToExecute('$("#' + HTMLName + '").maskMoney(''mask'',' +
      LValue + ');', False);
end;

procedure TDWInputMoney.InternalRenderHTML(var AHTMLTag: TDWElementTag);
begin
  inherited;
  if FIsStatic then
    begin
      AHTMLTag := TDWElementTag.CreateHTMLTag('p');
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
        AHTMLTag.AddStringParam('type', 'text');
        AHTMLTag.AddStringParam('data-prefix', FSymbol);
        AHTMLTag.AddStringParam('data-thousands', FThousandsSeparator);
        AHTMLTag.AddStringParam('data-decimal', FDecimalSeparator);
        AHTMLTag.AddIntegerParam('data-precision', FPrecision);
        AHTMLTag.AddBooleanParam('data-allow-zero', FAllowZero);
        AHTMLTag.AddBooleanParam('data-allow-negative', FAllowNegative);

        // data-symbol="R$ " data-thousands="." data-decimal="," //
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
  if not(Parent is TDWInputGroup) and (InputType <> bsitHidden) then
    AHTMLTag := DWCreateInputFormGroup(Self, Parent, AHTMLTag, Caption, HTMLName);
end;

procedure TDWInputMoney.InternalSetValue(const ASubmitValue: string; var ATextValue: string;
  var ASetFieldValue: Boolean);
begin
  ATextValue := StringReplace(ASubmitValue, FSymbol, '', [rfIgnoreCase, rfReplaceAll]);
  ATextValue := StringReplace(ATextValue, FThousandsSeparator, '', [rfIgnoreCase, rfReplaceAll]);
  ATextValue := StringReplace(ATextValue, FDecimalSeparator, LFormatSettings.DecimalSeparator,
    [rfIgnoreCase, rfReplaceAll]);
end;

procedure TDWInputMoney.SetAllowNegative(const Value: Boolean);
begin
  FAllowNegative := Value;
end;

procedure TDWInputMoney.SetAllowZero(const Value: Boolean);
begin
  FAllowZero := Value;
end;

procedure TDWInputMoney.SetAsCurrency(const Value: Currency);
begin
  Text := CurrToStr(Value, LFormatSettings)
end;

procedure TDWInputMoney.SetAutoComplete(const Value: Boolean);
begin
  FAutoComplete := Value;
end;

procedure TDWInputMoney.SetDecimalSeparator(const Value: string);
begin
  FDecimalSeparator := Value;
end;

procedure TDWInputMoney.SetPrecision(const Value: Byte);
begin
  FPrecision := Value;
end;

procedure TDWInputMoney.SetSymbol(const Value: string);
begin
  FSymbol := Value;
end;

procedure TDWInputMoney.SetThousandsSeparator(const Value: string);
begin
  FThousandsSeparator := Value;
end;

procedure TDWInputMoney.InternalRenderScript(const AHTMLName: string; AScript: TStringList);
begin
  inherited;
  AScript.Add('$(''#{%htmlname%}'').maskMoney();');
end;

initialization

LFormatSettings                 := TFormatSettings.Create('en-US'); // locale de us
LFormatSettings.DateSeparator   := '-';
LFormatSettings.LongDateFormat  := 'yyyy-mm-dd';
LFormatSettings.ShortDateFormat := LFormatSettings.LongDateFormat;
LFormatSettings.LongTimeFormat  := 'hh:nn:ss';
LFormatSettings.ShortTimeFormat := LFormatSettings.LongTimeFormat;

if DebugHook <> 0 then
  IWBSAddGlobalLinkFile('/<dwlibpath>/maskMoney/jquery.maskMoney.js')
else
  IWBSAddGlobalLinkFile('/<dwlibpath>/maskMoney/jquery.maskMoney.min.js');

end.
