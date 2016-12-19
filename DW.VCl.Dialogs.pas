unit DW.VCl.Dialogs;

interface

uses Classes, SysUtils, Controls, DW.VCl.Modal, DW.VCl.Region, DW.VCl.Labels,
  DW.VCl.Buttons, DW.VCl.CustomForm, DW.VCl.CustomRegion, DWElementTag;

type
  TDWDialogCloseButton = (iwbsdcNone, iwbsdcCaption, iwbsdcFooter, iwbsdcBoth);

  TDWDialog = class(TDWModal)
  private
    FContent: TDWRegion;
    FHeader: TDWRegion;
    FBody: TDWRegion;
    FFooter: TDWRegion;

    FBodyControl: TDWText;
    FTitleControl: TDWLabel;

    FAsyncDismissProc: TDWAsyncEventProc;

    procedure SetBodyText(const Value: string);
    function GetBodyText: string;
    procedure SetTitleText(const Value: string);
    function GetTitleText: string;
  protected
    procedure DoOnAsyncHide(AParams: TStringList); override;
  public
    constructor Create(AForm: TDWCustomForm; const ATitleText, ABodyText: string;
      ACloseButton: TDWDialogCloseButton = iwbsdcBoth; AAsyncDismissProc: TDWAsyncEventProc = nil);
      reintroduce; overload;
    constructor Create(const ATitleText, ABodyText: string;
      ACloseButton: TDWDialogCloseButton = iwbsdcBoth; AAsyncDismissProc: TDWAsyncEventProc = nil);
      reintroduce; overload;
    constructor Create(const ATitleText, ABodyText: string; AAsyncDismissProc: TDWAsyncEventProc);
      reintroduce; overload;

    function AddButton(AParent: TDWRegion; const ACaption: string;
      AAsyncClickProc: TDWAsyncEventProc = nil; ADismiss: boolean = True): TDWButton;

    function GetHeader: TDWRegion;
    function GetBody: TDWRegion;
    function GetFooter: TDWRegion;

    function GetBodyControl: TDWText;
    function GetTitleControl: TDWLabel;

    property BodyText: string read GetBodyText write SetBodyText;
    property TitleText: string read GetTitleText write SetTitleText;
  end;

  TDWAlertStyle    = (bsasSuccess, bsasInfo, bsasWarning, bsasDanger);
  TDWAlertPosition = (bsapDefault, bsapRightTop, bsapRightCenter, bsapRightBottom);

  TDWAlert = class(TDWCustomRegion)
  private
    FAlertVisible: boolean;
    FAlertPosition: TDWAlertPosition;
    FAlertStyle: TDWAlertStyle;
    FAlertText: string;
    FFade: boolean;
    FOnAsyncClose: TDWAsyncEventProc;

    FAlertLabel: TDWLabel;
    FCloseButton: TDWButton;
  protected
    procedure DoOnAsyncClose(AParams: TStringList); virtual;
    function GetCloseScript: string;
    procedure InternalRenderCss(var ACss: string); override;
    procedure InternalRenderScript(const AHTMLName: string; AScript: TStringList); override;
    procedure SetAlertStyle(AValue: TDWAlertStyle);
  public
    constructor Create(AForm: TDWCustomForm; const AAlertText: string;
      AAlertStyle: TDWAlertStyle = bsasSuccess); reintroduce; overload;
    constructor Create(const AAlertText: string; AAlertStyle: TDWAlertStyle = bsasSuccess);
      reintroduce; overload;
    destructor Destroy; override;

    function RenderHTML: TDWElementTag; override;

    function AddButton(const ACaption: string; AAsyncClickProc: TDWAsyncEventProc = nil): TDWButton;

    property AlertStyle: TDWAlertStyle read FAlertStyle write SetAlertStyle default bsasSuccess;
    property AlertPosition: TDWAlertPosition read FAlertPosition write FAlertPosition
      default bsapRightTop;
    property AlertText: string read FAlertText write FAlertText;
    property Fade: boolean read FFade write FFade default True;

    property AlertLabel: TDWLabel read FAlertLabel;

    property OnAsyncClose: TDWAsyncEventProc read FOnAsyncClose write FOnAsyncClose;
  end;

var
  sIWBSDialogCloseCaption: string = 'Close';

implementation

uses DWTypes, DWUtils, DW.VCl.Common;

{$REGION 'TDWDialog'}

constructor TDWDialog.Create(AForm: TDWCustomForm; const ATitleText, ABodyText: string;
  ACloseButton: TDWDialogCloseButton = iwbsdcBoth; AAsyncDismissProc: TDWAsyncEventProc = nil);
begin
  inherited Create(AForm);
  Parent := AForm;

  DestroyOnHide := True;
  ModalVisible  := True;

  FContent              := TDWRegion.Create(Owner);
  FContent.BSRegionType := bsrtModalContent;
  FContent.Parent       := Self;

  FAsyncDismissProc := AAsyncDismissProc;

  if ACloseButton in [iwbsdcCaption, iwbsdcBoth] then
    with TDWButton.Create(Owner) do
      begin
        Parent        := GetHeader;
        Top           := 0;
        Left          := 0;
        Caption       := '';
        BSButtonStyle := bsbsClose;
        DataDismiss   := bsbdModal;
      end;

  SetBodyText(ABodyText);
  SetTitleText(ATitleText);

  if ACloseButton in [iwbsdcFooter, iwbsdcBoth] then
    with TDWButton.Create(Owner) do
      begin
        Parent      := GetFooter;
        Caption     := sIWBSDialogCloseCaption;
        Top         := 0;
        Left        := MaxInt;
        DataDismiss := bsbdModal;
      end;
end;

constructor TDWDialog.Create(const ATitleText, ABodyText: string;
  AAsyncDismissProc: TDWAsyncEventProc);
begin
  Create(TDWCustomForm(DWApplication.ActiveForm), ATitleText, ABodyText, iwbsdcBoth,
    AAsyncDismissProc);
end;

constructor TDWDialog.Create(const ATitleText, ABodyText: string;
  ACloseButton: TDWDialogCloseButton = iwbsdcBoth; AAsyncDismissProc: TDWAsyncEventProc = nil);
begin
  Create(TDWCustomForm(DWApplication.ActiveForm), ATitleText, ABodyText, ACloseButton,
    AAsyncDismissProc);
end;

procedure TDWDialog.DoOnAsyncHide(AParams: TStringList);
begin
  if Assigned(FAsyncDismissProc) then
    FAsyncDismissProc(Self, AParams);
  inherited;
end;

function TDWDialog.GetBodyControl: TDWText;
begin
  if FBodyControl = nil then
    begin
      FBodyControl        := TDWText.Create(Owner);
      FBodyControl.Parent := GetBody;
      FBodyControl.Top    := 0;
      FBodyControl.Left   := 0;
    end;
  Result := FBodyControl;
end;

function TDWDialog.GetBodyText: string;
begin
  if FBodyControl <> nil then
    Result := FBodyControl.Lines.Text
  else
    Result := '';
end;

function TDWDialog.GetTitleControl: TDWLabel;
begin
  if FTitleControl = nil then
    begin
      FTitleControl        := TDWLabel.Create(Owner);
      FTitleControl.Parent := GetHeader;
      FTitleControl.Top    := 0;
      FTitleControl.Left   := 20;
    end;
  Result := FTitleControl;
end;

function TDWDialog.GetTitleText: string;
begin
  if FTitleControl <> nil then
    Result := FTitleControl.Caption
  else
    Result := '';
end;

function TDWDialog.GetHeader: TDWRegion;
begin
  if FHeader = nil then
    begin
      FHeader              := TDWRegion.Create(Owner);
      FHeader.Parent       := FContent;
      FHeader.BSRegionType := bsrtModalHeader;
      FHeader.Top          := 100;
    end;
  Result := FHeader;
end;

function TDWDialog.GetBody: TDWRegion;
begin
  if FBody = nil then
    begin
      FBody              := TDWRegion.Create(Owner);
      FBody.Parent       := FContent;
      FBody.BSRegionType := bsrtModalBody;
      FBody.Top          := 200;
    end;
  Result := FBody;
end;

function TDWDialog.GetFooter: TDWRegion;
begin
  if FFooter = nil then
    begin
      FFooter              := TDWRegion.Create(Owner);
      FFooter.Parent       := FContent;
      FFooter.BSRegionType := bsrtModalFooter;
      FFooter.Top          := 300;
    end;
  Result := FFooter;
end;

procedure TDWDialog.SetBodyText(const Value: string);
begin
  if Value <> '' then
    GetBodyControl.Lines.Text := Value;
end;

procedure TDWDialog.SetTitleText(const Value: string);
begin
  if Value <> '' then
    GetTitleControl.Caption := Value;
end;

function TDWDialog.AddButton(AParent: TDWRegion; const ACaption: string;
  AAsyncClickProc: TDWAsyncEventProc = nil; ADismiss: boolean = True): TDWButton;
begin
  Result         := TDWButton.Create(Owner);
  Result.Parent  := AParent;
  Result.Caption := ACaption;
  if Assigned(AAsyncClickProc) then
    begin
      Result.AsyncClickProc := procedure(Sender: TObject; EventParams: TStringList)
        begin
          AAsyncClickProc(Sender, EventParams);
          if ADismiss then
            begin
              FAsyncDismissProc := nil;
              ModalVisible      := False;
            end;
        end;
    end
  else
    Result.DataDismiss := bsbdModal;
end;
{$ENDREGION}
{$REGION 'TDWAlert'}

constructor TDWAlert.Create(AForm: TDWCustomForm; const AAlertText: string;
  AAlertStyle: TDWAlertStyle = bsasSuccess);
begin
  inherited Create(AForm);
  Parent         := AForm;
  FAlertVisible  := False;
  FAlertPosition := bsapRightTop;
  FAlertStyle    := bsasSuccess;
  FAlertText     := AAlertText;
  FFade          := True;

  FAlertLabel                := TDWLabel.Create(Self);
  FAlertLabel.Parent         := Self;
  FAlertLabel.Name           := Name + '_LABEL';
  FAlertLabel.Caption        := AAlertText;
  FCloseButton               := TDWButton.Create(Self);
  FCloseButton.Parent        := Self;
  FCloseButton.Name          := Name + '_CLOSEBTN';
  FCloseButton.Caption       := '';
  FCloseButton.BSButtonStyle := bsbsClose;
  FCloseButton.DataDismiss   := bsbdAlert;
end;

constructor TDWAlert.Create(const AAlertText: string; AAlertStyle: TDWAlertStyle = bsasSuccess);
begin
  Create(TDWCustomForm(DWApplication.ActiveForm), AAlertText, AAlertStyle);
end;

destructor TDWAlert.Destroy;
begin
  if FAlertVisible then
    begin
      DWApplication.CallBackResp.AddScriptToExecute(GetCloseScript);
      FAlertVisible := False;
    end;
  inherited;
end;

procedure TDWAlert.InternalRenderScript(const AHTMLName: string; AScript: TStringList);
var
  LCallbackName: String;
begin
  inherited;
  LCallbackName := DWApplication.RegisterCallBack(Self, ae_closed_bs_alert, DoOnAsyncClose);
  AScript.Add('$("#' + AHTMLName +
    '").on("closed.bs.alert", function(e){ executeAjaxCallBack("", null, "' + LCallbackName +
    '"); });');
end;

function TDWAlert.RenderHTML: TDWElementTag;
var
  xHTMLName: string;
begin
  xHTMLName := HTMLName;

  Result := inherited;

  FAlertVisible := True;
end;

procedure TDWAlert.SetAlertStyle(AValue: TDWAlertStyle);
begin
  FAlertStyle := AValue;
end;

procedure TDWAlert.InternalRenderCss(var ACss: string);
const
  aIWBSAlertStyle: array [bsasSuccess .. bsasDanger] of string = ('success', 'info', 'warning',
    'danger');
  aIWBSAlertPosition: array [bsapRightTop .. bsapRightBottom] of string = ('right-top',
    'right-center', 'right-bottom');
begin
  TDWBSCommon.AddCssClass(ACss, 'alert alert-' + aIWBSAlertStyle[FAlertStyle]);
  if FFade then
    TDWBSCommon.AddCssClass(ACss, 'fade in');
  if FAlertPosition <> bsapDefault then
    TDWBSCommon.AddCssClass(ACss, 'flyover flyover-' + aIWBSAlertPosition[FAlertPosition]);
  inherited;
end;

function TDWAlert.GetCloseScript: string;
begin
  Result := '$("#' + HTMLName + '").alert("close");';
end;

procedure TDWAlert.DoOnAsyncClose(AParams: TStringList);
begin
  if Assigned(FOnAsyncClose) then
    FOnAsyncClose(Self, AParams);
  FAlertVisible := False;
  Free;
end;

function TDWAlert.AddButton(const ACaption: string; AAsyncClickProc: TDWAsyncEventProc = nil)
  : TDWButton;
begin
  Result                := TDWButton.Create(Owner);
  Result.Parent         := Self;
  Result.Caption        := ACaption;
  Result.AsyncClickProc := AAsyncClickProc;
end;
{$ENDREGION}

end.
