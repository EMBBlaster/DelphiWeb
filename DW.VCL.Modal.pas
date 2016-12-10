unit DW.VCL.Modal;

interface

uses
  SysUtils, Classes, Controls, StrUtils, DW.VCL.CustomRegion, DWTypes, DWElementTag;

type
  TDWModal = class(TDWCustomRegion)
  private
    FWrapperSuffix: string;
    FDestroyOnHide: boolean;
    FDialogSize: TDWSize;
    FFade: boolean;
    FModalVisible: boolean;
    FOnAsyncShow: TDWAsyncProcedure;
    FOnAsyncHide: TDWAsyncProcedure;
  protected
    function GetShowScript: string;
    function GetHideScript: string;
    procedure SetModalVisible(AValue: boolean);
    procedure DoOnAsyncShow(AParams: TStringList); virtual;
    procedure DoOnAsyncHide(AParams: TStringList); virtual;
    procedure InternalRenderCss(var ACss: string); override;
    procedure InternalRenderScript(const AHTMLName: string; AScript: TStringList); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetRoleString: string; override;
    function RenderHTML: TDWElementTag; override;
  published
    property BSFade: boolean read FFade write FFade default false;
    property BSDialogSize: TDWSize read FDialogSize write FDialogSize default bsszDefault;
    property ModalVisible: boolean read FModalVisible write SetModalVisible default false;
    property DestroyOnHide: boolean read FDestroyOnHide write FDestroyOnHide default false;
    property OnAsyncShow: TDWAsyncProcedure read FOnAsyncShow write FOnAsyncShow;
    property OnAsyncHide: TDWAsyncProcedure read FOnAsyncHide write FOnAsyncHide;
  end;

implementation
  uses
    DW.VCL.Common, DWUtils;



constructor TDWModal.Create(AOwner: TComponent);
begin
  inherited;
  FDestroyOnHide := False;
  FDialogSize := bsszDefault;
  FFade := false;
  FModalVisible := false;
  FWrapperSuffix := '_wrp';
end;

destructor TDWModal.Destroy;
begin
  SetModalVisible(False);
  inherited;
end;

procedure TDWModal.InternalRenderCss(var ACss: string);
begin
  TDWBSCommon.AddCssClass(ACss, 'modal-dialog');
  if FDialogSize in [bsszLg,bsszSm] then
    TDWBSCommon.AddCssClass(ACss, 'modal-'+aDWSize[FDialogSize]);
  inherited;
end;

function TDWModal.GetRoleString: string;
begin
  Result := 'dialog';
end;

function TDWModal.GetShowScript: string;
begin
  Result := '$("#'+HTMLName+FWrapperSuffix+'").modal({backdrop: "static", "keyboard": true});';
end;

function TDWModal.GetHideScript: string;
begin
  Result := '$("#'+HTMLName+FWrapperSuffix+'").modal("hide");';
end;

procedure TDWModal.InternalRenderScript(const AHTMLName: string; AScript: TStringList);
var
  LCallBackName:string;
begin
  AScript.Add('$("#'+AHTMLName+FWrapperSuffix+'").off("shown.bs.modal").on("shown.bs.modal", function() { var elem; elem = $(this).find("[autofocus]"); if (elem.length !== 0) {elem.focus();} else {$(this).find("button:last").focus(); } });');
  if Assigned(FOnAsyncShow) then begin
    //AScript.Add('$("#'+AHTMLName+FWrapperSuffix+'").off("show.bs.modal").on("show.bs.modal", function(e){ executeAjaxEvent("", null, "'+AHTMLName+'.DoOnAsyncShow", true, null, true); });');
    //DWApplication.RegisterCallBack(Self, ae_show, DoOnAsyncShow);

    LCallbackName := DWApplication.RegisterCallBack(self, ae_click, DoOnAsyncShow);
    AScript.Add('$("#'+AHTMLName+FWrapperSuffix+'").off("show.bs.modal").on("show.bs.modal", ' + 'function (e) {' +
        'executeAjaxCallBack("", ' + JQSelector + '[0], "' + LCallbackName + '");' + '})');
  end;
  //AScript.Add('$("#'+AHTMLName+FWrapperSuffix+'").off("hidden.bs.modal").on("hidden.bs.modal", function(e){ executeAjaxEvent("", null, "'+AHTMLName+'.DoOnAsyncHide", true, null, true); });');
 //AContext.WebApplication.RegisterCallBack(AHTMLName+'.DoOnAsyncHide', DoOnAsyncHide);
  LCallbackName := DWApplication.RegisterCallBack(self, ae_hidden_bs_modal, DoOnAsyncHide);
  AScript.Add('$("#'+AHTMLName+FWrapperSuffix+'").off("hidden.bs.modal").on("hidden.bs.modal", ' + 'function (e) {' +
        'executeAjaxCallBack("", ' + JQSelector + '[0], "' + LCallbackName + '");' + '})');

  if FModalVisible then
    AScript.Add(GetShowScript);
  inherited;
end;

function TDWModal.RenderHTML: TDWElementTag;
var
  lCss: string;
begin
  Result := inherited;

  FMainID := HTMLName+FWrapperSuffix;

  Result := TDWElementTag.CreateHTMLTag('div');
  Result.Contents.AddElemetAsObject(FRegionDiv);
  Result.AddStringParam('id', FMainID);
  lCss := 'modal';
  if FFade then
    TDWBSCommon.AddCssClass(lCss, 'fade');
  Result.AddClassParam(LCss);
end;

procedure TDWModal.SetModalVisible(AValue: boolean);
begin
  if AValue <> FModalVisible then begin
    if not (csDesigning in ComponentState) and not (csLoading in ComponentState)  then
      if AValue then
        DWApplication.CallBackResp.AddScriptToExecute(GetShowScript, False)
      else
        DWApplication.CallBackResp.AddScriptToExecute(GetHideScript, False);
    FModalVisible := AValue;
  end;
end;

procedure TDWModal.DoOnAsyncShow(AParams: TStringList);
begin
  FOnAsyncShow(Self, AParams);
end;

procedure TDWModal.DoOnAsyncHide(AParams: TStringList);
begin
  FModalVisible := False;
  if Assigned(FOnAsyncHide) then
    FOnAsyncHide(Self, AParams);
  if FDestroyOnHide then begin
    AsyncRemoveControl;
    Release;
  end;
end;

end.
