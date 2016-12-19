unit DW.VCL.TabControl;

interface

uses
  SysUtils, Classes, Controls, Dialogs, StrUtils, DW.VCL.Region, DWElementTag;

type

  TIWBSCloseTabAction = (bstabFree, bstabHide, bstabNone);

  TIWBSTabOptions = class(TPersistent)
  private
    FFade: boolean;
    FPills: boolean;
    FJustified: boolean;
    FStacked: boolean;
    FCloseButtons: boolean;
    procedure SetCloseButtons(const Value: boolean);
  public
    constructor Create(AOwner: TComponent);
    procedure Assign(Source: TPersistent); override;
  published
    property Fade: boolean read FFade write FFade default false;
    property Pills: boolean read FPills write FPills default false;
    property Justified: boolean read FJustified write FJustified default false;
    property Stacked: boolean read FStacked write FStacked default false;
    property CloseButtons: boolean read FCloseButtons write SetCloseButtons default false;
  end;

  TDWTabControl = class;

  TDWTabPage = class(TDWRegion)
  private
    FTitle: string;
    FTabIndex: Integer;
    FTabControl: TDWTabControl;
    procedure SetTitle(const Value: string);
    procedure SetTabIndex(const Value: Integer);
    procedure SetTabControl(const Value: TDWTabControl);
  protected
    procedure InternalRenderCss(var ACss: string); override;
  published
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property TabIndex: Integer read FTabIndex write SetTabIndex;
    property TabControl: TDWTabControl read FTabControl write SetTabControl;
    property Title: string read FTitle write SetTitle;
  end;

  TIWBSTabCloseEvent = procedure(Sender: TObject; aTab: TDWTabPage;
    var aCloseAction: TIWBSCloseTabAction) of object;

  TDWTabControl = class(TDWRegion)
  private
    FPages: TList;
    FOldActivePage: Integer;
    FActivePage: Integer;
    FTabOptions: TIWBSTabOptions;
    FOnTabClose: TIWBSTabCloseEvent;
    function TabIndexToIndex(ATabOrder: Integer): Integer;
    procedure CheckActiveVisible;
    procedure SetTabOptions(const Value: TIWBSTabOptions);
    procedure SetActivePage(const Value: Integer);
    procedure DoOnTabClose(aParams: TStringList);
    procedure SetOnTabClose(const Value: TIWBSTabCloseEvent);
    function GetActiveTabPage: TDWTabPage;
    procedure SetActiveTabPage(const Value: TDWTabPage);
    procedure SetPages(const Value: TList);
  protected
    procedure DoAsyncCHange(aParams: TStringList); override;
    procedure SetValue(const AValue: string);
    procedure InternalRenderScript(const AHTMLName: string; AScript: TStringList); override;
    procedure InternalRenderStyle(AStyle: TStringList); override;
    // procedure InternalBeforeRenderControls(var aRenderStream: TIWRenderStream); override;
    // procedure InternalAfterRenderControls(var aRenderStream: TIWRenderStream); override;
    function RenderAsync: TDWElementXHTMLTag; override;
    procedure RenderComponents(aTagParent: TDWElementTag); override;
    function RenderCSSClass: string; override;
    function RenderHTML: TDWElementTag; override;
    procedure RenderScripts; override;
    function RenderStyle: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetTabPageCSSClass(ATabPage: TComponent): string;
    procedure SetTabPageVisibility(ATabIndex: Integer; Visible: boolean); overload;
    procedure SetTabPageVisibility(ATabPage: TDWTabPage; Visible: boolean); overload;
    function AddNewTab: TDWTabPage;
    property ActiveTabPage: TDWTabPage read GetActiveTabPage write SetActiveTabPage;
    procedure InsertTab(aTab: TDWTabPage);
    procedure RemoveTab(aTab: TDWTabPage);
  published
    property ActivePage: Integer read FActivePage write SetActivePage;
    property BSTabOptions: TIWBSTabOptions read FTabOptions write SetTabOptions;
    property Pages: TList read FPages write SetPages;
    property OnTabClose: TIWBSTabCloseEvent read FOnTabClose write SetOnTabClose;
  end;

implementation

uses DWGlobal, DWUtils, DWTypes, DW.VCL.Common, DW.CORE.MergeSort,
  DW.CORE.MergeSortFunc, DW.VCL.CustomForm;

{$REGION 'TIWBSTabOptions'}

constructor TIWBSTabOptions.Create(AOwner: TComponent);
begin
  FFade         := false;
  FPills        := false;
  FJustified    := false;
  FStacked      := false;
  FCloseButtons := false;
end;

procedure TIWBSTabOptions.SetCloseButtons(const Value: boolean);
begin
  if FCloseButtons <> Value then
    begin
      FCloseButtons := Value;
      { TODO 1 -oDELCIO -cIMPROVEMENT : Implemet UPDATEOPTIONS }
    end;
end;

procedure TIWBSTabOptions.Assign(Source: TPersistent);
begin
  if Source is TIWBSTabOptions then
    begin
      Fade      := TIWBSTabOptions(Source).Fade;
      Pills     := TIWBSTabOptions(Source).Pills;
      Justified := TIWBSTabOptions(Source).Justified;
      Stacked   := TIWBSTabOptions(Source).Stacked;
    end
  else
    inherited;
end;
{$ENDREGION}
{$REGION 'TIWBSTabControl'}

constructor TDWTabControl.Create(AOwner: TComponent);
begin
  inherited;
  FPages      := TList.Create;
  FTabOptions := TIWBSTabOptions.Create(Self);
  Height      := 112;
  Width       := 112;
end;

destructor TDWTabControl.Destroy;
begin
  FreeAndNil(FTabOptions);
  FreeAndNil(FPages);
  inherited;
end;

procedure TDWTabControl.DoAsyncCHange(aParams: TStringList);
var
  LPage: Integer;
begin
  LPage := StrToIntDef(aParams.Values['page'], -1);
  if (LPage > -1) then
    begin
      ActivePage := LPage;
    end;
  inherited;
end;

procedure TDWTabControl.DoOnTabClose(aParams: TStringList);
var
  LTabIndex: Integer;
  LCloseAction: TIWBSCloseTabAction;
  LPage: TDWTabPage;
begin
  LCloseAction := bstabHide;
  LTabIndex    := StrToIntDef(aParams.Values['page'], -1);
  if (LTabIndex > -1) and (LTabIndex < FPages.Count) then
    begin
      LPage := TDWTabPage(FPages[LTabIndex]);
      if Assigned(FOnTabClose) then
        FOnTabClose(Self, LPage, LCloseAction);
      case LCloseAction of
        bstabFree:
          LPage.Free;
        bstabHide:
          LPage.Hide;
        // bstabNone: ;
      end;
      Self.AsyncRefreshControl;
    end;
end;

function TDWTabControl.AddNewTab: TDWTabPage;
var
  LTabOrder: Integer;
begin
  Result := TDWTabPage.Create(Self);
  try
    Result.Title := 'Page' + IntToStr(FPages.Count);
    Result.Name  := Self.Name + 'Page' + IntToStr(FPages.Count);
    LTabOrder    := FPages.Count;
    // Result.TabOrder:=  FPages.Count;
    // FPages.Add(Result);
    Result.TabControl := Self;
    Result.Parent     := Self;
    Result.TabOrder   := LTabOrder;
    AsyncRefreshControl;
  except
    FreeAndNil(Result);
  end;
end;

procedure TDWTabControl.SetOnTabClose(const Value: TIWBSTabCloseEvent);
begin
  FOnTabClose := Value;
end;

procedure TDWTabControl.SetPages(const Value: TList);
begin
  if FPages <> Value then
    begin
      FPages.Assign(Value);
      FAsyncRefreshControl := True;
    end;
end;

procedure TDWTabControl.SetValue(const AValue: string);
var
  LIndex: Integer;
begin
  { TODO 1 -oDELCIO -cVERIFY : Check this }
  // if RequiresUpdateNotification(Parent) then
  // UpdateNotifiedInterface(Parent).NotifyUpdate(Self,AValue);
  LIndex := StrToIntDef(AValue, 0);
  if (LIndex < 0) or (LIndex >= FPages.Count) then
    begin
      FActivePage    := 0;
      FOldActivePage := -1;
    end
  else
    begin
      FActivePage    := TDWTabPage(FPages[LIndex]).TabOrder;
      FOldActivePage := FActivePage;
    end;
end;

procedure TDWTabControl.SetTabOptions(const Value: TIWBSTabOptions);
begin
  FTabOptions.Assign(Value);
  invalidate;
end;

function TDWTabControl.GetActiveTabPage: TDWTabPage;
begin
  if FActivePage < FPages.Count then
    Result := TDWTabPage(FPages[FActivePage]);
end;

procedure TDWTabControl.SetActivePage(const Value: Integer);
begin
  FActivePage := Value;
  invalidate;
end;

procedure TDWTabControl.SetActiveTabPage(const Value: TDWTabPage);
var
  I: Integer;
begin
  for I := 0 to FPages.Count - 1 do
    begin
      if TDWTabPage(FPages[I]) = Value then
        begin
          SetActivePage(I);
          Break;
        end;
    end;
end;

// procedure TIWBSTabControl.InternalAfterRenderControls(
// var aRenderStream: TIWRenderStream);
// begin
// aRenderStream.WriteLine('</div>');
// end;

// procedure TIWBSTabControl.InternalBeforeRenderControls(
// var aRenderStream: TIWRenderStream);
// begin
// aRenderStream.WriteLine('<div class="tab-content">');
// end;

procedure TDWTabControl.InsertTab(aTab: TDWTabPage);
begin
  FPages.Add(aTab);
  aTab.FTabControl := Self;
  aTab.Parent      := Self;

  // Item.Top := 0;
  { TODO 1 -oDELCIO -cVerify : verify insert item position in accordion }
  {
    if FItems.Count = 1 then
    Item.Top := 0
    else
    Item.Top := VertScrollBar.Range; }
  // Item.Left := 0;
  // Item.Align :=  alTop;
end;

procedure TDWTabControl.InternalRenderScript(const AHTMLName: string; AScript: TStringList);
var
  LCalbackName: string;
begin
  if not FTabOptions.Justified and not FTabOptions.Stacked then
    AScript.Add('$("#' + AHTMLName + '_tabs' + '").bootstrapDynamicTabs();');

  // save seleted tab on change, manually trigger change event because val don't do it
  AScript.Add('$("#' + AHTMLName + '_tabs").off("show.bs.tab").on("show.bs.tab", function(e){ $("#'
    + AHTMLName + '_input").val($(e.target).attr("tabindex")).change(); });');

  // Add Close Button event
  LCalbackName := DWApplication.RegisterCallBack(Self, ae_tabclose_dw, DoOnTabClose);
  AScript.Add('$("#' + AHTMLName +
    '_tabs span.tab-close-btn").off("click").on("click", function(e){ executeAjaxCallBack("&page=" + $(this).next().attr("tabindex"), '
    + JQSelector + '[0], "' + LCalbackName + '"); });');

  // To update server Tab Index to avoid change browser active tab if refresh page
  LCalbackName := DWApplication.RegisterCallBack(Self, ae_shown_bs_tab, DoAsyncCHange);
  AScript.Add('$("#' + AHTMLName +
    '_tabs").off("shown.bs.tab").on("shown.bs.tab", function(e){ executeAjaxCallBack("&page="+$(e.target).attr("tabindex"), '
    + JQSelector + '[0], "' + LCalbackName + '"); });');

  (* // event async change
    if Assigned(OnAsyncChange) then begin
    AScript.Add('$("#'+AHTMLName+'_tabs").off("shown.bs.tab").on("shown.bs.tab", function(e){ executeAjaxEvent("&page="+$(e.target).attr("tabindex"), null, "'+AHTMLName+'.DoOnAsyncChange", true, null, true); });');
    AContext.WebApplication.RegisterCallBack(AHTMLName+'.DoOnAsyncChange', DoOnAsyncChange);
    end; *)
end;

procedure TDWTabControl.InternalRenderStyle(AStyle: TStringList);
begin
  //
end;

procedure TDWTabControl.RemoveTab(aTab: TDWTabPage);
begin
  aTab.FTabControl := nil;
  FPages.Remove(aTab);
  RemoveControl(aTab);
end;

function TDWTabControl.RenderAsync: TDWElementXHTMLTag;
var
  xHTMLName: string;
begin
  Result    := nil;
  xHTMLName := HTMLName;

  if FAsyncRefreshControl or not FRendered then
    begin
      TDWRegionCommon.CancelChildAsyncRender(Self);
      { TODO 1 -oDELCIO -cVERIFY : Check this }
      // DoRender;
      TDWBSCommon.RenderAsync(xHTMLName, Self);
    end
  else
    begin
      TDWBSCommon.SetAsyncClass(xHTMLName, RenderCSSClass, FOldCss);
      TDWBSCommon.SetAsyncStyle(xHTMLName, RenderStyle, FOldStyle);
      TDWBSCommon.SetAsyncVisible(FMainID, Visible, FOldVisible);
      if FOldActivePage <> FActivePage then
        begin
          DWApplication.CallBackResp.AddScriptToExecuteFirst('$("#' + HTMLName + '_tabs a[tabindex='
            + IntToStr(TabIndexToIndex(FActivePage)) + ']").tab("show");', false);
          FOldActivePage := FActivePage;
        end;

      if Assigned(OnAfterAsyncChange) then
        OnAfterAsyncChange(Self);

      { TODO 1 -oDELCIO -cIMPLEMENT : Global Event OnAfterAsyncChange }
      { if Assigned(gIWBSOnAfterAsyncChange) then
        gIWBSOnAfterAsyncChange(Self, xHTMLName); }
    end;
end;

function TDWTabControl.TabIndexToIndex(ATabOrder: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I  := 0 to FPages.Count - 1 do
    if TDWTabPage(FPages[I]).TabOrder = ATabOrder then
      begin
        Result := I;
        Break;
      end;
end;

procedure TDWTabControl.CheckActiveVisible;
var
  I, LIndex: Integer;
begin
  LIndex := TabIndexToIndex(FActivePage);
  if (LIndex = -1) or not TDWTabPage(FPages.Items[LIndex]).Visible then
    for I := 0 to FPages.Count - 1 do
      if TDWTabPage(FPages[I]).Visible then
        begin
          FActivePage := TDWTabPage(FPages[I]).TabOrder;
          Break;
        end;
end;

procedure TDWTabControl.RenderComponents(aTagParent: TDWElementTag);
var
  LTabContent: TDWElementTag;
begin
  LTabContent := aTagParent.Contents.AddElement('div');
  LTabContent.AddClassParam('tab-content');
  inherited RenderComponents(LTabContent);
end;

function TDWTabControl.RenderCSSClass: string;
begin
  Result := 'iwbs-tabs ' + BSGridOptions.GetClassString;
end;

function TDWTabControl.RenderHTML: TDWElementTag;
var
  xHTMLName: string;
  xHTMLInput: string;
  I, TabIndex: Integer;
  tagTabs, tagLi, tagA, TagSpan: TDWElementTag;
  TabPage: TDWTabPage;
begin
  FOldCss        := RenderCSSClass;
  FOldStyle      := RenderStyle;
  FOldVisible    := Visible;
  FOldActivePage := FActivePage;

  MergeSortList(FPages, TabIndexSort);
  CheckActiveVisible;

  // read only one time
  xHTMLName  := HTMLName;
  xHTMLInput := xHTMLName + '_input';

  // main div
  FRegionDiv := TDWElementTag.CreateHTMLTag('div');
  FRegionDiv.AddStringParam('id', xHTMLName);
  FRegionDiv.AddClassParam(FOldCss);
  FRegionDiv.AddStringParam('style', RenderStyle);
  Result := FRegionDiv;

  // tabs region
  tagTabs := Result.Contents.AddElement('ul');
  tagTabs.AddStringParam('id', xHTMLName + '_tabs');
  tagTabs.AddClassParam('nav');
  if FTabOptions.Pills then
    tagTabs.AddClassParam('nav-pills')
  else
    tagTabs.AddClassParam('nav-tabs');

  if FTabOptions.Justified then
    tagTabs.AddClassParam('nav-justified');
  if FTabOptions.Stacked then
    tagTabs.AddClassParam('nav-stacked');

  tagTabs.AddStringParam('role', 'tablist');

  // build the tabs
  TabIndex := -1;
  for I    := 0 to FPages.Count - 1 do
    begin
      TabPage          := TDWTabPage(FPages.Items[I]);
      TabPage.TabIndex := I;
      if not TabPage.Visible and not RenderInvisibleControls then
        Continue;
      tagLi := tagTabs.Contents.AddElement('li');
      if (TabIndex = -1) and (FActivePage = TabPage.TabIndex) and TabPage.Visible then
        begin
          tagLi.AddClassParam('active');
          TabIndex := I;
        end;
      // Add icon Close
      if FTabOptions.CloseButtons then
        begin
          TagSpan := tagLi.Contents.AddElement('span');
          TagSpan.AddClassParam('tab-close-btn');
          TagSpan.AddStringParam('role', 'presentation');
          TagSpan.Contents.AddText('X');
          if not TabPage.Visible then
            TagSpan.AddStringParam('style', 'display:none');
        end;

      tagA := tagLi.Contents.AddElement('a');
      tagA.AddStringParam('data-toggle', IfThen(FTabOptions.Pills, 'pill', 'tab'));
      tagA.AddStringParam('href', '#' + TabPage.HTMLName);
      tagA.AddIntegerParam('tabindex', I);
      if not TabPage.Visible then
        tagA.AddStringParam('style', 'display: none');
      tagA.Contents.AddText(TabPage.Title);
    end;

  // this hidden input is for input seleted tab page
  Result.Contents.AddHiddenField(xHTMLInput, xHTMLInput, IntToStr(TabIndex));

  // Render Child Tabs
  RenderComponents(Result);
  // render scripts
  TDWBSCommon.RenderScript(Self, Result);
  FMainID := Result.Params.Values['id'];

  // initialize hidden input (after render scripts)
  if ParentContainer is TDWCustomForm then
    (Form as TDWCustomForm).HTMLPage.HTMLTag.HeadTag.InitComponents.Add
      ('DWInitControl(DWAjaxForm, "' + xHTMLInput + '", false, true);')
  else
    (Form as TDWCustomForm).HTMLPage.HTMLTag.HeadTag.InitComponents.Add
      ('DWInitControl(' + ParentContainer.HTMLName + ', "' + xHTMLInput + '", false, true);');
  FAsyncRefreshControl := false;
  FRendered            := True;
end;

procedure TDWTabControl.RenderScripts;
begin
  //
end;

function TDWTabControl.RenderStyle: string;
begin
  Result := TDWBSCommon.RenderStyle(Self);
end;

function TDWTabControl.GetTabPageCSSClass(ATabPage: TComponent): string;
begin
  Result := 'tab-pane';
  if BSTabOptions.Fade then
    Result := Result + ' fade';
  if TDWTabPage(ATabPage).TabIndex = FActivePage then
    Result := Result + ' active in';
end;

procedure TDWTabControl.SetTabPageVisibility(ATabIndex: Integer; Visible: boolean);
var
  LIndex: Integer;
begin
  LIndex := TabIndexToIndex(ATabIndex);
  if LIndex >= 0 then
    begin
      TDWTabPage(FPages.Items[LIndex]).Visible := Visible;
      CheckActiveVisible;
      DWApplication.CallBackResp.AddScriptToExecute('$("#' + HTMLName + '_tabs a[tabindex=' +
        IntToStr(LIndex) + ']").css("display", "' + iif(Visible, '', 'none') + '");');
    end;
end;

procedure TDWTabControl.SetTabPageVisibility(ATabPage: TDWTabPage; Visible: boolean);
begin
  SetTabPageVisibility(FPages.IndexOf(ATabPage), Visible);
end;
{$ENDREGION}
{ TDWTabPage }

constructor TDWTabPage.Create(AOwner: TComponent);
begin
  inherited;
  if FTitle = '' then
    FTitle := Name;
  Align    := alClient;
end;

destructor TDWTabPage.Destroy;
begin

  inherited;
end;

procedure TDWTabPage.InternalRenderCss(var ACss: string);
begin
  inherited InternalRenderCss(ACss);
  // Add CSS Class for active/inactive Tab and fade effect
  if FTabControl <> nil then
    ACss := FTabControl.GetTabPageCSSClass(Self) + ' ' + ACss;
end;

procedure TDWTabPage.SetTabControl(const Value: TDWTabControl);
var
  LRecreating: boolean;
begin
  if FTabControl <> Value then
    begin
      LRecreating := false;
      if not(csLoading in ComponentState) then
        begin
          LRecreating := csRecreating in ControlState;
          if not LRecreating then
            UpdateRecreatingFlag(True);
        end;

      try
        if FTabControl <> nil then
          FTabControl.RemoveTab(Self);
        Parent := Value;
        if Value <> nil then
          begin
            Value.InsertTab(Self);
            if not(csLoading in ComponentState) and not LRecreating then
              RecreateWnd;
          end;
      finally
        if not(csLoading in ComponentState) and not LRecreating then
          UpdateRecreatingFlag(false);
      end;
    end;
end;

procedure TDWTabPage.SetTabIndex(const Value: Integer);
begin
  FTabIndex := Value;
end;

procedure TDWTabPage.SetTitle(const Value: string);
begin
  FTitle := Value;
end;

initialization

RegisterClass(TDWTabPage);
IWBSAddGlobalLinkFile('/<dwlibpath>/dyntabs/bootstrap-dynamic-tabs.css');
IWBSAddGlobalLinkFile('/<dwlibpath>/dyntabs/bootstrap-dynamic-tabs.js');

end.
