unit DW.VCL.Common;

interface

uses Classes, System.SysUtils, System.StrUtils, System.UITypes, VCL.Controls,
  DWMarkupLinguageTag, DW.VCL.StyleRenderOptions, DWTypes, DWElementTag;

type

  TDWGridOptions = class(TPersistent)
  private
    FFloat: TDWGridFloat;
    FOwner: Tcontrol;
    FGridXsOffset: integer;
    FGridXsSpan: integer;
    FGridSmOffset: integer;
    FGridSmSpan: integer;
    FGridMDOffset: integer;
    FGridMdSpan: integer;
    FGridLGOffset: integer;
    FGridLGspan: integer;
    FVisibilityXs: TDWGridVisibility;
    FVisibilitySm: TDWGridVisibility;
    FVisibilityMd: TDWGridVisibility;
    FVisibilityLg: TDWGridVisibility;
    FVisibilityPr: TDWGridVisibility;
    procedure SetVisibilityLg(const Value: TDWGridVisibility);
    procedure SetVisibilityMd(const Value: TDWGridVisibility);
    procedure SetVisibilityPr(const Value: TDWGridVisibility);
    procedure SetVisibilitySm(const Value: TDWGridVisibility);
    procedure SetVisibilityXs(const Value: TDWGridVisibility);
    procedure SetFloat(const Value: TDWGridFloat);
    procedure SetGridLGOffset(const Value: integer);
    procedure SetGridLGspan(const Value: integer);
    procedure SetGridMDOffset(const Value: integer);
    procedure SetGridMdSpan(const Value: integer);
    procedure SetGridSmOffset(const Value: integer);
    procedure SetGridSmSpan(const Value: integer);
    procedure SetGridXsOffset(const Value: integer);
    procedure SetGridXsSpan(const Value: integer);
  public
    constructor Create(AOwner: Tcontrol); virtual;

    procedure Assign(Source: TPersistent); override;
    function GetClassString(ACustomXsOffset, ACustomSmOffset, ACustomMdOffset,
      ACustomLgOffset: integer): string; overload;
    function GetClassString: string; overload;
  published
    property Float: TDWGridFloat read FFloat write SetFloat default bsgfNone;
    property GridXsOffset: integer read FGridXsOffset write SetGridXsOffset default 0;
    property GridXsSpan: integer read FGridXsSpan write SetGridXsSpan default 0;
    property GridSmOffset: integer read FGridSmOffset write SetGridSmOffset default 0;
    property GridSmSpan: integer read FGridSmSpan write SetGridSmSpan default 0;
    property GridMdOffset: integer read FGridMDOffset write SetGridMDOffset default 0;
    property GridMdSpan: integer read FGridMdSpan write SetGridMdSpan default 0;
    property GridLgOffset: integer read FGridLGOffset write SetGridLGOffset default 0;
    property GridLgSpan: integer read FGridLGspan write SetGridLGspan default 0;
    property VisibilityXs: TDWGridVisibility read FVisibilityXs write SetVisibilityXs
      default bsgvDefault;
    property VisibilitySm: TDWGridVisibility read FVisibilitySm write SetVisibilitySm
      default bsgvDefault;
    property VisibilityMd: TDWGridVisibility read FVisibilityMd write SetVisibilityMd
      default bsgvDefault;
    property VisibilityLg: TDWGridVisibility read FVisibilityLg write SetVisibilityLg
      default bsgvDefault;
    property VisibilityPrint: TDWGridVisibility read FVisibilityPr write SetVisibilityPr
      default bsgvDefault;
  end;

  TDWBSCommon = class
  private
    class procedure DoHtmlTag(AControl: TComponent; aTag: TDWCustomElement); static;
  public
    class procedure AddCssClass(var ACss: string; const AClass: string);
    class procedure AsyncRemoveControl(const AHTMLName: string);
    class procedure DoRender(AControl: TComponent);
    class procedure DoAfterRender(AControl: TComponent);
    class procedure RenderAsync(const AHTMLName: string; AControl: Tcontrol);
    class function RenderHTMLTag(AControl: Tcontrol): string;
    class function RenderStyle(AComponent: Tcontrol): string;
    // Render All Scripts for Control and register CallBacks in DWApplication
    // equivalent to  IWBSRenderScript procedure in IWBootstrap
    class procedure RenderScript(AControl: Tcontrol; var AHTMLTag: TDWElementTag);
    class function ReplaceParams(AComponent: Tcontrol; const AScript: string;
      AFrom: integer = 1): string;
    class procedure SetNotVisible(AParams: TStrings);
    class procedure ValidateParamName(const AName: string);
    class procedure ValidateTagName(const AName: string);

    class procedure SetAsyncDisabled(const HTMLName: string; Value: Boolean; var OldValue: Boolean);
    class procedure SetAsyncReadOnly(const HTMLName: string; Value: Boolean; var OldValue: Boolean);
    class procedure SetAsyncVisible(const HTMLName: string; Value: Boolean; var OldValue: Boolean);
    class procedure SetAsyncClass(const HTMLName: string; const Value: string;
      var OldValue: string);
    class procedure SetAsyncStyle(const HTMLName: string; const Value: string;
      var OldValue: string);
    class procedure SetAsyncChecked(const HTMLName: string; const Value: Boolean;
      var OldValue: Boolean);
    class procedure SetAsyncText(const HTMLName: string; const Value: string; var OldValue: string);
    class procedure SetAsyncHtml(const HTMLName: string; const Value: string; var OldValue: string);
    class function TextToHTML(const AText: string; ReplaceEOLs: Boolean = True;
      ReplaceSpaces: Boolean = False): string;
  end;

  TDWRegionCommon = class
  public
    class procedure CancelChildAsyncRender(AControl: TComponent);
    class procedure DisableRenderOptions(StyleRenderOptions: TDWRenderOptions);
    class procedure PrepareChildComponentsForRender(AContainer: Tcontrol);
    class procedure RenderComponents(AContainer: Tcontrol; aTagParent: TDWElementTag);
  end;

implementation

uses DWUtils, DWGlobal, DW.VCL.Container, DW.VCL.CustomForm,
  DW.JSON.JsonData, DW.VCL.Frame, DW.VCL.CustomRegion, DW.VCL.Control,
  DW.VCL.Interfaces, DWForm;

constructor TDWGridOptions.Create(AOwner: Tcontrol);
begin
  inherited Create;
  FOwner := AOwner;
end;

function TDWGridOptions.GetClassString(ACustomXsOffset, ACustomSmOffset, ACustomMdOffset,
  ACustomLgOffset: integer): string;
  procedure AddCssValue(var s: string; const Value: string);
  begin
    if s <> '' then
      s := s + ' ';
    s   := s + Value;
  end;

var
  lNavBar: Boolean;
begin
  Result := '';
  if ACustomXsOffset + FGridXsOffset > 0 then
    AddCssValue(Result, 'col-xs-offset-' + IntToStr(ACustomXsOffset + FGridXsOffset));
  if ACustomSmOffset + FGridSmOffset > 0 then
    AddCssValue(Result, 'col-sm-offset-' + IntToStr(ACustomSmOffset + FGridSmOffset));
  if ACustomMdOffset + FGridMDOffset > 0 then
    AddCssValue(Result, 'col-md-offset-' + IntToStr(ACustomMdOffset + FGridMDOffset));
  if ACustomLgOffset + FGridLGOffset > 0 then
    AddCssValue(Result, 'col-lg-offset-' + IntToStr(ACustomLgOffset + FGridLGOffset));

  if (FGridXsSpan > 0) then
    AddCssValue(Result, 'col-xs-' + IntToStr(FGridXsSpan));
  if (FGridSmSpan > 0) then
    AddCssValue(Result, 'col-sm-' + IntToStr(FGridSmSpan));
  if (FGridMdSpan > 0) then
    AddCssValue(Result, 'col-md-' + IntToStr(FGridMdSpan));
  if (FGridLGspan > 0) then
    AddCssValue(Result, 'col-lg-' + IntToStr(FGridLGspan));

  if FVisibilityXs = bsgvBlock then
    AddCssValue(Result, 'visible-xs-block')
  else if FVisibilityXs = bsgvInline then
    AddCssValue(Result, 'visible-xs-inline')
  else if FVisibilityXs = bsgvInlineBlock then
    AddCssValue(Result, 'visible-xs-inline-block')
  else if FVisibilityXs = bsgvHidden then
    AddCssValue(Result, 'hidden-xs');

  if FVisibilitySm = bsgvBlock then
    AddCssValue(Result, 'visible-sm-block')
  else if FVisibilitySm = bsgvInline then
    AddCssValue(Result, 'visible-sm-inline')
  else if FVisibilitySm = bsgvInlineBlock then
    AddCssValue(Result, 'visible-sm-inline-block')
  else if FVisibilitySm = bsgvHidden then
    AddCssValue(Result, 'hidden-sm');

  if FVisibilityMd = bsgvBlock then
    AddCssValue(Result, 'visible-md-block')
  else if FVisibilityMd = bsgvInline then
    AddCssValue(Result, 'visible-md-inline')
  else if FVisibilityMd = bsgvInlineBlock then
    AddCssValue(Result, 'visible-md-inline-block')
  else if FVisibilityMd = bsgvHidden then
    AddCssValue(Result, 'hidden-md');

  if FVisibilityLg = bsgvBlock then
    AddCssValue(Result, 'visible-lg-block')
  else if FVisibilityLg = bsgvInline then
    AddCssValue(Result, 'visible-lg-inline')
  else if FVisibilityLg = bsgvInlineBlock then
    AddCssValue(Result, 'visible-lg-inline-block')
  else if FVisibilityLg = bsgvHidden then
    AddCssValue(Result, 'hidden-lg');

  if FVisibilityPr = bsgvBlock then
    AddCssValue(Result, 'visible-print-block')
  else if FVisibilityPr = bsgvInline then
    AddCssValue(Result, 'visible-print-inline')
  else if FVisibilityPr = bsgvInlineBlock then
    AddCssValue(Result, 'visible-print-inline-block')
  else if FVisibilityPr = bsgvHidden then
    AddCssValue(Result, 'hidden-print');

  if FFloat <> bsgfNone then
    begin
      lNavBar := (FOwner is TDWCustomRegion);
      if FFloat = bsgfLeft then
        if lNavBar then
          AddCssValue(Result, 'navbar-left')
        else
          AddCssValue(Result, 'pull-left')
      else if FFloat = bsgfRight then
        if lNavBar then
          AddCssValue(Result, 'navbar-right')
        else
          AddCssValue(Result, 'pull-right');
    end;
end;

procedure TDWGridOptions.SetFloat(const Value: TDWGridFloat);
begin
  FFloat := Value;
  FOwner.Invalidate;
end;

procedure TDWGridOptions.SetGridLGOffset(const Value: integer);
begin
  FGridLGOffset := Value;
  FOwner.Invalidate;
end;

procedure TDWGridOptions.SetGridLGspan(const Value: integer);
begin
  FGridLGspan := Value;
  FOwner.Invalidate;
end;

procedure TDWGridOptions.SetGridMDOffset(const Value: integer);
begin
  FGridMDOffset := Value;
  FOwner.Invalidate;
end;

procedure TDWGridOptions.SetGridMdSpan(const Value: integer);
begin
  FGridMdSpan := Value;
  FOwner.Invalidate;
end;

procedure TDWGridOptions.SetGridSmOffset(const Value: integer);
begin
  FGridSmOffset := Value;
  FOwner.Invalidate;
end;

procedure TDWGridOptions.SetGridSmSpan(const Value: integer);
begin
  FGridSmSpan := Value;
  FOwner.Invalidate;
end;

procedure TDWGridOptions.SetGridXsOffset(const Value: integer);
begin
  FGridXsOffset := Value;
  FOwner.Invalidate;
end;

procedure TDWGridOptions.SetGridXsSpan(const Value: integer);
begin
  FGridXsSpan := Value;
  FOwner.Invalidate;
end;

procedure TDWGridOptions.SetVisibilityLg(const Value: TDWGridVisibility);
begin
  FVisibilityLg := Value;
  FOwner.Invalidate;
end;

procedure TDWGridOptions.SetVisibilityMd(const Value: TDWGridVisibility);
begin
  FVisibilityMd := Value;
  FOwner.Invalidate;
end;

procedure TDWGridOptions.SetVisibilityPr(const Value: TDWGridVisibility);
begin
  FVisibilityPr := Value;
  FOwner.Invalidate;
end;

procedure TDWGridOptions.SetVisibilitySm(const Value: TDWGridVisibility);
begin
  FVisibilitySm := Value;
  FOwner.Invalidate;
end;

procedure TDWGridOptions.SetVisibilityXs(const Value: TDWGridVisibility);
begin
  FVisibilityXs := Value;
  FOwner.Invalidate;
end;

function TDWGridOptions.GetClassString: string;
begin
  Result := GetClassString(0, 0, 0, 0);
end;

procedure TDWGridOptions.Assign(Source: TPersistent);
begin
  if Source is TDWGridOptions then
    begin
      GridXsOffset := TDWGridOptions(Source).GridXsOffset;
      GridXsSpan   := TDWGridOptions(Source).GridXsSpan;
      GridSmOffset := TDWGridOptions(Source).GridSmOffset;
      GridSmSpan   := TDWGridOptions(Source).GridSmSpan;
      GridMdOffset := TDWGridOptions(Source).GridMdOffset;
      GridMdSpan   := TDWGridOptions(Source).GridMdSpan;
      GridLgOffset := TDWGridOptions(Source).GridLgOffset;
      GridLgSpan   := TDWGridOptions(Source).GridLgSpan;
    end
  else
    inherited;
end;

class procedure TDWBSCommon.RenderScript(AControl: Tcontrol; var AHTMLTag: TDWElementTag);
var
  LScriptTag: TDWElementTag;
  LTempTag: TDWElementTag;
  L_IControl: IDWControl;
  LJScript: TstringList;
begin
  if (not Supports(AControl, IDWControl, L_IControl)) or (L_IControl = nil) then
    Exit;
  LJScript := TstringList.Create;
  try
    // render Control Async Events and Register Callback in DWApplication
    LJScript.Text := L_IControl.RenderAsyncEvents;
    { TODO 1 -oDELCIO -cIMPLEMENT : Render Hint Events }
    // Render Control Scripts
    L_IControl.InternalRenderScript(L_IControl.HTMLName, LJScript);
    // Add Custom User Script
    LJScript.AddStrings(L_IControl.Script);

    if LJScript.Count > 0 then
      begin
        // change script params
        { TODO 1 -oDELCIO -cIMPROVE : SpeedUp this }
        LJScript.Text := TDWBSCommon.ReplaceParams(AControl, LJScript.Text);

        // if script not to be rendered inside Control Tag
        if Not L_IControl.ScriptInsideTag then
          begin
            // Created an div tag and put Control and script inside this
            LTempTag := TDWElementTag.CreateHTMLTag('div');
            LTempTag.AddStringParam('id', L_IControl.HTMLName + '_WRP');
            LTempTag.Contents.AddElemetAsObject(AHTMLTag);
            AHTMLTag := LTempTag;
          end;
        // Created the script Tag and add to control main tag
        LScriptTag := AHTMLTag.Contents.AddElement('script');
        // Add Script Text to Script Tag
        LScriptTag.Contents.AddText(LJScript.Text);
      end;
  finally
    LJScript.Free;
  end;
end;

class procedure TDWBSCommon.AddCssClass(var ACss: string; const AClass: string);
begin
  if ACss <> '' then
    ACss := ACss + ' ';
  ACss   := ACss + AClass;
end;

class procedure TDWBSCommon.AsyncRemoveControl(const AHTMLName: string);
begin
  // IWBSExecuteAsyncJScript('AsyncDestroyControl("'+AHTMLName+'");', False, True);
  DWApplication.CallbackResp.AddScriptToExecuteFirst('AsyncDestroyControl("' + AHTMLName +
    '");', False);
  { TODO 1 -oDELCIO -cIMPLEMENT : Change  AsyncDestroyControl on Javascript(original in iwbs.js) }
end;

class procedure TDWBSCommon.DoAfterRender(AControl: TComponent);
var
  i: integer;
  LComponent: TDWControl;
  LContainer: TDWContainer;
begin

  // if is an TDWControl
  if AControl.InheritsFrom(TDWControl) then
    begin
      LComponent := TDWControl(AControl);
      if LComponent <> nil then
        begin
          // Execute OnAfterRender Event
          if Assigned(LComponent.OnAfterRender) then
            LComponent.OnAfterRender(LComponent);
          { TODO 1 -oDELCIO -cIMPLEMENT : Implement Global OnAfterRender }
          (* if Assigned(gIWBSOnAfterRender) then
            gIWBSOnAfterRender(LComponent.InterfaceInstance); *)
        end;
    end;
  // if is an TDWContainer
  if AControl.InheritsFrom(TDWContainer) then
    begin
      LContainer := TDWContainer(AControl);
      if LContainer <> nil then
        begin
          // Execute OnAfterRender Event for all Child TDWControls and TDWcontainers
          for i := 0 to AControl.ComponentCount - 1 do
            begin
              if (LContainer.Components[i].InheritsFrom(TDWControl)) or
                (LContainer.Components[i].InheritsFrom(TDWContainer)) then
                DoAfterRender(LContainer.Components[i]);
            end;
        end;
    end;
end;

class procedure TDWBSCommon.DoRender(AControl: TComponent);
var
  i: integer;
  LComponent: TDWControl;
  LContainer: TDWContainer;
begin

  // if is an TDWControl
  if AControl.InheritsFrom(TDWControl) then
    begin
      LComponent := TDWControl(AControl);
      if LComponent <> nil then
        begin
          // Execute OnRender Event
          if Assigned(LComponent.OnRender) then
            LComponent.OnRender(LComponent);
          { TODO 1 -oDELCIO -cIMPLEMENT : Implement Global OnRender }
          (* if Assigned(gIWBSOnRender) then
            gIWBSOnRender(LComponent); *)
        end;
    end;
  // if is an TDWContainer
  if AControl.InheritsFrom(TDWContainer) then
    begin
      LContainer := TDWContainer(AControl);
      if LContainer <> nil then
        begin
          // Execute OnRender Event for all Child TDWControls and TDWcontainers
          for i := 0 to AControl.ComponentCount - 1 do
            begin
              if (LContainer.Components[i].InheritsFrom(TDWControl)) or
                (LContainer.Components[i].InheritsFrom(TDWContainer)) then
                DoRender(LContainer.Components[i]);
            end;
        end;
    end;
end;

class procedure TDWBSCommon.DoHtmlTag(AControl: TComponent; aTag: TDWCustomElement);
var
  i: integer;
  LComponent: TDWControl;
  LContainer: TDWContainer;
begin

  // if is an TDWControl
  if AControl.InheritsFrom(TDWControl) then
    begin
      LComponent := TDWControl(AControl);
      if LComponent <> nil then
        begin
          // Execute OnHTMLtag Event
          if Assigned(LComponent.OnHTMLtag) then
            LComponent.OnHTMLtag(aTag);
        end;
    end;
  { TODO 3 -oDELCIO -cIMPROVE : Put else here and in all Doxxx class methods }
  // if is an TDWContainer
  if AControl.InheritsFrom(TDWContainer) then
    begin
      LContainer := TDWContainer(AControl);
      if LContainer <> nil then
        begin
          // Execute OnHTMLtag Event for all Child TDWControls and TDWcontainers
          for i := 0 to AControl.ComponentCount - 1 do
            begin
              if (LContainer.Components[i].InheritsFrom(TDWControl)) or
                (LContainer.Components[i].InheritsFrom(TDWContainer)) then
                DoRender(LContainer.Components[i]);
            end;
        end;
    end;
end;

class procedure TDWBSCommon.RenderAsync(const AHTMLName: string; AControl: Tcontrol);
  function ParentTreeVisibility(AControlInt: TComponent): Boolean;
  var
    LContainer: TDWContainer;
    LControl: TDWControl;
  begin
    Result     := True;
    LControl   := nil;
    LContainer := nil;
    if AControlInt.InheritsFrom(TDWControl) then
      begin
        LControl := TDWControl(AControlInt);
        if LControl <> nil then
          begin
            if not LControl.Visible then
              begin
                LContainer := LControl.ParentContainer;
                if (LContainer <> nil) and (not LContainer.RenderInvisibleControls) then
                  Result := False;
              end;
            if Result and (LControl.ParentContainer <> nil) then
              Result := ParentTreeVisibility(LControl.ParentContainer);
          end;
      end
    else if AControlInt.InheritsFrom(TDWContainer) then
      begin
        LContainer := TDWContainer(AControlInt);
        if LContainer <> nil then
          begin
            if LContainer.ParentContainer <> nil then
              Result := ParentTreeVisibility(LContainer.ParentContainer);
          end;
      end;
  end;

var

  LParentContainer: TDWContainer;
  LContainer: TDWContainer;
  LParentSl: string;
  LHtmlTag: string;
begin

  // if not visible and parent.RenderInvisibleControls is false, do not render
  if not ParentTreeVisibility(AControl) then
    Exit;

  // check if component is assigned
  if AControl = nil then
    raise Exception.Create('AControl not assigned in TDWCommon.RenderAsync');

  // if is an DWContainer
  if AControl.InheritsFrom(TDWContainer) then
    begin
      LContainer := TDWContainer(AControl);
      // Execute OnRender Event for all  Child DWCOntrols and DWContainers
      if LContainer <> nil then
        DoRender(LContainer);
    end;

  // get parent container
  if AControl.InheritsFrom(TDWControl) then
    LParentContainer := TDWControl(AControl).ParentContainer
  else if AControl.InheritsFrom(TDWContainer) then
    LParentContainer := TDWContainer(AControl).ParentContainer;

  if LParentContainer <> nil then
    begin
      // if parent is an form
      if LParentContainer.InheritsFrom(TDWCustomForm) then
        LParentSl := 'body' // define ajax script creation parent as body
      else
        LParentSl := '#' + LParentContainer.HTMLName;

      // Render Component Tag to after add it to CallBack Ajax Creation Script
      LHtmlTag := RenderHTMLTag(AControl);

      // Add Script for Ajax Creation of Components in CallBackResponse
      // the creation of the controls is executed as first script in the callback response, so further scripts in callback could reference them
      // IWBSExecuteAsyncJScript(AContext.WebApplication, 'AsyncRenderControl("'+AHtmlName+'", "'+LParentSl+'", "'+IWBSTextToJsParamText(LHtmlTag)+'");', True, True);
      DWApplication.CallbackResp.AddScriptToUpdate('AsyncRenderControl("' + AHTMLName + '", "' +
        LParentSl + '", "' + DWTextToJsParamText(LHtmlTag) + '");', True);

      // Call OnAfterRender event
      DoAfterRender(AControl);
    end;
end;

class function TDWBSCommon.RenderHTMLTag(AControl: Tcontrol): string;
var
  LContainer: TDWContainer;
  LTag: TDWElementTag;
begin
  // Create and render control Tag
  if AControl.InheritsFrom(TDWControl) then
    LTag := TDWControl(AControl).RenderHTML
  else if AControl.InheritsFrom(TDWContainer) then
    LTag := TDWContainer(AControl).RenderHTML;
  if LTag = nil then
    raise Exception.Create('LTag Not Created in TDWCommon.RenderHTMLTag');

  try
    if not AControl.Visible then
      TDWBSCommon.SetNotVisible(LTag.Params);
    (* ---> Moved to  RenderHTML
      // render child components
      if AControl.InheritsFrom(TDWContainer) then
      begin
      LContainer := TDWContainer(AControl);
      if LContainer <> nil then
      TDWRegionCommon.RenderComponents(LContainer, LTag);
      end;
    *)

    Result := LTag.Render;

    DoHtmlTag(AControl, LTag);

  finally
    LTag.Free;
  end;
end;

class function TDWBSCommon.RenderStyle(AComponent: Tcontrol): string;
var
  xStyle: TstringList;
  i: integer;
begin
  Result := '';

  xStyle := TstringList.Create;
  try
    // assign user style
    if AComponent.InheritsFrom(TDWControl) then
      xStyle.Assign(TDWControl(AComponent).Style)
    else if AComponent.InheritsFrom(TDWContainer) then
      xStyle.Assign(TDWContainer(AComponent).Style);

    // z-index
    if AComponent.InheritsFrom(TDWControl) then
      begin
        if TDWControl(AComponent).ZIndex <> 0 then
          xStyle.Values['z-index'] := IntToStr(TDWControl(AComponent).ZIndex)
      end
    else if AComponent.InheritsFrom(TDWContainer) then
      begin
        if TDWContainer(AComponent).ZIndex <> 0 then
          xStyle.Values['z-index'] := IntToStr(TDWContainer(AComponent).ZIndex);
      end;

    // render cursor
    if AComponent.Cursor <> crDefault then
      begin
        if AComponent.InheritsFrom(TDWControl) then
          xStyle.Values['cursor'] := Copy(TDWControl(AComponent).RenderCursorStyle, 9, MaxInt)
        else if AComponent.InheritsFrom(TDWContainer) then
          xStyle.Values['cursor'] := Copy(TDWContainer(AComponent).RenderCursorStyle, 9, MaxInt);
      end;

    if AComponent.InheritsFrom(TDWControl) then
      TDWControl(AComponent).InternalRenderStyle(xStyle)
    else if AComponent.InheritsFrom(TDWContainer) then
      TDWContainer(AComponent).InternalRenderStyle(xStyle);

    for i := 0 to xStyle.Count - 1 do
      begin
        if Result <> '' then
          Result := Result + ';';
        Result   := Result + xStyle[i];
      end;
  finally
    xStyle.Free;
  end;
end;

class function TDWBSCommon.ReplaceParams(AComponent: Tcontrol; const AScript: string;
  AFrom: integer = 1): string;
var
  LF, LT, i: integer;
  LParam, LParNm: string;
  LFound: Boolean;
  LCompo: TComponent;
  L_IComp: IDWControl;
begin
  Result := AScript;
  if (not Supports(AComponent, IDWControl, L_IComp)) or (L_IComp = nil) then
    Exit;

  LF := PosEx('{%', Result, AFrom);
  if LF > 0 then
    begin
      LFound := False;

      LT := PosEx('%}', Result, LF);
      if LT > LF then
        LParam := Copy(Result, LF, LT - LF + 2);
      LParNm   := Copy(Result, LF + 2, LT - LF - 2);

      i := L_IComp.ScriptParams.IndexOfName(LParNm);
      if i >= 0 then
        begin
          Result := ReplaceStr(Result, LParam, L_IComp.ScriptParams.ValueFromIndex[i]);
          LFound := True;
        end;

      i := L_IComp.ScriptParams.IndexOf(LParNm);
      if i >= 0 then
        begin
          if L_IComp.ScriptParams.Objects[i] is TJsonObject then
            Result := ReplaceText(Result, LParam,
              TJsonObject(L_IComp.ScriptParams.Objects[i]).ToJSON)
          else
            Result := ReplaceText(Result, LParam, '');
        end;

      if not LFound and AnsiSameText('htmlname', LParNm) then
        begin
          Result := ReplaceStr(Result, LParam, L_IComp.HTMLName);
          LFound := True;
        end;

      if not LFound and (AComponent.Owner <> nil) then
        begin
          LCompo := AComponent.Owner.FindComponent(LParNm);
          if (LCompo <> nil) then
            begin
              if (not Supports(LCompo, IDWControl, L_IComp) or (L_IComp = nil)) then
                Exit;
              Result := ReplaceStr(Result, LParam, L_IComp.HTMLName);
              LFound := True;
            end;
        end;

      if LFound then
        Result := ReplaceParams(AComponent, Result)
      else
        Result := ReplaceParams(AComponent, Result, LF + 1);
    end;
end;

class procedure TDWBSCommon.ValidateParamName(const AName: string);
var
  i: integer;
begin
  for i := 1 to Length(AName) do
    if not CharInSet(AName[i], ['-', '.', '0' .. '9', 'A' .. 'Z', 'a' .. 'z']) then
      raise Exception.Create('Invalid character in param name ' + AName);
end;

class procedure TDWBSCommon.ValidateTagName(const AName: string);
var
  i: integer;
begin
  if AName = '' then
    Exception.Create('Tag name could not be empty');
  for i := 1 to Length(AName) do
    if ((i = 1) and not CharInSet(AName[i], ['A' .. 'Z', 'a' .. 'z'])) or
      ((i > 1) and not CharInSet(AName[i], ['0' .. '9', 'A' .. 'Z', 'a' .. 'z'])) then
      raise Exception.Create('Invalid character in tag name ' + AName);
end;

class procedure TDWBSCommon.SetNotVisible(AParams: TStrings);
var
  LStyle: string;
begin
  LStyle := AParams.Values['style'];
  LStyle := Trim(LStyle);
  if (LStyle <> '') and not AnsiEndsStr(';', LStyle) then
    LStyle := LStyle + ';';
  if not AnsiContainsStr(LStyle, 'visibility:') then
    LStyle := LStyle + 'visibility: hidden;';
  if not AnsiContainsStr(LStyle, 'display:') then
    LStyle := LStyle + 'display: none;';
  if LStyle <> '' then
    AParams.Values['style'] := LStyle;
end;

class procedure TDWBSCommon.SetAsyncDisabled(const HTMLName: string; Value: Boolean;
  var OldValue: Boolean);
begin
  if OldValue <> Value then
    begin
      DWApplication.CallbackResp.AddScriptToExecuteFirst('$("#' + HTMLName + '").prop("disabled",' +
        iif(Value, 'true', 'false') + ');', False);
      // IWBSExecuteAsyncJScript(AApplication, '$("#' + HTMLName + '").prop("disabled",' + iif(Value,
      // 'true', 'false') + ');', False, True);
      OldValue := Value;
    end;
end;

class procedure TDWBSCommon.SetAsyncReadOnly(const HTMLName: string; Value: Boolean;
  var OldValue: Boolean);
begin
  if OldValue <> Value then
    begin
      DWApplication.CallbackResp.AddScriptToExecuteFirst('$("#' + HTMLName + '").prop("readonly",' +
        iif(Value, 'true', 'false') + ');', False);
      // IWBSExecuteAsyncJScript(AApplication,'$("#'+HTMLName+'").prop("readonly",'+iif(Value,'true','false')+');', False, True);
      OldValue := Value;
    end;
end;

class procedure TDWBSCommon.SetAsyncVisible(const HTMLName: string; Value: Boolean;
  var OldValue: Boolean);
begin
  if OldValue <> Value then
    begin
      DWApplication.CallbackResp.AddScriptToExecuteFirst('$("#' + HTMLName + '").css("visibility","'
        + iif(Value, '', 'hidden') + '");', False);
      DWApplication.CallbackResp.AddScriptToExecuteFirst('$("#' + HTMLName + '").css("display","' +
        iif(Value, '', 'none') + '");', False);
      // IWBSExecuteAsyncJScript(AApplication,'$("#'+HTMLName+'").css("visibility","'+iif(Value,'','hidden')+'");', False, True);
      // IWBSExecuteAsyncJScript(AApplication,'$("#'+HTMLName+'").css("display","'+iif(Value,'','none')+'");', False, True);
      OldValue := Value;
    end;
end;

class procedure TDWBSCommon.SetAsyncText(const HTMLName: string; const Value: string;
  var OldValue: string);
begin
  if OldValue <> Value then
    begin
      DWApplication.CallbackResp.AddScriptToExecuteFirst('$("#' + HTMLName + '").val("' +
        DWTextToJsParamText(Value) + '");', True);
      // IWBSExecuteAsyncJScript(AApplication, '$("#' + HTMLName + '").val("' + TIWBaseHTMLControl.TextToJSStringLiteral(Value) + '");', True, True);
      OldValue := Value;
    end;
end;

class procedure TDWBSCommon.SetAsyncHtml(const HTMLName: string; const Value: string;
  var OldValue: string);
begin
  if OldValue <> Value then
    begin
      DWApplication.CallbackResp.AddScriptToExecuteFirst('$("#' + HTMLName + '").html("' +
        DWTextToJsParamText(Value) + '");', True);
      // IWBSExecuteAsyncJScript(AApplication,'$("#'+HTMLName+'").html("'+IWBSTextToJsParamText(Value)+'");', True, True);
      OldValue := Value;
    end;
end;

class function TDWBSCommon.TextToHTML(const AText: string; ReplaceEOLs: Boolean = True;
  ReplaceSpaces: Boolean = False): string;
var
  POrig, PDest: PChar;
  L_IsCallBack: Boolean;
begin
  L_IsCallBack := DWApplication.IsCallBack;
  SetLength(Result, Length(AText) * 10);
  POrig := PChar(AText);
  PDest := PChar(Result);
  while POrig^ <> #0 do
    begin
      case POrig^ of
        '&':
          begin
            FormatBuf(PDest^, 10 { 5*2 } , '&amp;', 10 { 5*2 } , []);
            Inc(PDest, 4);
          end;
        '<', '>':
          begin
            if POrig^ = '<' then
              FormatBuf(PDest^, 8 { 4*2 } , '&lt;', 8 { 4*2 } , [])
            else
              FormatBuf(PDest^, 8 { 4*2 } , '&gt;', 8 { 4*2 } , []);
            Inc(PDest, 3);
          end;
        '"':
          begin
            FormatBuf(PDest^, 12 { 6*2 } , '&quot;', 12 { 6*2 } , []);
            Inc(PDest, 5);
          end;
        '''':
          begin
            FormatBuf(PDest^, 10 { 5*2 } , '&#39;', 10 { 5*2 } , []);
            Inc(PDest, 4);
          end;
        '\':
          begin
            FormatBuf(PDest^, 10 { 5*2 } , '&#92;', 10 { 5*2 } , []);
            Inc(PDest, 4);
          end;
        #10:
          if ReplaceEOLs then
            begin
              FormatBuf(PDest^, 8 { 4*2 } , '<br>', 8 { 4*2 } , []);
              Inc(PDest, 3);
            end
          else
            PDest^ := POrig^;
        #13:
          if ReplaceEOLs then
            begin
              Dec(PDest);
            end
          else
            PDest^ := POrig^;
        #32:
          if ReplaceSpaces then
            begin
              if L_IsCallBack then
                begin
                  FormatBuf(PDest^, 20, '&amp;nbsp;', 20, []);
                  Inc(PDest, 9);
                end
              else
                begin
                  FormatBuf(PDest^, 12, '&nbsp;', 12, []);
                  Inc(PDest, 5);
                end;
            end
          else
            PDest^ := POrig^;
      else
        PDest^ := POrig^
      end;
      Inc(PDest);
      Inc(POrig);
    end;
  SetLength(Result, PDest - PChar(Result));
end;

class procedure TDWBSCommon.SetAsyncClass(const HTMLName: string; const Value: string;
  var OldValue: string);
begin
  if OldValue <> Value then
    begin
      DWApplication.CallbackResp.AddScriptToExecuteFirst
        ('$("#' + HTMLName + '").removeClass().addClass("' + Value + '");', False);
      // IWBSExecuteAsyncJScript(AApplication,'$("#'+HTMLName+'").removeClass().addClass("'+Value+'");', False, True);
      OldValue := Value;
    end;
end;

class procedure TDWBSCommon.SetAsyncStyle(const HTMLName: string; const Value: string;
  var OldValue: string);
begin
  if OldValue <> Value then
    begin
      DWApplication.CallbackResp.AddScriptToExecuteFirst('$("#' + HTMLName + '").prop("style","' +
        Value + '");', False);
      // IWBSExecuteAsyncJScript(AApplication,'$("#'+HTMLName+'").prop("style","'+Value+'");', False, True);
      OldValue := Value;
    end;
end;

class procedure TDWBSCommon.SetAsyncChecked(const HTMLName: string; const Value: Boolean;
  var OldValue: Boolean);
begin
  if OldValue <> Value then
    begin
      DWApplication.CallbackResp.AddScriptToExecuteFirst('$("#' + HTMLName + '").prop("checked",' +
        iif(Value, 'true', 'false') + ');', False);
      // IWBSExecuteAsyncJScript(AApplication,'$("#'+HTMLName+'").prop("checked",'+iif(Value,'true','false')+');', False, True);
      OldValue := Value;
    end;
end;

class procedure TDWRegionCommon.DisableRenderOptions(StyleRenderOptions: TDWRenderOptions);
begin
  StyleRenderOptions.RenderAbsolute := False;
  StyleRenderOptions.RenderBorder   := False;
  StyleRenderOptions.RenderFont     := False;
  StyleRenderOptions.RenderPadding  := False;
  StyleRenderOptions.RenderPosition := False;
  StyleRenderOptions.RenderSize     := False;
  StyleRenderOptions.RenderZIndex   := False;
end;

class procedure TDWRegionCommon.PrepareChildComponentsForRender(AContainer: Tcontrol);
var
  i: integer;
  LComponent: TComponent;
  LControl: TDWControl;
begin
  for i := 0 to AContainer.ComponentCount - 1 do
    begin

      LComponent := AContainer.Components[i];

      // Ignore non DWComponents
      if (not LComponent.InheritsFrom(TDWControl)) and (not LComponent.InheritsFrom(TDWContainer))
      then
        Continue;

      // TFrame
      if LComponent.InheritsFrom(TDWFrame) then
        begin
          raise Exception.Create
            ('Need to implement TDWRegionCommon.PrepareChildComponentsForRender for TDWFrame');

        end

      else if LComponent.ClassName = 'TDWTabPage' then
        begin
          raise Exception.Create
            ('Need to implement TDWRegionCommon.PrepareChildComponentsForRender for TDWTabPage');

        end;

      // disable child StyleRenderOptions
      if LComponent.InheritsFrom(TDWControl) then
        begin
          LControl := TDWControl(LComponent);
          if Assigned(LControl) then
            begin
              DisableRenderOptions(LControl.StyleRenderOptions);
            end;
        end;
      { TODO 1 -oDELCIO -cIMPLEMENT : global OnRender Event }
      (* // execute global OnRender hook
        if Assigned(gIWBSOnRender) then
        gIWBSOnRender(LComponent); *)
    end;
end;

class procedure TDWRegionCommon.RenderComponents(AContainer: Tcontrol; aTagParent: TDWElementTag);
var
  i: integer;
  LContainer: TDWContainer;
  LForm: TDWForm;
begin
  LContainer := (AContainer as TDWContainer);
  if LContainer.IsReleased then
    Exit;
  LForm := LContainer.Form as TDWForm;
  PrepareChildComponentsForRender(AContainer);
  for i := 0 to LContainer.ControlCount - 1 do
    begin
      LForm.LayoutRender.ProcessChildControls(LContainer.Controls[i], aTagParent);
    end;
end;

class procedure TDWRegionCommon.CancelChildAsyncRender(AControl: TComponent);
var
  i: integer;
  LComponent: TDWControl;
  LContainer: TDWContainer;
begin
  if AControl.InheritsFrom(TDWControl) then
    begin
      LComponent := TDWControl(AControl);
      if LComponent <> nil then
        LComponent.ResetAsyncRefreshControl;
    end
  else if AControl is TDWFrame then
    begin
      for i := 0 to AControl.ComponentCount - 1 do
        CancelChildAsyncRender(AControl.Components[i]);
    end
  else if AControl.InheritsFrom(TDWContainer) then
    begin
      LContainer := TDWContainer(AControl);
      if LContainer <> nil then
        begin
          for i := 0 to LContainer.ComponentCount - 1 do
            CancelChildAsyncRender(LContainer.Components[i]);
        end;
    end;
end;

end.
