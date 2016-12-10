unit DW.VCL.Buttons;

interface

uses
  SysUtils, Classes, db, StrUtils, Controls, DW.VCL.Control, DWTypes,
  DW.VCL.Container, DWElementTag;


type
  // TIWBSCustomButton.BSButtonStyle


  // Base class for TIWBSButton and TIWBSDropDown
  TDWCustomButton = class(TDWControl)
  private
    FBlockLevel: boolean;
    FButtonSize: TDWSize;
    FButtonStyle: TDWButtonStyle;
    FCaption: string;
    FGlyphicon: string;
    FRawText: boolean;

    procedure SetGlyphicon(const Value: string);
    procedure SetButtonStyle(const Value: TDWButtonStyle);
    procedure SetBlockLevel(const Value: boolean);
    procedure SetCaption(const Value: string);
    procedure SetRawText(const Value: boolean);
  public
    constructor Create(AOwner: TComponent); override;
  published
    // Boostrap Block Level Button @br
    // http://getbootstrap.com/css/#buttons-sizes
    property BSBlockLevel: boolean read FBlockLevel write SetBlockLevel default False;
    // Boostrap Button Size @br
    // http://getbootstrap.com/css/#buttons-sizes
    property BSButtonSize: TDWSize read FButtonSize write FButtonSize default bsszDefault;
    // Bootstrap Button Style @br
    // http://getbootstrap.com/css/#buttons-options
    property BSButtonStyle: TDWButtonStyle read FButtonStyle write SetButtonStyle default bsbsDefault;
    // Bootstrap Glyphicon @br
    // http://getbootstrap.com/components/#glyphicons
    property BSGlyphicon: string read FGlyphicon write SetGlyphicon;
    // Button Text
    property Caption: string read FCaption write SetCaption;
    // If true, the Caption will be rendered as Raw HTML
    property RawText: boolean read FRawText write SetRawText default False;
  end;

  // TIWBSButton.DataDismiss
  TIWBSButtonDataDismiss = (bsbdNone, bsbdModal, bsbdAlert);

  // TIWBSButton.ButtonType
  TIWBSButtonType = (bsbtButton, bsbtSubmit, bsbtReset);

  // TIWBSButton.ElementTypeType
  TIWBSButtonElementType = (bsetAuto, bsetAnchor, bsetButton);

  // TIWBSButton.AsyncClickProc
  TIWBSAsyncEventProc = reference to procedure(Sender: TObject; EventParams: TStringList);

  // Bootstrap Button @br
  // http://getbootstrap.com/css/#buttons @br
  // http://www.w3schools.com/bootstrap/bootstrap_buttons.asp
  TDWButton = class(TDWCustomButton)
  private
    FAsyncClickProc: TIWBSAsyncEventProc;
    FButtonType: TIWBSButtonType;
    FDataDismiss: TIWBSButtonDataDismiss;
    FDataParent: TDWContainer;
    FDataTarget: TDWContainer;
    FElementType: TIWBSButtonElementType;
    FHotKey: string;
    FHref: string;
    FTarget: string;
    FImageSrc: string;
    FImagePosition: TDWBtnImagePosition;

    procedure DoAsyncClickProc(Sender: TObject; EventParams: TStringList);
    procedure SetAsyncClickProc(Value: TIWBSAsyncEventProc);
    function IsHrefStored: Boolean;
    function IsTargetStored: Boolean;
    function IsAnchor: Boolean;
    procedure SetDataTarget(const Value: TDWContainer);
    procedure SetDataParent(const Value: TDWContainer);
    procedure SetHref(const Value: string);
    procedure SetTarget(const Value: string);
    procedure SetDataDismiss(const Value: TIWBSButtonDataDismiss);
    procedure SetImageSrc(const Value: string);
    procedure SetImagePosition(const Value: TDWBtnImagePosition);
    function GetDataToggle: string;
    procedure SetElementType(const Value: TIWBSButtonElementType);
  protected
    procedure InternalRenderCss(var ACss: string); override;
    procedure InternalRenderHTML(var AHTMLTag: TDWElementTag); override;
  public
    constructor Create(AOwner: TComponent); override;
    // Anonymous procedure that let you execute code when button is pressed without the need of declare an event. @br
    // Usefull when you create buttons at runtime
    property AsyncClickProc: TIWBSAsyncEventProc read FAsyncClickProc write SetAsyncClickProc;
  published
    // Button type
    property ButtonType: TIWBSButtonType read FButtonType write FButtonType default bsbtButton;
    // Used when button in placed on a TIWBSModal or TIWBSAlert. @br
    // Let the button automatically close the dialog.
    property DataDismiss: TIWBSButtonDataDismiss read FDataDismiss write SetDataDismiss default bsbdNone;
    // Specifies the parent region off collapsable regions to autoclose other regions when one is toggled. See accordion example. @br
    // http://getbootstrap.com/javascript/#collapse-example-accordion @br
    // http://www.w3schools.com/bootstrap/bootstrap_collapse.asp
    property DataParent: TDWContainer read FDataParent write SetDataParent;
    // Specifies the target region for for toggle visiblity of TIWBSModal or TIWBSRegion.Collapse @br
    // http://www.w3schools.com/bootstrap/bootstrap_modal.asp @br
    // http://www.w3schools.com/bootstrap/bootstrap_collapse.asp
    property DataTarget: TDWContainer read FDataTarget write SetDataTarget;
    // Toogle operation
    property DataToggle: string read GetDataToggle;
    // acceskey tag atribute @br
    // http://www.w3schools.com/tags/att_global_accesskey.asp
    property HotKey: string read FHotkey write FHotKey;
    // this property determines if the element will be type anchor or button, default is Auto
    property ElementType: TIWBSButtonElementType read FElementType write SetElementType default bsetAuto;
    // Destination address to jump when button is pressed. @br
    // Requires following property values: Anchor = true, DataTarget = nil, OnAsyncClic = nil. @br
    // http://www.w3schools.com/html/html_links.asp
    property Href: string read FHref write SetHref stored IsHrefStored;
    // The target attribute specifies where to open the linked document. Apply when Href is used. @br
    // http://www.w3schools.com/html/html_links.asp
    property Target: string read FTarget write SetTarget stored IsTargetStored;
    property ImageSrc:string read FImageSrc write SetImageSrc;
    property ImagePosition:TDWBtnImagePosition read FImagePosition write SetImagePosition;
  end;

const
  // @exclude
  aIWBSButtonStyle: array[bsbsDefault..bsbsClose] of string = ('btn-default', 'btn-primary', 'btn-success', 'btn-info', 'btn-warning', 'btn-danger', 'btn-link', 'close');

implementation
  uses DW.VCL.Region, DW.VCL.Common, DW.VCL.ButtonGroup, DW.VCL.NavBar,
  DWUtils, DW.VCL.CustomRegion, DW.VCL.InputForm, DW.VCL.Modal, DW.VCL.List;



{$region 'TIWBSCustomButton'}
constructor TDWCustomButton.Create(AOwner: TComponent);
begin
  inherited;
  FButtonSize := bsszDefault;
  FButtonStyle := bsbsDefault;
  FCaption := '';
  FGlyphicon := '';
  //FNeedsFormTag := True;
  Height := 25;
  Width := 200;
end;

procedure TDWCustomButton.SetBlockLevel(const Value: boolean);
begin
  FBlockLevel := Value;
  Invalidate;
end;

procedure TDWCustomButton.SetButtonStyle(const Value: TDWButtonStyle);
begin
  FButtonStyle := Value;
  Invalidate;
end;

procedure TDWCustomButton.SetCaption(const Value: string);
begin
  FCaption := Value;
  AsyncRefreshControl;
end;

procedure TDWCustomButton.SetGlyphicon(const Value: string);
begin
  FGlyphicon := Value;
  Invalidate;
end;
procedure TDWCustomButton.SetRawText(const Value: boolean);
begin
  FRawText := Value;
  AsyncRefreshControl;
end;
{$endregion}

{$region 'TIWBSButton'}
constructor TDWButton.Create(AOwner: TComponent);
begin
  inherited;
  FButtonType := bsbtButton;
  FDataDismiss := bsbdNone;
  FElementType := bsetAuto;
  FHotKey := '';
  FHref := '#';
  FTarget := '';
end;

procedure TDWButton.InternalRenderCss(var ACss: string);
begin
  inherited;

  if (Parent is TDWRegion) and (TDWRegion(Parent).BSRegionType = bsrtListGroup) then
    begin
      TDWBSCommon.AddCssClass(ACss, 'list-group-item');
    end
  else if (FDataTarget <> nil) and (FDataTarget is TDWNavBarCollapse) then
    begin
      TDWBSCommon.AddCssClass(ACss, 'navbar-toggle');
    end
  else if (Parent is TDWButtonGroup) or not IsAnchor then
    begin
      TDWBSCommon.AddCssClass(ACss, 'btn');
      if FButtonSize <> bsszDefault then
        TDWBSCommon.AddCssClass(ACss, 'btn-'+aDWSize[FButtonSize]);
      TDWBSCommon.AddCssClass(ACss, aIWBSButtonStyle[FButtonStyle]);
      if FBlockLevel then
        TDWBSCommon.AddCssClass(ACss, 'btn-block');

      if (Parent is TDWNavBarBase) then
        TDWBSCommon.AddCssClass(ACss, 'navbar-btn')
      else if (Parent is TDWRegion) and (TDWRegion(Parent).BSRegionType = bsrtDropDown) then
        TDWBSCommon.AddCssClass(ACss, 'dropdown-toggle');
    end;
end;

procedure TDWButton.InternalRenderHTML(var AHTMLTag: TDWElementTag);
const
  aIWBSButtonDataDismiss: array[bsbdNone..bsbdAlert] of string = ('', 'modal', 'alert');
var
  s: string;
  lAnchor: boolean;
  lTarget: string;
begin
  inherited;

  lAnchor := IsAnchor;

  AHTMLTag := TDWElementTag.CreateHTMLTag(iif(lAnchor,'a','button'));
  try
    AHTMLTag.AddStringParam('id', HTMLName);
    AHTMLTag.AddClassParam(ActiveCss);

    // button type
    if not lAnchor then
      if FButtonType = bsbtButton then
        AHTMLTag.AddStringParam('type', 'button')
      else if FButtonType = bsbtSubmit then
        AHTMLTag.AddStringParam('type', 'submit')
      else if FButtonType = bsbtReset then
        AHTMLTag.AddStringParam('type', 'reset');

    if ShowHint and (Hint <> '') then
      AHTMLTag.AddStringParam('title', Hint);

    if IsDisabled then
      AHTMLTag.Add('disabled');

    AHTMLTag.AddStringParam('style', ActiveStyle);

    if TabIndex <> 0 then
      AHTMLTag.AddStringParam('tabindex', IntToStr(TabIndex));

    // caption
    if FRawText then
      s := Caption
    else
      s := TDWBSCommon.TextToHTML(Caption);

    // hotkey
    if FHotKey <> '' then begin
      AHTMLTag.AddStringParam('accesskey', FHotKey);
      s := StringReplace(s, FHotKey, '<u>' + FHotKey + '</u>', [rfIgnoreCase]);
    end;

    // glyphicon
    if FGlyphicon <> '' then
      with AHTMLTag.Contents.AddElement('span') do begin
        AddClassParam('glyphicon glyphicon-'+FGlyphicon);
        AddBooleanParam('aria-hidden',true);
        s := ' ' + s;
      end;

    // close button
    if (FButtonStyle = bsbsClose) then begin
      AHTMLTag.AddStringParam('aria-label', 'Close');
      if (s = '') and (FGlyphicon = '') then
        s := '&times;';
    end;

    // data-dismiss
    if FDataDismiss <> bsbdNone then
      AHTMLTag.AddStringParam('data-dismiss', aIWBSButtonDataDismiss[FDataDismiss]);

    // datatarget / href
    if FDataTarget = nil then
      begin
        if lAnchor then begin
          AHTMLTag.AddStringParam('href', FHref);
          if FTarget = '' then
            if AnsiStartsStr('#', FHref) then
              lTarget := '_self'
            else
              lTarget := '_blank'
          else
            lTarget := FTarget;
          AHTMLTag.AddStringParam('target', lTarget);
        end;
      end
    else
      begin
        if lAnchor then
          AHTMLTag.AddStringParam('href', '#'+FDataTarget.HTMLName)
        else
          AHTMLTag.AddStringParam('data-target', '#'+FDataTarget.HTMLName);

        if FDataParent <> nil then
          AHTMLTag.AddStringParam('data-parent', '#'+FDataParent.HTMLName);

        // draw a menu button if no caption and no glyphicon
        if (s = '') and (FGlyphicon = '') then begin
          AHTMLTag.Contents.AddElement('span').AddClassParam('icon-bar');
          AHTMLTag.Contents.AddElement('span').AddClassParam('icon-bar');
          AHTMLTag.Contents.AddElement('span').AddClassParam('icon-bar');
        end;
      end;

    // datatoggle
    AHTMLTag.AddStringParam('data-toggle', DataToggle);

    // caption after glyphicon
    if (s <> '') and (FImageSrc = '') then
      AHTMLTag.Contents.AddText(s)
    else
      if FImagePosition = bsbtimgLeft then
        begin
          //image before Caption
          if FImageSrc <> '' then
            AHTMLTag.Contents.AddText('<img src="' + FImageSrc + '"></img>'
            );
           AHTMLTag.Contents.AddText(s);
        end
      else if FImagePosition = bsbtimgRight then
        begin
          //image After Caption
          AHTMLTag.Contents.AddText(s);
          if FImageSrc <> '' then
            AHTMLTag.Contents.AddText('<img src="' + FImageSrc + '"></img>'
            );
        end;
  except
    FreeAndNil(AHTMLTag);
    raise;
  end;

  if Parent is TDWInputGroup then
    AHTMLTag := DWCreateInputGroupAddOn(AHTMLTag, HTMLName, 'btn')
  else
    AHTMLTag := DWCreateFormGroup(Parent, DWFindParentInputForm(Parent), AHTMLTag, HTMLName, True);

  // wrap item if parent is list
  TDWList.WrapItem(Self, AHTMLTag);
end;

function TDWButton.IsAnchor: Boolean;
begin
  if FElementType = bsetAuto then
    begin
      if FDataTarget <> nil then
        Result := False
      else if FHref <> '#' then
        Result := True
      else if (Parent is TDWNavBarBase) then
        Result := False
      else if (Parent is TDWList) then
        Result := True
      else if (Parent is TDWRegion) and (TDWRegion(Parent).BSRegionType = bsrtListGroup) then
        Result := True
      else if (Parent is TDWButtonGroup) and TDWButtonGroup(Parent).BSJustified then
        Result := True
      else
        Result := False;
    end
  else
    Result := FElementType = bsetAnchor;
end;

function TDWButton.IsHrefStored: Boolean;
begin
  Result := FHref <> '#';
end;

function TDWButton.IsTargetStored: Boolean;
begin
  Result := FTarget <> '';
end;

procedure TDWButton.DoAsyncClickProc(Sender: TObject; EventParams: TStringList);
begin
  FAsyncClickProc(Sender, EventParams);
end;

function TDWButton.GetDataToggle: string;
begin
  if (Parent is TDWRegion) and (TDWRegion(Parent).BSRegionType = bsrtDropDown) then
    Result := 'dropdown'
  else if (FDataTarget = nil) then
    Result := ''
  else if (FDataTarget is TDWModal) then
    Result := 'modal'
  else if (FDataTarget is TDWCustomRegion) and TDWCustomRegion(FDataTarget).Collapse then
    Result := 'collapse'
  else if (FDataTarget is TDWNavBarCollapse) then
    Result := 'collapse';
end;

procedure TDWButton.SetAsyncClickProc(Value: TIWBSAsyncEventProc);
begin
  FAsyncClickProc := Value;
  OnAsyncClick := DoAsyncClickProc
end;
procedure TDWButton.SetHref(const Value: string);
begin
  FHref := Value;
  AsyncRefreshControl;
end;

procedure TDWButton.SetImagePosition(const Value: TDWBtnImagePosition);
begin
  FImagePosition := Value;
end;

procedure TDWButton.SetImageSrc(const Value: string);
begin
  FImageSrc := Value;
end;

procedure TDWButton.SetTarget(const Value: string);
begin
  FTarget := Value;
  AsyncRefreshControl;
end;

procedure TDWButton.SetDataDismiss(const Value: TIWBSButtonDataDismiss);
begin
  FDataDismiss := Value;
  AsyncRefreshControl;
end;

procedure TDWButton.SetDataParent(const Value: TDWContainer);
begin
  FDataParent := Value;
  AsyncRefreshControl;
end;

procedure TDWButton.SetDataTarget(const Value: TDWContainer);
begin
  FDataTarget := Value;
  AsyncRefreshControl;
end;
procedure TDWButton.SetElementType(const Value: TIWBSButtonElementType);
begin
  FElementType := Value;
  AsyncRefreshControl;
end;

{$endregion}

end.
