unit DW.VCL.Labels;

interface

uses Classes, SysUtils, Db, DW.VCL.DBControl, DW.VCL.Control, DWElementTag;

type
  TIWBSLabelStyle = (bslsNone, bslsDefault, bslsPrimary, bslsSuccess, bslsInfo, bslsWarning,
    bslsDanger, bslsBadget);

const
  aIWBSLabelStyle: array [bslsNone .. bslsBadget] of string = ('', 'default', 'primary', 'success',
    'info', 'warning', 'danger', 'badge');

type
  TDWLabel = class(TDWCustomDbControl)
  private
    FCaption: string;
    FForControl: TDWInputControl;
    FRawText: boolean;
    FOldText: string;
    FTagType: string;
    FLabelStyle: TIWBSLabelStyle;
    function RenderLabelText: string;
    procedure SetTagType(const Value: string);
    function IsTagTypeStored: boolean;
    procedure SetLabelStyle(const Value: TIWBSLabelStyle);
    procedure SetCaption(const Value: string);
    procedure SetRawText(const Value: boolean);
  protected
    procedure CheckData; override;
    procedure InternalRenderAsync(const AHTMLName: string); override;
    procedure InternalRenderCss(var ACss: string); override;
    procedure InternalRenderHTML(var AHTMLTag: TDWElementTag); override;
    procedure SetForControl(const Value: TDWInputControl);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Caption: string read FCaption write SetCaption;
    property ForControl: TDWInputControl read FForControl write SetForControl;
    property BSLabelStyle: TIWBSLabelStyle read FLabelStyle write SetLabelStyle default bslsNone;
    property RawText: boolean read FRawText write SetRawText default False;
    property TagType: string read FTagType write SetTagType stored IsTagTypeStored;
  end;

  TDWText = class(TDWCustomDbControl)
  private
    FAutoFormGroup: boolean;
    FLines: TStringList;
    FRawText: boolean;
    FOldText: string;
    FTagType: string;
    function RenderText: string;
    procedure OnLinesChange(ASender: TObject);
    procedure SetLines(const AValue: TStringList);
    function IsTagTypeStored: boolean;
    procedure SetTagType(const Value: string);
    procedure SetRawText(const Value: boolean);
    procedure SetAutoFormGroup(const Value: boolean);
  protected
    procedure CheckData; override;
    procedure InternalRenderAsync(const AHTMLName: string); override;
    procedure InternalRenderHTML(var AHTMLTag: TDWElementTag); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AutoFormGroup: boolean read FAutoFormGroup write SetAutoFormGroup default False;
    property Lines: TStringList read FLines write SetLines;
    property RawText: boolean read FRawText write SetRawText default False;
    property TagType: string read FTagType write SetTagType stored IsTagTypeStored;
  end;

  TIWBSGlyphicon = class(TDWControl)
  private
    FGlyphicon: string;
  protected
    procedure InternalRenderCss(var ACss: string); override;
    procedure InternalRenderHTML(var AHTMLTag: TDWElementTag); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property BSGlyphicon: string read FGlyphicon write FGlyphicon;
  end;

  TIWBSFile = class(TDWControl)
  private
    FMultiple: boolean;
    procedure SetMultiple(const Value: boolean);
  protected
    procedure InternalRenderHTML(var AHTMLTag: TDWElementTag); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Multiple: boolean read FMultiple write SetMultiple default False;
  end;

implementation

uses
  DW.VCL.Common, DW.VCL.List, DW.VCL.Region, DWTypes, DW.VCL.InputForm, DWUtils;

{$REGION 'TIWBSLabel'}

constructor TDWLabel.Create(AOwner: TComponent);
begin
  inherited;
  FRawText := False;
  FTagType := 'span';
  Height   := 25;
  Width    := 200;
end;

procedure TDWLabel.SetCaption(const Value: string);
begin
  FCaption := Value;
  Invalidate;
end;

procedure TDWLabel.SetForControl(const Value: TDWInputControl);
begin
  FForControl := Value;
  AsyncRefreshControl;
end;

procedure TDWLabel.SetLabelStyle(const Value: TIWBSLabelStyle);
begin
  FLabelStyle := Value;
  Invalidate;
end;

procedure TDWLabel.SetRawText(const Value: boolean);
begin
  FRawText := Value;
  Invalidate;
end;

procedure TDWLabel.SetTagType(const Value: string);
begin
  TDWBSCommon.ValidateTagName(Value);
  FTagType := Value;
  AsyncRefreshControl;
end;

function TDWLabel.RenderLabelText: string;
begin
  if FRawText then
    Result := Caption
  else
    Result := TDWBSCommon.TextToHTML(Caption);
end;

procedure TDWLabel.InternalRenderAsync(const AHTMLName: string);
begin
  inherited;
  TDWBSCommon.SetAsyncHtml(AHTMLName, RenderLabelText, FOldText);
end;

procedure TDWLabel.InternalRenderCss(var ACss: string);
begin
  inherited;

  if FLabelStyle <> bslsNone then
    TDWBSCommon.AddCssClass(ACss, aIWBSLabelStyle[FLabelStyle]);

  if Parent is TDWList then
    begin
      TDWBSCommon.AddCssClass(ACss, 'list-group-item');
      if FLabelStyle in [bslsSuccess, bslsInfo, bslsWarning, bslsDanger] then
        TDWBSCommon.AddCssClass(ACss, 'list-group-item-' + aIWBSLabelStyle[FLabelStyle])
    end
  else
    begin
      if FLabelStyle in [bslsDefault .. bslsDanger] then
        TDWBSCommon.AddCssClass(ACss, 'label label-' + aIWBSLabelStyle[FLabelStyle])
      else if FLabelStyle = bslsBadget then
        TDWBSCommon.AddCssClass(ACss, aIWBSLabelStyle[FLabelStyle]);
      if Parent is TDWRegion then
        begin
          if TDWRegion(Parent).BSRegionType = bsrtModalHeader then
            TDWBSCommon.AddCssClass(ACss, 'modal-title')
          else if TDWRegion(Parent).BSRegionType = bsrtPanelHeading then
            TDWBSCommon.AddCssClass(ACss, 'panel-title');
        end;
    end;
end;

procedure TDWLabel.InternalRenderHTML(var AHTMLTag: TDWElementTag);
begin
  inherited;
  FOldText := RenderLabelText;

  if Assigned(FForControl) then
    begin
      AHTMLTag := TDWElementTag.CreateHTMLTag('label');
      AHTMLTag.AddStringParam('for', ForControl.HTMLName);
    end
  else if Parent is TDWList then
    begin
      AHTMLTag := TDWElementTag.CreateHTMLTag('li');
    end
  else
    begin
      AHTMLTag := TDWElementTag.CreateHTMLTag(FTagType);
    end;
  AHTMLTag.AddStringParam('id', HTMLName);
  AHTMLTag.AddClassParam(ActiveCss);
  AHTMLTag.AddStringParam('style', ActiveStyle);
  AHTMLTag.Contents.AddText(FOldText);

  if Parent is TDWInputGroup then
    AHTMLTag := DWCreateInputGroupAddOn(AHTMLTag, HTMLName, 'addon');
end;

function TDWLabel.IsTagTypeStored: boolean;
begin
  Result := FTagType <> 'span';
end;

procedure TDWLabel.CheckData;
var
  LField: TField;
begin
  if CheckDataSource(DataSource, DataField, LField) then
    Caption := LField.DisplayText;
end;
{$ENDREGION}
{$REGION 'TIWBSText'}

constructor TDWText.Create(AOwner: TComponent);
begin
  inherited;
  FLines                   := TStringList.Create;
  FLines.TrailingLineBreak := False;
  FLines.OnChange          := OnLinesChange;
  FRawText                 := False;
  FTagType                 := 'div';
  Height                   := 100;
  Width                    := 200;
end;

destructor TDWText.Destroy;
begin
  FLines.Free;
  inherited;
end;

procedure TDWText.OnLinesChange(ASender: TObject);
begin
  Invalidate;
  if Script.Count > 0 then
    AsyncRefreshControl;
end;

procedure TDWText.SetAutoFormGroup(const Value: boolean);
begin
  FAutoFormGroup := Value;
  AsyncRefreshControl;
end;

procedure TDWText.SetLines(const AValue: TStringList);
begin
  FLines.Assign(AValue);
end;

procedure TDWText.SetRawText(const Value: boolean);
begin
  FRawText := Value;
  Invalidate;
end;

procedure TDWText.SetTagType(const Value: string);
begin
  TDWBSCommon.ValidateTagName(Value);
  FTagType := Value;
  AsyncRefreshControl;
end;

function TDWText.RenderText: string;
var
  i: integer;
  LLines: TStringList;
begin
  if FRawText then
    begin
      LLines := TStringList.Create;
      try
        LLines.Assign(FLines);

        // replace params before custom events
        LLines.Text := TDWBSCommon.ReplaceParams(Self, LLines.Text);

        (*
          { TODO 1 -oDELCIO -cIMPLEMENT :  CustomAsyncEvents}
          // replace inner events calls
          if IsStoredCustomAsyncEvents then
          for i := 0 to CustomAsyncEvents.Count-1 do
          TIWBSCustomAsyncEvent(CustomAsyncEvents.Items[i]).ParseParam(LLines);

          { TODO 1 -oDELCIO -cIMPLEMENT :  CustomRestEvents}
          // replace inner events calls
          if IsStoredCustomRestEvents then
          for i := 0 to CustomRestEvents.Count-1 do
          TIWBSCustomRestEvent(CustomRestEvents.Items[i]).ParseParam(LLines);
        *)
        Result := LLines.Text;
      finally
        LLines.Free;
      end;
    end
  else
    Result := TDWBSCommon.TextToHTML(Lines.Text);
end;

procedure TDWText.InternalRenderAsync(const AHTMLName: string);
begin
  inherited;
  TDWBSCommon.SetAsyncHtml(AHTMLName, RenderText, FOldText);
end;

procedure TDWText.InternalRenderHTML(var AHTMLTag: TDWElementTag);
begin
  inherited;
  FOldText := RenderText;

  AHTMLTag := TDWElementTag.CreateHTMLTag(FTagType);
  AHTMLTag.AddStringParam('id', HTMLName);
  AHTMLTag.AddClassParam(ActiveCss);
  AHTMLTag.AddStringParam('style', ActiveStyle);
  AHTMLTag.Contents.AddText(FOldText);

  if FAutoFormGroup and not(Parent is TDWInputGroup) then
    AHTMLTag := DWCreateInputFormGroup(Self, Parent, AHTMLTag, Caption, HTMLName);
end;

function TDWText.IsTagTypeStored: boolean;
begin
  Result := FTagType <> 'div';
end;

procedure TDWText.CheckData;
var
  LField: TField;
begin
  if CheckDataSource(DataSource, DataField, LField) then
    Lines.Text := LField.DisplayText;
end;
{$ENDREGION}
{$REGION 'TIWBSGlyphicon'}

constructor TIWBSGlyphicon.Create(AOwner: TComponent);
begin
  inherited;
  Height := 25;
  Width  := 25;
end;

procedure TIWBSGlyphicon.InternalRenderCss(var ACss: string);
begin
  inherited;
  if FGlyphicon <> '' then
    TDWBSCommon.AddCssClass(ACss, 'glyphicon glyphicon-' + FGlyphicon);
end;

procedure TIWBSGlyphicon.InternalRenderHTML(var AHTMLTag: TDWElementTag);
begin
  inherited;
  AHTMLTag := TDWElementTag.CreateHTMLTag('span');
  try
    AHTMLTag.AddStringParam('id', HTMLName);
    AHTMLTag.AddClassParam(ActiveCss);
    AHTMLTag.AddStringParam('style', ActiveStyle);
    if FGlyphicon <> '' then
      AHTMLTag.AddBooleanParam('aria-hidden', true)
    else
      AHTMLTag.Contents.AddText('&times;');
  except
    FreeAndNil(AHTMLTag);
    raise;
  end;
  if Parent is TDWInputGroup then
    AHTMLTag := DWCreateInputGroupAddOn(AHTMLTag, HTMLName, 'addon');
end;
{$ENDREGION}
{$REGION 'TIWBSFile' }

constructor TIWBSFile.Create(AOwner: TComponent);
begin
  inherited;
  FMultiple := False;
  Height    := 25;
  Width     := 121;
end;

procedure TIWBSFile.InternalRenderHTML(var AHTMLTag: TDWElementTag);
begin
  inherited;

  AHTMLTag := TDWElementTag.CreateHTMLTag('input');
  try
    AHTMLTag.AddClassParam(ActiveCss);
    AHTMLTag.AddStringParam('id', HTMLName);
    AHTMLTag.AddStringParam('name', HTMLName + iif(FMultiple, '[]', ''));
    AHTMLTag.AddStringParam('type', 'file');
    if ShowHint and (Hint <> '') then
      AHTMLTag.AddStringParam('title', Hint);
    if FMultiple then
      AHTMLTag.Add('multiple');

    // if AutoFocus then
    // AHTMLTag.Add('autofocus');
    // if IsReadOnly then
    // AHTMLTag.Add('readonly');
    if IsDisabled then
      AHTMLTag.Add('disabled');
    // AHTMLTag.AddStringParam('value', TextToHTML(FText));
    // if Required then
    // AHTMLTag.Add('required');
    // if PlaceHolder <> '' then
    // AHTMLTag.AddStringParam('placeholder', TextToHTML(PlaceHolder));
    AHTMLTag.AddStringParam('style', ActiveStyle);
  except
    FreeAndNil(AHTMLTag);
    raise;
  end;

  AHTMLTag := DWCreateFormGroup(Parent, DWFindParentInputForm(Parent), AHTMLTag, HTMLName, true);
end;

procedure TIWBSFile.SetMultiple(const Value: boolean);
begin
  FMultiple := Value;
  AsyncRefreshControl;
end;

end.
