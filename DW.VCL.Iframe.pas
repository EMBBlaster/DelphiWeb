unit DW.VCL.Iframe;

interface

uses Classes, DWElementTag, DW.VCL.CustomRegion;

type
  TDWIFrame = class(TDWCustomRegion)
  private
    FSrc: string;
    procedure SetSrc(const Value: string);
  protected
    FOldSrc: string;
    function RenderAsync: TDWElementXHTMLTag; override;
    function RenderHTML: TDWElementTag; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Src: string read FSrc write SetSrc;

  end;

implementation

uses
  DW.VCL.Common;

{ TDWIFrame }

constructor TDWIFrame.Create(AOwner: TComponent);
begin
  inherited;
  FTagName := 'iframe'
end;

destructor TDWIFrame.Destroy;
begin

  inherited;
end;

function TDWIFrame.RenderAsync: TDWElementXHTMLTag;
var
  xHTMLName: string;
begin
  Result    := nil;
  xHTMLName := HTMLName;

  if (FAsyncRefreshControl) or (not FRendered) or (FOldSrc <> FSrc) then
    begin
      TDWRegionCommon.CancelChildAsyncRender(Self);
      TDWBSCommon.RenderAsync(xHTMLName, Self);
    end
  else
    begin
      TDWBSCommon.SetAsyncClass(xHTMLName, RenderCSSClass, FOldCss);
      TDWBSCommon.SetAsyncStyle(xHTMLName, RenderStyle, FOldStyle);
      TDWBSCommon.SetAsyncVisible(FMainID, Visible, FOldVisible);

      if Assigned(OnAfterAsyncChange) then
        OnAfterAsyncChange(Self);
      { TODO 1 -oDELCIO -cIMPLEMENT : Global event OnAfterAsyncChange }
      { if Assigned(gIWBSOnAfterAsyncChange) then
        gIWBSOnAfterAsyncChange(Self, xHTMLName); }
    end;
end;

function TDWIFrame.RenderHTML: TDWElementTag;
begin
  FOldCss     := RenderCSSClass;
  FOldStyle   := RenderStyle;
  FOldVisible := Visible;
  FOldSrc     := FSrc;

  FRegionDiv := TDWElementTag.CreateHTMLTag(FTagName);
  FRegionDiv.AddStringParam('id', HTMLName);
  FRegionDiv.AddClassParam(FOldCss);
  FRegionDiv.AddStringParam('src', FOldSrc);
  // FRegionDiv.AddStringParam('role',GetRoleString);
  FRegionDiv.AddStringParam('style', RenderStyle);

  TDWBSCommon.RenderScript(Self, FRegionDiv);
  FMainID := FRegionDiv.Params.Values['id'];

  Result := FRegionDiv;

  FAsyncRefreshControl := False;
  FRendered            := True;
end;

procedure TDWIFrame.SetSrc(const Value: string);
begin
  if FSrc <> Value then
    begin
      FSrc := Value;
      AsyncRefreshControl;
    end;
end;

end.
