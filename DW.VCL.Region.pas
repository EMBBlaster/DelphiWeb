unit DW.VCL.Region;

interface
  uses
    Classes, DW.VCL.CustomRegion, DW.VCL.Common, DWTypes;



type
  TDWRegion = class(TDWCustomRegion)
  private
    FBackground: TDWRegionBack;
    FRegionType: TDWRegionType;
    FTagType: TDWRegionTagType;
    procedure SetRegionType(AValue: TDWRegionType);
    procedure SetBackground(AValue: TDWRegionBack);
    procedure SetTagType(const Value: TDWRegionTagType);
  protected
    procedure InternalRenderCss(var ACss: string); override;
  public
    function GetRoleString: string; override;
  published
    property BSBackground: TDWRegionBack read FBackground write SetBackground default bsrbDefault;
    property BSRegionType: TDWRegionType read FRegionType write SetRegionType default bsrtNone;
    property TagType: TDWRegionTagType read FTagType write SetTagType default bsttDiv;
  end;

implementation

uses DW.VCL.NavBar;

procedure TDWRegion.InternalRenderCss(var ACss: string);
begin
  TDWBSCommon.AddCssClass(ACss, aDWRegionType[FRegionType]);

  if (FRegionType = bsrtPanel) then
    TDWBSCommon.AddCssClass(ACss, 'panel-'+aDWRegionBack[FBackground])
  else if (FRegionType in [bsrtWell, bsrtWellLarge, bsrtWellSmall]) and (FBackground <> bsrbDefault) then
    TDWBSCommon.AddCssClass(ACss, 'well-'+aDWRegionBack[FBackground])
  else if (FBackground <> bsrbDefault)  then
    TDWBSCommon.AddCssClass(ACss, 'bg-'+aDWRegionBack[FBackground])
  else if (Parent is TDWNavBar) then
    if TagType = bsttDiv then
      TDWBSCommon.AddCssClass(ACss, 'navbar-btn')
    else
      TDWBSCommon.AddCssClass(ACss, 'navbar-text');

  inherited;
end;

function TDWRegion.GetRoleString: string;
begin
  if FRegionType = bsrtButtonToolbar then
    Result := 'toolbar'
  else
    Result := '';
end;

procedure TDWRegion.SetRegionType(AValue: TDWRegionType);
begin
  FRegionType := AValue;
  Invalidate;
end;

procedure TDWRegion.SetTagType(const Value: TDWRegionTagType);
begin
  FTagType := Value;
  FTagName := aDWRegionTagType[Value];
  AsyncRefreshControl;
end;

procedure TDWRegion.SetBackground(AValue: TDWRegionBack);
begin
  FBackground := AValue;
  Invalidate;
end;

end.
