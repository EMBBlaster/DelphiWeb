unit DW.VCL.NavBar;

interface

uses
  SysUtils, Classes, StrUtils, DW.VCL.CustomRegion, DWElementTag;

type

  TDWNavBarFixed = (bsnvfxNone, bsnvfxTop, bsnvfxBottom);

  TDWNavBarBase = class(TDWCustomRegion);

  TDWNavBar = class(TDWNavBarBase)
  private
    FFluid: boolean;
    FFixed: TDWNavBarFixed;
    FInverse: boolean;
  protected
    procedure InternalRenderCss(var ACss: string); override;
  public
    constructor Create(AOwner: TComponent); override;
    function RenderHTML: TDWElementTag; override;
  published
    property BSFluid: boolean read FFluid write FFluid default False;
    property BSInverse: boolean read FInverse write FInverse default False;
    property BSFixed: TDWNavBarFixed read FFixed write FFixed default bsnvfxNone;
  end;

  TDWNavBarHeader = class(TDWNavBarBase)
  protected
    procedure InternalRenderCss(var ACss: string); override;
  end;

  TDWNavBarCollapse = class(TDWNavBarBase)
  protected
    procedure InternalRenderCss(var ACss: string); override;
  end;

implementation
  uses
    DW.VCL.Common, DWUtils;



{ TDWNavBar }

constructor TDWNavBar.Create(AOwner: TComponent);
begin
  inherited;
  FFluid := False;
  FFixed := bsnvfxNone;
  FInverse := False;
  FTagName := 'nav';
end;

procedure TDWNavBar.InternalRenderCss(var ACss: string);
begin
  TDWBSCommon.AddCssClass(ACss, 'navbar navbar-'+iif(FInverse,'inverse', 'default'));
  if FFixed = bsnvfxTop then
    TDWBSCommon.AddCssClass(ACss, 'navbar-fixed-top')
  else if FFixed = bsnvfxBottom then
    TDWBSCommon.AddCssClass(ACss, ' navbar-fixed-bottom');
  inherited;
end;

function TDWNavBar.RenderHTML: TDWElementTag;
begin
  Result := Inherited;

  FRegionDiv := Result.Contents.AddElement('div');
  FRegionDiv.AddClassParam('container'+iif(FFluid, '-fluid', ''));
end;

{ TDWNavBarHeader }

procedure TDWNavBarHeader.InternalRenderCss(var ACss: string);
begin
  TDWBSCommon.AddCssClass(ACss, 'navbar-header');
  inherited;
end;

{ TDWNavBarCollapse }

procedure TDWNavBarCollapse.InternalRenderCss(var ACss: string);
begin
  TDWBSCommon.AddCssClass(ACss, 'navbar-collapse');
  TDWBSCommon.AddCssClass(ACss, 'collapse');
  inherited;
end;

end.
