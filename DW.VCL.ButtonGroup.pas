unit DW.VCL.ButtonGroup;

interface

uses
  SysUtils, Classes, Controls, StrUtils,
  DW.VCL.CustomRegion, DWTypes;

type
  TDWButtonGroup = class(TDWCustomRegion)
  private
    FVertical: boolean;
    FJustified: boolean;
    FSize: TDWSize;
    procedure SetJustified(const Value: boolean);
    procedure SetSize(const Value: TDWSize);
    procedure SetVertical(const Value: boolean);
  protected
    procedure InternalRenderCss(var ACss: string); override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetRoleString: string; override;
  published
    property BSVertical: boolean read FVertical write SetVertical default false;
    property BSJustified: boolean read FJustified write SetJustified default false;
    property BSButtonSize: TDWSize read FSize write SetSize default bsszDefault;
  end;

implementation

uses DW.VCL.NavBar, DW.VCL.Common;

constructor TDWButtonGroup.Create(AOwner: TComponent);
begin
  inherited;
  FVertical  := false;
  FJustified := false;
  FSize      := bsszDefault;
end;

function TDWButtonGroup.GetRoleString: string;
begin
  Result := 'group';
end;

procedure TDWButtonGroup.InternalRenderCss(var ACss: string);
begin
  if FVertical then
    TDWBSCommon.AddCssClass(ACss, 'btn-group-vertical')
  else
    TDWBSCommon.AddCssClass(ACss, 'btn-group');
  if FSize <> bsszDefault then
    TDWBSCommon.AddCssClass(ACss, 'btn-group-' + aDWSize[FSize]);
  if FJustified then
    TDWBSCommon.AddCssClass(ACss, 'btn-group-justified');
  if Parent is TDWNavBarBase then
    TDWBSCommon.AddCssClass(ACss, 'navbar-btn');
  inherited;
end;

procedure TDWButtonGroup.SetJustified(const Value: boolean);
begin
  FJustified := Value;
  AsyncRefreshControl;
end;

procedure TDWButtonGroup.SetSize(const Value: TDWSize);
begin
  FSize := Value;
  AsyncRefreshControl;
end;

procedure TDWButtonGroup.SetVertical(const Value: boolean);
begin
  FVertical := Value;
  AsyncRefreshControl;
end;

end.
