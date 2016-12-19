unit DW.VCL.List;

interface

uses
  SysUtils, Classes, Controls, StrUtils, DW.VCL.CustomRegion, DW.VCL.Control,
  DWElementTag;

type
  TIWBSListType = (bsltNone, bsltDropDownMenu, bsltGroup, bsltInline, bsltNav, bsltPager,
    bsltPagination, bsltPaginationLg, bsltPaginationSm, bsltBreadcrumb);

const
  aIWBSListType: array [bsltNone .. bsltBreadcrumb] of string = ('', 'dropdown-menu', 'list-group',
    'list-inline', 'nav navbar-nav', 'pager', 'pagination', 'pagination pagination-lg',
    'pagination pagination-sm', 'breadcrumb');

type
  TDWList = class(TDWCustomRegion)
  private
    FListType: TIWBSListType;
    procedure SetListType(const Value: TIWBSListType);
  protected
    procedure InternalRenderCss(var ACss: string); override;
    procedure SetParent(AParent: TWinControl); override;
  public
    constructor Create(AOwner: TComponent); override;

    class procedure WrapItem(AControl: TDWControl; var AHTMLTag: TDWElementTag);
  published
    property BSListType: TIWBSListType read FListType write SetListType default bsltNone;
  end;

implementation

uses
  DW.VCL.Common, DW.VCL.NavBar, DW.VCL.Region, DWTypes;

constructor TDWList.Create(AOwner: TComponent);
begin
  inherited;
  FTagName := 'ul';
end;

procedure TDWList.InternalRenderCss(var ACss: string);
begin
  TDWBSCommon.AddCssClass(ACss, aIWBSListType[FListType]);
  inherited;
end;

procedure TDWList.SetListType(const Value: TIWBSListType);
begin
  if Parent is TDWNavBarBase then
    FListType := bsltNav
  else
    FListType := Value;
  Invalidate;
end;

procedure TDWList.SetParent(AParent: TWinControl);
begin
  inherited;
  if (AParent is TDWNavBarBase) then
    FListType := bsltNav
  else if (AParent is TDWRegion) and (TDWRegion(Parent).BSRegionType = bsrtDropDown) then
    FListType := bsltDropDownMenu;
end;

class procedure TDWList.WrapItem(AControl: TDWControl; var AHTMLTag: TDWElementTag);
var
  xHTMLTag: TDWElementTag;
begin
  if AControl.Parent is TDWList then
    begin
      xHTMLTag := TDWElementTag.CreateHTMLTag('li');

      case TDWList(AControl.Parent).BSListType of
        bsltGroup:
          xHTMLTag.AddClassParam('list-group-item');
      end;

      xHTMLTag.Contents.AddElemetAsObject(AHTMLTag);
      AHTMLTag := xHTMLTag;
    end;
end;

end.
