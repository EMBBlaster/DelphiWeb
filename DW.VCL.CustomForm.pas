unit DW.VCL.CustomForm;

interface

uses Classes, Forms, Controls, DW.HTML.Page, DWRenderStream, DWCallbacks,
  DW.VCL.Container;

type

  TDWCustomForm = class(TDWModuleContainer)
  private
    FHTMLPage: TDWHTMLPage;
    FCaption: string;
    procedure SetCaption(const Value: string);

  public
    constructor Create(AOwner: TComponent); override;
    function HTMLPage: TDWHTMLPage;
    procedure ExecuteCallBack(aParams: TStringList; aCallBack: TDWCallBack); virtual; abstract;
    Function Render: TDWStream; virtual; abstract;
  published
    property Caption: string read FCaption write SetCaption;
  end;

implementation

{ TDWCustomForm }

constructor TDWCustomForm.Create(AOwner: TComponent);
begin
  inherited;
  FHTMLPage := TDWHTMLPage.Create(self);
  // to avoid store FHTMLPage to dfm
  if csDesigning in ComponentState then
    RemoveComponent(FHTMLPage);
  FHTMLPage.Name := 'HTMLPage';
  ControlStyle   := ControlStyle + [csAcceptsControls];
end;

function TDWCustomForm.HTMLPage: TDWHTMLPage;
begin
  Result := FHTMLPage;
end;

procedure TDWCustomForm.SetCaption(const Value: string);
begin
  FCaption := Value;
end;

end.
