unit DW.VCL.CustomForm;

interface

uses Classes, Forms, Controls, DW.HTML.Page, DWRenderStream, DWCallbacks,
  DW.VCL.Container;

type

  TDWCustomForm = class(TDWModuleContainer)
  private
    FHTMLPage: TDWHTMLPage;
    FCaption: string;
    FOnRender: TNotifyEvent;
    FOnShow: TNotifyEvent;
    procedure SetCaption(const Value: string);
    procedure SetOnRender(const Value: TNotifyEvent);
    procedure SetOnShow(const Value: TNotifyEvent);
  protected
    procedure DoOnRender; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function HTMLPage: TDWHTMLPage;
    procedure ExecuteCallBack(aParams: TStringList; aCallBack: TDWCallBack); virtual; abstract;
    Function Render: TDWStream; virtual;
    procedure Show;
  published
    property Caption: string read FCaption write SetCaption;
    property OnRender: TNotifyEvent read FOnRender write SetOnRender;
    property OnShow: TNotifyEvent read FOnShow write SetOnShow;

  end;

implementation

uses
  System.RTLConsts, DWUtils;

{ TDWCustomForm }

constructor TDWCustomForm.Create(AOwner: TComponent);
begin
  inherited;
  if (ClassType <> TDWCustomForm) and not(csDesignInstance in ComponentState) then
    begin
      if not InitInheritedComponent(Self, TDWCustomForm) then
        raise EResNotFound.CreateFmt(SResNotFound, [ClassName]);
    end
  else
    begin
      Width  := 320;
      Height := 240;
    end;
  FHTMLPage := TDWHTMLPage.Create(Self);
  // to avoid store FHTMLPage to dfm
  if csDesigning in ComponentState then
    RemoveComponent(FHTMLPage);
  FHTMLPage.Name := 'HTMLPage';
  ControlStyle   := ControlStyle + [csAcceptsControls];
  DoOnCreate;
end;

destructor TDWCustomForm.Destroy;
begin
  inherited;
end;

function TDWCustomForm.HTMLPage: TDWHTMLPage;
begin
  Result := FHTMLPage;
end;

function TDWCustomForm.Render: TDWStream;
begin
  DoOnRender;
end;

procedure TDWCustomForm.SetCaption(const Value: string);
begin
  FCaption := Value;
end;

procedure TDWCustomForm.SetOnRender(const Value: TNotifyEvent);
begin
  FOnRender := Value;
end;

procedure TDWCustomForm.SetOnShow(const Value: TNotifyEvent);
begin
  FOnShow := Value;
end;

procedure TDWCustomForm.Show;
begin
  DWApplication.ActiveForm := Self;
  DWApplication.AddForm(Self);
  if Assigned(FOnShow) then
    FOnShow(Self);
end;

procedure TDWCustomForm.DoOnRender;
begin
  if Assigned(FOnRender) then
    FOnRender(Self);
end;

end.
