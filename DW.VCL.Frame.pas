unit DW.VCL.Frame;

interface

uses Classes, VCL.Controls, DW.VCL.Region, DWElementTag;

type

  TDWCustomFrameClass = class of TDWCustomFrame;

  TDWCustomFrame = class(TDWRegion)
  private
    FOnDestroy: TNotifyEvent;
    FOnCreate: TNotifyEvent;
    procedure SetOnCreate(const Value: TNotifyEvent);
    procedure SetOnDestroy(const Value: TNotifyEvent);
  protected
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OnCreate: TNotifyEvent read FOnCreate write SetOnCreate;
    property OnDestroy: TNotifyEvent read FOnDestroy write SetOnDestroy;
  end;

  TDWFrame = class(TDWCustomFrame)

  end;

implementation

uses
  DW.VCL.Common, System.RTLConsts;

{ TDWFrame }

procedure TDWCustomFrame.AfterConstruction;
begin
  inherited;
  if Assigned(FOnCreate) then
    FOnCreate(Self);
end;

procedure TDWCustomFrame.BeforeDestruction;
begin
  inherited;
  if Assigned(FOnDestroy) then
    FOnDestroy(Self);
end;

constructor TDWCustomFrame.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csAcceptsControls];
  if (ClassType <> TDWCustomFrame) and not(csDesignInstance in ComponentState) then
    begin
      if not InitInheritedComponent(Self, TDWCustomFrame) then
        raise EResNotFound.CreateFmt(SResNotFound, [ClassName]);
    end
  else
    begin
      Width  := 320;
      Height := 240;
    end;
end;

destructor TDWCustomFrame.Destroy;
begin
  inherited;
end;

procedure TDWCustomFrame.SetOnCreate(const Value: TNotifyEvent);
begin
  FOnCreate := Value;
end;

procedure TDWCustomFrame.SetOnDestroy(const Value: TNotifyEvent);
begin
  FOnDestroy := Value;
end;

end.
