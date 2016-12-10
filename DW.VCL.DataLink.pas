unit DW.VCL.DataLink;

interface
  uses
    Classes, DB, DW.VCL.Control;
  type
    TDWDatalink = class(TDataLink)
    private
    FControl: TDWControl;
    procedure UpdateControlState;
  protected
    procedure ActiveChanged; override;
    procedure EditingChanged; override;
    procedure LayoutChanged; override;
    procedure DataEvent (Event: TDataEvent; Info:NativeInt); override;
    property Control:TDWControl read FControl;
  public
    constructor Create (AControl: TDWControl); reintroduce;
  end;

implementation

{ TDWDatalink }

procedure TDWDatalink.ActiveChanged;
begin
  UpdateControlState;
  inherited;
end;

constructor TDWDatalink.Create(AControl: TDWControl);
begin
  inherited Create;
  FControl:= AControl;
end;

procedure TDWDatalink.DataEvent(Event: TDataEvent; Info: NativeInt);
begin
  inherited;
end;

procedure TDWDatalink.EditingChanged;
begin
  UpdateControlState;
  inherited;
end;

procedure TDWDatalink.LayoutChanged;
begin
  UpdateControlState;
  inherited;
end;

procedure TDWDatalink.UpdateControlState;
begin
  if FControl <> nil then
    FControl.Invalidate;
end;

end.
