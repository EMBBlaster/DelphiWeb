unit DW.VCL.StyleRenderOptions;

interface

uses Classes;

type

  TDWRenderOptions = class(TComponent)
  protected
    FRenderOptions: array [0 .. 9] of Boolean;
    procedure SetRenderOptions(Index: Integer; const Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
  published
    property RenderSize: Boolean index 0 read FRenderOptions[0] write SetRenderOptions
      default False;
    property RenderPosition: Boolean index 1 read FRenderOptions[1] write SetRenderOptions
      default False;
    property RenderFont: Boolean index 2 read FRenderOptions[2] write SetRenderOptions
      default False;
    property RenderZIndex: Boolean index 3 read FRenderOptions[3] write SetRenderOptions
      default False;
    property RenderVisibility: Boolean index 4 read FRenderOptions[4] write SetRenderOptions
      default False;
    property RenderStatus: Boolean index 5 read FRenderOptions[5] write SetRenderOptions
      default False;
    property RenderAbsolute: Boolean index 6 read FRenderOptions[6] write SetRenderOptions
      default False;
    property RenderPadding: Boolean index 7 read FRenderOptions[7] write SetRenderOptions
      default False;
    property RenderBorder: Boolean index 8 read FRenderOptions[8] write SetRenderOptions
      default False;
    property UseDisplay: Boolean index 9 read FRenderOptions[9] write SetRenderOptions
      default False;
  end;

implementation

uses DW.VCL.Control;

{ TDWRenderOptions }

procedure TDWRenderOptions.Assign(Source: TPersistent);
begin
  inherited;

end;

constructor TDWRenderOptions.Create(AOwner: TComponent);
begin
  inherited;

end;

procedure TDWRenderOptions.SetRenderOptions(Index: Integer; const Value: Boolean);
begin
  if FRenderOptions[Index] <> Value then
    begin
      FRenderOptions[Index] := Value;
      TDWControl(Owner).Invalidate;
    end;
end;

end.
