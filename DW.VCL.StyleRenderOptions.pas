unit DW.VCL.StyleRenderOptions;

interface
  uses Classes;


type

  TDWRenderOptions = class(TComponent)
  protected
    FRenderOptions : array [0..9] of Boolean;
    procedure SetRenderOptions (Index: Integer; const Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign (Source: TPersistent); override;
  published
    property RenderSize:Boolean index 0 read FRenderOptions write SetRenderOptions default True;
    property RenderPosition:Boolean index 1 read FRenderOptions  write SetRenderOptions default True;
    property RenderFont:Boolean index 2 read FRenderOptions write SetRenderOptions default True;
    property RenderZIndex:Boolean index 3 read FRenderOptions  write SetRenderOptions default True;
    property RenderVisibility:Boolean index 4 read FRenderOptions write SetRenderOptions default True;
    property RenderStatus:Boolean index 5 read FRenderOptions write SetRenderOptions default True;
    property RenderAbsolute:Boolean index 6 read FRenderOptions write SetRenderOptions default True;
    property RenderPadding:Boolean index 7 read FRenderOptions write SetRenderOptions default True;
    property RenderBorder:Boolean index 8 read FRenderOptions  write SetRenderOptions default True;
    property UseDisplay:Boolean index 9 read FRenderOptions write SetRenderOptions default False;
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

procedure TDWRenderOptions.SetRenderOptions(Index: Integer;
  const Value: Boolean);
begin
  if FRenderOptions[Index] <> Value then
    begin
      FRenderOptions[Index]:= Value;
      TDWControl(Owner).Invalidate;
    end;
end;

end.
