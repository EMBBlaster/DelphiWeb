unit DW.VCL.DBControl;

interface
   uses
     Classes, System.SysUtils, DW.VCL.Control, DB, DWElementTag, DW.VCL.DataLink;

  type
  // Base class for DW data aware controls
  TDWCustomDbControl = class(TDWInputControl)
  private
    FMaxLength: Integer;
    procedure SetMaxLength(const AValue:integer);
  protected
    FDataLink: TDWDataLink;
    FDataField: string;
    FDataSource: TDataSource;
    procedure CheckData; virtual;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure SetDataField(const AValue: string); virtual;
    procedure SetDataSource(const Value: TDataSource); virtual;
    property MaxLength: Integer read FMaxLength write SetMaxLength;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure InternalRenderCss(var ACss: string); override;
    function RenderAsync: TDWElementXHTMLTag; override;
    function RenderHTML: TDWElementTag; override;
  published
    property DataSource: TDataSource read FDataSource write SetDataSource;
    property DataField: string read FDataField write SetDataField;
  end;



implementation
  uses
    DWUtils;
{ TIWBSCustomDbControl }

procedure TDWCustomDbControl.CheckData;
begin
  //
end;

constructor TDWCustomDbControl.Create(AOwner: TComponent);
begin
  inherited;
  FDataLink := nil;
  FDataField := '';
end;

destructor TDWCustomDbControl.Destroy;
begin
  FreeAndNil(FDataLink);
  inherited;
end;


procedure TDWCustomDbControl.InternalRenderCss(var ACss: string);
begin
  inherited;

end;

procedure TDWCustomDbControl.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited Notification(AComponent, AOperation);
  if AOperation = opRemove then
    if FDatasource = AComponent then
      SetDataSource(nil);
end;

function TDWCustomDbControl.RenderAsync: TDWElementXHTMLTag;
begin
  CheckData;
  Result := inherited;
end;

function TDWCustomDbControl.RenderHTML: TDWElementTag;
begin
  CheckData;
  Result := inherited;
end;

procedure TDWCustomDbControl.SetDataField(const AValue: string);
var
  xFld: TField;
begin
  if not SameText(AValue, FDataField) then begin
    FDataField := AValue;
    MaxLength := 0;
    if FDataField <> '' then begin
      xFld := GetDataSourceField(FDataSource, FDataField);
      if Assigned(xFld) and (xFld is TStringField) then
        MaxLength := TStringField(xFld).Size;
    end;
    Invalidate;
  end;
end;

procedure TDWCustomDbControl.SetDataSource(const Value: TDataSource);
begin
  if Value <> FDataSource then begin
    FDataSource := Value;
    if Value = nil then
      begin
        FDataField := '';
        FreeAndNil(FDataLink);
      end
    else
      begin
        if FDataLink = nil then
          FDataLink := TDWDataLink.Create(Self);
        FDataLink.DataSource := FDataSource;
      end;
    Invalidate;
  end;
end;

procedure TDWCustomDbControl.SetMaxLength(const AValue: integer);
begin
  if FMaxLength <> AValue then begin
    FMaxLength := AValue;
    AsyncRefreshControl;
  end;
end;

end.
