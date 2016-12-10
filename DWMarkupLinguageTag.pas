unit DWMarkupLinguageTag;

interface

uses Classes, System.SysUtils, DWRenderStream;

type
  TDWClosingTag = (ctdwAuto, ctdwFalse, ctdwTrue, ctdwSimpleClose);

  TDWCustomElement = class(TPersistent)
  private
    FParentElement: TDWCustomElement;
    FDestroying: System.Boolean;
    FChanged: Boolean;
  protected
    FClosingTag: TDWClosingTag;
    procedure SetParentElement(const Value: TDWCustomElement); virtual;
    procedure RenderElement(ABuffer: TDWStream; AIndent: Integer = 0); virtual;
    procedure InitializeElement(AParentElement: TDWCustomElement); virtual;
  public
    constructor Create(AParentElement: TDWCustomElement); virtual;
    destructor Destroy; override;
    procedure Assign(ASource: Classes.TPersistent); override;
    procedure Render(ABuffer: TDWStream); overload; virtual;
    function Render: String; overload; virtual;
    procedure Changed(aValue: Boolean = True); virtual;
    property Destroying: System.Boolean read FDestroying;
    property ParentElement: TDWCustomElement read FParentElement write SetParentElement;
  end;

implementation

{ TDWMarkupLanguageElement }

procedure TDWCustomElement.Assign(ASource: Classes.TPersistent);
begin
  if ASource <> Self then
    begin
      inherited Assign(ASource);
      Changed;
    end;
end;

procedure TDWCustomElement.Changed(aValue: Boolean = True);
begin
  FChanged := aValue;
end;

constructor TDWCustomElement.Create(AParentElement: TDWCustomElement);
begin
  FChanged       := True;
  FParentElement := AParentElement;
end;

destructor TDWCustomElement.Destroy;
begin

  inherited;
end;

procedure TDWCustomElement.InitializeElement(AParentElement: TDWCustomElement);
begin
  raise Exception.Create('Render not Implemented for TDWCustomElement, use descendant class');
end;

procedure TDWCustomElement.Render(ABuffer: TDWStream);
begin
  raise Exception.Create('Render not Implemented for TDWCustomElement, use descendant class');
end;

function TDWCustomElement.Render: String;
begin
  raise Exception.Create('Render not Implemented for TDWCustomElement, use descendant class');
end;

procedure TDWCustomElement.RenderElement(ABuffer: TDWStream; AIndent: Integer = 0);
begin
  raise Exception.Create
    ('RenderElement not Implemented for TDWCustomElement, use descendant class');
end;

procedure TDWCustomElement.SetParentElement(const Value: TDWCustomElement);
begin
  if FParentElement <> Value then
    begin
      FParentElement := Value;
      Changed;
    end;
end;

end.
