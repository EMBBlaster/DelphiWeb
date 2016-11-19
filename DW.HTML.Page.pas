unit DW.HTML.Page;

interface

uses System.Classes, DWElementTag;

type
  TDWHTMLTag = class;

  TDWHeadTag = class(TDWElementTag)
  private
    FTitle: TDWElementTag;
    FInitScript: TDWElementTag;
    procedure Prepare;
  public
    constructor Create(aHTMLTAG: TDWHTMLTag); reintroduce;
    procedure Clear;
    property Title: TDWElementTag read FTitle;
    property InitScript: TDWElementTag read FInitScript;
  end;

  TDWBodyTag = class(TDWElementTag)
  private
    procedure Prepare;
  public
    constructor Create(aHTMLTAG: TDWHTMLTag); reintroduce;
    procedure Clear;
  end;

  TDWHTMLTag = class(TDWElementTag)
  private
    FDWHeadTag: TDWHeadTag;
    FDWBodyTag: TDWBodyTag;
    procedure SetBodyTag(const Value: TDWBodyTag);
    procedure SetHeadTag(const Value: TDWHeadTag);
  public
    constructor Create(AOwner: TComponent); reintroduce;
    procedure Clear;
    property HeadTag: TDWHeadTag read FDWHeadTag write SetHeadTag;
    property BodyTag: TDWBodyTag read FDWBodyTag write SetBodyTag;
  end;

  TDWHTMLPage = class(TComponent)
  private
    FDocType: string;
    FHTMLTAG: TDWHTMLTag;
    procedure SetHTMLTag(const Value: TDWHTMLTag);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Clear;
    property DocType: string read FDocType write FDocType;
    property HTMLTag: TDWHTMLTag read FHTMLTAG write SetHTMLTag;

  end;

implementation

{ TDWHTMLPage }

procedure TDWHTMLPage.Clear;
begin
  FHTMLTAG.Clear;
end;

constructor TDWHTMLPage.Create(AOwner: TComponent);
begin
  inherited;
  FDocType := '<!DOCTYPE HTML>';
  FHTMLTAG := TDWHTMLTag.Create(Self);
end;

procedure TDWHTMLPage.SetHTMLTag(const Value: TDWHTMLTag);
begin
  FHTMLTAG := Value;
end;

{ TDWHTMLTag }

procedure TDWHTMLTag.Clear;
begin
  FDWHeadTag.Clear;
  FDWBodyTag.Clear;
end;

constructor TDWHTMLTag.Create(AOwner: TComponent);
begin
  inherited CreateHTMLTag('html');
  FDWHeadTag := TDWHeadTag.Create(Self);
  FDWBodyTag := TDWBodyTag.Create(Self);
  Self.Content.AddElemetAsObject(FDWHeadTag);
  Self.Content.AddElemetAsObject(FDWBodyTag);
end;

procedure TDWHTMLTag.SetBodyTag(const Value: TDWBodyTag);
begin
  FDWBodyTag := Value;
end;

procedure TDWHTMLTag.SetHeadTag(const Value: TDWHeadTag);
begin
  FDWHeadTag := Value;
end;

{ TDWBodyTag }

procedure TDWBodyTag.Clear;
begin
  inherited Clear;
  Prepare;
end;

constructor TDWBodyTag.Create(aHTMLTAG: TDWHTMLTag);
begin
  inherited CreateHTMLTag('body', aHTMLTAG);
end;

procedure TDWBodyTag.Prepare;
begin
  AddStringParam('onload', 'Initialize();');
  AddStringParam('onunload', 'Finalize();');
end;

{ TDWHeadTag }

procedure TDWHeadTag.Clear;
begin
  FTitle.Clear;
  FInitScript.Clear;
  inherited Clear;
  Prepare;
end;

constructor TDWHeadTag.Create(aHTMLTAG: TDWHTMLTag);
begin
  inherited CreateHTMLTag('head', aHTMLTAG);
  FTitle      := TDWElementTag.CreateHTMLTag('title');
  FInitScript := TDWElementTag.CreateHTMLTag('script');
  Clear;
end;

procedure TDWHeadTag.Prepare;
begin
  Content.AddElemetAsObject(FTitle, true);
  Content.AddElemetAsObject(FInitScript, true);
  FInitScript.Content.AddText('function Initialize() {', true);
  FInitScript.Content.AddText('   DW.initDW();', true);
  FInitScript.Content.AddText('   DWInitComponents();', true);
  FInitScript.Content.AddText('};', true);
end;

end.
