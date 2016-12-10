unit DW.HTML.Page;

interface

uses System.Classes, DWElementTag;

type
  TDWHTMLTag = class;

  TDWHeadTag = class(TDWElementTag)
  private
    FTitle: TDWElementTag;
    FInitScript: TDWElementTag;
    FFinScript:TDWElementTag;
    FInitComponents: TStrings;
    procedure Prepare;
    procedure SetInitComponents(const Value: TStrings);
  public
    constructor Create(aHTMLTAG: TDWHTMLTag); reintroduce;
    destructor Destroy; override;
    function Render: String; overload; override;
    procedure Clear;
    property Title: TDWElementTag read FTitle;
    property InitScript: TDWElementTag read FInitScript;
    property InitComponents:TStrings read FInitComponents write SetInitComponents;
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
  FComponentStyle := FComponentStyle + [csTransient];
  FComponentStyle := FComponentStyle - [csSubComponent];
  FDocType        := '<!DOCTYPE HTML>';
  FHTMLTAG        := TDWHTMLTag.Create(Self);
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
  Self.Contents.AddElemetAsObject(FDWHeadTag);
  Self.Contents.AddElemetAsObject(FDWBodyTag);
end;

constructor TDWHTMLTag.Create(AOwner: TComponent);
begin
  inherited CreateHTMLTag('html');
  FDWHeadTag := TDWHeadTag.Create(Self);
  FDWBodyTag := TDWBodyTag.Create(Self);
  Self.Contents.AddElemetAsObject(FDWHeadTag);
  Self.Contents.AddElemetAsObject(FDWBodyTag);
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
  FFinScript.Clear;
  FInitComponents.Clear;
  inherited Clear;
  Prepare;
end;

constructor TDWHeadTag.Create(aHTMLTAG: TDWHTMLTag);
begin
  inherited CreateHTMLTag('head', aHTMLTAG);
  FTitle      := TDWElementTag.CreateHTMLTag('title');
  FInitScript := TDWElementTag.CreateHTMLTag('script');
  FFinScript:=TDWElementTag.CreateHTMLTag('script');
  FInitComponents:= TStringList.Create;
  Clear;
end;

destructor TDWHeadTag.Destroy;
begin
  FTitle.Free;
  FInitScript.Free;
  FInitComponents.Free;
  inherited;
end;

procedure TDWHeadTag.Prepare;
begin
  Contents.AddElemetAsObject(FTitle, true);
  Contents.AddElemetAsObject(FInitScript, true);
  Contents.AddElemetAsObject(FFinScript, true);
  //Initialize
  FInitScript.Contents.AddText('function Initialize() {', true);
  FInitScript.Contents.AddText('   DW.initDW();', true);
  FInitScript.Contents.AddText('   DWInitComponents();', true);
  FInitScript.Contents.AddText('};', true);
  //Finalize
  FFinScript.Contents.AddText('function Finalize() {', true);
  FFinScript.Contents.AddText('};', true);
end;

function TDWHeadTag.Render: String;
begin
  //Add components to be initiated in function DWInitComponents on InitScript
  FInitScript.Contents.AddText('function DWInitComponents() {' + #10#13);
  FInitScript.Contents.AddText(FInitComponents.Text);
  FInitScript.Contents.AddText(#10#13 + '};');
  //Render the tag and childs
  Result:= inherited Render;
end;

procedure TDWHeadTag.SetInitComponents(const Value: TStrings);
begin
  FInitComponents.Assign(Value);
end;

end.
