unit DWElementTag;

interface

uses Classes, System.SysUtils, DWMarkupLinguageTag, DWRenderStream;

type
  TDWElementTagCollection = class;

  TDWElementTag = class(TDWCustomElement)
  private
    FTag: string;
    FParams: TStrings;
    FContent: TDWElementTagCollection;
  protected
    procedure SetParentElement (const Value: TDWCustomElement); override;
  public
    constructor CreateHTMLTag(ATag: string; aParentTag: TDWElementTag = nil; aCloseTag:TDWClosingTag = ctdwAuto); virtual;
    Destructor Destroy; override;
    procedure AddClassParam(aClassName: string);
    procedure AddStringParam(aParamName: string; aParamValue: String);
    procedure AddIntegerParam(aParamName: string; aParamValue: Integer);
    procedure AddBooleanParam(aParamName: string; aParamValue: Boolean);
    procedure Render(ABuffer: TDWStream); overload; override;
    function Render: String; overload; override;
    procedure Clear;
    property Content: TDWElementTagCollection read FContent;
    property Params:TStrings read FParams;
  end;


  TDWElementText = class(TDWCustomElement)
  private
    FText: String;
  protected
    procedure SetParentElement(const Value: TDWCustomElement); override;
    procedure RenderElement(ABuffer: TDWStream;  AIndent: Integer = 0); override;
  public
    procedure Assign (ASource: TPersistent); override;
    procedure Render(ABuffer: TDWStream); overload; override;
    function Render: String; overload; override;
    procedure Clear;
    property Text:string read FText write FText;
  end;

  TDWElementBinary = class(TDWCustomElement)
  private
    FBuffer: TDWStream;
  protected
    procedure SetParentElement(const Value: TDWCustomElement); override;
    procedure RenderElement(ABuffer: TDWStream;  AIndent: Integer = 0); override;
    procedure InitializeElement (AParentElement: TDWCustomElement); override;
  public
    destructor Destroy; override;
    procedure Render(ABuffer: TDWStream); overload; override;
    function Render: String; overload; override;
    procedure Clear;
    property Buffer:TDWStream read FBuffer   write FBuffer;
  end;

  TDWElementTagCollection = class(Classes.TList)
  private
    FParentElement: TDWCustomElement;
  protected
    function GetHTMLElement(AIndex: Integer): TDWCustomElement;
  public
    constructor Create(AParentElement: TDWCustomElement); virtual;
    destructor Destroy; override;
    procedure Clear; override;
    procedure Assign(ASource: TDWElementTagCollection);
    function AddElemetAsObject(const ATag: TDWCustomElement; const ASetParentElement: Boolean = True): TDWCustomElement;
    function AddElement(const ATag: String; const ASetParentElement: Boolean = True): TDWElementTag;
    function AddText(const AText: String;  const ASetParentElement: Boolean = True): TDWElementText;
    function AddBuffer(const ABuffer: TDWStream; const ASetParentElement: Boolean): TDWElementBinary;
    procedure AddElementCollection(AElementCollection: TDWElementTagCollection; AFreeSourceCollection: Boolean; const ASetParentElement: Boolean);
    property Items[AIndex: Integer]: TDWCustomElement read GetHTMLElement; default;
  end;

  TDWElementXHTMLTag = class(TDWElementTag)
  private
    FCDATA: Boolean;
    procedure SetCDATA(const Value: Boolean);
  public
    constructor CreateHTMLTag(ATag: string; aParentTag: TDWElementTag = nil; AsCDATA:Boolean = True);
    property CDATA:Boolean read FCDATA write SetCDATA default True;
  end;


implementation

{ TDWHTMLTag }

procedure TDWElementTag.AddBooleanParam(aParamName: string; aParamValue: Boolean);
begin
  if aParamValue then
    FParams.Values[aParamName] := '"true"'
  else
    FParams.Values[aParamName] := '"false"';
  Changed;
end;

procedure TDWElementTag.AddClassParam(aClassName: string);
begin
  FParams.Values['class'] := FParams.Values['class'] + ' ' + aClassName;
  Changed;
end;

procedure TDWElementTag.AddIntegerParam(aParamName: string; aParamValue: Integer);
begin
  FParams.Values[aParamName] := InttoStr(aParamValue);
  Changed;
end;

procedure TDWElementTag.AddStringParam(aParamName, aParamValue: String);
begin
  FParams.Values[aParamName] := '"' + aParamValue + '"';
  Changed;
end;

procedure TDWElementTag.Clear;
begin
  FParams.Clear;
  FContent.Clear;
  Changed;
end;

constructor TDWElementTag.CreateHTMLTag(ATag: string; aParentTag: TDWElementTag = nil; aCloseTag:TDWClosingTag = ctdwAuto);
begin
  inherited Create(aParentTag);
  FTag                       := ATag;
  FParams                    := TStringList.Create;
  FParams.NameValueSeparator := '=';
  FParams.Delimiter          := ' ';
  FParams.StrictDelimiter    := True;
  FParams.QuoteChar          := #0;
  FContent:= TDWElementTagCollection.Create(Self);
 // FContent                   := '';
end;

destructor TDWElementTag.Destroy;
begin
  FParams.Free;
  inherited;
end;

procedure TDWElementTag.Render(ABuffer: TDWStream);
begin

end;

function TDWElementTag.Render: String;
var
  LContent:string;
  I: Integer;
begin
  LContent:='';
  for I := 0 to FContent.Count -1 do
    LContent:= LContent + FContent[I].Render;

  Result := '<' + FTag;

  if FParams.Count > 0 then
     Result := Result + ' ' + FParams.DelimitedText;

  if (FClosingTag = ctdwAuto)  then
  { TODO 1 -oDELCIO -cIMPLEMENTAR : change for for tags of img, br, etc }
    Result := Result + '>' + LContent + '</' + FTag + '>';
  if FClosingTag = ctdwTrue then
    Result := Result + '>' + LContent + '</' + FTag + '>';
  if FClosingTag = ctdwFalse then
  { TODO 1 -oDELCIO -cIMPLEMENTAR : change for for tags of img, br, etc }
    Result := Result + '>' + LContent;
  if FClosingTag = ctdwSimpleClose then
    Result := Result + ' ' + LContent + '>';
end;


procedure TDWElementTag.SetParentElement(const Value: TDWCustomElement);
var
  OldIndex:Integer;
begin
  //remove from Older Parent Content List
  if (ParentElement <> nil)
  and (ParentElement is TDWElementTag) then
    begin
      //Check if it's already in the content list of Parent
      OldIndex:= (ParentElement as TDWElementTag).Content.IndexOf(Self);
      if OldIndex > -1 then
        //Remove from Content list
        (ParentElement as TDWElementTag).Content.Delete(OldIndex);
    end;
  //Set ParentElement Property
  inherited;
  //add to new Parent Content List
  if (ParentElement <> nil)
  and (ParentElement is TDWElementTag) then
    begin
      //Check if it's not already in the content list
      if (ParentElement as TDWElementTag).Content.IndexOf(Self) < 0 then
        //Add to Content list
        (ParentElement as TDWElementTag).Content.AddElemetAsObject(Self, False);
    end;
end;

{ TDWElementTagCollection }

function TDWElementTagCollection.AddBuffer(const ABuffer: TDWStream;
  const ASetParentElement: Boolean): TDWElementBinary;
var
  EL:TDWElementBinary;
begin
  EL:= TDWElementBinary.Create(nil);
  EL.Buffer.CopyFrom(ABuffer, 0);
  if ASetParentElement then
    EL.ParentElement:= FParentElement;
  Result:= EL;
end;

function TDWElementTagCollection.AddElement(const ATag: String; const ASetParentElement: Boolean = True)
  : TDWElementTag;
var
  EL:TDWElementTag;
begin
  EL:= TDWElementTag.CreateHTMLTag(ATag);
  inherited Add(EL);
  if ASetParentElement then
    EL.ParentElement:= FParentElement;
  Result:= EL;
end;

procedure TDWElementTagCollection.AddElementCollection(AElementCollection: TDWElementTagCollection;
  AFreeSourceCollection: Boolean; const ASetParentElement: Boolean);
begin

end;

function TDWElementTagCollection.AddElemetAsObject(const ATag: TDWCustomElement;
  const ASetParentElement: Boolean = True): TDWCustomElement;
begin
  inherited Add(ATag);
  if ASetParentElement then
    ATag.ParentElement:= FParentElement;
  Result:= ATag;
end;

function TDWElementTagCollection.AddText(const AText: String; const ASetParentElement: Boolean = True)
  : TDWElementText;
var
  EL:TDWElementText;
begin
  if AText <> '' then
    begin
      EL:= TDWElementText.Create(nil);
      EL.Text:= AText;
      inherited Add(EL);
      if ASetParentElement then
        EL.ParentElement:= FParentElement;
      Result:= EL;
    end;
end;

procedure TDWElementTagCollection.Assign(ASource: TDWElementTagCollection);
begin
  inherited Assign(ASource, laCopy);
end;

procedure TDWElementTagCollection.Clear;
begin
  inherited Clear;

end;

constructor TDWElementTagCollection.Create(AParentElement: TDWCustomElement);
begin
  inherited Create;
  FParentElement:= AParentElement;
end;

destructor TDWElementTagCollection.Destroy;
begin

  inherited;
end;

function TDWElementTagCollection.GetHTMLElement(AIndex: Integer): TDWCustomElement;
begin
  Result:= TDWCustomElement(inherited Get(AIndex));
end;

{ TDWElementText }

procedure TDWElementText.Assign(ASource: TPersistent);
begin
  inherited;

end;

procedure TDWElementText.Render(ABuffer: TDWStream);
begin

end;

function TDWElementText.Render: String;
begin
  Result := Text;
end;

procedure TDWElementText.RenderElement(ABuffer: TDWStream; AIndent: Integer);
begin


end;

procedure TDWElementText.SetParentElement(const Value: TDWCustomElement);
var
  OldIndex:Integer;
begin
  //remove from Older Parent Content List
  if (ParentElement <> nil)
  and (ParentElement is TDWElementTag) then
    begin
      //Check if it's already in the content list of Parent
      OldIndex:= (ParentElement as TDWElementTag).Content.IndexOf(Self);
      if OldIndex > 0 then
        //Remove from Content list
        (ParentElement as TDWElementTag).Content.Delete(OldIndex);
    end;
  //Set ParentElement Property
  inherited;
  //add to new Parent Content List
  if (ParentElement <> nil)
  and (ParentElement is TDWElementTag) then
    begin
      //Check if it's not already in the content list
      if (ParentElement as TDWElementTag).Content.IndexOf(Self) < 0 then
        //Add to Content list
        (ParentElement as TDWElementTag).Content.AddElemetAsObject(Self, False);
    end;
end;

procedure TDWElementText.Clear;
begin
  FText:= '';
  Changed;
end;

{ TDWElementBinary }

destructor TDWElementBinary.Destroy;
begin

  inherited;
end;

procedure TDWElementBinary.InitializeElement(AParentElement: TDWCustomElement);
begin
//  inherited;

end;

procedure TDWElementBinary.Render(ABuffer: TDWStream);
begin

end;

function TDWElementBinary.Render: String;
begin
  Result:= FBuffer.DataString;
end;

procedure TDWElementBinary.RenderElement(ABuffer: TDWStream; AIndent: Integer);
begin
//  inherited;

end;

procedure TDWElementBinary.SetParentElement(const Value: TDWCustomElement);
var
  OldIndex:Integer;
begin
  //remove from Older Parent Content List
  if (ParentElement <> nil)
  and (ParentElement is TDWElementTag) then
    begin
      //Check if it's already in the content list of Parent
      OldIndex:= (ParentElement as TDWElementTag).Content.IndexOf(Self);
      if OldIndex > 0 then
        //Remove from Content list
        (ParentElement as TDWElementTag).Content.Delete(OldIndex);
    end;
  //Set ParentElement Property
  inherited;
  //add to new Parent Content List
  if (ParentElement <> nil)
  and (ParentElement is TDWElementTag) then
    begin
      //Check if it's not already in the content list
      if (ParentElement as TDWElementTag).Content.IndexOf(Self) < 0 then
        //Add to Content list
        (ParentElement as TDWElementTag).Content.AddElemetAsObject(Self, False);
    end;
end;

procedure TDWElementBinary.Clear;
begin
  FBuffer.Clear;
  Changed;
end;

{ TDWElementXHTMLTag }

constructor TDWElementXHTMLTag.CreateHTMLTag(ATag: string;
  aParentTag: TDWElementTag; AsCDATA: Boolean);
begin
  inherited CreateHTMLTag(ATag, aParentTag, ctdwAuto);
  FCDATA:= AsCDATA;
end;

procedure TDWElementXHTMLTag.SetCDATA(const Value: Boolean);
begin
  FCDATA := Value;
end;

end.
