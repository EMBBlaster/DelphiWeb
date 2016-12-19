unit DWElementTag;

interface

uses Classes, System.SysUtils, DWMarkupLinguageTag, DWRenderStream;

type
  TDWElementTagCollection = class;

  TDWElementTag = class(TDWCustomElement)
  private
    FTag: string;
    FParams: TStrings;
    FNoValueAttributes: TStrings;
    FContents: TDWElementTagCollection;
  protected
    procedure SetParentElement(const Value: TDWCustomElement); override;
  public
    constructor CreateHTMLTag(ATag: string; aParentTag: TDWElementTag = nil;
      aCloseTag: TDWClosingTag = ctdwAuto); virtual;
    Destructor Destroy; override;
    procedure Add(aParamName: string);
    procedure AddClassParam(aClassName: string);
    procedure AddStringParam(aParamName: string; aParamValue: String; AllowEmpty: Boolean = False);
    procedure AddIntegerParam(aParamName: string; aParamValue: Integer);
    procedure AddBooleanParam(aParamName: string; aParamValue: Boolean);
    procedure Render(ABuffer: TDWStream); overload; override;
    function Render: String; overload; override;
    procedure Clear;
    property Contents: TDWElementTagCollection read FContents;
    property Params: TStrings read FParams;
  end;

  TDWElementText = class(TDWCustomElement)
  private
    FText: String;
  protected
    procedure SetParentElement(const Value: TDWCustomElement); override;
    procedure RenderElement(ABuffer: TDWStream; AIndent: Integer = 0); override;
  public
    procedure Assign(ASource: TPersistent); override;
    procedure Render(ABuffer: TDWStream); overload; override;
    function Render: String; overload; override;
    procedure Clear;
    property Text: string read FText write FText;
  end;

  TDWElementBinary = class(TDWCustomElement)
  private
    FBuffer: TDWStream;
  protected
    procedure SetParentElement(const Value: TDWCustomElement); override;
    procedure RenderElement(ABuffer: TDWStream; AIndent: Integer = 0); override;
    procedure InitializeElement(AParentElement: TDWCustomElement); override;
  public
    destructor Destroy; override;
    procedure Render(ABuffer: TDWStream); overload; override;
    function Render: String; overload; override;
    procedure Clear;
    property Buffer: TDWStream read FBuffer write FBuffer;
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
    function AddElemetAsObject(const ATag: TDWCustomElement;
      const ASetParentElement: Boolean = True): TDWCustomElement;
    function AddElement(const ATag: String; const ASetParentElement: Boolean = True): TDWElementTag;
    function AddText(const AText: String; const ASetParentElement: Boolean = True): TDWElementText;
    function AddBuffer(const ABuffer: TDWStream; const ASetParentElement: Boolean)
      : TDWElementBinary;
    procedure AddElementCollection(AElementCollection: TDWElementTagCollection;
      AFreeSourceCollection: Boolean; const ASetParentElement: Boolean);
    function AddHiddenField(aId: string; aName: string; aValue: string): TDWElementTag;
    property Items[AIndex: Integer]: TDWCustomElement read GetHTMLElement; default;
  end;

  TDWElementXHTMLTag = class(TDWElementTag)
  private
    FCDATA: Boolean;
    procedure SetCDATA(const Value: Boolean);
  public
    constructor CreateHTMLTag(ATag: string; aParentTag: TDWElementTag = nil;
      AsCDATA: Boolean = True); reintroduce;
    function Render: String; overload; override;
    property CDATA: Boolean read FCDATA write SetCDATA default True;
  end;

implementation

{ TDWHTMLTag }

procedure TDWElementTag.Add(aParamName: string);
begin
  FNoValueAttributes.Add(aParamName);
end;

procedure TDWElementTag.AddBooleanParam(aParamName: string; aParamValue: Boolean);
begin
  if aParamValue then
    FParams.Values[aParamName] := '"true"'
  else
    FParams.Values[aParamName] := '"false"';
  Changed;
end;

procedure TDWElementTag.AddClassParam(aClassName: string);
var
  Aux: string;
begin
  aClassName := Trim(aClassName);
  if aClassName <> '' then
    begin
      if FParams.Values['class'] <> '' then
        begin
          Aux := StringReplace(FParams.Values['class'], '"', '', [rfReplaceAll]);
          Aux := Trim(Aux);
          FParams.Values['class'] := '"' + Aux + ' ' + aClassName + '"';
        end
      else
        FParams.Values['class'] := '"' + aClassName + '"';
      Changed;
    end;
end;

procedure TDWElementTag.AddIntegerParam(aParamName: string; aParamValue: Integer);
begin
  FParams.Values[aParamName] := InttoStr(aParamValue);
  Changed;
end;

procedure TDWElementTag.AddStringParam(aParamName, aParamValue: String;
  AllowEmpty: Boolean = False);
var
  LIndex: Integer;
begin
  if (aParamValue = '') and (not AllowEmpty) then
    begin
      LIndex := FParams.IndexOf(aParamName);
      if LIndex > -1 then
        FParams.Delete(LIndex);
    end
  else
    FParams.Values[aParamName] := '"' + aParamValue + '"';
  Changed;
end;

procedure TDWElementTag.Clear;
begin
  FParams.Clear;
  FContents.Clear;
  Changed;
end;

constructor TDWElementTag.CreateHTMLTag(ATag: string; aParentTag: TDWElementTag = nil;
  aCloseTag: TDWClosingTag = ctdwAuto);
begin
  inherited Create(aParentTag);
  FTag                       := ATag;
  FParams                    := TStringList.Create;
  FNoValueAttributes         := TStringList.Create;
  FParams.NameValueSeparator := '=';
  FParams.Delimiter          := ' ';
  FParams.StrictDelimiter    := True;
  FParams.QuoteChar          := #0;
  FContents                  := TDWElementTagCollection.Create(Self);
  // FContent                   := '';
end;

destructor TDWElementTag.Destroy;
begin
  FNoValueAttributes.Free;
  FParams.Free;
  inherited;
end;

procedure TDWElementTag.Render(ABuffer: TDWStream);
begin

end;

function TDWElementTag.Render: String;
var
  LContent: string;
  I: Integer;
begin
  LContent   := '';
  for I      := 0 to FContents.Count - 1 do
    LContent := LContent + FContents[I].Render;

  Result := '<' + FTag;

  for I := 0 to FNoValueAttributes.Count - 1 do
    begin
      Result := Result + ' ' + FNoValueAttributes[I];
    end;

  if FParams.Count > 0 then
    Result := Result + ' ' + FParams.DelimitedText;

  if (FClosingTag = ctdwAuto) then
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
  OldIndex: Integer;
begin
  // remove from Older Parent Content List
  if (ParentElement <> nil) and (ParentElement is TDWElementTag) then
    begin
      // Check if it's already in the content list of Parent
      OldIndex := (ParentElement as TDWElementTag).Contents.IndexOf(Self);
      if OldIndex > -1 then
        // Remove from Content list
        (ParentElement as TDWElementTag).Contents.Delete(OldIndex);
    end;
  // Set ParentElement Property
  inherited;
  // add to new Parent Content List
  if (ParentElement <> nil) and (ParentElement is TDWElementTag) then
    begin
      // Check if it's not already in the content list
      if (ParentElement as TDWElementTag).Contents.IndexOf(Self) < 0 then
        // Add to Content list
        (ParentElement as TDWElementTag).Contents.AddElemetAsObject(Self, False);
    end;
end;

{ TDWElementTagCollection }

function TDWElementTagCollection.AddBuffer(const ABuffer: TDWStream;
  const ASetParentElement: Boolean): TDWElementBinary;
var
  EL: TDWElementBinary;
begin
  EL := TDWElementBinary.Create(nil);
  EL.Buffer.CopyFrom(ABuffer, 0);
  if ASetParentElement then
    EL.ParentElement := FParentElement;
  Result             := EL;
end;

function TDWElementTagCollection.AddElement(const ATag: String;
  const ASetParentElement: Boolean = True): TDWElementTag;
var
  EL: TDWElementTag;
begin
  EL := TDWElementTag.CreateHTMLTag(ATag);
  inherited Add(EL);
  if ASetParentElement then
    EL.ParentElement := FParentElement;
  Result             := EL;
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
    ATag.ParentElement := FParentElement;
  Result               := ATag;
end;

function TDWElementTagCollection.AddHiddenField(aId, aName, aValue: string): TDWElementTag;
begin
  Result := nil;
  if ((aId <> '') or (aName <> '')) then
    begin
      Result := TDWElementTag.CreateHTMLTag('input');
      if aId <> '' then
        Result.AddStringParam('id', aId);
      if aName <> '' then
        Result.AddStringParam('name', aName);
      Result.AddStringParam('type', 'hidden');
      inherited Add(Result);
      Result.ParentElement := FParentElement;
    end;
end;

function TDWElementTagCollection.AddText(const AText: String;
  const ASetParentElement: Boolean = True): TDWElementText;
var
  EL: TDWElementText;
begin
  Result := nil;
  if AText <> '' then
    begin
      EL      := TDWElementText.Create(nil);
      EL.Text := AText;
      inherited Add(EL);
      if ASetParentElement then
        EL.ParentElement := FParentElement;
      Result             := EL;
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
  FParentElement := AParentElement;
end;

destructor TDWElementTagCollection.Destroy;
begin

  inherited;
end;

function TDWElementTagCollection.GetHTMLElement(AIndex: Integer): TDWCustomElement;
begin
  Result := TDWCustomElement(inherited Get(AIndex));
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
  OldIndex: Integer;
begin
  // remove from Older Parent Content List
  if (ParentElement <> nil) and (ParentElement is TDWElementTag) then
    begin
      // Check if it's already in the content list of Parent
      OldIndex := (ParentElement as TDWElementTag).Contents.IndexOf(Self);
      if OldIndex > 0 then
        // Remove from Content list
        (ParentElement as TDWElementTag).Contents.Delete(OldIndex);
    end;
  // Set ParentElement Property
  inherited;
  // add to new Parent Content List
  if (ParentElement <> nil) and (ParentElement is TDWElementTag) then
    begin
      // Check if it's not already in the content list
      if (ParentElement as TDWElementTag).Contents.IndexOf(Self) < 0 then
        // Add to Content list
        (ParentElement as TDWElementTag).Contents.AddElemetAsObject(Self, False);
    end;
end;

procedure TDWElementText.Clear;
begin
  FText := '';
  Changed;
end;

{ TDWElementBinary }

destructor TDWElementBinary.Destroy;
begin

  inherited;
end;

procedure TDWElementBinary.InitializeElement(AParentElement: TDWCustomElement);
begin
  // inherited;

end;

procedure TDWElementBinary.Render(ABuffer: TDWStream);
begin

end;

function TDWElementBinary.Render: String;
begin
  Result := FBuffer.DataString;
end;

procedure TDWElementBinary.RenderElement(ABuffer: TDWStream; AIndent: Integer);
begin
  // inherited;

end;

procedure TDWElementBinary.SetParentElement(const Value: TDWCustomElement);
var
  OldIndex: Integer;
begin
  // remove from Older Parent Content List
  if (ParentElement <> nil) and (ParentElement is TDWElementTag) then
    begin
      // Check if it's already in the content list of Parent
      OldIndex := (ParentElement as TDWElementTag).Contents.IndexOf(Self);
      if OldIndex > 0 then
        // Remove from Content list
        (ParentElement as TDWElementTag).Contents.Delete(OldIndex);
    end;
  // Set ParentElement Property
  inherited;
  // add to new Parent Content List
  if (ParentElement <> nil) and (ParentElement is TDWElementTag) then
    begin
      // Check if it's not already in the content list
      if (ParentElement as TDWElementTag).Contents.IndexOf(Self) < 0 then
        // Add to Content list
        (ParentElement as TDWElementTag).Contents.AddElemetAsObject(Self, False);
    end;
end;

procedure TDWElementBinary.Clear;
begin
  FBuffer.Clear;
  Changed;
end;

{ TDWElementXHTMLTag }

constructor TDWElementXHTMLTag.CreateHTMLTag(ATag: string; aParentTag: TDWElementTag;
  AsCDATA: Boolean);
begin
  inherited CreateHTMLTag(ATag, aParentTag, ctdwAuto);
  FCDATA := AsCDATA;
end;

function TDWElementXHTMLTag.Render: String;
begin
  if FCDATA then
    begin
      { TODO 1 -oDELCIO -cIMPROVE : SpeedUp this }
      Result := StringReplace(inherited Render, '<item>', '<item><![CDATA[', [rfReplaceAll]);
      Result := StringReplace(Result, '</item>', ']]></item>', [rfReplaceAll]);
    end
  else
    Result := inherited Render;
end;

procedure TDWElementXHTMLTag.SetCDATA(const Value: Boolean);
begin
  FCDATA := Value;
end;

end.
