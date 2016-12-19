unit DW.VCL.Select2;

interface

uses
  Classes, System.SysUtils, StrUtils, DB, DW.VCL.Input, DWElementTag;

type
  TDWSelect2Item = class(TCollectionItem)
  private
    FKey: string;
    FDisplayText: string;
    procedure SetDisplayText(const Value: string);
    procedure SetKey(const Value: string);
  published
    property Key: string read FKey write SetKey;
    property DisplayText: string read FDisplayText write SetDisplayText;
  end;

  TDWSelect2Items = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TDWSelect2Item;
    procedure SetItem(Index: Integer; const Value: TDWSelect2Item);
  public
    property Items[Index: Integer]: TDWSelect2Item read GetItem write SetItem;
    function Add: TDWSelect2Item;
  end;

  TAsyncSearchEvent = procedure(Sender: TObject; SeachTerm: TStrings) of object;

  TDWSelect2 = class(TDWSelect)
  private
    FOnAsyncSearch: TAsyncSearchEvent;
    // to update script options when Component options are changed
    procedure UpdateOptions;
    // this event we return a json with the options that the Select2 request
    procedure DoOnAsyncSearch(aParams: TStrings; var aReply: string);
    procedure SetOnAsyncSearch(const Value: TAsyncSearchEvent);
  protected
    procedure OnItemsChange(ASender: TObject); override;
    procedure InternalRenderHTML(var AHTMLTag: TDWElementTag); override;
    procedure InternalRenderStyle(AStyle: TStringList); override;
    procedure InternalRenderScript(const AHTMLName: string; AScript: TStringList); override;
    procedure InternalRenderAsync(const AHTMLName: string); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function RenderAsync: TDWElementXHTMLTag; override;
    function RenderHTML: TDWElementTag; override;
  published
    property ScriptInsideTag default False;
    property OnAsyncSearch: TAsyncSearchEvent read FOnAsyncSearch write SetOnAsyncSearch;
  end;

implementation

uses DWUtils, DW.VCL.FluidForm, DWGlobal;

{ TIWBSSelect2 }

constructor TDWSelect2.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  inherited ScriptInsideTag := False;
  // UpdateOptions;
end;

destructor TDWSelect2.Destroy;
begin
  inherited;
end;

procedure TDWSelect2.DoOnAsyncSearch(aParams: TStrings; var aReply: string);
var
  data: string;
  line: string;
  i: Integer;
begin
  if Assigned(FOnAsyncSearch) then
    FOnAsyncSearch(Self, aParams);

  data  := '[';
  for i := 0 to Items.Count - 1 do
    begin
      line := '{"id":"' + InttoStr(i) + '","text":"' + Items.Names[i] + '","value":"' +
        Items.ValueFromIndex[i] + '"}';
      if i > 0 then
        data := data + ',';
      data   := data + line;
    end;
  data := data + ']';

  aReply := ('{"items": ' + data + '}');
end;

procedure TDWSelect2.InternalRenderAsync(const AHTMLName: string);
var
  LSelectedIdx: string;
  i: Integer;
begin
  inherited;
  { if (FText <> FOldText) then begin
    LSelectedIdx := '';
    if MultiSelect then
    begin
    for i := 0 to Length(FItemsSelected)-1 do
    if FItemsSelected[i] then begin
    if LSelectedIdx <> '' then
    LSelectedIdx := LSelectedIdx + ',';
    LSelectedIdx := LSelectedIdx + IntToStr(i);
    end;
    end
    else if FItemIndex >= 0 then
    LSelectedIdx := IntToStr(FItemIndex);
    IWBSExecuteAsyncJScript(AApplication, '$("#'+AHTMLName+'").val(['+LSelectedIdx+']);', False, True);
    FOldText := FText;
    end; }

end;

procedure TDWSelect2.InternalRenderHTML(var AHTMLTag: TDWElementTag);
begin
  inherited;
  AHTMLTag.AddStringParam('style', ActiveStyle);
end;

procedure TDWSelect2.InternalRenderScript(const AHTMLName: string; AScript: TStringList);
begin
  inherited;
  AScript.Add('$(''#{%htmlname%}'').select2({%options%});');
end;

procedure TDWSelect2.InternalRenderStyle(AStyle: TStringList);
begin
  inherited;
  if ParentContainer is TDWFluidForm then
    AStyle.Values['width'] := InttoStr(Width) + 'px';
end;

(* function TIWBSSelect2.IsScriptStored: Boolean;
  begin
  { TODO 1 -oDELCIO -cIMPROVEMENT : MOVE TO InternalRenderScripts }
  Result := Script.Text <> '$(''#{%htmlname%}'').select2({%options%});';
  end; *)

procedure TDWSelect2.OnItemsChange(ASender: TObject);
begin
  inherited;
  UpdateOptions;
end;

function TDWSelect2.RenderAsync: TDWElementXHTMLTag;
begin
  UpdateOptions;
  Result := inherited RenderAsync;
end;

function TDWSelect2.RenderHTML: TDWElementTag;
begin
  UpdateOptions;
  Result := inherited RenderHTML;
end;

procedure TDWSelect2.SetOnAsyncSearch(const Value: TAsyncSearchEvent);
begin
  FOnAsyncSearch := Value;
end;

procedure TDWSelect2.UpdateOptions;
var
  OptTxt: TStrings;
  LRestCallback: string;
begin
  OptTxt := TStringList.Create;
  try
    OptTxt.NameValueSeparator := ':';
    OptTxt.Delimiter          := ',';
    OptTxt.QuoteChar          := ' ';
    OptTxt.StrictDelimiter    := True;

    LRestCallback := DWApplication.RegisterRestCallBack(Self, HTMLName + '.dataurl',
      DoOnAsyncSearch);

    OptTxt.Values['ajax'] := '{url: "' + LRestCallback + '",' + 'delay: 200,' + 'dataType: "json",'
      + 'processResults: function (data) { ' + 'return { results: data.items ' + '};}}';

    OptTxt.Values['tags']          := 'true';
    OptTxt.Values['placeholder']   := '"Selecione Uma Opção"';
    OptTxt.Values['allowclear']    := 'true';
    OptTxt.Values['width']         := '"style"'; // To Work with IWBSFluidForm
    ScriptParams.Values['options'] := '{' + OptTxt.DelimitedText + '}';

    { if CustomRestEvents.Count = 0 then
      CustomRestEvents.Add;
      CustomRestEvents[0].EventName       := 'dataurl';
      CustomRestEvents[0].OnRestEvent     := DoOnAsyncSearch; }
  finally
    OptTxt.Free;
  end;
end;

{ TIWBSSelect2Item }

procedure TDWSelect2Item.SetDisplayText(const Value: string);
begin
  FDisplayText := Value
end;

procedure TDWSelect2Item.SetKey(const Value: string);
begin
  FKey := Value;
end;

{ TIWBSSelect2Items }

function TDWSelect2Items.Add: TDWSelect2Item;
begin
  Result := TDWSelect2Item(inherited Add);
end;

function TDWSelect2Items.GetItem(Index: Integer): TDWSelect2Item;
begin
  Result := TDWSelect2Item(inherited GetItem(Index));
end;

procedure TDWSelect2Items.SetItem(Index: Integer; const Value: TDWSelect2Item);
begin
  inherited SetItem(Index, Value);
end;

initialization

// Enable CSS and JS for Select2 Plugin
if DebugHook <> 0 then
  begin
    IWBSAddGlobalLinkFile('/<dwlibpath>/select2/css/select2.css');
    IWBSAddGlobalLinkFile('/<dwlibpath>/select2/js/select2.full.js');
  end
else
  begin
    IWBSAddGlobalLinkFile('/<dwlibpath>/select2/css/select2.min.css');
    IWBSAddGlobalLinkFile('/<dwlibpath>/select2/js/select2.full.min.js');
  end;

end.
