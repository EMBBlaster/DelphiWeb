unit DW.VCL.CustomRegion;

interface

uses Classes, Controls, System.SysUtils, System.Types, Vcl.Graphics, System.StrUtils,
DW.VCL.Container, DW.VCL.Common, DWTypes, DWElementTag,
DW.VCL.ScriptParams, DW.VCL.Frame;

type
  // base for all Regions
  // exclude DWForms, DWFrames
  TDWCustomRegion = class(TDWContainer)
  private
    FOldCss: string;
    FOldStyle: string;
    FOldVisible: boolean;

    FAsyncRefreshControl: boolean;

    //FCustomAsyncEvents: TIWBSCustomAsyncEvents; -> future implementation
    //FCustomRestEvents: TIWBSCustomRestEvents;   -> future implementation
    FGridOptions: TDWGridOptions;
    //FScript: TStringList; -> in parent
    //FScriptInsideTag: boolean; -> in Parent
    //FScriptParams: TDWScriptParams; -> in parent
    //FStyle: TStringList;  -> in Parent
    //FReleased: boolean; -> verify if need to add in parent class
    FText: string;

    FCollapseVisible: boolean;
    FCollapse: boolean;

    FRawText: boolean;

    FOnAfterRender: TNotifyEvent;
    FOnAfterAsyncChange: TNotifyEvent;

    //function IsScriptEventsStored: Boolean; virtual; -> in parent class
    function RegionDiv: TDWElementTag;

    function  RenderText: string;

    //procedure OnScriptChange(ASender : TObject); -> in Parent Class
    //procedure OnStyleChange(ASender : TObject);  -> in parent class
    //function GetCustomAsyncEvents: TIWBSCustomAsyncEvents; -> future implementation
    //procedure SetCustomAsyncEvents(const Value: TIWBSCustomAsyncEvents); -> future implementation
   // function GetCustomRestEvents: TIWBSCustomRestEvents;  -> future implementation
   // procedure SetCustomRestEvents(const Value: TIWBSCustomRestEvents); -> future implementation
    procedure SetGridOptions(const AValue: TDWGridOptions);
   // function GetScript: TStringList;  -> in parent class
   // procedure SetScript(const AValue: TStringList); -> in parent class
   // function GetScriptParams: TDWScriptParams;
    //procedure SetScriptParams(const AValue: TDWScriptParams);
    //function GetStyle: TStringList;  -> in parent class
   // procedure SetStyle(const AValue: TStringList); -> in parent class
    //function GetScriptInsideTag: boolean; -> in parent class
    //procedure SetScriptInsideTag(const Value: boolean); -> in parent class
   // function get_ScriptEvents: TIWScriptEvents;  -> in parent class
    function GetAfterRender: TNotifyEvent;
   // procedure set_ScriptEvents(const Value: TIWScriptEvents);  -> in parent class
    procedure SetAfterRender(const Value: TNotifyEvent);
    procedure SetCollapse(const Value: boolean);
    procedure SetCollapseVisible(const Value: boolean);
    procedure SetRawText(const Value: boolean);
    procedure SetText(const Value: string);
    //procedure SetCss(const Value: string); //-> in parent class
  protected

    FRegionDiv: TDWElementTag;
    FTagName: string;

    {$hints off}
    //function get_Visible: Boolean; override;  --> Not necessary
    //procedure set_Visible(Value: Boolean); override;  --> Not necessary
    //procedure SetParent(AParent: TWinControl); override;  --> Not necessary
    {$hints on}
    //paint the control in ide design mode
    procedure Paint; override;

    //function ContainerPrefix: string; override;

    procedure InternalRenderCss(var ACss: string); override;
    procedure InternalRenderScript(const AHTMLName: string; AScript: TStringList);override;
    procedure InternalRenderStyle(AStyle: TStringList); override;
   // property Released: boolean read FReleased;  -> verify if need to add in parent class
    function RenderAsync: TDWElementXHTMLTag; override;
    procedure RenderComponents(aTagParent:TDWElementTag); override;
    function RenderCSSClass: string; override;
    function RenderHTML: TDWElementTag; override;
    //procedure RenderScripts; override;
    function RenderStyle: string; override;
    function SupportsInput: Boolean;
    //procedure InternalBeforeRenderControls(var aRenderStream: ); virtual;
    //procedure InternalAfterRenderControls(var aRenderStream: ); virtual;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Render AsyncEvents(ClallBacks)
    function RenderAsyncEvents: string; Override;
    // Let you destroy the region inside a self event handler
    //procedure Release;  -> verify if need to add in parent class
    //procedure AsyncRefreshControl; -> in parent class
    //procedure ResetAsyncRefreshControl; -> in parent class
   // procedure AsyncRemoveControl;

    //procedure ApplyAsyncChanges;
    //return the Role Attribute
    function GetRoleString: string; virtual;
    function GetCssString: string;
    //function IsStoredCustomAsyncEvents: Boolean; -> future implementation
    //function IsStoredCustomRestEvents: Boolean; -> future implementation
    function JQSelector: string;
    procedure SetFocus; override;
    function FindParentFrame: TDWFrame;
    property TagType: string read FTagName;
  published
    property Align;
    property BSGridOptions: TDWGridOptions read FGridOptions write SetGridOptions;
   // property ClipRegion default False;
    //property CustomAsyncEvents: TIWBSCustomAsyncEvents read GetCustomAsyncEvents write SetCustomAsyncEvents stored IsStoredCustomAsyncEvents;  -> future implementation
    //property CustomRestEvents: TIWBSCustomRestEvents read GetCustomRestEvents write SetCustomRestEvents stored IsStoredCustomRestEvents;    -> future implementation
    //property Css: string read FCss write SetCss;  //-> in parent class
    property Collapse: boolean read FCollapse write SetCollapse default False;
    property CollapseVisible: boolean read FCollapseVisible write SetCollapseVisible default False;
    { TODO 1 -oDELCIO -cIMPLEMENT :  ExtraTagParams}
    //property ExtraTagParams;
    //property LayoutMgr;
    property RawText: boolean read FRawText write SetRawText default False;
    //property RenderInvisibleControls default True; -> in parent class
    //property ScriptEvents: TIWScriptEvents read get_ScriptEvents write set_ScriptEvents stored IsScriptEventsStored;  -> in parent class
   // property Script: TStringList read GetScript write SetScript; -> in parent class
    //property ScriptInsideTag: boolean read GetScriptInsideTag write SetScriptInsideTag default True; -> in parent class
    //property ScriptParams: TIWBSScriptParams read GetScriptParams write SetScriptParams; -> in parent class
   // property Style: TStringList read GetStyle write SetStyle;   -> in parent class
    property Text: string read FText write SetText;
    //property ZIndex default 0; -> in parent class

    // Occurs after component is rendered.
    property OnAfterRender: TNotifyEvent read GetAfterRender write SetAfterRender;

    // Occurs after component is changed on an Asyn call, it doesn't occurs if the control is fully rendered
    property OnAfterAsyncChange: TNotifyEvent read FOnAfterAsyncChange write FOnAfterAsyncChange;

    property OnHTMLTag;
  end;

implementation
  uses
    DWUtils, DW.VCL.Region, DW.VCL.InputForm;


constructor TDWCustomRegion.Create(AOwner: TComponent);
begin
  inherited;
  FAsyncRefreshControl := False;
 // FReleased := False; -> verify if need to add in parent class
 //FCustomAsyncEvents := nil;
 // FCustomRestEvents := nil;
  FGridOptions := TDWGridOptions.Create(Self);
  FTagName := 'div';
  //ClipRegion := False;

end;

destructor TDWCustomRegion.Destroy;
begin
  //FreeAndNil(FCustomAsyncEvents);
  //FreeAndNil(FCustomRestEvents);
  FreeAndNil(FGridOptions);
  //FreeAndNil(FScript);
  //FreeAndNil(FScriptParams);
  //FreeAndNil(FStyle);
  inherited;
end;

procedure TDWCustomRegion.SetRawText(const Value: boolean);
begin
  FRawText := Value;
  AsyncRefreshControl;
end;

function TDWCustomRegion.JQSelector: string;
begin
  Result := '$("#'+HTMLName+'")';
end;

procedure TDWCustomRegion.Paint;
var
  LRect : TRect;
  s: string;
  w: integer;
  LLines: string;
  LScript: string;
begin
  LRect := Rect(0, 0, Self.Width, Self.Height);
  Canvas.Brush.Color := clWhite;
  Canvas.Pen.Color := clGray;
  Canvas.Rectangle(LRect);

  with TDWCustomRegion(Self) do begin
    Canvas.Font.Name := CNST_PROPORTIONALFONT;
    Canvas.Font.Size := 10;
    Canvas.Font.Color := clGray;
    Canvas.Font.Style := [];

    // draw css classes
    s := GetCssString;
    if Self is TDWFormControl then begin
      if s <> '' then
        s := ' '+s;
      s := 'form-Self'+s;
    end;
    s := '['+s+']';
    if not RawText then
      s := '<'+TagType+'> ' + s;
    s := Name+' '+s;
    w := Canvas.TextWidth(s);
    LRect := Rect(Self.ClientWidth-w-10, 2, Self.Width, Self.Height);
    Canvas.TextRect(LRect,s,[]);

    // draw embeded text and script
    LLines := Text;
    if RawText then
      begin
        Canvas.Font.Name := CNST_PROPORTIONALFONT;
        if Script.Count > 0 then
          LScript := #13#10'<script>'#13#10+Script.Text+'</script>'
        else
          LScript := '';
        LLines := '<'+TagType+'>'+IfThen(LLines <> '',#13#10+LLines);
        if ScriptInsideTag then
          LLines := LLines+LScript;
        LLines := LLines+#13#10'</'+TagType+'>';
        if not ScriptInsideTag then
          LLines := LLines+LScript;
      end
    else
      begin
        Canvas.Font.Name := CNST_DEFAULTFONTNAME;
        if Self is TDWRegion then
          case TDWRegion(Self).TagType of
            bsttDiv, bsttP: Canvas.Font.Height := -12;
            bsttH1: Canvas.Font.Height := -36;
            bsttH2: Canvas.Font.Height := -30;
            bsttH3: Canvas.Font.Height := -24;
            bsttH4: Canvas.Font.Height := -18;
            bsttH5: Canvas.Font.Height := -14;
            bsttH6: Canvas.Font.Height := -12;
          end;
      end;

    if LLines <> '' then begin
      LRect := Rect(10, 18, Self.Width-10, Self.Height-3);
      Canvas.Font.Color := clBlack;
      Canvas.TextRect(LRect,LLines,[]);
    end;
  end;
end;

procedure TDWCustomRegion.SetFocus;
begin
  //IWBSExecuteAsyncJScript(JQSelector+'.focus()');
  DWApplication.CallBackResp.AddScriptToExecute(JQSelector+'.focus()');
end;


function TDWCustomRegion.GetRoleString: string;
begin
  result := '';
end;

procedure TDWCustomRegion.SetGridOptions(const AValue: TDWGridOptions);
begin
  FGridOptions.Assign(AValue);
  Invalidate;
end;

function TDWCustomRegion.GetAfterRender: TNotifyEvent;
begin
  Result := FOnAfterRender;
end;

function TDWCustomRegion.GetCssString: string;
begin
  Result := RenderCSSClass;
end;

procedure TDWCustomRegion.SetAfterRender(const Value: TNotifyEvent);
begin
  FOnAfterRender := Value;
end;

procedure TDWCustomRegion.SetCollapse(const Value: boolean);
begin
  FCollapse := Value;
  Invalidate;
end;

procedure TDWCustomRegion.SetCollapseVisible(const Value: boolean);
begin
  FCollapseVisible := Value;
  Invalidate;
end;

procedure TDWCustomRegion.SetText(const Value: string);
begin
  FText := TrimRight(Value);
  AsyncRefreshControl;
end;

function TDWCustomRegion.RegionDiv: TDWElementTag;
begin
  Result := FRegionDiv;
end;

function TDWCustomRegion.SupportsInput: Boolean;
begin
  Result := False;
end;

(*procedure TIWBSCustomRegion.InternalAfterRenderControls(
begin
// Occurs before render Child Controls
end;*)

(*procedure TIWBSCustomRegion.InternalBeforeRenderControls(
begin
  // Occurs after render Child Controls
end; *)

procedure TDWCustomRegion.InternalRenderCss(var ACss: string);
begin
  if FCollapse then begin
    TDWBSCommon.AddCssClass(ACss, 'collapse');
    if FCollapseVisible then
      TDWBSCommon.AddCssClass(ACss, 'in');
  end;
end;

procedure TDWCustomRegion.InternalRenderScript(const AHTMLName: string; AScript: TStringList);
begin
  //
end;

procedure TDWCustomRegion.InternalRenderStyle(AStyle: TStringList);
begin
  //
end;

function TDWCustomRegion.RenderAsync: TDWElementXHTMLTag;
var
  xHTMLName: string;
begin
  Result := nil;
  xHTMLName := HTMLName;

  if FAsyncRefreshControl or not FRendered then
    begin
      TDWRegionCommon.CancelChildAsyncRender(Self);
      TDWBSCommon.RenderAsync(xHTMLName, Self);
    end
  else
    begin
      TDWBSCommon.SetAsyncClass(xHTMLName, RenderCSSClass, FOldCss);
      TDWBSCommon.SetAsyncStyle(xHTMLName, RenderStyle, FOldStyle);
      TDWBSCommon.SetAsyncVisible(FMainID, Visible, FOldVisible);

      if Assigned(FOnAfterAsyncChange) then
        FOnAfterAsyncChange(Self);
      { TODO 1 -oDELCIO -cIMPLEMENT : Global event OnAfterAsyncChange  }
      {if Assigned(gIWBSOnAfterAsyncChange) then
        gIWBSOnAfterAsyncChange(Self, xHTMLName); }
    end;
end;

function TDWCustomRegion.RenderAsyncEvents: string;
begin
//
end;

procedure TDWCustomRegion.RenderComponents(aTagParent:TDWElementTag);
begin
  TDWRegionCommon.RenderComponents(Self, aTagParent);
end;

function TDWCustomRegion.RenderCSSClass: string;
begin
  Result := FGridOptions.GetClassString;
  if Css <> '' then begin
    if Result <> '' then
      Result := Result + ' ';
    Result := Result + Css;
  end;
  InternalRenderCss(Result);
end;

function TDWCustomRegion.RenderHTML: TDWElementTag;
begin
  FOldCss := RenderCSSClass;
  FOldStyle := RenderStyle;
  FOldVisible := Visible;

  FRegionDiv := TDWElementTag.CreateHTMLTag(FTagName);
  FRegionDiv.AddStringParam('id',HTMLName);
  FRegionDiv.AddClassParam(FOldCss);
  FRegionDiv.AddStringParam('role',GetRoleString);
  FRegionDiv.AddStringParam('style',RenderStyle);

  if FText <> '' then
    FRegionDiv.Contents.AddText(RenderText);

  TDWBSCommon.RenderScript(Self, FRegionDiv);
  FMainID := FRegionDiv.Params.Values['id'];

  { TODO 1 -oDELCIO -cIMPLEMENT : Global Render custom attribute with name of component}
  (*if gIWBSAttributeCmpName <> '' then
    FRegionDiv.Params.Values[gIWBSAttributeCmpName] := name;*)

  Result := FRegionDiv;

  FAsyncRefreshControl := False;
  FRendered := True;
end;


function TDWCustomRegion.RenderStyle: string;
begin
  Result := TDWBSCommon.RenderStyle(Self);
end;

function TDWCustomRegion.RenderText: string;
var
  LLines: TStringList;
begin
  if RawText then
    begin
      LLines := TStringList.Create;
      try
        LLines.Text := FText;

        // replace params before custom events
        LLines.Text := TDWBSCommon.ReplaceParams(Self, LLines.Text);

        { TODO 1 -oDELCIO -cIMPLEMENT : Custom Async Events }
        (*// replace inner events calls
        if IsStoredCustomAsyncEvents then
          for i := 0 to CustomAsyncEvents.Count-1 do
            TIWBSCustomAsyncEvent(CustomAsyncEvents.Items[i]).ParseParam(LLines);*)
        { TODO 1 -oDELCIO -cIMPLEMENT : Custom Rest Events }
        // replace inner events calls
        {if IsStoredCustomRestEvents then
          for i := 0 to CustomRestEvents.Count-1 do
            TIWBSCustomRestEvent(CustomRestEvents.Items[i]).ParseParam(LLines);}

        Result := LLines.Text;
      finally
        LLines.Free;
      end;
    end
  else
    Result := TDWBSCommon.TextToHTML(FText);
end;

function TDWCustomRegion.FindParentFrame: TDWFrame;
var
  LParentFrame:TDWFrame;
  ControlP: TControl;
begin
  LParentFrame:= nil;
  ControlP:= Self;
  while (not (ControlP.Parent = nil))  do
    begin
      ControlP := ControlP.Parent;
      if ControlP is TDWFrame then
        begin
          LParentFrame:= TDWFrame(ControlP);
          Break;
        end;
    end;
  Result:= LParentFrame;
end;

end.
