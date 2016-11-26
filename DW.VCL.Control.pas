{ *******************************************************************************
  DW.VCL(Delphi Web Visual Component Library) is Based on
  IWBootsTrap Framework :  http://kattunga.github.io/IWBootstrapFramework/

  Delphi Web is
  Developped by Delcio Sbeghen @ SRP Sistemas
  delcio @ eavnet.com.br (remove spaces)
  under MIT Licence
}

unit DW.VCL.Control;

interface

uses Classes, VCL.Controls, DWElementTag, DW.VCL.CustomForm, DWTypes,
  DW.VCL.ScriptEvents, DW.VCL.StyleRenderOptions, DW.VCL.Container;

type

  // TDWControl is the Base for DW Controls
  TDWControl = class(TGraphicControl)
  private
    //Defines whether the control will be fully updated in the next Async Call
    FAsyncFullRefresh: boolean;
    // define if control already Rendered
    FRendered: boolean;
    //Additional User defined Style to be render
    FStyle:TStringList;
    //Events
    FOnAsyncClick: TDWAsyncProcedure;
    FOnAsyncMouseDown: TDWAsyncProcedure;
    FOnAsyncDoubleClick: TDWAsyncProcedure;
    FOnAsyncMouseMove: TDWAsyncProcedure;
    FOnAsyncMouseUp: TDWAsyncProcedure;
    FStyleRenderOptions: TDWRenderOptions;
    FOnAsyncMouseOutwrite: TDWAsyncProcedure;
    FOnAsyncMouseOver: TDWAsyncProcedure;
    // The z-index property specifies the stack order of an element in a display browser
    // An element with greater stack order is always in front of an element with a lower stack order.
    FZIndex: Integer;
    // JavaScripts to be executted on browser when especified event occurs
    FScriptEvents: TDWScriptEvents;
    // property "class" of HTML Element, used for CSS styles
    FCssClass: string;
  protected
    // Render Cursor Style and add to "style" tag property
    function RenderCursorStyle: string; virtual;
    // Render Size Style and add to "style" tag property (See StyleRenderOptions)
    function RenderSizeStyle: string; virtual;
    // Render Position Style and add to "style" tag property(See StyleRenderOptions)
    function RenderPositionStyle: string; virtual;
    // Render Text alignament Style and add to "style" tag property
    function RenderTextAlignStyle: String; virtual;
    // Render Vertical alignament Style and add to "style" tag property
    function RenderVerticalAlignStyle: string; virtual;
    // Render Backgruound Color Style and add to "style" tag property
    function RenderBGColor: string; virtual;
    // Render HTML "class" tag property
    function RenderCSSClass: string; virtual;
    // Render HTML "style" tag property
    function RenderStyle: string; virtual;
    //Setters for Properties, to description see respective property
    procedure SetCssClass(const Value: string); virtual;
    procedure SetOnAsyncClick(const Value: TDWAsyncProcedure); virtual;
    procedure SetOnAsyncDoubleClick(const Value: TDWAsyncProcedure); virtual;
    procedure SetOnAsyncMouseDown(const Value: TDWAsyncProcedure); virtual;
    procedure SetOnAsyncMouseMove(const Value: TDWAsyncProcedure); virtual;
    procedure SetOnAsyncMouseOut(const Value: TDWAsyncProcedure); virtual;
    procedure SetOnAsyncMouseOver(const Value: TDWAsyncProcedure); virtual;
    procedure SetOnAsyncMouseUp(const Value: TDWAsyncProcedure); virtual;
    procedure SetScriptEvents(const Value: TDWScriptEvents); virtual;
    procedure SetStyle(const Value: TStringList); virtual;
    procedure SetStyleRenderOptions(const Value: TDWRenderOptions); virtual;
    procedure SetZIndex(const Value: Integer); virtual;
    //Execute event OnAsyncClick
    procedure DoAsyncClick(aParams:TStringList); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    // Get Form where Component it is
    function Form: TDWCustomForm;
    // Render control on full form load or on FRendered = false
    // Return one TDWElementTag with element HTML
    function RenderHTML: TDWElementTag; virtual;
    // Render control on form Async Calls
    // Return one TDWElementXHTMLTag with element XHTML
    function RenderAsync: TDWElementXHTMLTag; virtual;
    // Render AsyncEvents(ClallBacks)
    function RenderAsyncEvents:string;
    // Invalidate control force control to refresh on Design Mode
    // and to be Refreshed(RenderAsync) on form AsyncCalls
    procedure Invalidate; override;
    // Repaint control in Only in Design Mode
    procedure RepaintControl; virtual;
    // Force a full refresh of the control during an Async call.
    // Usually there is no need to use this method, only if some property changed during async calls is not reflected.
    procedure AsyncFullRefreshControl;
    // Cancel AsyncFullRefreshControl
    // Usually there is no need to use this method. It is for internal use.
    procedure CancelAsyncFullRefreshControl;
    // Remove a control from html flow. You should execute this when destroying a control durinc async calls before Freeing @br
    // If you are destroying a region is enought to execute this in that region, you don't need to execute it in each child control.
    procedure AsyncRemoveControl;
    // returns a string representing the the JQSelector for this object.
    // @preformatted(IWBSCustomControl.JQSelector > '$(#"htmlname")')
    function JQSelector: string;
    // get the list of AsyncEvents actives in this control
    function AsyncEvents: TDWAsyncEventTypeSet; virtual;
    //Return name of element in HTML.
    //If RootParent is an TDWCustomForm, HTMLName is same as Name,
    //else if RootParent is an TDWFrame, HTMLName is FrameName_ComponentName
    //this is necessary because one frame can be placed into one Form,
    //and this can cause duplicate names in same HTML Page
    function HTMLName:string;
    //Return the root container of a component.
    //if component is in one TDWCustomForm, this return a Form,
    //else if component is in one TDWCustomFrame, this return a Frame
    //for compatibility the object returned is an TDWContainer,
    //but this never return containers, like a Panels, etc
    //the returned element to be casted usin some like this:
    //@preformatted(
    //if ReturnedObject.InheritsFrom(TDWCustomForm) then //its one Form
    //  TDWCustomForm(ReturnedObject)."your code here"
    //else if ReturnedObject.InheritsFrom(TDWCustomFrame) then  //its one Frame
    //    TDWCustomFrame(ReturnedObject)."your code here")
    function RootParent:TDWContainer;
    // Set focus on component in an Ajax Callback
    procedure SetFocus;
    // Specifies whether the control responds to mouse, keyboard, and timer events.
    property Enabled;
    // JavaScripts to be executted on browser when especified event occurs
    property ScriptEvents: TDWScriptEvents read FScriptEvents write SetScriptEvents;
    // Set the font of Control (See StyleRenderOptions)
    property Font;
    // Color of Component
    property Color;
    // property "class" of HTML Element, used for CSS styles
    property CssClass: string read FCssClass write SetCssClass;
    // Defines which style properties will be rendered
    property StyleRenderOptions: TDWRenderOptions read FStyleRenderOptions write SetStyleRenderOptions;
    // List of inline styles in pairs name: value
    property Style: TStringList read FStyle write SetStyle;
    // The z-index property specifies the stack order of an element.
    // An element with greater stack order is always in front of an element with a lower stack order.
    // Note: z-index only works on positioned elements (position:absolute, position:relative, or position:fixed).
    // see: http://www.w3schools.com/csSref/pr_pos_z-index.asp
    property ZIndex: Integer read FZIndex write SetZIndex;
    //Events
    property OnAsyncClick: TDWAsyncProcedure read FOnAsyncClick write SetOnAsyncClick;
    property OnAsyncDoubleClick: TDWAsyncProcedure read FOnAsyncDoubleClick write SetOnAsyncDoubleClick;
    property OnAsyncMouseMove: TDWAsyncProcedure read FOnAsyncMouseMove write SetOnAsyncMouseMove;
    property OnAsyncMouseOver: TDWAsyncProcedure read FOnAsyncMouseOver write SetOnAsyncMouseOver;
    property OnAsyncMouseOut: TDWAsyncProcedure read FOnAsyncMouseOutwrite write SetOnAsyncMouseOut;
    property OnAsyncMouseDown: TDWAsyncProcedure read FOnAsyncMouseDown write SetOnAsyncMouseDown;
    property OnAsyncMouseUp: TDWAsyncProcedure read FOnAsyncMouseUp write SetOnAsyncMouseUp;
  published

  end;

  // TDWInputControl is the base for all Controls that accept input
  TDWInputControl = class(TDWControl)
  private
    FTabStop: boolean;
    FTabOrder: Integer;
    //Events
    FOnAsyncSelect: TDWAsyncProcedure;
    FOnAsyncExit: TDWAsyncProcedure;
    FOnAsyncKeyPress: TDWAsyncProcedure;
    FOnAsyncKeyDown: TDWAsyncProcedure;
    FOnAsyncChange: TDWAsyncProcedure;
    FOnAsyncEnter: TDWAsyncProcedure;
    FOnAsyncKeyUp: TDWAsyncProcedure;

    procedure SetTabStop(const Value: boolean);
    function GetTabOrder: TTabOrder;
    procedure SetTabOrder(const Value: TTabOrder);
    procedure UpdateTabOrder(Value: TTabOrder);
    procedure SetOnAsyncChange(const Value: TDWAsyncProcedure);
    procedure SetOnAsyncEnter(const Value: TDWAsyncProcedure);
    procedure SetOnAsyncExit(const Value: TDWAsyncProcedure);
    procedure SetOnAsyncKeyDown(const Value: TDWAsyncProcedure);
    procedure SetOnAsyncKeyPress(const Value: TDWAsyncProcedure);
    procedure SetOnAsyncKeyUp(const Value: TDWAsyncProcedure);
    procedure SetOnAsyncSelect(const Value: TDWAsyncProcedure);
  public
    constructor Create(AOwner: TComponent); override;
    // get the list of AsyncEvents actives in this control
    function AsyncEvents: TDWAsyncEventTypeSet; override;
  published
    property TabStop: boolean read FTabStop write SetTabStop default True;
    // Corresponds to html tabindex attribute. It will be rendered if tabindex <> 0. Set to -1 to disable tabstop
    property TabOrder: TTabOrder read GetTabOrder write SetTabOrder default -1;
    property Text;
    property OnAsyncChange: TDWAsyncProcedure read FOnAsyncChange write SetOnAsyncChange;
    property OnAsyncEnter: TDWAsyncProcedure read FOnAsyncEnter write SetOnAsyncEnter;
    property OnAsyncExit: TDWAsyncProcedure read FOnAsyncExit write SetOnAsyncExit;
    property OnAsyncKeyDown: TDWAsyncProcedure read FOnAsyncKeyDown write SetOnAsyncKeyDown;
    property OnAsyncKeyUp: TDWAsyncProcedure read FOnAsyncKeyUp write SetOnAsyncKeyUp;
    property OnAsyncKeyPress: TDWAsyncProcedure read FOnAsyncKeyPress write SetOnAsyncKeyPress;
    property OnAsyncSelect: TDWAsyncProcedure read FOnAsyncSelect write SetOnAsyncSelect;

  end;

  procedure DWRenderScript(AControl: TDWControl; var AHTMLTag: TDWElementTag);


implementation

uses DWUtils;

type
  ThackDwControl = class(TDWControl);

procedure DWRenderScript(AControl: TDWControl; var AHTMLTag: TDWElementTag);
var
  LScriptTag:TDWElementTag;
begin
  LScriptTag:= AHTMLTag.Content.AddElement('script');
  //render Async Events and Register Callback in DWApplication
  LScriptTag.Content.AddText(AControl.RenderAsyncEvents);
  //LScriptTag.Content.AddText(ThackDwControl(AControl).ren);
end;


{ TDWControl }

function TDWControl.AsyncEvents: TDWAsyncEventTypeSet;
begin
  Result:= [];
  if Assigned(FOnAsyncClick) then
    Result:= [ae_click];
  if Assigned(FOnAsyncMouseDown) then
    Include(Result, ae_mousedown);
  if Assigned(FOnAsyncDoubleClick) then
    Include(Result, ae_dblclick);
  if Assigned(FOnAsyncMouseMove) then
    Include(Result, ae_mousemove);
  if Assigned(FOnAsyncMouseUp) then
    Include(Result, ae_mouseup);
  if Assigned(FOnAsyncMouseOutwrite) then
    Include(Result, ae_mouseout);
  if Assigned(FOnAsyncMouseOver) then
    Include(Result, ae_mouseover);
end;

procedure TDWControl.AsyncFullRefreshControl;
begin

end;

procedure TDWControl.AsyncRemoveControl;
begin

end;

procedure TDWControl.CancelAsyncFullRefreshControl;
begin

end;

constructor TDWControl.Create(AOwner: TComponent);
begin
  inherited;
  Font.Size            := 12;
  FRendered            := False;
  FAsyncFullRefresh := True;
end;

procedure TDWControl.DoAsyncClick(aParams: TStringList);
begin
  if Assigned(FOnAsyncClick) then
    FOnAsyncClick(Self, aParams);
end;


function TDWControl.RenderAsync: TDWElementXHTMLTag;
begin

end;

function TDWControl.RenderAsyncEvents: string;
begin
  if Assigned(FOnAsyncClick) then
    begin
      Result:= JQSelector + '.off("click.DW").on("click.DW", ' +
          'function (e) {' +
                'executeAjaxCallBack("", '+ JQSelector + '[0], "' + Form.Name + '.' + HTMLName +'");' +
          '})';
      //DWApplication.CallbackResp.AddScriptToExecute(LScript);
      DWApplication.RegisterCallBack(Self, ae_click, DoAsyncClick);
    end;
end;

function TDWControl.RenderBGColor: string;
begin

end;

function TDWControl.RenderCSSClass: string;
begin

end;

function TDWControl.RenderCursorStyle: string;
begin

end;

function TDWControl.RenderHTML: TDWElementTag;
var
  AuxStr:string;
begin
  //render element tag
  Result := TDWElementTag.CreateHTMLTag('');
  //render Style
  AuxStr:= RenderStyle;
  if AuxStr <> '' then
    Result.AddStringParam('style', AuxStr);
  //render scripts

  //render Async Events





end;

function TDWControl.RenderPositionStyle: string;
begin

end;

function TDWControl.RenderSizeStyle: string;
begin

end;

function TDWControl.RenderStyle: string;
begin

end;

function TDWControl.RenderTextAlignStyle: String;
begin

end;

function TDWControl.RenderVerticalAlignStyle: string;
begin

end;

procedure TDWControl.RepaintControl;
begin

end;

function TDWControl.RootParent: TDWContainer;
var
  CompTest: TControl;
  LForm:TDWCustomForm;
begin
  Result := nil;
  { TODO 1 -oDELCIO -cIMPLEMENT :  Find RootParent for Frame}
  (*CompTest := self;
  while Assigned(CompTest) and (not(CompTest is TDWCustomFrame)) do
    CompTest := CompTest.Parent; *)
  if Result = nil then
    Result:= TDWContainer(Form);
end;

procedure TDWControl.SetCssClass(const Value: string);
begin
  FCssClass := Value;
end;

procedure TDWControl.SetFocus;
begin

end;

procedure TDWControl.SetOnAsyncClick(const Value: TDWAsyncProcedure);
begin
  FOnAsyncClick := Value;
end;

procedure TDWControl.SetOnAsyncDoubleClick(const Value: TDWAsyncProcedure);
begin
  FOnAsyncDoubleClick := Value;
end;

procedure TDWControl.SetOnAsyncMouseDown(const Value: TDWAsyncProcedure);
begin
  FOnAsyncMouseDown := Value;
end;

procedure TDWControl.SetOnAsyncMouseMove(const Value: TDWAsyncProcedure);
begin
  FOnAsyncMouseMove := Value;
end;

procedure TDWControl.SetOnAsyncMouseOut(const Value: TDWAsyncProcedure);
begin
  FOnAsyncMouseOutwrite := Value;
end;

procedure TDWControl.SetOnAsyncMouseOver(const Value: TDWAsyncProcedure);
begin
  FOnAsyncMouseOver := Value;
end;

procedure TDWControl.SetOnAsyncMouseUp(const Value: TDWAsyncProcedure);
begin
  FOnAsyncMouseUp := Value;
end;

procedure TDWControl.SetScriptEvents(const Value: TDWScriptEvents);
begin
  FScriptEvents := Value;
end;

procedure TDWControl.SetStyle(const Value: TStringList);
begin
  FStyle := Value;
end;

procedure TDWControl.SetStyleRenderOptions(const Value: TDWRenderOptions);
begin
  FStyleRenderOptions := Value;
end;

procedure TDWControl.SetZIndex(const Value: Integer);
begin
  FZIndex := Value;
end;

function TDWControl.Form: TDWCustomForm;
begin
  Result := DWFindParentForm(Self);
end;

function TDWControl.HTMLName: string;
var
  LRootParent:TDWContainer;
begin
  LRootParent:= RootParent;
  //
  if LRootParent.InheritsFrom(TDWCustomForm) then
    HTMLName:= Name
  else
    HTMLName:= LRootParent.Name + '_' + Name;
end;

procedure TDWControl.Invalidate;
begin
  inherited;

end;

function TDWControl.JQSelector: string;
begin
  Result:= '$("#' + HTMLName + '")';
end;

{ TDWInputControl }

function TDWInputControl.AsyncEvents: TDWAsyncEventTypeSet;
begin
  Result:= inherited;
  if Assigned(FOnAsyncSelect) then
    Include(Result, ae_select);
  if Assigned(FOnAsyncExit) then
    Include(Result, ae_blur);
  if Assigned(FOnAsyncKeyPress) then
    Include(Result, ae_keypress);
  if Assigned(FOnAsyncKeyDown) then
    Include(Result, ae_keydown);
  if Assigned(FOnAsyncChange) then
    Include(Result, ae_change);
  if Assigned(FOnAsyncEnter) then
    Include(Result, ae_focus);
  if Assigned(FOnAsyncKeyUp) then
    Include(Result, ae_keyup);
end;

constructor TDWInputControl.Create(AOwner: TComponent);
begin
  inherited;
  FTabStop  := True;
  FTabOrder := -1;
end;

function TDWInputControl.GetTabOrder: TTabOrder;
begin
  if (Parent <> nil) and (Parent.GetTabList <> nil) then
    Result := Parent.GetTabList.IndexOf(Self)
  else
    Result := -1;
end;

procedure TDWInputControl.SetOnAsyncChange(const Value: TDWAsyncProcedure);
begin
  FOnAsyncChange := Value;
end;

procedure TDWInputControl.SetOnAsyncEnter(const Value: TDWAsyncProcedure);
begin
  FOnAsyncEnter := Value;
end;

procedure TDWInputControl.SetOnAsyncExit(const Value: TDWAsyncProcedure);
begin
  FOnAsyncExit := Value;
end;

procedure TDWInputControl.SetOnAsyncKeyDown(const Value: TDWAsyncProcedure);
begin
  FOnAsyncKeyDown := Value;
end;

procedure TDWInputControl.SetOnAsyncKeyPress(const Value: TDWAsyncProcedure);
begin
  FOnAsyncKeyPress := Value;
end;

procedure TDWInputControl.SetOnAsyncKeyUp(const Value: TDWAsyncProcedure);
begin
  FOnAsyncKeyUp := Value;
end;

procedure TDWInputControl.SetOnAsyncSelect(const Value: TDWAsyncProcedure);
begin
  FOnAsyncSelect := Value;
end;

procedure TDWInputControl.SetTabOrder(const Value: TTabOrder);
begin
  if csReadingState in ControlState then
    FTabOrder := Value
  else
    UpdateTabOrder(Value);
end;

procedure TDWInputControl.UpdateTabOrder(Value: TTabOrder);
var
  CurIndex, Count: Integer;
begin
  CurIndex := GetTabOrder;
  if CurIndex >= 0 then
    begin
      Count := Parent.GetTabList.Count;
      if Value < 0 then
        Value := 0;
      if Value >= Count then
        Value := Count - 1;
      if Value <> CurIndex then
        begin
          Parent.GetTabList.Delete(CurIndex);
          Parent.GetTabList.Insert(Value, Self);
        end;
    end;
end;

procedure TDWInputControl.SetTabStop(const Value: boolean);
begin
  if FTabStop <> Value then
    begin
      FTabStop := Value;
      Invalidate;
    end;
end;

end.
