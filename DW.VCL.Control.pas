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
  DW.VCL.ScriptEvents, DW.VCL.StyleRenderOptions;

type

  // TDWControl is the Base for DW Controls
  TDWControl = class(TGraphicControl)
  private
    //Defines whether the control will be fully updated in the next Async Call
    FAsyncFullRefresh: boolean;
    // define if control already Rendered
    FRendered: boolean;
    FOnAsyncClick: TDWAsyncProcedure;
  protected
    // Register CallBack for all async events Assigned
    procedure RegisterAsyncEvents; virtual;
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
    function AsyncEvents: TDWAsyncEventTypeSet;
    // Set focus on component in an Ajax Callback
    procedure SetFocus;
    // Specifies whether the control responds to mouse, keyboard, and timer events.
    property Enabled;
    // Java Scripts to be executted on browser when especified event occurs
    property ScriptEvents: TDWScriptEvents;
    // Set the font of Control (See StyleRenderOptions)
    property Font;
    // Color of Component
    property Color;
    // property "class" of HTML Element, used for CSS styles
    property CssClass: string;
    // Defines which style properties will be rendered
    property StyleRenderOptions: TDWRenderOptions;
    // List of inline styles in pairs name: value
    property Style: TStringList read GetStyle write SetStyle;
    // The z-index property specifies the stack order of an element.
    // An element with greater stack order is always in front of an element with a lower stack order.
    // Note: z-index only works on positioned elements (position:absolute, position:relative, or position:fixed).
    // see: http://www.w3schools.com/csSref/pr_pos_z-index.asp
    property ZIndex: Integer;
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
    procedure SetTabStop(const Value: boolean);
    function GetTabOrder: TTabOrder;
    procedure SetTabOrder(const Value: TTabOrder);
    procedure UpdateTabOrder(Value: TTabOrder);
  public
    constructor Create(AOwner: TComponent); override;
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

implementation

uses DW.VCL.Container, DWUtils;

{ TDWControl }

constructor TDWControl.Create(AOwner: TComponent);
begin
  inherited;
  Font.Size            := 12;
  FRendered            := False;
  FAsyncRefreshControl := True;
end;

function TDWControl.RenderHTML: TDWElementTag;
begin
  Result := nil;
  //
end;

function TDWControl.Form: TDWCustomForm;
begin
  Result := DWFindParentForm(Self);
end;

{ TDWInputControl }

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
