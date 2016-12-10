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

uses Classes, System.SysUtils, System.Types, Vcl.Graphics, VCL.Controls,
  Winapi.Windows, DWElementTag, DW.VCL.CustomForm, DWTypes,
  DW.VCL.ScriptEvents, DW.VCL.StyleRenderOptions, DW.VCL.Container,
  DW.VCL.ScriptParams, DW.VCL.Common, DW.VCL.Interfaces;

type

  // TDWControl is the Base for DW Controls
  TDWControl = class(TGraphicControl, IDWControl)
  private
    // Html id of main Tag
    // not same as HtmlName because same controls add one main div,
    // example Bootstrap FormGroup
    FMainID: string;

    // Defines whether the control will be fully updated in the next Async Call
    FAsyncRefreshControl: boolean;
    // define if control already Rendered
    FRendered: boolean;
    // Additional User defined Style to be render
    FStyle: TStringList;
    // Corresponds to html tabindex attribute. It will be rendered if tabindex <> 0.
    // Set to -1 to disable tabstop
    FTabIndex: Integer;
    // define if scripts is rendered inside the control tag or new Main tag
    // is created and put script on this
    FScriptInsideTag: boolean;
    // Events
    FOnAsyncClick: TDWAsyncProcedure;
    FOnAsyncMouseDown: TDWAsyncProcedure;
    FOnAsyncDoubleClick: TDWAsyncProcedure;
    FOnAsyncMouseMove: TDWAsyncProcedure;
    FOnAsyncMouseUp: TDWAsyncProcedure;
    FStyleRenderOptions: TDWRenderOptions;
    FOnAsyncMouseOut: TDWAsyncProcedure;
    FOnAsyncMouseOver: TDWAsyncProcedure;
    // The z-index property specifies the stack order of an element in a display browser
    // An element with greater stack order is always in front of an element with a lower stack order.
    FZIndex: Integer;
    // JavaScripts to be executted on browser when especified event occurs
    FScriptEvents: TDWScriptEvents;
    // property "class" of HTML Element, used for CSS styles
    FCss: string;
    FScript: TStringList;
    FScriptParams: TDWScriptParams;
    FOnAfterRender: TNotifyEvent;
    FOnRender: TNotifyEvent;
    FOnHTMLtag: TDWOnHtmlTagProcedure;
    FOnAfterAsyncChange: TNotifyEvent;
    FEditable: Boolean;
    procedure SetScript(const Value: TStringList);
    procedure OnScriptChange(ASender: TObject);
    procedure SetScriptInsideTag(const Value: boolean);
    procedure SetScriptParams(const Value: TDWScriptParams);
    procedure OnStyleChange(ASender: TObject);
    procedure SetOnAfterRender(const Value: TNotifyEvent);
    procedure SetOnRender(const Value: TNotifyEvent);
    procedure SetOnHTMLtag(const Value: TDWOnHtmlTagProcedure);
    function IsScriptEventsStored:Boolean;
    procedure SetEditable(const Value: Boolean);
    function GetScriptInsideTag: boolean;
    function GetScriptParams: TDWScriptParams;
    procedure DoAsyncMouseDown(aParams: TStringList);
    procedure DoAsyncMouseMove(aParams: TStringList);
    procedure DoAsyncMouseUp(aParams: TStringList);
    procedure DoAsyncDblClick(aParams: TStringList);
    procedure DoAsyncMouseOut(aParams: TStringList);
    procedure DoAsyncMouseOver(aParams: TStringList);
    function GetScript: TStringList;
  protected
    // Oldxxxx is used to compare values in Async Call,
    // to verify updateds property in controls
    FOldCss: string;
    FOldDisabled: boolean;
    FOldReadOnly: boolean;
    FOldStyle: string;
    FOldVisible: boolean;
    //paint the control in ide design mode
    procedure Paint; override;
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

    // Render HTML "style" tag property
    function RenderStyle: string; virtual;
    // Setters for Properties, to description see respective property
    procedure SetCss(const Value: string); virtual;
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
    // Execute event OnAsyncClick
    procedure DoAsyncClick(aParams: TStringList); virtual;
    // return true if control is disabled
    function IsDisabled: boolean; virtual;
    // return true if Component is ReadOnly
    function IsReadOnly: boolean; virtual;

    function InputSelector: string; virtual;
    // return the Suffix for items of an input EX:
    // For RadioGroup the InputSelector is '_INPUT'.
    // RadioGroup HtmlName = Radio
    // Then to select the RadioGroup, use '$("#' + HtmlName + '")'
    // and to select item, use  '$("#' + HtmlName + InputSelector + '")'
    function InputSuffix: string; virtual;
    // used to render HTML in descendant class
    procedure InternalRenderHTML(var AHTMLTag: TDWElementTag); virtual;
    // used to render XHR on Async Calls in descendant class
    procedure InternalRenderAsync(const aHTMLName: string); virtual;
    procedure InternalRenderScript(const AHTMLName: string; AScript: TStringList);virtual;
    function IsDesignMode:Boolean;
    property ActiveCss: string read FOldCss;
    property ActiveStyle: string read FOldStyle;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // used to render "class" attribute  on Async Calls in descendant class
    procedure InternalRenderCss(var ACss: string); virtual;
        // Render HTML "class" tag property
    function RenderCSSClass: string; virtual;
    // Get Form where Component it is
    function Form: TDWCustomForm;
    // Render control on full form load or on FRendered = false
    // Return one TDWElementTag with element HTML
    function RenderHTML: TDWElementTag; virtual;
    // Render control on form Async Calls
    // Return one TDWElementXHTMLTag with element XHTML
    function RenderAsync: TDWElementXHTMLTag; virtual;
    // Render AsyncEvents(ClallBacks)
    function RenderAsyncEvents: string; virtual;
    // Render Cursor Style and add to "style" tag property
    function RenderCursorStyle: string; virtual;
    // Render Control Inline Style
    procedure InternalRenderStyle(AStyle: TStringList); virtual;
    // Invalidate control force control to refresh on Design Mode
    // and to be Refreshed(RenderAsync) on form AsyncCalls
    procedure Invalidate; override;
    // Repaint control in Only in Design Mode
    procedure RepaintControl; virtual;
    // Force a full refresh of the control during an Async call.
    // Usually there is no need to use this method, only if some property changed during async calls is not reflected.
    procedure AsyncRefreshControl;
    // Cancel AsyncRefreshControl
    // Usually there is no need to use this method. It is for internal use.
    procedure ResetAsyncRefreshControl;
    // Remove a control from html flow. You should execute this when destroying a control durinc async calls before Freeing @br
    // If you are destroying a region is enought to execute this in that region, you don't need to execute it in each child control.
    procedure AsyncRemoveControl;
    // returns a string representing the the JQSelector for this object.
    // @preformatted(DWControl.JQSelector > '$(#"htmlname")')
    function JQSelector: string;
    // get the list of AsyncEvents actives in this control
    function AsyncEvents: TDWAsyncEventTypeSet; virtual;
    // Return name of element in HTML.
    // If RootParent is an TDWCustomForm, HTMLName is same as Name,
    // else if RootParent is an TDWFrame, HTMLName is FrameName_ComponentName
    // this is necessary because one frame can be placed into one Form,
    // and this can cause duplicate names in same HTML Page
    function HTMLName: string;
    // Return the root container of a component.
    // if component is in one TDWCustomForm, this return a Form,
    // else if component is in one TDWCustomFrame, this return a Frame
    // for compatibility the object returned is an TDWContainer,
    // but this never return containers, like a Panels, etc
    // the returned element to be casted usin some like this:
    // @preformatted(
    // if ReturnedObject.InheritsFrom(TDWCustomForm) then //its one Form
    // TDWCustomForm(ReturnedObject)."your code here"
    // else if ReturnedObject.InheritsFrom(TDWCustomFrame) then  //its one Frame
    // TDWCustomFrame(ReturnedObject)."your code here")
    function RootParent: TDWContainer;
    // Return the first Parent Container
    function ParentContainer: TDWContainer;
    // Set focus on component in an Ajax Callback
    procedure SetFocus;
  published
    // Specifies whether the control responds to mouse, keyboard, and timer events.
    property Enabled;
    //set if the control will be editable or read-only
    property Editable:Boolean read FEditable write SetEditable default True;
    // JavaScripts to be executted on browser when especified event occurs
    property ScriptEvents: TDWScriptEvents read FScriptEvents write SetScriptEvents stored IsScriptEventsStored;
    // Set the font of Control (See StyleRenderOptions)
    property Font;
    // Color of Component
    property Color;
    // property "class" of HTML Element, used for CSS styles
    property Css: string read FCss write SetCss;
    // Defines which style properties will be rendered
    property StyleRenderOptions: TDWRenderOptions read FStyleRenderOptions
      write SetStyleRenderOptions;
    // Specifies if the script will be rendered inside the control tag or not. @br
    // If true the script will be rendered inside the tag. @br
    // If false a new div will be created to surround the control and the script will be rendered in this div, outside the control tag. @br
    // this is necessary script can't be placed inside the tag, for example in input controls.
    property ScriptInsideTag: boolean read GetScriptInsideTag write SetScriptInsideTag default True;
    // Params that will be replaced in scripts and in some controls content, for example in TDWText. @br
    // Params are specified in scripts as: {%param%}.
    property ScriptParams: TDWScriptParams read GetScriptParams write SetScriptParams;
    // List of inline styles in pairs name: value
    property Style: TStringList read FStyle write SetStyle;
    // The z-index property specifies the stack order of an element.
    // An element with greater stack order is always in front of an element with a lower stack order.
    // Note: z-index only works on positioned elements (position:absolute, position:relative, or position:fixed).
    // see: http://www.w3schools.com/csSref/pr_pos_z-index.asp
    property ZIndex: Integer read FZIndex write SetZIndex;
    // Corresponds to html tabindex attribute. It will be rendered if tabindex <> 0. Set to -1 to disable tabstop
    property TabIndex: Integer read FTabIndex write FTabIndex default 0;
    // Specifies user javascript code that will be rendered and executed with this object. @br
    // You can define ScriptParams inside the script. ScriptParams are specified in scripts as: {%param%}. @br
    // With property ScriptInsideTag you can define if the script will be rendered inside or outside the script.
    property Script: TStringList read GetScript write SetScript;
    // Events
    property OnAsyncClick: TDWAsyncProcedure read FOnAsyncClick write SetOnAsyncClick;
    property OnAsyncDoubleClick: TDWAsyncProcedure read FOnAsyncDoubleClick
      write SetOnAsyncDoubleClick;
    property OnAsyncMouseMove: TDWAsyncProcedure read FOnAsyncMouseMove write SetOnAsyncMouseMove;
    property OnAsyncMouseOver: TDWAsyncProcedure read FOnAsyncMouseOver write SetOnAsyncMouseOver;
    property OnAsyncMouseOut: TDWAsyncProcedure read FOnAsyncMouseOut write SetOnAsyncMouseOut;
    property OnAsyncMouseDown: TDWAsyncProcedure read FOnAsyncMouseDown write SetOnAsyncMouseDown;
    property OnAsyncMouseUp: TDWAsyncProcedure read FOnAsyncMouseUp write SetOnAsyncMouseUp;
    // Occurs after component is rendered.
    property OnAfterRender: TNotifyEvent read FOnAfterRender write SetOnAfterRender;
    // Occurs before component is rendered.
    property OnRender: TNotifyEvent read FOnRender write SetOnRender;
    // Occurs after HTMLTag is created
    property OnHTMLtag: TDWOnHtmlTagProcedure read FOnHTMLtag write SetOnHTMLtag;
    // Occurs after component is changed on an Asyn call, it doesn't occurs if the control is fully rendered
    property OnAfterAsyncChange: TNotifyEvent read FOnAfterAsyncChange write FOnAfterAsyncChange;
  end;

  // TDWInputControl is the base for all Controls that accept input
  TDWInputControl = class(TDWControl, IDWInput)
  private
    // Events
    FOnAsyncSelect: TDWAsyncProcedure;
    FOnAsyncExit: TDWAsyncProcedure;
    FOnAsyncKeyPress: TDWAsyncProcedure;
    FOnAsyncKeyDown: TDWAsyncProcedure;
    FOnAsyncChange: TDWAsyncProcedure;
    FOnAsyncEnter: TDWAsyncProcedure;
    FOnAsyncKeyUp: TDWAsyncProcedure;

    procedure SetTabStop(const Value: boolean);
    function GetTabStop: boolean;
    procedure SetOnAsyncChange(const Value: TDWAsyncProcedure);
    procedure SetOnAsyncEnter(const Value: TDWAsyncProcedure);
    procedure SetOnAsyncExit(const Value: TDWAsyncProcedure);
    procedure SetOnAsyncKeyDown(const Value: TDWAsyncProcedure);
    procedure SetOnAsyncKeyPress(const Value: TDWAsyncProcedure);
    procedure SetOnAsyncKeyUp(const Value: TDWAsyncProcedure);
    procedure SetOnAsyncSelect(const Value: TDWAsyncProcedure);
    procedure DoAsyncCHange(aParams: TStringList);
    procedure DoAsyncSelect(aParams: TStringList);
    procedure DoAsyncExit(aParams: TStringList);
    procedure DoAsyncKeyPress(aParams: TStringList);
    procedure DoAsyncKeyDown(aParams: TStringList);
    procedure DoAsyncEnter(aParams: TStringList);
    procedure DoAsyncKeyUp(aParams: TStringList);
  protected
    FOldText: string;
    //Used to change value on receive async calls
    procedure SetValue(const AValue: string); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    // get the list of AsyncEvents actives in this control
    function AsyncEvents: TDWAsyncEventTypeSet; override;
    // Render control on form Async Calls
    // Return one TDWElementXHTMLTag with element XHTML
    function RenderAsync: TDWElementXHTMLTag; Override;
    // Render AsyncEvents(ClallBacks)
    function RenderAsyncEvents: string; override;
  published
    property TabStop: boolean read GetTabStop write SetTabStop default True;
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

uses DWUtils, DW.VCL.CustomInput, DW.VCL.Input;

type
  ThackDwControl = class(TDWControl);



{ TDWControl }

function TDWControl.AsyncEvents: TDWAsyncEventTypeSet;
begin
  Result := [];
  if Assigned(FOnAsyncClick) then
    Result := [ae_click];
  if Assigned(FOnAsyncMouseDown) then
    Include(Result, ae_mousedown);
  if Assigned(FOnAsyncDoubleClick) then
    Include(Result, ae_dblclick);
  if Assigned(FOnAsyncMouseMove) then
    Include(Result, ae_mousemove);
  if Assigned(FOnAsyncMouseUp) then
    Include(Result, ae_mouseup);
  if Assigned(FOnAsyncMouseOut) then
    Include(Result, ae_mouseout);
  if Assigned(FOnAsyncMouseOver) then
    Include(Result, ae_mouseover);
end;

procedure TDWControl.AsyncRefreshControl;
begin
  FAsyncRefreshControl := True;
  Invalidate;
end;

procedure TDWControl.AsyncRemoveControl;
begin

end;

procedure TDWControl.ResetAsyncRefreshControl;
begin
  FAsyncRefreshControl := False;
end;

constructor TDWControl.Create(AOwner: TComponent);
begin
  inherited;
  FEditable:= True;
  Font.Size                 := 12;
  FRendered                 := False;
  FAsyncRefreshControl      := True;
  FMainID                   := '';
  FTabIndex                 := 0;
  FScript                   := TStringList.Create;
  FScript.OnChange          := OnScriptChange;
  FScriptInsideTag          := True;
  FScriptParams             := TDWScriptParams.Create;
  FScriptParams.OnChange    := OnScriptChange;
  FScriptEvents:= TDWScriptEvents.Create(Self);
  FStyle                    := TStringList.Create;
  FStyle.OnChange           := OnStyleChange;
  FStyle.NameValueSeparator := ':';
  FStyleRenderOptions:= TDWRenderOptions.Create(Self);
  FStyleRenderOptions.SetSubComponent(True);
  FStyleRenderOptions.RenderSize:= False;
  FStyleRenderOptions.RenderPosition:= False;
  FStyleRenderOptions.RenderFont:= False;
  FStyleRenderOptions.RenderZIndex:= False;
  FStyleRenderOptions.RenderVisibility:= False;
  FStyleRenderOptions.RenderStatus:= False;
  FStyleRenderOptions.RenderAbsolute:= False;
  FStyleRenderOptions.RenderPadding:= False;
  FStyleRenderOptions.RenderBorder:= False;
  FStyleRenderOptions.UseDisplay:= False;
  //default parent
  if (Parent = nil)
  and (AOwner <> nil)
  and (AOwner is TWinControl) then
   Parent:= TWinControl(Aowner);
  //default Name
  if name = '' then
    name := DWGetUniqueComponentName(Owner, Copy(ClassName,2,MaxInt));
end;

destructor TDWControl.Destroy;
begin
  // FreeAndNil(FCustomAsyncEvents);
  // FreeAndNil(FCustomRestEvents);
  FreeAndNil(FScript);
  FreeAndNil(FScriptParams);
  FreeAndNil(FStyle);
  FStyleRenderOptions.Free;
  inherited;
end;

procedure TDWControl.OnStyleChange(ASender: TObject);
begin
  Invalidate;
end;

procedure TDWControl.Paint;
var
  LRect, LIcon: TRect;
  s, c: string;
  LMultiLine: boolean;
begin
  LRect := Rect(0, 0, Width, Height);

  Canvas.Brush.Color := clWhite;
  Canvas.Pen.Color := clGray;
  Canvas.Font.Name := CNST_DEFAULTFONTNAME;
  Canvas.Font.Size := 10;
  Canvas.Font.Color := clBlack;
  Canvas.Rectangle(LRect);

  Inc(LRect.Top, 2);
  Inc(LRect.Left, 2);
  Dec(LRect.Bottom, 2);
  Dec(LRect.Right, 2);
  Canvas.Pen.Color := clLtGray;
  Canvas.Rectangle(LRect);

  if Self is TDWCustomInput then begin
    LMultiLine := False;

    s := TDWCustomInput(Self).DataField;
    if Self is TDWCustomTextInput then
      begin
        if s = '' then
          s := TDWInput(Self).Text;
        if s = '' then begin
          s := TDWInput(Self).PlaceHolder;
          Canvas.Font.Color := clLtGray;
        end;
        if Self is TDWMemo then
          LMultiLine := True;
      end
    else if Self is TDWSelect then
      begin
        LMultiLine := TDWSelect(Self).Size <> 1;
        if s = '' then
          if LMultiLine then
            s := TDWSelect(Self).Items.Text
          else if TDWSelect(Self).Items.Count > 0 then
            s := TDWSelect(Self).Items[0];
        if not LMultiLine then begin
          LIcon := Rect(LRect.Right-18,LRect.Top+1,LRect.Right-1,LRect.Bottom-1);
          Canvas.Font.Name := CNST_GLYPHICONSFONT;
          Canvas.Brush.Color := clLtGray;
          Canvas.Rectangle(LIcon);
          c := GetGlyphiconChar('chevron-down', 'V');
          if c <> '' then begin
            DrawTextEx(Canvas.Handle, PChar(c), 1, LIcon, DT_CENTER+DT_SINGLELINE+DT_VCENTER, nil);
            Canvas.Font.Name := CNST_DEFAULTFONTNAME;
            Canvas.Brush.Color := clWhite;
            Dec(LRect.Right, 20);
          end;
        end;
      end;

    Inc(LRect.Top, 1);
    Inc(LRect.Left, 8);
    Dec(LRect.Bottom, 1);
    Dec(LRect.Right, 8);
    if LMultiLine then
      Canvas.TextRect(LRect,s,[])
    else
      DrawTextEx(Canvas.Handle, PChar(s), Length(s), LRect, DT_SINGLELINE+DT_VCENTER, nil);
  end;
end;

function TDWControl.ParentContainer: TDWContainer;
begin
  Result := GetParentContainer(self);
end;

procedure TDWControl.OnScriptChange(ASender: TObject);
begin
  AsyncRefreshControl;
end;

procedure TDWControl.DoAsyncClick(aParams: TStringList);
begin
  if Assigned(FOnAsyncClick) then
    FOnAsyncClick(self, aParams);
end;

procedure TDWControl.DoAsyncMouseDown(aParams: TStringList);
begin
  if Assigned(FOnAsyncMouseDown) then
    FOnAsyncMouseDown(self, aParams);
end;

procedure TDWControl.DoAsyncMouseMove(aParams: TStringList);
begin
  if Assigned(FOnAsyncMouseMove) then
    FOnAsyncMouseMove(self, aParams);
end;

procedure TDWControl.DoAsyncMouseUp(aParams: TStringList);
begin
  if Assigned(FOnAsyncMouseUp) then
    FOnAsyncMouseUp(self, aParams);
end;

procedure TDWControl.DoAsyncDblClick(aParams: TStringList);
begin
  if Assigned(FOnAsyncDoubleClick) then
    FOnAsyncDoubleClick(self, aParams);
end;

procedure TDWControl.DoAsyncMouseOut(aParams: TStringList);
begin
  if Assigned(FOnAsyncMouseOut) then
    FOnAsyncMouseOut(self, aParams);
end;

procedure TDWControl.DoAsyncMouseOver(aParams: TStringList);
begin
  if Assigned(FOnAsyncMouseOver) then
    FOnAsyncMouseOver(self, aParams);
end;

function TDWControl.RenderAsync: TDWElementXHTMLTag;
var
  xHTMLName: string;
  xInputSelector: string;
begin
  Result := nil;

  if FAsyncRefreshControl or not FRendered then
    begin
      xHTMLName := FMainID;
      TDWBSCommon.RenderAsync(xHTMLName, self);
    end
  else
    begin
      xHTMLName := HTMLName;
      if InputSelector <> '' then
        xInputSelector := FMainID + InputSelector
      else
        xInputSelector := xHTMLName + InputSuffix;

      TDWBSCommon.SetAsyncClass(xHTMLName, RenderCSSClass, FOldCss);
      TDWBSCommon.SetAsyncDisabled(xInputSelector, IsDisabled, FOldDisabled);
      TDWBSCommon.SetAsyncReadOnly(xInputSelector, IsReadOnly, FOldReadOnly);
      TDWBSCommon.SetAsyncStyle(xHTMLName, RenderStyle, FOldStyle);
      TDWBSCommon.SetAsyncVisible(FMainID, Visible, FOldVisible);
      InternalRenderAsync(xHTMLName);

      if Assigned(FOnAfterAsyncChange) then
        FOnAfterAsyncChange(self);

      { TODO 1 -oDELCIO -cIMPLEMENT : Global OnAfterAsyncChange Event }
      (* if Assigned(gIWBSOnAfterAsyncChange) then
        gIWBSOnAfterAsyncChange(Self, xHTMLName); *)
    end;

end;

function TDWControl.RenderAsyncEvents: string;
var
  LCallbackName: string;
begin
  if Assigned(FOnAsyncClick) then
    begin
      LCallbackName := DWApplication.RegisterCallBack(self, ae_click, DoAsyncClick);
      Result        := JQSelector + '.off("click.DW").on("click.DW", ' + 'function (e) {' +
        'executeAjaxCallBack("", ' + JQSelector + '[0], "' + LCallbackName + '");' + '})';
    end;
  if Assigned(FOnAsyncMouseDown) then
    begin
      LCallbackName := DWApplication.RegisterCallBack(self, ae_mousedown, DoAsyncMouseDown);
      Result        := JQSelector + '.off("mousedown.DW").on("mousedown.DW", ' + 'function (e) {' +
        'executeAjaxCallBack("", ' + JQSelector + '[0], "' + LCallbackName + '");' + '})';
    end;
  if Assigned(FOnAsyncDoubleClick) then
    begin
      LCallbackName := DWApplication.RegisterCallBack(self, ae_dblclick, DoAsyncDblClick);
      Result        := JQSelector + '.off("dblclick.DW").on("dblclick.DW", ' + 'function (e) {' +
        'executeAjaxCallBack("", ' + JQSelector + '[0], "' + LCallbackName + '");' + '})';
    end;
  if Assigned(FOnAsyncMouseMove) then
    begin
      LCallbackName := DWApplication.RegisterCallBack(self, ae_mousemove, DoAsyncMouseMove);
      Result        := JQSelector + '.off("mousemove.DW").on("mousemove.DW", ' + 'function (e) {' +
        'executeAjaxCallBack("", ' + JQSelector + '[0], "' + LCallbackName + '");' + '})';
    end;
  if Assigned(FOnAsyncMouseUp) then
    begin
      LCallbackName := DWApplication.RegisterCallBack(self, ae_mouseup, DoAsyncMouseUp);
      Result        := JQSelector + '.off("mouseup.DW").on("mouseup.DW", ' + 'function (e) {' +
        'executeAjaxCallBack("", ' + JQSelector + '[0], "' + LCallbackName + '");' + '})';
    end;
  if Assigned(FOnAsyncMouseOut) then
    begin
      LCallbackName := DWApplication.RegisterCallBack(self, ae_mouseout, DoAsyncMouseOut);
      Result        := JQSelector + '.off("mouseout.DW").on("mouseout.DW", ' + 'function (e) {' +
        'executeAjaxCallBack("", ' + JQSelector + '[0], "' + LCallbackName + '");' + '})';
    end;
  if Assigned(FOnAsyncMouseOver) then
    begin
      LCallbackName := DWApplication.RegisterCallBack(self, ae_mouseover, DoAsyncMouseOver);
      Result        := JQSelector + '.off("mouseover.DW").on("mouseover.DW", ' + 'function (e) {' +
        'executeAjaxCallBack("", ' + JQSelector + '[0], "' + LCallbackName + '");' + '})';
    end;
end;

function TDWControl.RenderBGColor: string;
begin

end;

function TDWControl.RenderCSSClass: string;
begin
  Result := Css;
  InternalRenderCss(Result);
end;

function TDWControl.RenderCursorStyle: string;
begin
  { TODO 1 -oDELCIO -cIMPLEMENT : Rende Cursor Style }
end;

function TDWControl.RenderHTML: TDWElementTag;
begin
  Result       := nil;
  FOldCss      := RenderCSSClass;
  FOldDisabled := IsDisabled;
  FOldReadOnly := IsReadOnly;
  FOldStyle    := RenderStyle;
  FOldVisible  := Visible;

  InternalRenderHTML(Result);
  if Result = nil then
    raise Exception.Create('HTML tag not created');

  TDWBSCommon.RenderScript(self, Result);
  FMainID              := Result.Params.Values['id'];
  FAsyncRefreshControl := False;
  FRendered            := True;
end;

function TDWControl.RenderPositionStyle: string;
begin

end;

function TDWControl.RenderSizeStyle: string;
begin

end;

function TDWControl.RenderStyle: string;
begin
  Result := TDWBSCommon.RenderStyle(self);
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
  //CompTest: TControl;
  LForm: TDWCustomForm;
begin
  Result := nil;
  { TODO 1 -oDELCIO -cIMPLEMENT :  Find RootParent for Frame }
  (* CompTest := self;
    while Assigned(CompTest) and (not(CompTest is TDWCustomFrame)) do
    CompTest := CompTest.Parent; *)
  if Result = nil then
    Result := TDWContainer(Form);
end;

procedure TDWControl.SetCss(const Value: string);
begin
  FCss := Value;
end;

procedure TDWControl.SetEditable(const Value: Boolean);
begin
  FEditable := Value;
end;

procedure TDWControl.SetFocus;
begin

end;

procedure TDWControl.SetOnAfterRender(const Value: TNotifyEvent);
begin
  FOnAfterRender := Value;
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
  FOnAsyncMouseOut := Value;
end;

procedure TDWControl.SetOnAsyncMouseOver(const Value: TDWAsyncProcedure);
begin
  FOnAsyncMouseOver := Value;
end;

procedure TDWControl.SetOnAsyncMouseUp(const Value: TDWAsyncProcedure);
begin
  FOnAsyncMouseUp := Value;
end;

procedure TDWControl.SetOnHTMLtag(const Value: TDWOnHtmlTagProcedure);
begin
  FOnHTMLtag := Value;
end;

procedure TDWControl.SetOnRender(const Value: TNotifyEvent);
begin
  FOnRender := Value;
end;

procedure TDWControl.SetScript(const Value: TStringList);
begin
  FScript.Assign(Value);
end;

procedure TDWControl.SetScriptEvents(const Value: TDWScriptEvents);
begin
  FScriptEvents := Value;
end;

procedure TDWControl.SetScriptInsideTag(const Value: boolean);
begin
  if FScriptInsideTag <> Value then
    begin
      FScriptInsideTag := Value;
      AsyncRefreshControl;
    end;
end;

procedure TDWControl.SetScriptParams(const Value: TDWScriptParams);
begin
  FScriptParams.Assign(Value);
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
  Result := DWFindParentForm(self);
end;

function TDWControl.GetScript: TStringList;
begin
  Result:= FScript;
end;

function TDWControl.GetScriptInsideTag: boolean;
begin
  Result:= FScriptInsideTag;
end;

function TDWControl.GetScriptParams: TDWScriptParams;
begin
  Result:= FScriptParams;
end;

function TDWControl.HTMLName: string;
var
  LRootParent: TDWContainer;
begin
  LRootParent := RootParent;
  //
  if LRootParent.InheritsFrom(TDWCustomForm) then
    HTMLName := Name
  else
    HTMLName := LRootParent.Name + '_' + Name;
end;

function TDWControl.InputSelector: string;
begin
  Result := '';
end;

function TDWControl.InputSuffix: string;
begin
  Result := '';
end;

procedure TDWControl.InternalRenderAsync(const aHTMLName: string);
begin
  //
end;

procedure TDWControl.InternalRenderCss(var ACss: string);
begin
  //
end;

procedure TDWControl.InternalRenderHTML(var AHTMLTag: TDWElementTag);
begin
  //
end;

procedure TDWControl.InternalRenderScript(const AHTMLName: string;
  AScript: TStringList);
begin
  //
end;

procedure TDWControl.InternalRenderStyle(AStyle: TStringList);
begin
  if StyleRenderOptions.RenderSize then
    begin
      AStyle.Values['height']:= IntToStr(Height) + 'px';
      AStyle.Values['width']:= IntToStr(Width) + 'px';
    end;
  if StyleRenderOptions.RenderPosition then
    begin
      AStyle.Values['left']:= IntToStr(Left) + 'px';
      AStyle.Values['top']:= IntToStr(Top) + 'px';
    end;
  if StyleRenderOptions.RenderFont then
    begin
      AStyle.Values['font-family']:= Font.Name;
      AStyle.Values['font-size']:= IntToStr(Font.Size) + 'px';
    end;
  if StyleRenderOptions.RenderZIndex then
    begin
      if ZIndex <> 0 then
        AStyle.Values['z-index']:= IntToStr(ZIndex);
    end;
  if StyleRenderOptions.RenderVisibility then
    begin
      if not Visible then
        AStyle.Values['visibility']:= 'hidden';
    end;
  if StyleRenderOptions.RenderPadding then
    begin
      { TODO 1 -oDELCIO -cIMPLEMENT : Render Controls Padding }
    end;
  if StyleRenderOptions.RenderBorder then
    begin
      { TODO 1 -oDELCIO -cIMPLEMENT : Render Controls Border }
    end;  if StyleRenderOptions.RenderAbsolute then
    begin
      { TODO 1 -oDELCIO -cIMPLEMENT : Render Controls Absolute }
    end;
end;

procedure TDWControl.Invalidate;
begin
  inherited;

end;

function TDWControl.IsDesignMode: Boolean;
begin
  Result:= csDesigning in ComponentState;
end;

function TDWControl.IsDisabled: boolean;
begin
  Result := not Enabled;
end;

function TDWControl.IsReadOnly: boolean;
begin
  Result := True;
end;

function TDWControl.IsScriptEventsStored: Boolean;
begin
  Result := FScriptEvents.Count > 0;
end;

function TDWControl.JQSelector: string;
begin
  Result := '$("#' + HTMLName + '")';
end;

{ TDWInputControl }

function TDWInputControl.AsyncEvents: TDWAsyncEventTypeSet;
begin
  Result := inherited;
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
  FTabIndex := -1;
end;

function TDWInputControl.RenderAsync: TDWElementXHTMLTag;
begin
  Result := inherited;
end;

function TDWInputControl.RenderAsyncEvents: string;
var
  LCallbackName: string;
begin
  inherited;
  if Assigned(FOnAsyncChange) then
    begin
      LCallbackName := DWApplication.RegisterCallBack(self, ae_change, DoAsyncCHange);
      Result        := JQSelector + '.off("change.DW").on("change.DW", ' + 'function (e) {' +
        'executeAjaxCallBack("", ' + JQSelector + '[0], "' + LCallbackName + '");' + '})';
      // DWApplication.CallbackResp.AddScriptToExecute(LScript);
    end;
  if Assigned(FOnAsyncSelect) then
    begin
      LCallbackName := DWApplication.RegisterCallBack(self, ae_select, DoAsyncSelect);
      Result        := JQSelector + '.off("select.DW").on("select.DW", ' + 'function (e) {' +
        'executeAjaxCallBack("", ' + JQSelector + '[0], "' + LCallbackName + '");' + '})';
    end;
  if Assigned(FOnAsyncExit) then
    begin
      LCallbackName := DWApplication.RegisterCallBack(self, ae_blur, DoAsyncExit);
      Result        := JQSelector + '.off("blur.DW").on("blur.DW", ' + 'function (e) {' +
        'executeAjaxCallBack("", ' + JQSelector + '[0], "' + LCallbackName + '");' + '})';
    end;
  if Assigned(FOnAsyncKeyPress) then
    begin
      LCallbackName := DWApplication.RegisterCallBack(self, ae_keypress, DoAsyncKeyPress);
      Result        := JQSelector + '.off("keypress.DW").on("keypress.DW", ' + 'function (e) {' +
        'executeAjaxCallBack("", ' + JQSelector + '[0], "' + LCallbackName + '");' + '})';
    end;
  if Assigned(FOnAsyncKeyDown) then
    begin
      LCallbackName := DWApplication.RegisterCallBack(self, ae_keydown, DoAsyncKeyDown);
      Result        := JQSelector + '.off("keydown.DW").on("keydown.DW", ' + 'function (e) {' +
        'executeAjaxCallBack("", ' + JQSelector + '[0], "' + LCallbackName + '");' + '})';
    end;
  if Assigned(FOnAsyncEnter) then
    begin
      LCallbackName := DWApplication.RegisterCallBack(self, ae_focus, DoAsyncEnter);
      Result        := JQSelector + '.off("focus.DW").on("focus.DW", ' + 'function (e) {' +
        'executeAjaxCallBack("", ' + JQSelector + '[0], "' + LCallbackName + '");' + '})';
    end;
  if Assigned(FOnAsyncKeyUp) then
    begin
      LCallbackName := DWApplication.RegisterCallBack(self, ae_keyup, DoAsyncKeyUp);
      Result        := JQSelector + '.off("keyup.DW").on("keyup.DW", ' + 'function (e) {' +
        'executeAjaxCallBack("", ' + JQSelector + '[0], "' + LCallbackName + '");' + '})';
    end;
end;

procedure TDWInputControl.DoAsyncCHange(aParams: TStringList);
begin
  if Assigned(FOnAsyncChange) then
    FOnAsyncChange(self, aParams);
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

function TDWInputControl.GetTabStop: boolean;
begin
  Result := FTabIndex <> -1;
end;

procedure TDWInputControl.SetTabStop(const Value: boolean);
begin
  if (FTabIndex = -1) and Value then
    begin
      FTabIndex := 0;
      Invalidate;
    end
  else if (FTabIndex <> -1) and (Not Value) then
    begin
      FTabIndex := -1;
      Invalidate;
    end
end;

procedure TDWInputControl.SetValue(const AValue: string);
begin
  Text:= AValue;
end;


procedure TDWInputControl.DoAsyncSelect(aParams: TStringList);
begin
  if Assigned(FOnAsyncSelect) then
    FOnAsyncSelect(self, aParams);
end;

procedure TDWInputControl.DoAsyncExit(aParams: TStringList);
begin
  if Assigned(FOnAsyncExit) then
    FOnAsyncExit(self, aParams);
end;

procedure TDWInputControl.DoAsyncKeyPress(aParams: TStringList);
begin
  if Assigned(FOnAsyncKeyPress) then
    FOnAsyncKeyPress(self, aParams);
end;

procedure TDWInputControl.DoAsyncKeyDown(aParams: TStringList);
begin
  if Assigned(FOnAsyncKeyDown) then
    FOnAsyncKeyDown(self, aParams);
end;

procedure TDWInputControl.DoAsyncEnter(aParams: TStringList);
begin
  if Assigned(FOnAsyncEnter) then
    FOnAsyncEnter(self, aParams);
end;

procedure TDWInputControl.DoAsyncKeyUp(aParams: TStringList);
begin
  if Assigned(FOnAsyncKeyUp) then
    FOnAsyncKeyUp(self, aParams);
end;

end.
