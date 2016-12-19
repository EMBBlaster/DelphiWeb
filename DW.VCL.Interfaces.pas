unit DW.VCL.Interfaces;

interface

uses Classes, Controls, DW.VCL.ScriptParams, DWElementTag;

type

  // interface for all DWControls and Containers
  IDWControl = interface(IInterfaceComponentReference)
    ['{01D165DB-3058-4A07-89B3-54D1EC895366}']
    function GetScriptParams: TDWScriptParams;
    procedure SetScriptParams(const Value: TDWScriptParams);
    function GetScriptInsideTag: Boolean;
    procedure SetScriptInsideTag(const Value: Boolean);
    function GetScript: TStringList;
    procedure SetScript(const Value: TStringList);
    // Free the Control on next thread loop
    procedure Release;
    procedure InternalRenderScript(const AHTMLName: string; AScript: TStringList);

    // Return name of element in HTML.
    // If RootParent is an TDWCustomForm, HTMLName is same as Name,
    // else if RootParent is an TDWFrame, HTMLName is FrameName_ComponentName
    // this is necessary because one frame can be placed into one Form,
    // and this can cause duplicate names in same HTML Page
    function HTMLName: string;
    // Render AsyncEvents(ClallBacks)
    function RenderAsyncEvents: string;
    // Render control on full form load or on FRendered = false
    // Return one TDWElementTag with element HTML
    function RenderHTML: TDWElementTag;
    // Render HTML "class" tag property
    function RenderCSSClass: string;
    // Render control on form Async Calls
    // Return one TDWElementXHTMLTag with element XHTML
    function RenderAsync: TDWElementXHTMLTag;
    // Get Form where Component it is
    function Form: TControl;
    // return true if control is in DWAppplication release list
    // and waiting to be released in next DWApplication Loop
    function IsReleased: Boolean;
    // Params that will be replaced in scripts and in some controls content, for example in TDWText. @br
    // Params are specified in scripts as: {%param%}.
    property ScriptParams: TDWScriptParams read GetScriptParams write SetScriptParams;
    // Specifies if the script will be rendered inside the control tag or not. @br
    // If true the script will be rendered inside the tag. @br
    // If false a new div will be created to surround the control and the script will be rendered in this div, outside the control tag. @br
    // this is necessary script can't be placed inside the tag, for example in input controls.
    property ScriptInsideTag: Boolean read GetScriptInsideTag write SetScriptInsideTag;
    // User Defined Scripts
    property Script: TStringList read GetScript write SetScript;
  end;

  IDWInput = interface(IDWControl)
    ['{6078F30A-F033-40F6-8043-7042DAA5995A}']
    procedure SetValue(const AValue: string);
  end;

implementation

end.
