unit DWLayoutRender;

interface

uses Classes, System.SysUtils, System.StrUtils, VCL.Controls, DW.VCL.CustomForm,
  DW.VCL.Interfaces, DWElementTag;

type

  { This is responsable for transform the form in an HTML Page }
  TDWLayoutRender = class(TComponent)
  private
    FLinkFiles: TStringList;
    FLocalThemeCss: string;
    procedure SetLocalThemeThemeCss(const Value: string);
  public
    constructor Create(AOnwer: TComponent); override;
    destructor Destroy; override;
    procedure AddLinkFile(const AFile: string);
    function ParseLinkFile(const AUrlBase, AFile: string; ADisableCache: boolean = True): string;
    procedure ProcessChildControls(aControl: TControl; aParentElement: TDWElementTag);
    procedure ProcessForm(aForm: TDWCustomForm);
    procedure ProcessFormAsync(aForm: TDWCustomForm);
    // Need to be addeded after all css files
    property LocalThemeCss: string read FLocalThemeCss write SetLocalThemeThemeCss;
  end;

implementation

uses DWUtils, DW.VCL.Control, DW.VCL.Container,
  DW.VCL.Common, DWGlobal, DW.VCL.DBControl, DW.HTML.Page;

{ TDWLayoutManager }

procedure TDWLayoutRender.AddLinkFile(const AFile: string);
begin
  if FLinkFiles = nil then
    FLinkFiles := TStringList.Create;
  if FLinkFiles.IndexOf(AFile) = -1 then
    FLinkFiles.Add(AFile);
end;

constructor TDWLayoutRender.Create(AOnwer: TComponent);
begin
  inherited;

end;

destructor TDWLayoutRender.Destroy;
begin

  inherited;
end;

function TDWLayoutRender.ParseLinkFile(const AUrlBase, AFile: string;
  ADisableCache: boolean): string;
var
  LFile: string;
  LDisableCache: boolean;
begin
  LDisableCache := ADisableCache;

  LFile := ReplaceStr(AFile, '/<dwlibpath>/', DWServer.LibDir);
  if AnsiStartsStr('//', LFile) or AnsiContainsStr(LFile, '://') then
    LDisableCache := False
  else
    LFile := MakeValidFileUrl(AUrlBase, LFile);

  if AnsiEndsStr('.js', LFile) then
    Result := '<script type="text/javascript" src="' + LFile +
      IfThen(LDisableCache, '?v=' + DWServer.RefreshCacheParam) + '"></script>'

  else if AnsiEndsStr('.css', LFile) then
    Result := '<link rel="stylesheet" type="text/css" href="' + LFile +
      IfThen(LDisableCache, '?v=' + DWServer.RefreshCacheParam) + '">'

  else
    raise Exception.Create('Unknown file type');

end;

procedure TDWLayoutRender.ProcessFormAsync(aForm: TDWCustomForm);
  procedure ProcessChildControlsAsync(aControl: TControl);
  var
    // CurEl: TDWElementTag;
    C: Integer;
    L_IControl: IDWControl;
  begin
    if Supports(aControl, IDWControl, L_IControl) and (L_IControl <> nil) then
      begin
        // not render controls marked for release
        if L_IControl.IsReleased then
          Exit;
        // render element tag
        L_IControl.RenderAsync;
      end;
    // if control is an container
    if csAcceptsControls in aControl.ControlStyle then
      begin
        // process all child controls
        for C := 0 to (aControl as TDWContainer).ControlCount - 1 do
          begin
            ProcessChildControlsAsync((aControl as TDWContainer).Controls[C]);
          end;
      end;
  end;

var
  I: Integer;
begin
  // Render all Child Changes
  for I := 0 to aForm.ControlCount - 1 do
    begin
      ProcessChildControlsAsync(aForm.Controls[I]);
    end;
end;

procedure TDWLayoutRender.ProcessChildControls(aControl: TControl; aParentElement: TDWElementTag);
var
  CurEl: TDWElementTag;
  C: Integer;
  L_IControl: IDWControl;
  L_IPArent: IDWControl;
  LForm: TDWCustomForm;
begin
  if Supports(aControl, IDWControl, L_IControl) then
    begin
      // not render controls waiting to be released
      if L_IControl.IsReleased then
        Exit;
      // render element tag
      CurEl := L_IControl.RenderHTML;
      // Render element style
      CurEl.AddStringParam('style', TDWBSCommon.RenderStyle(aControl));
      // Add element to your parent
      aParentElement.Contents.AddElemetAsObject(CurEl);
      // if control accept input, init control for callback values
      if aControl.InheritsFrom(TDWInputControl) then
        begin
          LForm := (Self.Owner as TDWCustomForm);
          // if control parent is forrm
          if aControl.Parent.InheritsFrom(TDWCustomForm) then
            // init control in form
            LForm.HTMLPage.HTMLTag.HeadTag.InitComponents.Add('DWInitControl(DWAjaxForm, "' +
              (aControl as TDWControl).HTMLName + '", true, true);')
          else if (aControl.Parent <> nil) // if Control has parent
            and (Supports(aControl.Parent, IDWControl, L_IPArent)) then
            // init control in your parent
            LForm.HTMLPage.HTMLTag.HeadTag.InitComponents.Add('DWInitControl(' + L_IPArent.HTMLName
              + ', "' + (aControl as TDWControl).Name + '", true, true);');
        end;

      { ---> Moved to RenderHTML
        // if control is an container
        if csAcceptsControls in AControl.ControlStyle then
        begin
        // process all child controls
        (AControl as TDWContainer).RenderComponents(CurEl);

        (*for C := 0 to (AControl as TDWContainer).ControlCount - 1 do
        begin
        ProcessChildControls((AControl as TDWContainer).Controls[C], CurEl);
        end; *)
        end; }
    end;

end;

procedure TDWLayoutRender.ProcessForm(aForm: TDWCustomForm);
var
  I: Integer;
  DWInitComponents: string;

begin
  with aForm.HTMLPage.HTMLTag do
    begin
      // Render Head
      // HeadTag.Contents.AddText('<meta charset="Windows-1252">');
      HeadTag.Title.Contents.AddText(aForm.Caption, True);
      HeadTag.Contents.AddText
        ('<meta name="viewport" content="width=device-width, initial-scale=1">');
      // JQuery
      // HeadTag.Contents.AddText('<script src="https://code.jquery.com/jquery-3.1.1.js" ' +
      // 'integrity="sha256-16cdPddA6VdVInumRGo6IbivbERE8p7CQR3HzTBuELA=" crossorigin="anonymous"></script>');
      HeadTag.Contents.AddText(ParseLinkFile(DWServer.UrlBase,
        DWServer.DocDir + '\dwlib\js\jquery-3.1.1.js', False));

      // Bootstrap
      // HeadTag.Contents.AddText('<link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.css">');
      HeadTag.Contents.AddText(ParseLinkFile(DWServer.UrlBase,
        DWServer.DocDir + '\dwlib\css\bootstrap.css', False));
      HeadTag.Contents.AddText(ParseLinkFile(DWServer.UrlBase,
        DWServer.DocDir + '\dwlib\css\bootstrap-theme.css', False));

      // HeadTag.Contents.AddText('<link rel="stylesheet" href="https://bootswatch.com/cerulean/bootstrap.css">');
      // HeadTag.Contents.AddText('<script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.js"></script>');
      HeadTag.Contents.AddText(ParseLinkFile(DWServer.UrlBase,
        DWServer.DocDir + '\dwlib\js\bootstrap.js', False));
      // Global Link Files
      if gIWBSLinkFiles <> nil then
        for I := 0 to gIWBSLinkFiles.Count - 1 do
          HeadTag.Contents.AddText(ParseLinkFile(DWServer.UrlBase, gIWBSLinkFiles[I]), False);

      // DelphiWeb
      HeadTag.Contents.AddText(ParseLinkFile(DWServer.UrlBase,
        DWServer.DocDir + '\dwlib\css\dw.css', False));

      HeadTag.Contents.AddText(ParseLinkFile(DWServer.UrlBase,
        DWServer.DocDir + '\dwlib\dwDebug.js', False));
      HeadTag.Contents.AddText(ParseLinkFile(DWServer.UrlBase,
        DWServer.DocDir + '\dwlib\dwBrowser.js', False));
      HeadTag.Contents.AddText(ParseLinkFile(DWServer.UrlBase,
        DWServer.DocDir + '\dwlib\dwAjax.js', False));
      HeadTag.Contents.AddText(ParseLinkFile(DWServer.UrlBase,
        DWServer.DocDir + '\dwlib\dwMain.js', False));
      // HeadTag.Contents.AddText(ParseLinkFile(DWServer.UrlBase,
      // DWServer.DocDir + '\dwlib\dwEvents.js', False));
      HeadTag.Contents.AddText(ParseLinkFile(DWServer.UrlBase,
        DWServer.DocDir + '\dwlib\dwControl.js', False));
      HeadTag.Contents.AddText(ParseLinkFile(DWServer.UrlBase,
        DWServer.DocDir + '\dwlib\dwControls.js', False));
      HeadTag.Contents.AddText(ParseLinkFile(DWServer.UrlBase,
        DWServer.DocDir + '\dwlib\dwFunctions.js', False));
      // HeadTag.Contents.AddText(ParseLinkFile(DWServer.UrlBase,
      // DWServer.DocDir + '\dwlib\dwDom.js', False));

      // Render Body
      for I := 0 to aForm.ControlCount - 1 do
        begin
          ProcessChildControls(aForm.Controls[I], BodyTag);
        end;

      // Render AjaxForm
      // -->> this is responsable for send changed fields back to the server on Async Calls
      with BodyTag.Contents.AddElement('form', True) do
        begin
          // method="POST" name="DWAjaxForm" action="/$/"
          AddStringParam('method', 'POST');
          AddStringParam('action', '/' + aForm.Name);
          AddStringParam('ID', 'DWAjaxForm');
          AddStringParam('name', 'DWAjaxForm');
        end;

      // DWInitComponents function
      // DWInitComponents := 'function DWInitComponents() {' + #10#13 + DWInitComponents +
      // #10#13 + '};';
      // HeadTag.InitScript.Contents.AddText(DWInitComponents, True);
    end;
end;

procedure TDWLayoutRender.SetLocalThemeThemeCss(const Value: string);
begin

end;

end.
