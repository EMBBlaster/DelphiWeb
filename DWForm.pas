unit DWForm;

interface

uses
  Winapi.Windows, dialogs, Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, DW.Vcl.Control, DWElementTag, DW.HTML.Page,
  DWRenderStream, DWLayoutRender, DW.Vcl.CustomForm, DWCallbacks;

type
  TDWFormClass = class of TDWForm;

  TDWForm = class(TdwCustomForm)
  private
    FLayoutRender: TDWLayoutRender;
    procedure SetLayoutRender(const Value: TDWLayoutRender);
    { Private declarations }
  protected
    // Hide non necessary propertys
    property Margins;
    property Left;
    property Top;
    property Hint;
    property HelpType;
    property HelpKeyword;
    property HelpContext;
    property CustomHint;
    property Cursor;
    property AlignWithMargins;
    property ParentCustomHint;
  public
    procedure ExecuteCallBack(aParams: TStringList; aCallBack: TDWCallBack); override;
    Function Render: TDWStream; override;
  published
    property LayoutRender: TDWLayoutRender read FLayoutRender write SetLayoutRender;
  end;

implementation

uses DW.Vcl.Interfaces;


// {$R *.dfm}

{ TDWForm }

type
  THrackRemoveThis = class(TDWInputControl);

procedure TDWForm.ExecuteCallBack(aParams: TStringList; aCallBack: TDWCallBack);
var
  C: Integer;
  L_IControl: IDWInput;
begin
  // update Components values
  for C := 0 to ComponentCount - 1 do
    begin
      if (aParams.Values[Components[C].Name] <> '') and
        (Supports(Components[C], IDWInput, L_IControl)) and (L_IControl <> nil) then
        begin
          // update control value
          L_IControl.SetValue(aParams.Values[Components[C].Name]);
          // THrackRemoveThis(Components[C]).FOldText := TDWInputControl(Components[C]).Text;
        end;
    end;

  if Assigned(aCallBack.CallBackProcedure) then
    aCallBack.CallBackProcedure(aParams);

  FLayoutRender.ProcessFormAsync(Self);

end;

Function TDWForm.Render: TDWStream;
begin
  inherited;
  if not Assigned(LayoutRender) then
    FLayoutRender := TDWLayoutRender.Create(Self);
  HTMLPage.Clear;
  FLayoutRender.ProcessForm(Self);
  (*
    for I := 0 to ComponentCount -1 do
    begin
    if Components[i].InheritsFrom(TDWControl) then
    begin
    Element:= (Components[i] as TDWControl).RenderHTML;
    Element.ParentElement:= HTMLPage.HTMLTag.BodyTag;
    end;
    end; *)
  Result := TDWStream.Create;
  Result.WriteString(HTMLPage.HTMLTag.Render);
  // Showmessage(HTMLPage.HTMLTag.Render);
end;

procedure TDWForm.SetLayoutRender(const Value: TDWLayoutRender);
begin
  FLayoutRender := Value;
end;

end.
