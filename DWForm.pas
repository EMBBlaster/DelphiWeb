unit DWForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, DW.VCL.Control, DWElementTag, DW.HTML.Page,
  DWRenderStream, DWLayoutRender, DW.VCL.CustomForm, DWCallbacks;

type
  TDWFormClass = class of TDWForm;

  TDWForm = class(TdwCustomForm)
  private
    FUserSession: TObject;
    FLayoutRender: TDWLayoutRender;

    procedure SetUserSession(const Value: TObject);
    procedure SetLayoutRender(const Value: TDWLayoutRender);
    { Private declarations }
  public
    procedure ExecuteCallBack(aParams:TStringList; aCallBack:TDWCallBack);
    Function Render: TDWStream;
    property UserSession:TObject read FUserSession write SetUserSession;
  published
    property LayoutRender:TDWLayoutRender read FLayoutRender write SetLayoutRender;
  end;


implementation
 uses DWUserSessionUnit;


{$R *.dfm}

{ TDWForm }


procedure TDWForm.ExecuteCallBack(aParams: TStringList; aCallBack: TDWCallBack);
var
  C: Integer;
  Sender:TObject;
begin
  Sender:= nil;

  //update Components values
  for C := 0 to ComponentCount -1 do
    begin
      if (Components[C].InheritsFrom(TDWInputControl))
      and (aParams.Values[Components[C].Name] <> '') then
        begin
          TDWInputControl(Components[C]).Text:= aParams.Values[Components[C].Name];
        end;
    end;

  { TODO 1 -oDELCIO -cIMPLEMENT: Get Sender Based on Component name of originated event in browser  }
  if Assigned(aCallBack.CallBackFunction) then
    aCallBack.CallBackFunction(Sender, aParams);
end;

Function TDWForm.Render:TDWStream;
begin
  if not Assigned(LayoutRender) then
    FLayoutRender:= TDWLayoutRender.Create(Self);
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
      end;*)
    Result:= TDWStream.Create;
    Result.WriteString(HTMLPage.HTMLTag.Render);

end;

procedure TDWForm.SetLayoutRender(const Value: TDWLayoutRender);
begin
  FLayoutRender := Value;
end;

procedure TDWForm.SetUserSession(const Value: TObject);
begin
  FUserSession := Value;
end;

end.
