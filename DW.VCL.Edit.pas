unit DW.VCL.Edit;

interface

uses Classes, Winapi.Windows, System.SysUtils, VCL.Themes, VCL.StdCtrls, VCL.Controls, VCL.Graphics,
  DW.VCL.Control, DWElementTag, DWUtils;

type
  TDWCustomEdit = class(TDWInputControl)
  private
    FAlignment: TAlignment;
    FMaxLength: Integer;
    FPasswordChar: Char;
    FReadOnly: Boolean;

    FCharCase: TEditCharCase;
    // FCreating: Boolean;
    // FModified: Boolean;
    // FInBufferedPrintClient: Boolean;
    FOnChange: TNotifyEvent;
    // FOldSelLength: Integer;
    // FOldSelStart: Integer;
    FNumbersOnly: Boolean;
    FTextHint: string;
    // FSaveReadOnly: Boolean;
    // procedure AdjustHeight;
    // function GetModified: Boolean;
    // function GetCanUndo: Boolean;
    function GetReadOnly: Boolean;
    procedure SetCharCase(Value: TEditCharCase);
    procedure SetMaxLength(Value: Integer);
    // procedure SetModified(Value: Boolean);
    procedure SetNumbersOnly(Value: Boolean);
    procedure SetPasswordChar(Value: Char);
    procedure SetReadOnly(Value: Boolean);
    procedure SetTextHint(const Value: string);
    // procedure UpdateHeight;
  protected
    procedure Change; dynamic;
    procedure SetAlignment(Value: TAlignment);
    property CharCase: TEditCharCase read FCharCase write SetCharCase default ecNormal;
    property MaxLength: Integer read FMaxLength write SetMaxLength default 0;
    property NumbersOnly: Boolean read FNumbersOnly write SetNumbersOnly default False;
    property PasswordChar: Char read FPasswordChar write SetPasswordChar default #0;
    property ParentColor default False;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    procedure Paint; override;
    procedure InternalRenderHTML(var AHTMLTag: TDWElementTag); override;
    procedure InternalRenderAsync(const aHTMLName: string); Override;
    // Render Control Inline Style
    procedure InternalRenderStyle(AStyle: TStringList); Override;
  public
    constructor Create(AOwner: TComponent); override;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property Text;
    property TextHint: string read FTextHint write SetTextHint;

    property Canvas;
  published
    property TabStop default True;
    property Caption;
    property Font;
    property OnAsyncClick;
    property OnAsyncChange;
  end;

  TDWEdit = class(TDWCustomEdit)
  published
    property Align;
    property Alignment;
    property Anchors;
    // property AutoSelect;
    // property AutoSize;
    // property BevelEdges;
    // property BevelInner;
    // property BevelKind default bkNone;
    /// property BevelOuter;
    // property BevelWidth;
    // property BiDiMode;
    // property BorderStyle;
    property CharCase;
    // property Color;
    // property Constraints;
    // property Ctl3D;
    // property DragCursor;
    // property DragKind;
    // property DragMode;
    property Enabled;
    property Font;
    // property ImeMode;
    // property ImeName;
    property MaxLength;
    property NumbersOnly;
    // property ParentBiDiMode;
    // property ParentColor;
    // property ParentCtl3D;
    // property ParentDoubleBuffered;
    // property ParentFont;
    // property ParentShowHint;
    property PasswordChar;
    // property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabIndex;
    property TabStop;
    property Text;
    property TextHint;
    // property Touch;
    property Visible;
    // property StyleElements;
    property OnChange;
    // property OnClick;
    // property OnContextPopup;
    // property OnDblClick;
    // property OnDragDrop;
    // property OnDragOver;
    // property OnEndDock;
    // property OnEndDrag;
    // property OnEnter;
    // property OnExit;
    // property OnGesture;
    // property OnKeyDown;
    // property OnKeyPress;
    // property OnKeyUp;
    // property OnMouseActivate;
    // property OnMouseDown;
    // property OnMouseEnter;
    // property OnMouseLeave;
    // property OnMouseMove;
    // property OnMouseUp;
    // property OnStartDock;
    // property OnStartDrag

  end;

implementation

uses DW.VCL.Common, DW.VCL.InputForm;

{ TDWCustomEdit }

procedure TDWCustomEdit.Change;
begin
  Invalidate;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor TDWCustomEdit.Create(AOwner: TComponent);
const
  EditStyle = [csClickEvents, csSetCaption, csDoubleClicks, csFixedHeight, csPannable];
begin
  inherited Create(AOwner);
  ControlStyle := EditStyle + [csFramed];
  Width        := 121;
  Height       := 25;
  TabStop      := True;
  FAlignment   := taLeftJustify;
  FNumbersOnly := False;
  FTextHint    := '';
  // TipMode := tipOpen;
  // ShowAccelChar := True;
end;

function TDWCustomEdit.GetReadOnly: Boolean;
begin
  Result := FReadOnly;
end;

procedure TDWCustomEdit.InternalRenderAsync(const aHTMLName: string);
begin
  TDWBSCommon.SetAsyncText(aHTMLName, Text, FOldText);
end;

procedure TDWCustomEdit.InternalRenderHTML(var AHTMLTag: TDWElementTag);
begin
  FOldText := Text;
  AHTMLTag := TDWElementTag.CreateHtmlTag('input');
  with AHTMLTag do
    begin
      AddStringParam('type', 'text');
      AddStringParam('class', 'form-control');
      AddStringParam('value', FOldText);
    end;

    AHTMLTag := TDWElementTag.CreateHTMLTag('input');
      try
        AHTMLTag.AddClassParam(ActiveCss);
        AHTMLTag.AddStringParam('id', HTMLName);
        AHTMLTag.AddStringParam('name', HTMLName);
        AHTMLTag.AddStringParam('type', 'text');
        if ShowHint and (Hint <> '') then
          AHTMLTag.AddStringParam('title', Hint);
        {if AutoFocus then
          AHTMLTag.Add('autofocus');}
        if IsReadOnly then
          AHTMLTag.Add('readonly');
        if IsDisabled then
          AHTMLTag.Add('disabled');
        if MaxLength > 0 then
          AHTMLTag.AddIntegerParam('maxlength', MaxLength);
        AHTMLTag.AddStringParam('value', Text);
        {if Required then
          AHTMLTag.Add('required'); }
        {if PlaceHolder <> '' then
          AHTMLTag.AddStringParam('placeholder', TextToHTML(PlaceHolder)); }
        if TabIndex <> 0 then
          AHTMLTag.AddStringParam('tabindex', IntToStr(TabIndex));
        {AHTMLTag.AddStringParam('autocomplete', IfThen(FAutoComplete, 'on', 'off'));
        if Validator <> nil then
          Validator.RenderValidation(AHTMLTag);}
        //AHTMLTag.AddStringParam('style', ActiveStyle);
      except
        FreeAndNil(AHTMLTag);
        raise;
      end;
   (*if FMask <> '' then
    begin
      MaskTag:= TDWElementTag.CreateTag('script');
      MaskTag.Contents.AddText( '$("#' + HTMLName + '").mask("' + FMask+ '",{placeholder:" "});');
      AHTMLTag.Contents.AddTagAsObject(MaskTag);
    end;*)
   if not (Parent is TDWInputGroup) then
    begin
      AHTMLTag := DWCreateInputFormGroup(Self, Parent, AHTMLTag, Caption, HTMLName);
      //browsers not respect autocomplete "off" in password inputs,
      //to solve this, put another input password hidden,
      //it elude browser with password change form
      { TODO 1 -oDELCIO -cIMPLEMENT : Create an TEditPassword  }
      (*if (InputType = bsitPassword) and (not FAutoComplete) then
        begin
          FakeAutocomp:= TDWElementTag.CreateTag('input');
          FakeAutocomp.AddStringParam('style',
            'visibility: hidden; height:0; margin:0; border:0; padding:0;display:block;');
          FakeAutocomp.AddStringParam('type', 'password');
          if Caption <> '' then
            AHTMLTag.Contents.Insert(1, FakeAutocomp)
          else
            AHTMLTag.Contents.Insert(0, FakeAutocomp);
        end; *)
    end;



end;

procedure TDWCustomEdit.InternalRenderStyle(AStyle: TStringList);
begin
   inherited;
end;

procedure TDWCustomEdit.Paint;
begin
  inherited;

end;

procedure TDWCustomEdit.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
    begin
      FAlignment := Value;
      Invalidate;
    end;
end;

procedure TDWCustomEdit.SetCharCase(Value: TEditCharCase);
begin
  if FCharCase <> Value then
    begin
      FCharCase := Value;
      Invalidate;
    end;
end;

procedure TDWCustomEdit.SetMaxLength(Value: Integer);
begin
  if FMaxLength <> Value then
    begin
      FMaxLength := Value;
      Invalidate;
    end;
end;

procedure TDWCustomEdit.SetNumbersOnly(Value: Boolean);
begin
  if FNumbersOnly <> Value then
    begin
      FNumbersOnly := Value;
      Invalidate;
    end;
end;

procedure TDWCustomEdit.SetPasswordChar(Value: Char);
begin
  if FPasswordChar <> Value then
    begin
      FPasswordChar := Value;
      Invalidate;
    end;
end;

procedure TDWCustomEdit.SetReadOnly(Value: Boolean);
begin
  if FReadOnly <> Value then
    begin
      Invalidate;
    end;
end;

procedure TDWCustomEdit.SetTextHint(const Value: string);
begin
  if FTextHint <> Value then
    begin
      FTextHint := Value;
      Invalidate;
    end;
end;

end.
