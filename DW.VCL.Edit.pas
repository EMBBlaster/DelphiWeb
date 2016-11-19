unit DW.VCL.Edit;

interface

uses Classes, Winapi.Windows, System.SysUtils, VCL.Themes, VCL.StdCtrls, VCL.Controls, VCL.Graphics,
  DW.VCL.Control, DWElementTag;

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
  //  FOldSelLength: Integer;
  //  FOldSelStart: Integer;
    FNumbersOnly: Boolean;
    FTextHint: string;
 //   FSaveReadOnly: Boolean;
//    procedure AdjustHeight;
 //   function GetModified: Boolean;
//    function GetCanUndo: Boolean;
    function GetReadOnly: Boolean;
    procedure SetCharCase(Value: TEditCharCase);
    procedure SetMaxLength(Value: Integer);
//    procedure SetModified(Value: Boolean);
    procedure SetNumbersOnly(Value: Boolean);
    procedure SetPasswordChar(Value: Char);
    procedure SetReadOnly(Value: Boolean);
    procedure SetTextHint(const Value: string);
//    procedure UpdateHeight;
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
  public
    constructor Create(AOwner: TComponent); override;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property Text;
    property TextHint: string read FTextHint write SetTextHint;

    property Canvas;
    function RenderHTML: TDWElementTag; override;
  published
    property TabStop default True;
    property Caption;
    property Font;
  end;

  TDWEdit = class(TDWCustomEdit)
      published
    property Align;
    property Alignment;
    property Anchors;
    //property AutoSelect;
    //property AutoSize;
    //property BevelEdges;
    //property BevelInner;
    //property BevelKind default bkNone;
    ///property BevelOuter;
    //property BevelWidth;
    //property BiDiMode;
    //property BorderStyle;
    property CharCase;
    //property Color;
    //property Constraints;
    //property Ctl3D;
   // property DragCursor;
   // property DragKind;
  //  property DragMode;
    property Enabled;
    property Font;
   // property ImeMode;
   // property ImeName;
    property MaxLength;
    property NumbersOnly;
   // property ParentBiDiMode;
   // property ParentColor;
   // property ParentCtl3D;
  //  property ParentDoubleBuffered;
   // property ParentFont;
  //  property ParentShowHint;
    property PasswordChar;
   // property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property TextHint;
  //  property Touch;
    property Visible;
 //   property StyleElements;
    property OnChange;
//    property OnClick;
  //  property OnContextPopup;
  //  property OnDblClick;
 //   property OnDragDrop;
 //   property OnDragOver;
  //  property OnEndDock;
  //  property OnEndDrag;
 //   property OnEnter;
 //   property OnExit;
  //  property OnGesture;
  //  property OnKeyDown;
  //  property OnKeyPress;
  //  property OnKeyUp;
  //  property OnMouseActivate;
 //   property OnMouseDown;
 //   property OnMouseEnter;
 //   property OnMouseLeave;
//   property OnMouseMove;
//    property OnMouseUp;
 //   property OnStartDock;
 //   property OnStartDrag
  end;


implementation

{ TDWCustomEdit }

procedure TDWCustomEdit.Change;
begin
  Invalidate;
  if Assigned(FOnChange) then FOnChange(Self);
end;



constructor TDWCustomEdit.Create(AOwner: TComponent);
const
  EditStyle = [csClickEvents, csSetCaption, csDoubleClicks, csFixedHeight, csPannable];
begin
  inherited Create(AOwner);
  ControlStyle := EditStyle + [csFramed];
  Width := 121;
  Height := 25;
  TabStop := True;
  FAlignment := taLeftJustify;
  FNumbersOnly := False;
  FTextHint := '';
  //TipMode := tipOpen;
  //ShowAccelChar := True;
end;

function TDWCustomEdit.GetReadOnly: Boolean;
begin
  Result := FReadOnly;
end;

procedure TDWCustomEdit.Paint;
begin
  inherited;

end;

function TDWCustomEdit.RenderHTML: TDWElementTag;
var
  Style:string;
begin
  Style:= 'font-size:' + IntToStr(Font.Size) + 'px; top:' + IntToStr(Top) +
              'px; left:' + IntToStr(Left) +
              'px; width:' + IntToStr(Width) +
              'px; height:' + IntToStr(Height) + 'px; position:absolute;';

  Result:= TDWElementTag.CreateHtmlTag('input');
  Result.AddStringParam('style', Style);
  Result.AddStringParam('value', Text);
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
