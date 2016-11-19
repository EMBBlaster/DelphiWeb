unit DW.VCL.Labels;

interface
  uses Classes, Winapi.Windows, System.SysUtils,  Vcl.Themes, Vcl.StdCtrls, Vcl.Controls, Vcl.Graphics,
  DW.VCL.Control, DWElementTag;

  type
    TDWLabel = class(TDWControl)
    private
        FDrawTextProc: TFNDrawText;
    FWordWrap: Boolean;
    FAlignment: TAlignment;
    procedure UpdateDrawTextProc;
    procedure DoDrawNormalText(DC: HDC; const Text: UnicodeString;
      var TextRect: TRect; TextFlags: Cardinal);
    procedure SetWordWrap(const Value: Boolean);
    procedure SetAlignment(const Value: TAlignment);
    procedure DoDrawText(var Rect: TRect; Flags: Integer);
    procedure AdjustBounds;
    protected
    procedure Paint; override;
    public
      constructor Create(AOwner: TComponent); override;
      property Canvas;
      function RenderHTML: TDWElementTag; override;
    published
      property Alignment: TAlignment read FAlignment write SetAlignment  default taLeftJustify;
      property Caption;
      property Font;
      property WordWrap: Boolean read FWordWrap write SetWordWrap default False;
    end;



implementation

{ TDWLabel }

constructor TDWLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  Width := 65;
  Height := 17;
  AutoSize := True;
  //ShowAccelChar := True;
    ControlStyle := ControlStyle + [csOpaque];
  UpdateDrawTextProc;

end;

procedure TDWLabel.UpdateDrawTextProc;
begin
    FDrawTextProc := DoDrawNormalText;
end;

procedure TDWLabel.DoDrawNormalText(DC: HDC; const Text: UnicodeString;
  var TextRect: TRect; TextFlags: Cardinal);
begin
  Winapi.Windows.DrawTextW(DC, Text, Length(Text), TextRect, TextFlags);
end;


procedure TDWLabel.Paint;
const
  Alignments: array[TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);
  WordWraps: array[Boolean] of Word = (0, DT_WORDBREAK);
var
  Rect: TRect;
  DrawStyle: Longint;
begin
  with Canvas do
  begin
    Rect := ClientRect;
    //if not Transparent then
    //begin
      Brush.Color := Self.Color;
      Brush.Style := bsSolid;
      //if not (csGlassPaint in ControlState) then
        FillRect(Rect);
     // else
      //  FillGlassRect(Canvas, Rect);
    //end;

    Brush.Style := bsClear;
    { DoDrawText takes care of BiDi alignments }
    DrawStyle := DT_EXPANDTABS or WordWraps[FWordWrap] or Alignments[FAlignment];
    { Calculate vertical layout }
   (* if FLayout <> tlTop then
    begin
      CalcRect := Rect;
      DoDrawText(CalcRect, DrawStyle or DT_CALCRECT);
      if FLayout = tlBottom then OffsetRect(Rect, 0, Height - CalcRect.Bottom)
      else OffsetRect(Rect, 0, (Height - CalcRect.Bottom) div 2);
    end; *)
    DoDrawText(Rect, DrawStyle);
  end;
end;

function TDWLabel.RenderHTML: TDWElementTag;
var
  Style:string;
begin
  Style:= 'font-size:' + IntToStr(Font.Size) + 'px; top:' + IntToStr(Top) +
              'px; left:' + IntToStr(Left) +
              'px; width:' + IntToStr(Width) +
              'px; height:' + IntToStr(Height) + 'px; position:absolute;';

  Result:= TDWElementTag.CreateHtmlTag('label');
  Result.AddStringParam('style', Style);
  Result.Content.AddText(Caption);
end;

procedure TDWLabel.SetAlignment(const Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Invalidate;
  end;
end;

procedure TDWLabel.SetWordWrap(const Value: Boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    AdjustBounds;
    Invalidate;
  end;
end;

procedure TDWLabel.DoDrawText(var Rect: TRect; Flags: Longint);
const
  EllipsisStr = '...';
  Ellipsis: array[TEllipsisPosition] of Longint = (0, DT_PATH_ELLIPSIS,
    DT_END_ELLIPSIS, DT_WORD_ELLIPSIS);
var
  Text: string;
 // NewRect: TRect;
 // Height, Delim: Integer;
begin
  Text := Caption;
  if (Flags and DT_CALCRECT <> 0) and
     ((Text = '') {or FShowAccelChar and (Text[1] = '&') and (Length(Text) = 1)}) then
    Text := Text + ' ';

  if Text <> '' then
  begin
    {if not FShowAccelChar then Flags := Flags or DT_NOPREFIX;}
    Flags := DrawTextBiDiModeFlags(Flags);
    Canvas.Font := Font;
    (*if (FEllipsisPosition <> epNone) and not FAutoSize then
    begin
      DText := Text;
      Flags := Flags and not DT_EXPANDTABS;
      Flags := Flags or Ellipsis[FEllipsisPosition];
      if FWordWrap and (FEllipsisPosition in [epEndEllipsis, epWordEllipsis]) then
      begin
        repeat
          NewRect := Rect;
          Dec(NewRect.Right, Canvas.TextWidth(EllipsisStr));
          FDrawTextProc(Canvas.Handle, DText, NewRect, Flags or DT_CALCRECT);
          Height := NewRect.Bottom - NewRect.Top;
          if (Height > ClientHeight) and (Height > Canvas.Font.Height) then
          begin
            Delim := LastDelimiter(' '#9, Text);
            if Delim = 0 then
              Delim := Length(Text);
            Dec(Delim);
  {$IF NOT DEFINED(CLR)}
            if ByteType(Text, Delim) = mbLeadByte then
              Dec(Delim);
  {$ENDIF}
            Text := Copy(Text, 1, Delim);
            DText := Text + EllipsisStr;
            if Text = '' then
              Break;
          end else
            Break;
        until False;
      end;
      if Text <> '' then
        Text := DText;
    end; *)

    if Enabled or StyleServices.Enabled then
      FDrawTextProc(Canvas.Handle, Text, Rect, Flags)
    else
    begin
      OffsetRect(Rect, 1, 1);
      Canvas.Font.Color := clBtnHighlight;
      FDrawTextProc(Canvas.Handle, Text, Rect, Flags);
      OffsetRect(Rect, -1, -1);
      Canvas.Font.Color := clBtnShadow;
      FDrawTextProc(Canvas.Handle, Text, Rect, Flags);
    end;
  end;
end;

procedure TDWLabel.AdjustBounds;
const
  WordWraps: array[Boolean] of Word = (0, DT_WORDBREAK);
var
  DC: HDC;
  X: Integer;
  Rect: TRect;
  AAlignment: TAlignment;
begin
  if not (csReading in ComponentState) {and FAutoSize} then
  begin
    Rect := ClientRect;
    DC := GetDC(0);
    try
      Canvas.Handle := DC;
      DoDrawText(Rect, (DT_EXPANDTABS or DT_CALCRECT or MASK_TF_COMPOSITED) or WordWraps[FWordWrap]);
      Canvas.Handle := 0;
    finally
      ReleaseDC(0, DC);
    end;
    X := Left;
    AAlignment := FAlignment;
    if UseRightToLeftAlignment then
      ChangeBiDiModeAlignment(AAlignment);
    if AAlignment = taRightJustify then
      Inc(X, Width - Rect.Right);
    SetBounds(X, Top, Rect.Right, Rect.Bottom);
  end;
end;

end.
