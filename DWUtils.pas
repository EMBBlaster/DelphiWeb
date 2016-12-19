unit DWUtils;

interface

uses Classes, Windows, Forms, Controls, DW.CORE.Server, System.SysUtils, System.StrUtils,
  DW.VCL.CustomForm, DW.CORE.DWApplication, DW.VCL.Container, DWElementTag,
  DW.VCL.InputForm, DW.VCL.Region, DWTypes, DB, DW.VCL.Frame;

function DWServer: TDWServer;
function MakeValidFileUrl(const ARootUrl: String; const AFileUrl: String): string;
// Find a parent Form of one Control
function DWFindParentForm(aControl: TControl): TDWCustomForm;
function DWFindParentFrame(aContainer:TDWContainer): TDWFrame;
function AnsiToUnicode(const Str: PAnsiChar; ACodePage: LongWord): UnicodeString; overload;
function AnsiToUnicode(const Str: RawByteString; ACodePage: LongWord): UnicodeString; overload;
function AnsiToUnicode(const Str: RawByteString): UnicodeString; {$IFDEF USE_INLINE} inline;
{$ENDIF} overload; overload;
function DWMbToWc(CodePage: LongWord; Flags: Cardinal; MbStr: PAnsiChar; MbStrLen: Integer;
  WStr: PWideChar; WStrLen: Integer): Integer;
function Base64Encode(Input: String): String;
function DWGetTickCount: LongWord;
// Return the first Parent Container of an TDWControl
function GetParentContainer(aControl: TControl): TDWContainer;
// add scape chars and adjust string to be passed on callback
function DWTextToJsParamText(AText: string): string;
function IIf(Expressao: Variant; ParteTRUE, ParteFALSE: Variant): Variant;
function DWCreateInputFormGroup(AControl, AParent: TControl; ATag: TDWElementTag; const ACaption, AHTMLName: string): TDWElementTag;
function DWFindParentInputForm(AParent: TControl): TDWInputForm;
function DWCreateFormGroup(AParent: TControl; AParentForm: TDWCustomInputForm; ATag: TDWElementTag; const AHTMLName: string; ASpanDiv: boolean): TDWElementTag;
function GetDataSourceField(aDataSource:TDataSource; aDataFild:string):TField;
function CheckDataSource(aDataSource:TDataSource; aDataField:string; var LField:TField):Boolean;
//Get an new component Name
function DWGetUniqueComponentName(AOwner: TComponent; const APrefix: string): string;
function CharIsNum(const C: Char): Boolean;
function CharIsAlpha(const C: Char): Boolean;
function CharIsAlphaNum(const C: Char): Boolean;
function DWCreateInputGroupAddOn(ATag: TDWElementTag; const AHTMLName, css: string): TDWElementTag;
function DWCreateCheckBoxFormGroup(AParent: TControl; ATag: TDWElementTag; const ACss, ACaption, AHint, AHTMLName: string; AShowHint: boolean): TDWElementTag;
function GetGlyphiconChar(const AGlyphicon: string; const AFallBackTo: string = ''): string;
function EscapeJsonString(const AValue: string): string;
//used to get a valid image from an TBlobField
function DWGetFieldBlobStream(ADataSet: TDataSet; AField: TBlobField): TStream;
// return the DWApplication for this session
function DWApplication: TDWApplication;

implementation

uses
  DW.VCL.ButtonGroup, DW.VCL.Common, DW.VCL.GlyphIcons;



function DWApplication: TDWApplication;
begin
  Result := TDWAppThread(TDWAppThread.Current).DWapp;
end;

function DWServer: TDWServer;
begin
  Result := TDWServer.GetDWServer;
end;

function MakeValidFileUrl(const ARootUrl: String; const AFileUrl: String): string;
var
  RootUrl: string;
  FileUrl: string;
begin
  if ARootUrl <> '' then
    RootUrl := ARootUrl
  else
    RootUrl := DWServer.UrlBase;
  FileUrl   := ExtractRelativePath(DWServer.DocDir + '\', AFileUrl);
  FileUrl   := StringReplace(FileUrl, '\', '/', [rfReplaceAll]);
  Result    := { RootUrl + } '/' + FileUrl;
end;

function DWFindParentForm(aControl: TControl): TDWCustomForm;
var
  CompTest: TControl;
begin
  Result := nil;
  try
    CompTest := aControl;
    while Assigned(CompTest) and (not(CompTest.InheritsFrom(TDWCustomForm))) do
      CompTest := CompTest.Parent;
    if CompTest = nil then
      begin
        if TControl(aControl).Owner is TFrame then
          begin
            CompTest := TControl(aControl.Owner.Owner);
            while (not(CompTest.InheritsFrom(TDWCustomForm))) and (CompTest <> nil) do
              CompTest := TControl(CompTest.Owner);
          end;
      end;
    if CompTest <> nil then
      if CompTest.InheritsFrom(TDWCustomForm) then
        Result := CompTest as TDWCustomForm;
  except

  end;
end;

function AnsiToUnicode(const Str: PAnsiChar; ACodePage: LongWord): UnicodeString;
var
  Len, Len2: Integer;
begin
  if (Str <> nil) then
    begin
      Len := DWMbToWc(ACodePage, 0, Str, -1, nil, 0);
      if Len > 1 then
        begin // counts the null-terminator
          SetLength(Result, Len - 1);
          Len2 := DWMbToWc(ACodePage, 0, Str, -1, Pointer(Result), Len);
          if Len2 <> Len then // May happen, very rarely
            begin
              if Len2 > 0 then
                SetLength(Result, Len2 - 1)
              else
                Result := '';
            end;
        end
      else
        Result := '';
    end
  else
    Result := '';
end;

function AnsiToUnicode(const Str: RawByteString; ACodePage: LongWord): UnicodeString;
var
  Len, Len2: Integer;
begin
  Len := Length(Str);
  if Len > 0 then
    begin
      Len := DWMbToWc(ACodePage, 0, Pointer(Str), Len, nil, 0);
      SetLength(Result, Len);
      if Len > 0 then
        begin
          Len2 := DWMbToWc(ACodePage, 0, Pointer(Str), Length(Str), Pointer(Result), Len);
          if Len2 <> Len then // May happen, very rarely
            SetLength(Result, Len2);
        end;
    end
  else
    Result := '';
end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }
function AnsiToUnicode(const Str: RawByteString): UnicodeString;
begin
  Result := AnsiToUnicode(Str, CP_ACP);
end;

function DWMbToWc(CodePage: LongWord; Flags: Cardinal; MbStr: PAnsiChar; MbStrLen: Integer;
  WStr: PWideChar; WStrLen: Integer): Integer;
begin
  Result := MultiByteToWideChar(CodePage, Flags, MbStr, MbStrLen, WStr, WStrLen);
end;

function Base64Encode(Input: String): String;
var
  Final: String;
  Count: Integer;
  Len: Integer;
const
  Base64Out: array [0 .. 64] of Char = ('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L',
    'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 'a', 'b', 'c', 'd', 'e',
    'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x',
    'y', 'z', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '+', '/', '=');
begin
  Final := '';
  Count := 1;
  Len   := Length(Input);
  while Count <= Len do
    begin
      Final := Final + Base64Out[(Byte(Input[Count]) and $FC) shr 2];
      if (Count + 1) <= Len then
        begin
          Final := Final + Base64Out[((Byte(Input[Count]) and $03) shl 4) +
            ((Byte(Input[Count + 1]) and $F0) shr 4)];
          if (Count + 2) <= Len then
            begin
              Final := Final + Base64Out[((Byte(Input[Count + 1]) and $0F) shl 2) +
                ((Byte(Input[Count + 2]) and $C0) shr 6)];
              Final := Final + Base64Out[(Byte(Input[Count + 2]) and $3F)];
            end
          else
            begin
              Final := Final + Base64Out[(Byte(Input[Count + 1]) and $0F) shl 2];
              Final := Final + '=';
            end
        end
      else
        begin
          Final := Final + Base64Out[(Byte(Input[Count]) and $03) shl 4];
          Final := Final + '==';
        end;
      Count := Count + 3;
    end;
  Result := Final;
end;

function DWGetTickCount: LongWord;
{$IFDEF MSWINDOWS}
begin
  Result := {$IFDEF RTL_NAMESPACES}Winapi.{$ENDIF}Windows.GetTickCount;
end;
{$ENDIF}
{$IFDEF POSIX}
{$IFDEF LINUX}

var
  t: tms;
begin
  Result := Cardinal(Int64(Cardinal(times(t)) * 1000) div sysconf(_SC_CLK_TCK));
end;
{$ENDIF}
{$IFDEF MACOS}
begin
  Result := AbsoluteToNanoseconds(UpTime) div 1000000;
end;
{$ENDIF MACOS}
{$ENDIF POSIX}

function GetParentContainer(aControl: TControl): TDWContainer;
var
  LParent: TControl;
begin
  Result  := nil;
  LParent := aControl.Parent;
  while not(LParent = nil) do
    begin
      if LParent.InheritsFrom(TDWContainer) then
        begin
          Result := LParent as TDWContainer;
          Break;
        end
      else
        LParent := LParent.Parent;
    end;
end;

function DWTextToJsParamText(AText: string): string;
begin
  Result := ReplaceStr(AText, '\', '\\');
  Result := ReplaceStr(Result, '"', '\"');
  Result := ReplaceStr(Result, #39, '\'#39);
  Result := ReplaceStr(Result, #10, '\n');
  Result := ReplaceStr(Result, #13, '');
end;

function IIf(Expressao: Variant; ParteTRUE, ParteFALSE: Variant): Variant;
begin
  if Expressao then
    Result := ParteTRUE
  else
    Result := ParteFALSE;
end;

function DWCreateInputFormGroup(AControl, AParent: TControl; ATag: TDWElementTag; const ACaption, AHTMLName: string): TDWElementTag;
var
  lablTag, editTag: TDWElementTag;
  InputForm: TDWCustomInputForm;
begin
  InputForm := DWFindParentInputForm(AParent);
  if ACaption <> '' then
    begin
      Result := TDWElementTag.CreateHTMLTag('div');
      try
        Result.AddClassParam('form-group');
        Result.AddStringParam('id',AHTMLName+'_FG');
        lablTag := Result.Contents.AddElement('label');
        lablTag.AddClassParam('control-label');
        lablTag.AddStringParam('for', AHTMLName);
        lablTag.Contents.AddText(ACaption);
        { TODO 1 -oDELCIO -cIMPLEMENT : !!!! INPUT FORM TYPES !!!! }
        (*if (InputForm <> nil) and (InputForm is TDWInputForm) and (TDWInputForm(InputForm).BSFormType = bsftHorizontal) then
          begin
            lablTag.AddClassParam(TDWInputForm(InputForm).BSFormOptions.CaptionsSize.GetClassString);
            editTag := Result.Contents.AddElement('div');
            editTag.AddClassParam(TDWInputForm(InputForm).BSFormOptions.InputsSize.GetClassString);
            editTag.Contents.AddElemenAsObject(aTag);
          end
        else *)
          Result.Contents.AddElemetAsObject(ATag);
      except
        FreeAndNil(Result);
        FreeAndNil(ATag);
        raise;
      end;
    end
  else
    Result := DWCreateFormGroup(AParent, InputForm, ATag, AHTMLName, True);
end;

function DWCreateFormGroup(AParent: TControl; AParentForm: TDWCustomInputForm; ATag: TDWElementTag; const AHTMLName: string; ASpanDiv: boolean): TDWElementTag;
var
  LSpanDiv: TDWElementTag;
begin
  if (AParentForm <> nil) and
     not ((AParent is TDWRegion) and (TDWRegion(AParent).BSRegionType = bsrtFormGroup)) and
     not (AParent is TDWButtonGroup) then
    begin
      Result := TDWElementTag.CreateHTMLTag('div');
      Result.AddClassParam('form-group');
      Result.AddStringParam('id',AHTMLName+'_FG');
      if ASpanDiv
      and (AParentForm is TDWInputForm)
      and (TDWInputForm(AParentForm).BSFormType = bsftHorizontal) then
        begin
          LSpanDiv := Result.Contents.AddElement('div');
          LSpanDiv.AddClassParam(TDWInputForm(AParentForm).BSFormOptions.GetOffsetClassString);
          LSpanDiv.Contents.AddElemetAsObject(aTag);
        end
      else
        Result.Contents.AddElemetAsObject(aTag);
    end
  else
    Result := ATag;
end;

function DWFindParentInputForm(AParent: TControl): TDWInputForm;
begin
  if AParent is TDWInputForm then
    Result := TDWInputForm(AParent)
  else if AParent.Parent <> nil then
    Result := DWFindParentInputForm(AParent.Parent)
  else
    Result := nil;
end;

function DWGetUniqueComponentName(AOwner: TComponent; const APrefix: string): string;
var
  i: Integer;
begin
  if AOwner = nil then
    Exit;

  Result:= APrefix;
  i:= 0;
  while Assigned(AOwner.FindComponent(Result)) do begin
    inc(i);
    Result:= APrefix + IntToStr(i);
  end;
end;

function GetDataSourceField(aDataSource:TDataSource; aDataFild:string):TField;
begin
  Result:= nil;
  if (aDataSource <> nil) and (aDataSource.DataSet <> nil) then
    Result:= aDataSource.DataSet.FindField(aDataFild);
end;

function CheckDataSource(aDataSource:TDataSource; aDataField:string; var LField:TField):Boolean;
begin
  Result:= False;
  LField:= GetDataSourceField(aDataSource, aDataField);
  if LField <> nil then
    Result:=True;
end;

function DWCreateCheckBoxFormGroup(AParent: TControl; ATag: TDWElementTag; const ACss, ACaption, AHint, AHTMLName: string; AShowHint: boolean): TDWElementTag;
var
  lablTag: TDWElementTag;
  InputForm: TDWCustomInputForm;
begin
  InputForm := DWFindParentInputForm(AParent);
  Result := TDWElementTag.CreateHTMLTag('div');
  try
    Result.AddStringParam('id', AHTMLName+'_FG');
    if (InputForm <> nil)
    and (InputForm is TDWInputForm)
    and (TDWInputForm(InputForm).BSFormType = bsftInline) then
      Result.AddClassParam(ACss+'-inline')
    else
      Result.AddClassParam(ACss);
    if (InputForm <> nil)
    and (InputForm is TDWInputForm)
    and (TDWInputForm(InputForm).BSFormType = bsftHorizontal) then
      Result.AddClassParam(TDWInputForm(InputForm).BSFormOptions.GetOffsetClassString);
    lablTag := Result.Contents.AddElement('label');
    lablTag.AddStringParam('id', AHTMLName+'_CHKBCAPTION');
    if AShowHint and (AHint <> '') then
      lablTag.AddStringParam('title', AHint);
    lablTag.Contents.AddElemetAsObject(ATag);
    lablTag.Contents.AddText(TDWBSCommon.TextToHTML(ACaption));

    Result := DWCreateFormGroup(AParent, InputForm, Result, AHTMLName, False);
  except
    FreeAndNil(Result);
    FreeAndNil(ATag);
    raise;
  end;
end;


{$region 'String functions'}

function CharIsNum(const C: Char): Boolean;
begin
  Result := ( C in ['0'..'9'] ) ;
end ;

function CharIsAlpha(const C: Char): Boolean;
begin
  Result := ( C in ['A'..'Z','a'..'z'] ) ;
end ;

function CharIsAlphaNum(const C: Char): Boolean;
begin
  Result := ( CharIsAlpha( C ) or CharIsNum( C ) );
end ;

{$endregion}

{$region 'InputGroup functions'}
function DWCreateInputGroupAddOn(ATag: TDWElementTag; const AHTMLName, css: string): TDWElementTag;
begin
  Result := TDWElementTag.CreateHTMLTag('span');
  try
    Result.AddClassParam('input-group-'+css);
    Result.Contents.AddElemetAsObject(ATag);
  except
    FreeAndNil(Result);
    FreeAndNil(ATag);
    raise;
  end;
end;
{$endregion}

function GetGlyphiconChar(const AGlyphicon: string; const AFallBackTo: string = ''): string;
var
  i: integer;
begin
  if (AGlyphicon <> '') then
    i := StrToIntDef(slGlyphicons.Values[AGlyphicon], 0)
  else
    i := 0;
  if i = 0 then
    Result := AFallBackTo
  else
    Result := Char(i);
end;

function EscapeJsonString(const AValue: string): string;
const
  ESCAPE = '\';
  // QUOTATION_MARK = '"';
  REVERSE_SOLIDUS = '\';
  SOLIDUS = '/';
  BACKSPACE = #8;
  FORM_FEED = #12;
  NEW_LINE = #10;
  CARRIAGE_RETURN = #13;
  HORIZONTAL_TAB = #9;
var
  AChar: Char;
begin
  Result := '';
  for AChar in AValue do
  begin
    case AChar of
      // !! Double quote (") is handled by TJSONString
      // QUOTATION_MARK: Result := Result + ESCAPE + QUOTATION_MARK;
      REVERSE_SOLIDUS: Result := Result + ESCAPE + REVERSE_SOLIDUS;
      SOLIDUS: Result := Result + ESCAPE + SOLIDUS;
      BACKSPACE: Result := Result + ESCAPE + 'b';
      FORM_FEED: Result := Result + ESCAPE + 'f';
      NEW_LINE: Result := Result + ESCAPE + 'n';
      CARRIAGE_RETURN: Result := Result + ESCAPE + 'r';
      HORIZONTAL_TAB: Result := Result + ESCAPE + 't';
      else
      begin
        if (Integer(AChar) < 32) or (Integer(AChar) > 126) then
          Result := Result + ESCAPE + 'u' + IntToHex(Integer(AChar), 4)
        else
          Result := Result + AChar;
      end;
    end;
  end;
end;


// this comes from TBlobField.SaveToStreamPersist, is the only way to directly obtain a valid image without usen a TPicture
type
  TGraphicHeader = record
    Count: Word;                { Fixed at 1 }
    HType: Word;                { Fixed at $0100 }
    Size: Longint;              { Size not including header }
  end;

function DWGetFieldBlobStream(ADataSet: TDataSet; AField: TBlobField): TStream;
var
  Size: Longint;
  GraphicHeader: TGraphicHeader;
begin
  Result := ADataSet.CreateBlobStream(AField, bmRead);
  Size := Result.Size;
  if Size >= SizeOf(TGraphicHeader) then begin
    Result.Read(GraphicHeader, SizeOf(GraphicHeader));
    if (GraphicHeader.Count <> 1) or (GraphicHeader.HType <> $0100) or
      (GraphicHeader.Size <> Size - SizeOf(GraphicHeader)) then
      Result.Position := 0;
  end;
end;

function DWFindParentFrame(aContainer:TDWContainer): TDWFrame;
var
  LParentFrame:TDWFrame;
  ControlP: TControl;
begin
  LParentFrame:= nil;
  ControlP:= aContainer;
  while (not (ControlP.Parent = nil))  do
    begin
      ControlP := ControlP.Parent;
      if ControlP is TDWFrame then
        begin
          LParentFrame:= TDWFrame(ControlP);
          Break;
        end;
    end;
  Result:= LParentFrame;
end;


end.
