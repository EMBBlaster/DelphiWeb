unit DWUtils;

interface

uses Windows, Forms, Controls, DW.CORE.Server, System.SysUtils, System.StrUtils, DWUserSessionUnit,
DW.VCL.CustomForm;

function DWServer: TDWServer;
function MakeValidFileUrl(const ARootUrl: String; const AFileUrl: String):string;
//Find a parent Form of one Control
function DWFindParentForm(aControl:TControl): TDWCustomForm;
function  AnsiToUnicode(const Str: PAnsiChar; ACodePage: LongWord): UnicodeString; overload;
function  AnsiToUnicode(const Str: RawByteString; ACodePage: LongWord): UnicodeString; overload;
function  AnsiToUnicode(const Str: RawByteString): UnicodeString; {$IFDEF USE_INLINE} inline; {$ENDIF} overload; overload;
function  DWMbToWc(CodePage: LongWord; Flags: Cardinal;
                        MbStr: PAnsiChar; MbStrLen: Integer; WStr: PWideChar;
                        WStrLen: Integer): Integer;
function Base64Encode(Input : String) : String;
function DWGetTickCount: LongWord;

implementation

function DWServer: TDWServer;
begin
  Result:= TDWServer.GetDWServer;
end;


function MakeValidFileUrl(const ARootUrl: String; const AFileUrl: String):string;
var
  RootUrl:string;
  FileUrl:string;
begin
  if ARootUrl <> '' then
    RootUrl:= ARootUrl
  else
    RootUrl:= DWServer.UrlBase;
  FileUrl:= ExtractRelativePath(DWServer.DocDir + '\', AFileUrl);
  FileUrl:= StringReplace(FileUrl, '\', '/', [rfReplaceAll]);
  Result:=  {RootUrl + }'/' + FileUrl;
end;


function DWFindParentForm(aControl:TControl): TDWCustomForm;
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
    Len, Len2 : Integer;
begin
    if (Str <> nil) then begin
        Len := DWMbToWc(ACodePage, 0, Str, -1, nil, 0);
        if Len > 1 then begin // counts the null-terminator
            SetLength(Result, Len - 1);
            Len2 := DWMbToWc(ACodePage, 0, Str, -1,
                                Pointer(Result), Len);
            if Len2 <> Len then  // May happen, very rarely
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
    Len, Len2 : Integer;
begin
    Len := Length(Str);
    if Len > 0 then begin
        Len := DWMbToWc(ACodePage, 0, Pointer(Str),
                                   Len, nil, 0);
        SetLength(Result, Len);
        if Len > 0 then
        begin
            Len2 := DWMbToWc(ACodePage, 0, Pointer(Str), Length(Str),
                                Pointer(Result), Len);
            if Len2 <> Len then // May happen, very rarely
                SetLength(Result, Len2);
        end;
    end
    else
        Result := '';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function AnsiToUnicode(const Str: RawByteString): UnicodeString;
begin
    Result := AnsiToUnicode(Str, CP_ACP);
end;

function  DWMbToWc(CodePage: LongWord; Flags: Cardinal; MbStr: PAnsiChar;
  MbStrLen: Integer; WStr: PWideChar; WStrLen: Integer): Integer;
begin
    Result := MultiByteToWideChar(CodePage, Flags, MbStr, MbStrLen, WStr, WStrLen);
end;

function Base64Encode(Input : String) : String;
var
    Final : String;
    Count : Integer;
    Len   : Integer;
const
    Base64Out: array [0..64] of Char =
    ('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
     'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
     'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
     'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
     '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '+', '/', '=');
begin
    Final := '';
    Count := 1;
    Len   := Length(Input);
    while Count <= Len do begin
        Final := Final + Base64Out[(Byte(Input[Count]) and $FC) shr 2];
        if (Count + 1) <= Len then begin
            Final := Final + Base64Out[((Byte(Input[Count]) and $03) shl 4) +
                                       ((Byte(Input[Count+1]) and $F0) shr 4)];
            if (Count+2) <= Len then begin
                Final := Final + Base64Out[((Byte(Input[Count+1]) and $0F) shl 2) +
                                           ((Byte(Input[Count+2]) and $C0) shr 6)];
                Final := Final + Base64Out[(Byte(Input[Count+2]) and $3F)];
            end
            else begin
                Final := Final + Base64Out[(Byte(Input[Count+1]) and $0F) shl 2];
                Final := Final + '=';
            end
        end
        else begin
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

end.
