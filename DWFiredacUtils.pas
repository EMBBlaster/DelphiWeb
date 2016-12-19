unit DWFiredacUtils;

interface
  uses System.Classes, System.Variants, System.SysUtils, System.StrUtils;

  function ReplaceSpecialChars(Texto:string):string;
  function GetKeyFromParams(AParams: TStringList):Word;
  function GetShiftStateFromParams(AParams: TStringList):TShiftState;

implementation


function ReplaceSpecialChars(Texto:string):string;
Var A : Integer ;
    Letra : AnsiChar ;
    AnsiStr, Ret : AnsiString ;
      function TiraAcento( const AChar : AnsiChar ) : AnsiChar ;
      begin
        case AChar of
          'à','á','ã','ä','â','ª' : Result := 'a' ;
          'À','Á','Ã','Ä','Â' : Result := 'A' ;
          'è','é',    'ë','ê' : Result := 'e' ;
          'È','É',    'Ë','Ê' : Result := 'E' ;
          'ì','í',    'ï','î' : Result := 'i' ;
          'Ì','Í',    'Ï','Î' : Result := 'I' ;
          'ò','ó','õ','ö','ô','º' : Result := 'o' ;
          'Ò','Ó','Õ','Ö','Ô' : Result := 'O' ;
          'ù','ú',    'ü','û' : Result := 'u' ;
          'Ù','Ú',    'Ü','Û' : Result := 'U' ;
          'ç'                 : Result := 'c' ;
          'Ç'                 : Result := 'C' ;
          'ñ'                 : Result := 'n' ;
          'Ñ'                 : Result := 'N' ;
        else
          Result := AChar ;
        end;
      end ;

begin
  Result  := '' ;
  Ret     := '' ;
  AnsiStr := AnsiString( Texto );
  For A := 1 to Length( AnsiStr ) do
  begin
     Letra := TiraAcento( AnsiStr[A] ) ;
     if not (Letra in [#32..#126,#13,#10,#8]) then    {Letras / numeros / pontos / sinais}
        Letra := ' ' ;
     Ret := Ret + Letra ;
  end ;
  Result := String(Ret)
End;


function GetKeyFromParams(AParams: TStringList):Word;
begin
  try
    Result:= StrToIntDef(AParams.Values['which'] ,0);
  except
    Result:=0;
  end;
end;

function GetShiftStateFromParams(AParams: TStringList):TShiftState;
var
  Modif:string;
begin
  Result:=[];
  try
    Modif:= AParams.Values['modifiers'];
    if ContainsStr(Modif, 'SHIFT_MASK') then Include(Result, ssShift);
    if ContainsStr(Modif, 'CTRL_MASK') then Include(Result, ssCtrl);
    if ContainsStr(Modif, 'ALT_MASK') then Include(Result, ssAlt);
  except
    Result:=[];
  end;
end;

end.
