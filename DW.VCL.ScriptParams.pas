unit DW.VCL.ScriptParams;

interface

uses Classes, DW.JSON.JsonData;

type

  TDWScriptParams = class(TStringList)
  private
    function GetJson(const Name: string): TJsonObject;
    procedure SetJson(const Name: string; const Value: TJsonObject);
  public
    constructor Create;
    property JSON[const Name: string]: TJsonObject read GetJson write SetJson;
  end;

implementation

constructor TDWScriptParams.Create;
begin
  inherited;
  Duplicates  := dupError;
  OwnsObjects := True;
end;

function TDWScriptParams.GetJson(const Name: string): TJsonObject;
var
  i: integer;
begin
  i := IndexOf(Name);
  if i < 0 then
    begin
      Result := TJsonObject.Create;
      AddObject(Name, Result);
    end
  else if Objects[i] = nil then
    begin
      Result     := TJsonObject.Create;
      Objects[i] := Result;
    end
  else
    Result := TJsonObject(Objects[i]);
end;

procedure TDWScriptParams.SetJson(const Name: string; const Value: TJsonObject);
begin
  JSON[Name].Assign(Value);
end;

end.
