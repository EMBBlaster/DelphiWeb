{*******************************************************************************
  DW.VCL(Delphi Web Visual Component Library) is Based on
  IWBootsTrap Framework :  http://kattunga.github.io/IWBootstrapFramework/

  Delphi Web is
  Developped by Delcio Sbeghen @ SRP Sistemas
  delcio @ eavnet.com.br (remove spaces)
  and Colaborators
  under MIT Licence
}

unit DW.VCL.ScriptEvents;

interface
   uses Classes, System.SysUtils ,System.StrUtils, DWTypes;

  type

  //This class permit to add JavaScript to execute when events occurs in Browser
  TDWScriptEvent = class (TCollectionItem)
  private
    //type of Event, see Unit DWTypes
    FEventType:TDWAsyncEventType;
    //JavaScript to Execute
    FJavaScriptCode:TStringList;
    procedure SetJavaScriptCode(const Value: TStringList);
  protected
    //Only for Delphi Object Inspector display name of this
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection; aEventType:TDWAsyncEventType); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    // Return the javascript code
    function GetScript(ASemiColon: boolean = True): string;
    //Return name of Event
    function EventName:string;
  published
    //type of Event, see Unit DWTypes
    property EventType:TDWAsyncEventType read FEventType write FEventType;
    //JavaScript to Execute on Browser
    property JavaScriptCode:TStringList read  FJavaScriptCode write SetJavaScriptCode;
  end;

  TDWScriptEvents = class (TOwnedCollection)
  private
    function GetItems(I: Integer): TDWScriptEvent;
    procedure SetItems(I: Integer; const Value: TDWScriptEvent);
  public
    constructor Create(AOwner: TPersistent);
    function ByName(const AEventName: string): TDWScriptEvent;
    property Items[I: Integer]: TDWScriptEvent read GetItems write SetItems; default;
  end;

implementation





{ TDWScriptEvent }

procedure TDWScriptEvent.Assign(Source: TPersistent);
begin
  if Source is TDWScriptEvent then
    begin
      Self.FEventType:= (Source as TDWScriptEvent).FEventType;
      Self.FJavaScriptCode:= (Source as TDWScriptEvent).FJavaScriptCode;
    end
  else
    inherited;
end;

constructor TDWScriptEvent.Create(Collection: TCollection;
  aEventType: TDWAsyncEventType);
begin
  inherited;
  FJavaScriptCode.Create;
end;

destructor TDWScriptEvent.Destroy;
begin
  FJavaScriptCode.Free;
  inherited;
end;

function TDWScriptEvent.EventName: string;
begin
  Result:= AsyncEventTypeToName(FEventType);
end;

function TDWScriptEvent.GetDisplayName: string;
begin
  Result:= EventName;
  if Result = '' then Result := inherited GetDisplayName;
end;

function TDWScriptEvent.GetScript(ASemiColon: boolean): string;
begin
  Result:= FJavaScriptCode.Text;
  if ASemiColon then
    Result:= Result + ';';
end;

procedure TDWScriptEvent.SetJavaScriptCode(const Value: TStringList);
begin
  FJavaScriptCode := Value;
end;

{ TDWAsyncEvents }

function TDWScriptEvents.ByName(const AEventName: string): TDWScriptEvent;
var
  I: Integer;
begin
  Result:= nil;
  for I := 0 to Count -1 do
    begin
      if SameText(Items[I].EventName, AEventName) then
        begin
          Result:= Items[I];
          Break;
        end;
    end;
end;

constructor TDWScriptEvents.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TDWScriptEvent);
end;

function TDWScriptEvents.GetItems(I: Integer): TDWScriptEvent;
begin
  Result := TDWScriptEvent(inherited Items[I]);
end;

procedure TDWScriptEvents.SetItems(I: Integer; const Value: TDWScriptEvent);
begin
  inherited SetItem(I, Value);
end;

end.
