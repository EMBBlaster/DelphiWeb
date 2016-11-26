unit DW.XHR.CallBackResp;

interface
   uses Classes, DWElementTag;

   type
     TDWXhrCallbackResp = class
       FUpdateTag:TDWElementXHTMLTag;
       FExecuteTag:TDWElementXHTMLTag;
     public
       constructor Create;
       destructor Destroy; override;
       procedure AddScriptToExecute(aScript:string; AsCDATA:Boolean = True);
     end;


implementation

{ TDWXhrCallbackResp }

procedure TDWXhrCallbackResp.AddScriptToExecute(aScript: string;
  AsCDATA: Boolean);
var
  LTag: TDWElementXHTMLTag;
begin
  LTag:= TDWElementXHTMLTag.CreateHTMLTag('execscript', FExecuteTag, AsCDATA);
  LTag.Content.AddText(aScript);
  FExecuteTag.Content.AddElemetAsObject(LTag, True);
end;

constructor TDWXhrCallbackResp.Create;
begin
  inherited;
  FUpdateTag:= TDWElementXHTMLTag.CreateHTMLTag('update', nil, False);
  FExecuteTag:= TDWElementXHTMLTag.CreateHTMLTag('execute', nil, False);
end;

destructor TDWXhrCallbackResp.Destroy;
begin
  FUpdateTag.Free;
  FExecuteTag.Free;
  inherited;
end;

end.
