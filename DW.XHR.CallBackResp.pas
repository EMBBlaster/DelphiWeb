unit DW.XHR.CallBackResp;

interface

uses Classes, DWElementTag;

type
  TDWXhrCallbackResp = class
    FDWApplication: TObject;
    FExecuteFirstTag: TDWElementXHTMLTag;
    FUpdateTag: TDWElementXHTMLTag;
    FExecuteTag: TDWElementXHTMLTag;
  public
    constructor Create(aDWApplication: TObject);
    destructor Destroy; override;
    procedure Clear;
    function Render: string;
    procedure AddScriptToExecute(aScript: string; AsCDATA: Boolean = True);
    procedure AddScriptToUpdate(aScript: string; AsCDATA: Boolean = True);
    procedure AddScriptToExecuteFirst(aScript: string; AsCDATA: Boolean = True);


  end;

implementation

uses DW.CORE.DWApplication;

{ TDWXhrCallbackResp }

procedure TDWXhrCallbackResp.AddScriptToExecute(aScript: string; AsCDATA: Boolean);
var
  LTag: TDWElementXHTMLTag;
begin
  if not TDWApplication(FDWApplication).IsCallBack then
    Exit;
  LTag := TDWElementXHTMLTag.CreateHTMLTag('item', FExecuteTag, AsCDATA);
  LTag.Contents.AddText(aScript);
  FExecuteTag.Contents.AddElemetAsObject(LTag, True);
end;

procedure TDWXhrCallbackResp.AddScriptToExecuteFirst(aScript: string; AsCDATA: Boolean);
var
  LTag: TDWElementXHTMLTag;
begin
  if not TDWApplication(FDWApplication).IsCallBack then
    Exit;
  LTag := TDWElementXHTMLTag.CreateHTMLTag('item', FExecuteFirstTag, AsCDATA);
  LTag.Contents.AddText(aScript);
  FExecuteFirstTag.Contents.AddElemetAsObject(LTag, True);
end;

procedure TDWXhrCallbackResp.AddScriptToUpdate(aScript: string;
  AsCDATA: Boolean);
var
  LTag: TDWElementXHTMLTag;
begin
  if not TDWApplication(FDWApplication).IsCallBack then
    Exit;
  LTag := TDWElementXHTMLTag.CreateHTMLTag('item', FUpdateTag, AsCDATA);
  LTag.Contents.AddText(aScript);
  FUpdateTag.Contents.AddElemetAsObject(LTag, True);
end;

procedure TDWXhrCallbackResp.Clear;
begin
  FExecuteFirstTag.Clear;
  FUpdateTag.Clear;
  FExecuteTag.Clear;
end;

constructor TDWXhrCallbackResp.Create(aDWApplication: TObject);
begin
  inherited Create;
  FDWApplication   := aDWApplication;
  FExecuteFirstTag := TDWElementXHTMLTag.CreateHTMLTag('executefirst', nil, False);
  FUpdateTag       := TDWElementXHTMLTag.CreateHTMLTag('update', nil, False);
  FExecuteTag      := TDWElementXHTMLTag.CreateHTMLTag('execute', nil, False);
end;

destructor TDWXhrCallbackResp.Destroy;
begin
  FExecuteFirstTag.Free;
  FUpdateTag.Free;
  FExecuteTag.Free;
  inherited;
end;

function TDWXhrCallbackResp.Render: string;
begin
  Result := '<?xml version="1.0" encoding="UTF-8" standalone="yes"?><response>';
  Result := Result + FExecuteFirstTag.Render;
  Result := Result + FUpdateTag.Render;
  Result := Result + FExecuteTag.Render;
  Result := Result + '</response>'
end;

end.
