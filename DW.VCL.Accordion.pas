unit DW.VCL.Accordion;

interface
  uses   System.Classes, Controls, System.StrUtils, DW.VCL.Region, DWElementTag;

  type

  TDWAccordion = class;

  TDWAccordionItem  = class(TDWRegion)
  private
    FAccordion: TDWAccordion;
    FCaption: string;
    FID: Integer;
    FHTMLResult:TDWElementTag;
    procedure SetAccordion(const Value: TDWAccordion);
    procedure SetCaption(const Value: string);
    procedure SetID(const Value: Integer);
    protected
      function RenderHTML: TDWElementTag; override;
      procedure RenderComponents(aTagParent:TDWElementTag); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

 published
    property Accordion:TDWAccordion read FAccordion write SetAccordion;
    property Caption:string read FCaption write SetCaption;
    property ID:Integer read FID write SetID;
  end;

  TDWAccordionItems = class(TCollection)

  end;



  TDWAccordion = class(TDWRegion)
  private
    FItems: TList;
    FItemsPadding: string;
    procedure SetItemsPadding(const Value: string);
   protected
      function RenderHTML: TDWElementTag; override;
    public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CreateItem(AOwner: TComponent): TDWAccordionItem;
    procedure InsertItem(Item: TDWAccordionItem);
    procedure RemoveItem(Item: TDWAccordionItem);

  published
    property ItemsPadding:string read FItemsPadding write SetItemsPadding;
  end;


implementation

uses
  System.SysUtils;

{ TIWBSAccordion }

constructor TDWAccordion.Create(AOwner: TComponent);
begin
  inherited;
  FItems:= TList.Create;
end;

function TDWAccordion.CreateItem(AOwner: TComponent): TDWAccordionItem;
begin
  Result := TDWAccordionItem.Create(AOwner);
  Result.Parent := Self;
  Result.Accordion := Self;
end;

destructor TDWAccordion.Destroy;
begin
  inherited;
end;

procedure TDWAccordion.InsertItem(Item: TDWAccordionItem);
begin
  FItems.Add(Item);
  Item.FAccordion := Self;
  Item.Top := 0;
  { TODO 1 -oDELCIO -cVerify : verify insert item position in accordion }
  {
  if FItems.Count = 1 then
    Item.Top := 0
  else
    Item.Top := VertScrollBar.Range;}
  Item.Left := 0;
  Item.Align :=  alTop;
 { if not (csLoading in ComponentState) then
    AutoScrollInView(Item); }
end;

procedure TDWAccordion.RemoveItem(Item: TDWAccordionItem);
begin
  Item.FAccordion := nil;
  FItems.Remove(Item);
  RemoveControl(Item);
end;



function TDWAccordion.RenderHTML: TDWElementTag;
begin
  Result:= inherited;
  Result.AddClassParam('panel-group');
end;

procedure TDWAccordion.SetItemsPadding(const Value: string);
begin
  if FItemsPadding <> Value then
    begin
      if (Value <> '')
      and ( (Copy(Value, Length(Value) -1, 2) <> 'px')
          and (Copy(Value, Length(Value), 1) <> '%')) then
        raise Exception.Create('ItemsPadding must be of the format: 1px or 1%');
      FItemsPadding := Value;
    end;
end;

{ TIBSAccordionItem }



constructor TDWAccordionItem.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TDWAccordionItem.Destroy;
begin

  inherited;
end;

procedure TDWAccordionItem.RenderComponents(aTagParent:TDWElementTag);
var
  aComponents:string;
  i:Integer;
  aBody:TDWElementTag;
  aHeader:TDWElementTag;
begin
  //Render children componentes inside Item
  inherited;
  //Extract children components of Item
  for i := 0 to  FHTMLResult.Contents.Count -1 do
    begin
      aComponents:= aComponents + FHTMLResult.Contents.Items[i].Render;
    end;
  //Clear Item
  FHTMLResult.Contents.Clear;
  //Add class css to Item
  FHTMLResult.AddClassParam('panel-default');
  //Create the Header
  aHeader:= TDWElementTag.CreateHTMLTag('div');
  aHeader.AddClassParam('panel-heading');
  aHeader.Contents.AddText('<h4 class="panel-title">' +
                              '<a style="display:block;width:100%;" data-toggle="collapse" data-parent="#' + FAccordion.HTMLName
                                    + '" href="#collapse' + HTMLName + '">' + FCaption +
                              '</a>' +
                           '</h4>');
  //Add the Header to Item
  FHTMLResult.Contents.AddElemetAsObject(aHeader);
  //Create the Body
  aBody:= TDWElementTag.CreateHTMLTag('div');
  aBody.AddClassParam('panel-collapse');
  aBody.AddClassParam('collapse');
  aBody.AddStringParam('id', 'collapse' + HTMLName);
  if FAccordion.ItemsPadding <> '' then
    aBody.AddStringParam('padding', FAccordion.ItemsPadding);
  if FAccordion = nil then
    raise Exception.Create('Accordion Property for Item ' + Self.Name + ' is not set.');

  aBody.Contents.AddText({'<div id="collapse' + HTMLName + '" class="panel-collapse collapse">' + }
                            '<div class="panel-body"'
                            + ifthen(FAccordion.ItemsPadding <> '', ' style="padding:' + FAccordion.ItemsPadding + ';"','' ) + '>' +
                                aComponents + {<-- Add Components to Body}
                            '</div>'{ +
                        '</div>'});
  //Add Body(and components) to Item
  FHTMLResult.Contents.AddElemetAsObject(aBody);
end;

function TDWAccordionItem.RenderHTML: TDWElementTag;
var
  aTAG:TDWElementTag;
  aHeader:string;
  aBody:string;
begin
  Result:= inherited;
  //Save Html reference to use in "RenderComponents"
  FHTMLResult:= Result;
end;

procedure TDWAccordionItem.SetAccordion(const Value: TDWAccordion);
var
  LRecreating: Boolean;
begin
  if FAccordion <> Value then
  begin
    LRecreating := False;
    if not (csLoading in ComponentState) then
    begin
      LRecreating := csRecreating in ControlState;
      if not LRecreating then
        UpdateRecreatingFlag(True);
    end;

    try
      if FAccordion <> nil then
        FAccordion.RemoveItem(Self);
      Parent := Value;
      if Value <> nil then
      begin
        Value.InsertItem(Self);
        if not (csLoading in ComponentState) and not LRecreating then
          RecreateWnd;
      end;
    finally
      if not (csLoading in ComponentState) and not LRecreating then
        UpdateRecreatingFlag(False);
    end;
  end;
end;

procedure TDWAccordionItem.SetCaption(const Value: string);
begin
  FCaption := Value;
end;

procedure TDWAccordionItem.SetID(const Value: Integer);
begin
  FID := Value;
end;

initialization
    RegisterClass(TDWAccordionItem);

end.
