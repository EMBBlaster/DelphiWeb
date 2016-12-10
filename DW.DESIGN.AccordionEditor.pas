unit DW.DESIGN.AccordionEditor;


interface
  uses
     DesignEditors, DesignIntf, Forms, DW.VCL.Accordion;

  type

 TDWAccordionEditor = class(TComponentEditor)
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;


implementation

{ TCSDBGridEditor }

procedure TDWAccordionEditor.ExecuteVerb(Index: Integer);
var
  Item:TDWAccordionItem;
begin
  inherited;
  case Index of
    0:  begin
          Item:= TDWAccordionItem(Designer.CreateChild(TDWAccordionItem, Component));
          TDWAccordion(Component).InsertItem(Item);
         // Item:= TDWAccordion(Component).CreateItem(Component);
         // Item.Name:= Designer.UniqueName(Item.ClassName);
          //TDWAccordion(Component).CreateItem(Component).Name:= Designer.UniqueName(Item.ClassName);;
        end;
    (*1:  Application.MessageBox(PChar('    TSDDBGrid' + #13#10#13#10 + '版本: 6.0'
      + ' (编译版本: 6.0.6.12)' + #13#10#13#10 + ''),
      'About ...', 64); *)
  end;
end;

function TDWAccordionEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Add Item';
   // 1:  Result := 'Remove Item';

  end;
end;

function TDWAccordionEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

end.
