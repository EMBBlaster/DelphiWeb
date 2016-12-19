unit DW.DESIGN.TabControlEditor;

interface

uses
  DesignEditors, DesignIntf, Forms, DW.VCL.TabControl;

type

  TDWTabControlEditor = class(TComponentEditor)
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

implementation

{ TCSDBGridEditor }

procedure TDWTabControlEditor.ExecuteVerb(Index: Integer);
var
  Tab: TDWTabPage;
begin
  inherited;
  case Index of
    0:
      begin
        Tab := TDWTabPage(Designer.CreateChild(TDWTabPage, Component));
        TDWTabControl(Component).InsertTab(Tab);
        // Item:= TDWAccordion(Component).CreateItem(Component);
        // Item.Name:= Designer.UniqueName(Item.ClassName);
        // TDWAccordion(Component).CreateItem(Component).Name:= Designer.UniqueName(Item.ClassName);;
      end;
    (* 1:  Application.MessageBox(PChar('    TSDDBGrid' + #13#10#13#10 + '版本: 6.0'
      + ' (编译版本: 6.0.6.12)' + #13#10#13#10 + ''),
      'About ...', 64); *)
  end;
end;

function TDWTabControlEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:
      Result := 'Add TabPage';
    // 1:  Result := 'Remove Item';

  end;
end;

function TDWTabControlEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

end.
