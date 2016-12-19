unit DW.VCL.JQGrid;

interface

uses System.Classes, System.SysUtils, System.Math, StrUtils, DB, DW.VCL.Labels,
  DWElementTag;

type

  TBsJQGSortOrder   = (bsgSortAsc, bsgSortDesc);
  TBsJQGPagPosition = (bsgPagTop, bsgPagBottom, bsgPagBoth);

  TBsColDataAlign = (bscAlDefault, bscAlLeft, bscAlRight, bscAlCenter);
  // TBsColDataVertAlign = (bscAlVerTop, bscAlVerBottom, bscAlVerMiddle);
  TbsJQGColInputType = (bsjqgText, bsjqgSelect, bsjqgCustom);

  TDWGridColumn = class(TCollectionItem)
  private
    FFieldName: string;
    FTitle: string;
    FCSSclass: string;
    FDataAlign: TBsColDataAlign;
    FHeaderAlign: TBsColDataAlign;
    // FFooterAlign: TBsColDataAlign;
    // FDataVertAlign: TBsColDataVertAlign;
    FWidth: string;
    FSortable: Boolean;
    FInputType: TbsJQGColInputType;
    FResizable: Boolean;
    FVisible: Boolean;
    FEditable: Boolean;
    procedure SetFieldName(const Value: string);
    procedure SetTitle(const Value: string);
    procedure SetCSSclass(const Value: string);
    procedure SetDataAlign(const Value: TBsColDataAlign);
    procedure SetHeaderAlign(const Value: TBsColDataAlign);
    // procedure SetFooterAlign(const Value: TBsColDataAlign);
    // procedure SetDataVertAlign(const Value: TBsColDataVertAlign);
    procedure SetWidth(const Value: string);
    procedure SetSortable(const Value: Boolean);
    function IsWidthStored: Boolean;
    function IsCssClassStored: Boolean;
    procedure SetInputType(const Value: TbsJQGColInputType);
    procedure SetResizable(const Value: Boolean);
    procedure SetVisible(const Value: Boolean);
    procedure SetEditable(const Value: Boolean);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    // The Dataset Field for column
    property FieldName: string read FFieldName write SetFieldName;
    // the title of Column
    property Title: string read FTitle write SetTitle;
    // the css class for all cell in this column
    property CSSclass: string read FCSSclass write SetCSSclass stored IsCssClassStored;
    // the alignament data in all cell in this column
    property DataAlign: TBsColDataAlign read FDataAlign write SetDataAlign default bscAlDefault;
    // property DataVertAlign:TBsColDataVertAlign read FDataVertAlign write SetDataVertAlign default bscAlVerMiddle;
    // the alignament of header tittle
    property HeaderAlign: TBsColDataAlign read FHeaderAlign write SetHeaderAlign
      default bscAlCenter;
    // property FooterAlign: TBsColDataAlign read FFooterAlign write SetFooterAlign default bscAlLeft;
    // the width of column in px or %
    property Width: string read FWidth write SetWidth stored IsWidthStored;
    // if Column is sortable or not
    property Sortable: Boolean read FSortable write SetSortable default True;
    // if column is editable or not
    property Editable: Boolean read FEditable write SetEditable default True;
    // the input type for column in edit mode. Require Editable = True;
    property InputType: TbsJQGColInputType read FInputType write SetInputType;
    // if the column is resizable in browser
    property Resizable: Boolean read FResizable write SetResizable default True;
    // if the column is visible
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

  TDWJQGrid = class(TDWText)
  private
    FColumns: TOwnedCollection;
    FPagination: Boolean;
    FMobileResponsive: Boolean;
    FFormatData: TFormatSettings;
    FSortColumn: string;
    FSortOrder: TBsJQGSortOrder;
    FShowHeader: Boolean;
    FShowFooter: Boolean;
    FShowRefresh: Boolean;
    FPaginationPosition: TBsJQGPagPosition;
    FclickToSelect: Boolean;
    FSingleSelect: Boolean;
    data: string;
    FGroupingField: string;
    FGroupSummary: Boolean;
    FGroupCollapse: Boolean;
    FGroupLabel: string;
    FKeyField: string;
    FRowNum: Integer;
    procedure SetTagType(const Value: string);
    function IsTagTypeStored: Boolean;
    function IsSortFieldNameStored: Boolean;
    procedure SetColumns(const Value: TOwnedCollection);
    // to update script JQGrid options when Component options are changed
    procedure UpdateOptions;
    // if no columns set an datasource is setted, add all dataset columns
    procedure VerifyColumns;
    procedure SetMobileResponsive(const Value: Boolean);
    procedure SetPagination(const Value: Boolean);
    // this event we return a json with the rows that the bootstrap JQGrid request
    procedure DbJQGridCustomRestEvents0RestEvent(aParams: TStrings; var aReply: string);
    function GetColumns: TOwnedCollection;
    function GetTagType: string;
    procedure SetSortColumn(const Value: string);
    procedure SetSortOrder(const Value: TBsJQGSortOrder);
    procedure SetShowHeader(const Value: Boolean);
    procedure SetShowFooter(const Value: Boolean);
    procedure SetShowRefresh(const Value: Boolean);
    procedure SetPaginationPosition(const Value: TBsJQGPagPosition);
    procedure SetclickToSelect(const Value: Boolean);
    procedure SetSingleSelect(const Value: Boolean);
    procedure SetGroupingField(const Value: string);
    procedure SetGroupCollapse(const Value: Boolean);
    procedure SetGroupSummary(const Value: Boolean);
    procedure SetGroupLabel(const Value: string);
    procedure DoOnRowSel(aParams: TStrings; var aReply: string);
    procedure DoEditRow(aParams: TStrings; var aReply: string);
    procedure SetKeyField(const Value: string);
    // To Set Intaernal Datasource
    procedure SetDataSource(const Value: TDataSource); override;
    procedure SetRowNum(const Value: Integer);
  protected
    procedure InternalRenderScript(const AHTMLName: string; AScript: TStringList); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function RenderAsync: TDWElementXHTMLTag; override;
    function RenderHTML: TDWElementTag; override;
  published
    property TagType: string read GetTagType write SetTagType stored IsTagTypeStored;
    property ScriptInsideTag default False;
    // Collection of Columns in the grid.
    // To acess Columns.Items[n] properties do a TypeCast eg: TIWBSJQGridColumn(Columns.Items[n]).FieldName
    property Columns: TOwnedCollection read GetColumns write SetColumns;
    // property Pagination: Boolean read FPagination write SetPagination default True;
    // property MobileResponsive: Boolean read FMobileResponsive write SetMobileResponsive default True;
    // property SortFieldName:string read FSortColumn write SetSortColumn stored IsSortFieldNameStored;
    // property SortOrder: TBsJQGSortOrder read FSortOrder write SetSortOrder default bsgSortAsc;
    // property ShowHeader: Boolean read FShowHeader write SetShowHeader default True;
    // property ShowFooter:Boolean read FShowFooter write SetShowFooter default False;
    // property ShowRefresh:Boolean read FShowRefresh write SetShowRefresh default True;
    // property PaginationPosition:TBsJQGPagPosition read FPaginationPosition write SetPaginationPosition default bsgPagBottom;
    // property ClickToSelect:Boolean read FclickToSelect write SetclickToSelect default True;
    // property SingleSelect:Boolean read FSingleSelect write SetSingleSelect default True;
    // Field name for Grouping rows
    property GroupingField: string read FGroupingField write SetGroupingField;
    // To show group summary, require GroupingField;
    property GroupSummary: Boolean read FGroupSummary write SetGroupSummary default False;
    // To show group rows collapsed, require GroupingField;
    property GroupCollapse: Boolean read FGroupCollapse write SetGroupCollapse default False;
    // Text to show before group header value
    property GroupLabel: string read FGroupLabel write SetGroupLabel;
    // The key for record navigation and edition
    // !!!!ATENTION!!!!! this field should be the primary key or
    // unique value field in the table, else data can be saved in the wrong records
    property KeyField: string read FKeyField write SetKeyField;
    property RowNum: Integer read FRowNum write SetRowNum default 20;
  end;

implementation

uses DWUtils, DWUrlHandlerRest, DWGlobal;

{ TIWBSJQGrid }

constructor TDWJQGrid.Create(AOwner: TComponent);
begin
  inherited;
  inherited TagType         := 'table';
  inherited ScriptInsideTag := False;
  FColumns                  := TOwnedCollection.Create(Self, TDWGridColumn);
  FPagination               := True;
  FMobileResponsive         := True;
  FFormatData               := TFormatSettings.Create('en-US');
  FSortColumn               := '';
  FSortOrder                := bsgSortAsc;
  FShowHeader               := True;
  FShowFooter               := False;
  FShowRefresh              := True;
  FPaginationPosition       := bsgPagBottom;
  FclickToSelect            := True;
  FSingleSelect             := True;
  FGroupingField            := '';
  FGroupSummary             := False;
  FGroupCollapse            := False;
  FGroupLabel               := '';
  FKeyField                 := '';
  FRowNum                   := 20;
  // UpdateOptions;
end;

procedure TDWJQGrid.DbJQGridCustomRestEvents0RestEvent(aParams: TStrings; var aReply: string);
  function GetFieldForColumn(aColumnIndex: Integer): TField;
  begin
    try
      Result := DataSource.DataSet.FieldByName(TDWGridColumn(FColumns.Items[aColumnIndex])
        .FieldName);
    except
      Result := nil;
    end;
  end;

var

  line: string;
  bmrk: TBookmark;
  r, i, f, t: Integer;
  Lcallback: string;
  Rows, Page: Integer;
begin
  Lcallback := aParams.Values['callback'];
  // f:=0;
  // t:= DataSource.DataSet.RecordCount;
  Rows := StrToIntDef(aParams.Values['rows'], FRowNum);
  Page := StrToIntDef(aParams.Values['page'], 1);
  f    := (Page * Rows) - Rows;
  t    := Min(f + Rows, DataSource.DataSet.RecordCount);

  DataSource.DataSet.DisableControls;
  bmrk := DataSource.DataSet.Bookmark;
  try
    data  := '';
    for r := f + 1 to t do
      begin
        DataSource.DataSet.RecNo := r;

        line  := '';
        for i := 0 to FColumns.Count - 1 do
          begin
            if i > 0 then
              line := line + ',';
            if (GetFieldForColumn(i) is TNumericField) then
              line := line + TDWGridColumn(FColumns.Items[i]).FFieldName + ':"' +
                FloatToStr(GetFieldForColumn(i).AsFloat, FFormatData) + '"'
            else if (DataSource.DataSet.Fields[i] is TStringField) or
              (GetFieldForColumn(i) is TMemoField) then
              line := line + TDWGridColumn(FColumns.Items[i]).FFieldName + ':"' +
                EscapeJsonString(GetFieldForColumn(i).AsString) + '"'
            else if GetFieldForColumn(i) <> nil then
              line := line + TDWGridColumn(FColumns.Items[i]).FFieldName + ':"' +
                GetFieldForColumn(i).AsString + '"'
            else
              line := line + TDWGridColumn(FColumns.Items[i]).FFieldName + ':""';
          end;
        if data <> '' then
          data := data + ',';
        data   := data + '{' + line + '}';
      end;
    aReply := (Lcallback + '({"records":"' + IntToStr(DataSource.DataSet.RecordCount) + '","page":'
      + IntToStr(Page) + ',"total":' + IntToStr(Ceil(DataSource.DataSet.RecordCount / Rows)) +
      ',"rows": [' + data + ']})');
  finally
    DataSource.DataSet.GotoBookmark(bmrk);
    DataSource.DataSet.EnableControls;
  end;
end;

destructor TDWJQGrid.Destroy;
begin

  FColumns.Free;
  inherited;
end;

procedure TDWJQGrid.DoEditRow(aParams: TStrings; var aReply: string);
var
  KeyValue: Variant;
  Value: Variant;
  Field: TField;
  i: Integer;
begin
  if aParams.Values['oper'] = 'edit' then
    begin
      if FKeyField = '' then
        raise Exception.Create('No KeyField set. This must be a primarykey or unique field.');
      KeyValue := aParams.Values[FKeyField];
      if DataSource.DataSet.FieldByName(FKeyField).Value <> KeyValue then
        begin
          if Not DataSource.DataSet.Locate(FKeyField, KeyValue, [loCaseInsensitive]) then
            begin
              DWApplication.ShowMessage('Record not found on the server');
              DWApplication.CallBackResp.AddScriptToExecute
                ('$("#' + HTMLName + '").trigger("reloadGrid")');
            end;
        end;

      if DataSource.DataSet.FieldByName(FKeyField).Value = KeyValue then
        begin
          for i := 0 to aParams.Count - 1 do
            begin
              if aParams.Names[i] <> 'oper' then
                begin
                  Field := DataSource.DataSet.FieldByName(aParams.Names[i]);
                  Value := aParams.Values[aParams.Names[i]];
                  if Field <> nil then
                    begin
                      if Field.Value <> Value then
                        begin
                          if Field.DataSet.State in [dsEdit, dsInsert] then
                            Field.Value := Value
                          else
                            begin
                              Field.DataSet.Edit;
                              Field.Value := Value;
                              Field.DataSet.Post;
                            end;
                        end;
                    end;
                end;
            end;
        end
      else
        begin
          raise Exception.Create('Server not in correct record');
        end;
    end;
end;

procedure TDWJQGrid.DoOnRowSel(aParams: TStrings; var aReply: string);
var
  Value: Variant;
begin
  if FKeyField = '' then
    raise Exception.Create('No KeyField set. This must be a primarykey or unique field.');
  Value := aParams.Values['key'];
  if Value = '' then
    Exit;

  case DataSource.DataSet.FieldByName(FKeyField).DataType of
    ftSmallint, ftInteger, ftWord, ftLongWord, ftShortint, ftByte:
      VarCast(Value, Value, varInteger);
    ftLargeint, ftAutoInc:
      VarCast(Value, Value, varInt64);
    ftBoolean:
      VarCast(Value, Value, varBoolean);
    ftFloat:
      VarCast(Value, Value, varDouble);
    ftCurrency, ftBCD, ftOraInterval, ftExtended:
      VarCast(Value, Value, varCurrency);
    ftDate:
      VarCast(Value, Value, varDate);
    ftTime:
      VarCast(Value, Value, varDate);
    ftDateTime, ftOraTimeStamp, ftTimeStampOffset:
      VarCast(Value, Value, varDate);
    ftBytes, ftVarBytes, ftBlob, ftGraphic, ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor,
      ftADT, ftArray, ftReference, ftDataSet, ftOraBlob, ftOraClob, ftVariant, ftInterface,
      ftIDispatch, ftGuid, ftTimeStamp, ftFMTBcd, // 32..37
    ftConnection, ftParams, ftStream, ftObject, ftSingle:
      raise Exception.Create('Invalid Key Column for Grid ' + HTMLName);
  end;

  if Not DataSource.DataSet.Locate(FKeyField, Value, [loCaseInsensitive]) then
    begin
      DWApplication.ShowMessage('Record not found on the server');
      DWApplication.CallBackResp.AddScriptToExecute('$("#' + HTMLName + '").trigger("reloadGrid")');
    end;
end;

function TDWJQGrid.GetColumns: TOwnedCollection;
begin
  { if not Assigned(FColumns) then
    FColumns:= TOwnedCollection.Create(Self, TIwSrpJQGridColum); }
  Result := FColumns;
end;

function TDWJQGrid.GetTagType: string;
begin
  Result := inherited TagType;
end;

procedure TDWJQGrid.InternalRenderScript(const AHTMLName: string; AScript: TStringList);
begin
  inherited;
  UpdateOptions;

  AScript.Add('var w = $("#{%htmlname%}").parent().width();' + 'w = w - 20;' +
    '$("#{%htmlname%}").jqGrid({%options%});' +

    'var lastRowSel{%htmlname%};' + 'var InEditMode{%htmlname%};' +
    'function internalSelectRow(id) {' +
    'var gridEditable{%htmlname%} = ($("#{%htmlname%}").getGridParam(''editurl'') !== "clientArray");'
    + 'if (id && id !== lastRowSel{%htmlname%}) {' +
    '$("#{%htmlname%}").jqGrid(''saveRow'',lastRowSel{%htmlname%});' +
    'if (gridEditable{%htmlname%}) {' +
    '$("#{%htmlname%}").jqGrid(''editRow'',id, {keys:true, focusField: 1});' + '};' +
    'lastRowSel{%htmlname%} = id;' +
    // 'setTimeout(function(){' +
    'InEditMode{%htmlname%} = ($($("#' + HTMLName +
    '").jqGrid("getInd",lastRowSel{%htmlname%},true)).attr("editable") === ''1'') || false;' +
    'executeAjaxEvent("&key="+id, null, "{%htmlname%}.DoOnRowSel", false, null, true);' +
    // '}, 10);'+
    '}' + '};' +

    'function internalBeforeRequest(){' +
    // 'var InEditMode = ($($("#' + HTMLName + '").jqGrid("getInd",lastRowSel,true)).attr("editable") === ''1'') || false;' +
    'return !InEditMode{%htmlname%};' + '};'

    );
  DWApplication.RegisterRestCallBack(Self, AHTMLName + '.DoOnRowSel', DoOnRowSel);
end;

function TDWJQGrid.IsSortFieldNameStored: Boolean;
begin
  Result := FSortColumn <> '';
end;

function TDWJQGrid.IsTagTypeStored: Boolean;
begin
  Result := TagType <> 'table';
end;

function TDWJQGrid.RenderAsync: TDWElementXHTMLTag;
var
  CurrDatasetPage: Integer;
begin
  if FColumns.Count = 0 then
    UpdateOptions;
  Result := inherited;
  if (DataSource <> nil) and (not(DataSource.DataSet.State in [dsEdit, dsInsert])) then
    begin
      CurrDatasetPage := Round((DataSource.DataSet.RecNo / FRowNum) + 0.49999);
      DWApplication.CallBackResp.AddScriptToExecute('if (!InEditMode' + HTMLName + ') {' + '$("#' +
        HTMLName + '").trigger("reloadGrid" ,[{page:' + IntToStr(CurrDatasetPage) + '}]);' +
        'setTimeout(function(){' + '$("#' + HTMLName + '").jqGrid("resetSelection");' + '$("#' +
        HTMLName + '").jqGrid("setSelection","' + DataSource.DataSet.FieldByName(FKeyField).AsString
        + '", false);' + '}, 500);' + '};');
    end;

  (* IWBSExecuteAsyncJScript(' var GridInEdit =


    $("#' + HTMLName + '").trigger("reloadGrid")');

    $($("#list").jqGrid("getInd",rowid,true)).attr("editable") === "1") {
    // the row having id=rowid is in editing mode
    } *)
end;

function TDWJQGrid.RenderHTML: TDWElementTag;
begin
  if FColumns.Count = 0 then
    UpdateOptions;
  Result := inherited;
end;

procedure TDWJQGrid.VerifyColumns;
var
  J: Integer;
  KeyOk: Boolean;
begin
  if (FColumns.Count = 0) and (Assigned(DataSource)) then
    begin
      for J := 0 to DataSource.DataSet.FieldCount - 1 do
        begin
          with TDWGridColumn(FColumns.Add) do
            begin
              FieldName := DataSource.DataSet.Fields[J].FieldName;
              Title     := DataSource.DataSet.Fields[J].DisplayName;
            end;
        end;
    end;
  KeyOk := False;
  for J := 0 to FColumns.Count - 1 do
    begin
      if AnsiCompareText(FKeyField, TDWGridColumn(FColumns.Items[J]).FieldName) = 0 then
        begin
          KeyOk := True;
          Break;
        end;
    end;
  // add Key Column
  if not KeyOk then
    with TDWGridColumn(FColumns.Add) do
      begin
        FieldName := FKeyField;
        Title     := 'KEY';
        Visible   := False;
        Editable  := False;
      end;
end;

procedure TDWJQGrid.SetclickToSelect(const Value: Boolean);
begin
  if FclickToSelect <> Value then
    begin
      FclickToSelect := Value;
      UpdateOptions;
    end;
end;

procedure TDWJQGrid.SetColumns(const Value: TOwnedCollection);
begin
  { if not Assigned(FColumns) then
    FColumns:= TOwnedCollection.Create(Self, TIwSrpJQGridColum); }
  if Value <> FColumns then
    begin
      FColumns.Assign(Value);
      UpdateOptions;
    end;
end;

procedure TDWJQGrid.SetDataSource(const Value: TDataSource);
begin
  inherited;
end;

procedure TDWJQGrid.SetGroupCollapse(const Value: Boolean);
begin
  FGroupCollapse := Value;
end;

procedure TDWJQGrid.SetGroupingField(const Value: string);
begin
  FGroupingField := Value;
end;

procedure TDWJQGrid.SetGroupLabel(const Value: string);
begin
  FGroupLabel := Value;
end;

procedure TDWJQGrid.SetGroupSummary(const Value: Boolean);
begin
  FGroupSummary := Value;
end;

procedure TDWJQGrid.SetKeyField(const Value: string);
begin
  FKeyField := Value;
end;

procedure TDWJQGrid.SetMobileResponsive(const Value: Boolean);
begin
  if FMobileResponsive <> Value then
    begin
      FMobileResponsive := Value;
      UpdateOptions;
    end;
end;

procedure TDWJQGrid.SetPagination(const Value: Boolean);
begin
  FPagination := Value;
  UpdateOptions;
end;

procedure TDWJQGrid.SetPaginationPosition(const Value: TBsJQGPagPosition);
begin
  if FPaginationPosition <> Value then
    begin
      FPaginationPosition := Value;
      UpdateOptions;
    end;
end;

procedure TDWJQGrid.SetRowNum(const Value: Integer);
begin
  FRowNum := Value;
end;

procedure TDWJQGrid.SetShowFooter(const Value: Boolean);
begin
  if FShowFooter <> Value then
    begin
      FShowFooter := Value;
      UpdateOptions;
    end;
end;

procedure TDWJQGrid.SetShowHeader(const Value: Boolean);
begin
  if FShowHeader <> Value then
    begin
      FShowHeader := Value;
      UpdateOptions;
    end;
end;

procedure TDWJQGrid.SetShowRefresh(const Value: Boolean);
begin
  if FShowRefresh <> Value then
    begin
      FShowRefresh := Value;
      UpdateOptions;
    end;
end;

procedure TDWJQGrid.SetSingleSelect(const Value: Boolean);
begin
  if FSingleSelect <> Value then
    begin
      FSingleSelect := Value;
      UpdateOptions;
    end;
end;

procedure TDWJQGrid.SetSortColumn(const Value: string);
begin
  if FSortColumn <> Value then
    begin
      FSortColumn := Value;
      UpdateOptions;
    end;
end;

procedure TDWJQGrid.SetSortOrder(const Value: TBsJQGSortOrder);
begin
  if FSortOrder <> Value then
    begin
      FSortOrder := Value;
      UpdateOptions;
    end;
end;

procedure TDWJQGrid.SetTagType(const Value: string);
begin
  inherited TagType := Value;
  UpdateOptions;
end;

procedure TDWJQGrid.UpdateOptions;
var
  OptColumns: string;
  OptColumn, OptTxt: TStrings;
  J: Integer;
  Field: TField;
  LGridEditable: Boolean;
begin
  VerifyColumns;
  LGridEditable := False;
  OptColumns    := '';

  OptColumn := TStringList.Create;
  try
    OptColumn.NameValueSeparator := ':';
    OptColumn.Delimiter          := ',';
    OptColumn.QuoteChar          := ' ';
    OptColumn.StrictDelimiter    := True;

    for J := 0 to FColumns.Count - 1 do
      begin
        OptColumn.Clear;
        with TDWGridColumn(FColumns.Items[J]) do
          begin
            OptColumn.Values['label'] := '''' + FTitle + '''';
            OptColumn.Values['name']  := '''' + FFieldName + '''';
            if AnsiUpperCase(FFieldName) = AnsiUpperCase(FKeyField) then
              OptColumn.Values['key'] := 'true';
            if Not FVisible then
              OptColumn.Values['hidden'] := 'true';
            if not FSortable then
              OptColumn.Values['sortable'] := 'false';
            if not FResizable then
              OptColumn.Values['resizable'] := 'false';

            case FHeaderAlign of
              bscAlLeft:
                OptColumn.Values['labelAlign'] := '"left"';
              bscAlRight:
                OptColumn.Values['labelAlign'] := '"right"';
              bscAlDefault:
                OptColumn.Values['labelAlign'] := '"likeData"';
              // bscAlCenter is default
            end;

            case FDataAlign of
              bscAlLeft:
                OptColumn.Values['align'] := '"left"';
              bscAlRight:
                OptColumn.Values['align'] := '"right"';
              bscAlCenter:
                OptColumn.Values['align'] := '"center"';
            end;

            if FCSSclass <> '' then
              OptColumn.Values['classes'] := '''' + FCSSclass + '''';

            if Editable then
              begin
                LGridEditable := True;
                if AnsiUpperCase(FKeyField) <> AnsiUpperCase(FieldName) then
                  OptColumn.Values['editable'] := 'true'
                else
                  OptColumn.Values['editable'] := 'false'; // KeyField is not editable
              end
            else
              OptColumn.Values['editable'] := 'false';

            if Assigned(DataSource) and Assigned(DataSource.DataSet) then
              begin
                Field := DataSource.DataSet.FieldByName(FieldName);

                case Field.DataType of
                  ftUnknown:
                    ;
                  ftString:
                    begin
                      if Editable then
                        OptColumn.Values['edittype'] := '"text"';
                    end;
                  ftSmallint:
                    begin
                      if Editable then
                        OptColumn.Values['edittype'] := '"text"';
                      OptColumn.Values['formatter']  := '"integer"';
                      OptColumn.Values['align']      := '"right"';
                    end;
                  ftInteger:
                    begin
                      if Editable then
                        OptColumn.Values['edittype'] := '"text"';
                      OptColumn.Values['formatter']  := '"integer"';
                      OptColumn.Values['align']      := '"right"';
                    end;
                  ftWord:
                    begin

                    end;
                  ftBoolean:
                    begin
                      if Editable then
                        begin
                          OptColumn.Values['edittype']    := '"checkbox"';
                          OptColumn.Values['editoptions'] := '{value:"True:False"}';
                        end;
                      OptColumn.Values['formatter'] := '"checkbox"';
                      OptColumn.Values['align']     := '"center"';
                    end;
                  ftFloat:
                    begin
                      if Editable then
                        OptColumn.Values['edittype'] := '"text"';
                      OptColumn.Values['formatter']  := '"number"';
                      OptColumn.Values['align']      := '"right"';
                    end;
                  ftCurrency:
                    begin
                      if Editable then
                        OptColumn.Values['edittype'] := '"text"';
                      OptColumn.Values['formatter']  := '"currency"';
                      OptColumn.Values['align']      := '"right"';
                    end;
                  ftBCD:
                    begin
                      if Editable then
                        OptColumn.Values['edittype'] := '"text"';
                      OptColumn.Values['formatter']  := '"number"';
                      OptColumn.Values['align']      := '"right"';
                    end;
                  ftDate:
                    begin
                      // OptColumn.Values['formatter']:='"date"';
                      OptColumn.Values['align'] := '"center"';

                    end;
                  ftTime:
                    begin
                      // OptColumn.Values['formatter']:='"date"';
                      OptColumn.Values['align'] := '"center"';
                    end;
                  ftDateTime:
                    begin
                      // OptColumn.Values['formatter']:='"date"';
                      OptColumn.Values['align'] := '"center"';
                    end;
                  ftBytes:
                    begin

                    end;
                  ftVarBytes:
                    begin

                    end;
                  ftAutoInc:
                    begin
                      OptColumn.Values['formatter'] := '"integer"';
                      OptColumn.Values['align']     := '"right"';
                    end;
                  ftBlob:
                    begin

                    end;
                  ftMemo:
                    begin
                      if Editable then
                        OptColumn.Values['edittype'] := '"textarea"';
                    end;
                  ftGraphic:
                    begin
                      if Editable then
                        OptColumn.Values['edittype'] := '"textarea"';
                    end;
                  ftFmtMemo:
                    begin
                      if Editable then
                        OptColumn.Values['edittype'] := '"textarea"';
                    end;
                  ftParadoxOle:
                    begin

                    end;
                  ftDBaseOle:
                    begin

                    end;
                  ftTypedBinary:
                    begin

                    end;
                  ftCursor:
                    begin

                    end;
                  ftFixedChar:
                    begin
                      if Editable then
                        OptColumn.Values['edittype'] := '"text"';
                    end;
                  ftWideString:
                    begin
                      if Editable then
                        OptColumn.Values['edittype'] := '"text"';
                    end;
                  ftLargeint:
                    begin
                      if Editable then
                        OptColumn.Values['edittype'] := '"text"';
                      OptColumn.Values['formatter']  := '"integer"';
                    end;
                  ftADT:
                    begin

                    end;
                  ftArray:
                    begin

                    end;
                  ftReference:
                    begin

                    end;
                  ftDataSet:
                    begin

                    end;
                  ftOraBlob:
                    begin

                    end;
                  ftOraClob:
                    begin

                    end;
                  ftVariant:
                    begin
                      if Editable then
                        OptColumn.Values['edittype'] := '"textarea"';
                    end;
                  ftInterface:
                    begin

                    end;
                  ftIDispatch:
                    begin

                    end;
                  ftGuid:
                    begin

                    end;
                  ftTimeStamp:
                    begin

                    end;
                  ftFMTBcd:
                    begin

                    end;
                  ftFixedWideChar:
                    begin
                      if Editable then
                        OptColumn.Values['edittype'] := '"text"';
                    end;
                  ftWideMemo:
                    begin
                      if Editable then
                        OptColumn.Values['edittype'] := '"textarea"';
                    end;
                  ftOraTimeStamp:
                    begin

                    end;
                  ftOraInterval:
                    begin

                    end;
                  ftLongWord:
                    begin

                    end;
                  ftShortint:
                    begin
                      if Editable then
                        OptColumn.Values['edittype'] := '"text"';
                      OptColumn.Values['formatter']  := '"integer"';
                      OptColumn.Values['align']      := '"right"';
                    end;
                  ftByte:
                    begin
                      if Editable then
                        OptColumn.Values['edittype'] := '"text"';
                      OptColumn.Values['formatter']  := '"integer"';
                      OptColumn.Values['align']      := '"right"';
                    end;
                  ftExtended:
                    begin
                      if Editable then
                        OptColumn.Values['edittype'] := '"text"';
                      OptColumn.Values['formatter']  := '"number"';
                      OptColumn.Values['align']      := '"right"';
                    end;
                  ftConnection:
                    begin

                    end;
                  ftParams:
                    begin
                      if Editable then
                        OptColumn.Values['edittype'] := '"textarea"';
                    end;
                  ftStream:
                    begin
                      if Editable then
                        OptColumn.Values['edittype'] := '"text"';
                    end;
                  ftTimeStampOffset:
                    begin

                    end;
                  ftObject:
                    begin

                    end;
                  ftSingle:
                    begin

                    end;
                end;

              end;

            (*
              case FDataVertAlign of
              bscAlVerTop:
              OptColumns := OptColumns + '","valign":"top';
              bscAlVerBottom:
              OptColumns := OptColumns + '","valign":"bottom';
              bscAlVerMiddle:
              OptColumns := OptColumns + '","valign":"middle';
              end;
              case FFooterAlign of
              bscAlLeft:
              OptColumns := OptColumns + '","falign":"left';
              bscAlRight:
              OptColumns := OptColumns + '","falign":"right';
              bscAlCenter:
              OptColumns := OptColumns + '","falign":"center';
              end; *)
            if (FWidth <> '') then
              OptColumn.Values['width'] := FWidth;

            if OptColumns = '' then
              OptColumns := '{' + OptColumn.DelimitedText + '}'
            else
              OptColumns := OptColumns + ',{' + OptColumn.DelimitedText + '}';
          end;

      end;

    OptTxt := TStringList.Create;
    try
      OptTxt.NameValueSeparator := ':';
      OptTxt.Delimiter          := ',';
      OptTxt.QuoteChar          := ' ';
      OptTxt.StrictDelimiter    := True;
      try

        OptTxt.Values['url'] := '"' + DWApplication.RegisterRestCallBack(Self,
          HTMLName + '.dataurl', DbJQGridCustomRestEvents0RestEvent) + '"';
      except

      end;
      OptTxt.Values['mtype'] := '"GET"';

      OptTxt.Values['datatype'] := '"jsonp"';
      // OptTxt.Values['data']       := 'mydata';
      OptTxt.Values['height']   := '200';
      OptTxt.Values['colModel'] := '[' + OptColumns + ']';
      OptTxt.Values['page']     := '1';

      OptTxt.Values['rowNum']           := IntToStr(FRowNum);
      OptTxt.Values['scrollPopUp']      := 'true';
      OptTxt.Values['scrollLeftOffset'] := '"83%"';
      OptTxt.Values['viewrecords']      := 'true';
      OptTxt.Values['scroll']           := '1';
      OptTxt.Values['scrollrows']       := 'true';
      OptTxt.Values['emptyrecords']     := '''Scroll to bottom to retrieve new page''';
      OptTxt.Values['pager']            := '"#jqGridPager"';
      OptTxt.Values['guiStyle']         := '"bootstrap"';
      OptTxt.Values['responsive']       := 'true';
      OptTxt.Values['width']            := '"auto"';
      OptTxt.Values['autowidth']        := 'true';
      OptTxt.Values['regional']         := '"pt-br"';
      if FGroupingField <> '' then
        begin
          OptTxt.Values['grouping']     := 'true';
          OptTxt.Values['groupingView'] := '{groupField: [''' + FGroupingField + '''],' +
            'groupColumnShow: [true],' + 'groupText: ["' + FGroupLabel + '<b>{0}</b>"],' +
            'groupOrder: ["asc"],' + 'groupSummary: [' + IfThen(FGroupSummary, 'true', 'false') +
            '],' + 'groupCollapse: ' + IfThen(FGroupCollapse, 'true', 'false') + ',' +
            'groupDataSorted : true }';

        end
      else
        OptTxt.Values['grouping'] := 'false';

      OptTxt.Values['onSelectRow']   := 'internalSelectRow';
      OptTxt.Values['beforeRequest'] := 'internalBeforeRequest';

      if LGridEditable then
        begin
          OptTxt.Values['editurl'] := '"' + DWApplication.RegisterRestCallBack(Self,
            HTMLName + '.editurl', DoEditRow) + '"';
        end;

      (* OptTxt.Values['viewrecords']          := 'true';
        OptTxt.Values['width']          := 'w';
        OptTxt.Values['responsive']          := 'true';
        OptTxt.Values['styleUI']          := '"Bootstrap"';
        OptTxt.Values['caption']          := '"Load jqGrid through Javascript Array"';
      *)
      (*
        OptTxt.Values['mobileResponsive'] := IfThen(FMobileResponsive, 'true', 'false');
        // OptTxt.Values['paginationVAlign'] := '"top"';
        if FSortColumn <> '' then
        OptTxt.Values['sortName']  := '"' + FSortColumn + '"';
        OptTxt.Values['sortOrder']   := IfThen(FSortOrder = bsgSortAsc, '"asc"', '"desc"');
        OptTxt.Values['showHeader']  := IfThen(FShowHeader, 'true', 'false');
        OptTxt.Values['showFooter']  := IfThen(FShowFooter, 'true', 'false');
        OptTxt.Values['showRefresh'] := IfThen(FShowRefresh, 'true', 'false');
        case FPaginationPosition of
        bsgPagTop:
        OptTxt.Values['paginationVAlign'] := '"top"';
        bsgPagBottom:
        OptTxt.Values['paginationVAlign'] := '"bottom"';
        bsgPagBoth:
        OptTxt.Values['paginationVAlign'] := '"both"';
        end;
        OptTxt.Values['clickToSelect'] := IfThen(FclickToSelect, 'true', 'false');
        OptTxt.Values['singleSelect']  := IfThen(FSingleSelect, 'true', 'false'); *)

      ScriptParams.Values['options'] := '{' + OptTxt.DelimitedText + '}';

    finally
      OptTxt.Free;
    end;
  finally
    OptColumn.Free;
  end;
end;

{ TIWBSJQGridColumn }

constructor TDWGridColumn.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FCSSclass  := '';
  FDataAlign := bscAlDefault;
  // FDataVertAlign := bscAlVerMiddle;
  FHeaderAlign := bscAlCenter;
  // FFooterAlign   := bscAlLeft;
  FWidth     := '';
  FSortable  := True;
  FResizable := True;
  FVisible   := True;
  FEditable  := True;
end;

destructor TDWGridColumn.Destroy;
begin

  inherited;
end;

function TDWGridColumn.IsCssClassStored: Boolean;
begin
  Result := FCSSclass <> '';
end;

function TDWGridColumn.IsWidthStored: Boolean;
begin
  Result := FWidth <> '';
end;

procedure TDWGridColumn.SetCSSclass(const Value: string);
begin
  if FCSSclass <> Value then
    begin
      FCSSclass := Value;
      // (GetOwner as TIWBSJQGrid).UpdateOptions;
    end;
end;

procedure TDWGridColumn.SetDataAlign(const Value: TBsColDataAlign);
begin
  if FDataAlign <> Value then
    begin
      FDataAlign := Value;
      // (GetOwner as TIWBSJQGrid).UpdateOptions;
    end;
end;

(* procedure TIWBSJQGridColumn.SetDataVertAlign(const Value: TBsColDataVertAlign);
  begin
  if FDataVertAlign <> Value then
  begin
  FDataVertAlign := Value;
  // (GetOwner as TIWBSJQGrid).UpdateOptions;
  end;
  end; *)

procedure TDWGridColumn.SetEditable(const Value: Boolean);
begin
  FEditable := Value;
end;

procedure TDWGridColumn.SetFieldName(const Value: string);
begin
  FFieldName := Value;
end;

(* procedure TIWBSJQGridColumn.SetFooterAlign(const Value: TBsColDataAlign);
  begin
  if FFooterAlign <> Value then
  begin
  FFooterAlign := Value;
  // (GetOwner as TIWBSJQGrid).UpdateOptions;
  end;
  end; *)

procedure TDWGridColumn.SetHeaderAlign(const Value: TBsColDataAlign);
begin
  if FHeaderAlign <> Value then
    begin
      FHeaderAlign := Value;
      // (GetOwner as TIWBSJQGrid).UpdateOptions;
    end;
end;

procedure TDWGridColumn.SetInputType(const Value: TbsJQGColInputType);
begin
  FInputType := Value;
end;

procedure TDWGridColumn.SetResizable(const Value: Boolean);
begin
  FResizable := Value;
end;

procedure TDWGridColumn.SetSortable(const Value: Boolean);
begin
  if FSortable <> Value then
    begin
      FSortable := Value;
      // (GetOwner as TIWBSJQGrid).UpdateOptions;
    end;
end;

procedure TDWGridColumn.SetTitle(const Value: string);
begin
  FTitle := Value;
end;

procedure TDWGridColumn.SetVisible(const Value: Boolean);
begin
  FVisible := Value;
end;

procedure TDWGridColumn.SetWidth(const Value: string);
begin
  if FWidth <> Value then
    begin
      FWidth := Value;
      // (GetOwner as TIWBSJQGrid).UpdateOptions;
    end;
end;

initialization

// Enable CSS and JS for JQGrid Plugin
if DebugHook <> 0 then
  begin
    IWBSAddGlobalLinkFile('/<dwlibpath>/jqgrid/ui.jqgrid.css');
    IWBSAddGlobalLinkFile('/<dwlibpath>/jqgrid/jquery.jqGrid.js');
    IWBSAddGlobalLinkFile('/<dwlibpath>/jqgrid/i18n/grid.locale-pt-br.js');
  end
else
  begin
    IWBSAddGlobalLinkFile('/<dwlibpath>/jqgrid/ui.jqgrid.min.css');
    IWBSAddGlobalLinkFile('/<dwlibpath>/jqgrid/jquery.jqGrid.min.js');
    IWBSAddGlobalLinkFile('/<dwlibpath>/jqgrid/i18n/grid.locale-pt-br.js');
  end;

(* if LGridEditable then   //Not necessary, include in ui.jqgrid.js;
  begin
  IWBSAddGlobalLinkFile('/<iwbspath>/jqgrid/grid.common.js');
  IWBSAddGlobalLinkFile('/<iwbspath>/jqgrid/grid.inlinedit.js');
  IWBSAddGlobalLinkFile('/<iwbspath>/jqgrid/jquery.fmatter.js')
  end; *)

end.
