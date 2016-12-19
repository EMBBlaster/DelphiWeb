unit DW.VCL.FIREDAC.Search;

interface

uses

  Classes, Winapi.Windows, VCL.Graphics, System.SysUtils, FireDAC.Comp.Client,
  VCL.Forms, FireDAC.Stan.Option, Data.DB, System.Math,DWElementTag,
  VCL.Controls, DW.VCL.Input, DW.VCL.Buttons, DW.VCL.CustomForm, DW.VCL.Table;

type
  ThackGraphic = class(TGraphicControl);

  (*TsMyListGrid = class(TIWDBGrid)
  private
    FLineHeight: Integer;
    procedure SetLineHeight(const Value: Integer);
  protected
    // procedure DrawColumnCell(const Rect: TRect; DataCol: integer; Column: TColumn; State: TGridDrawState); overload;
  public
    function RenderHTML(Acontext: TIWCompContext): TDWElementTag; override;
    property LineHeight: Integer read FLineHeight write SetLineHeight;
  end; *)

  TColunaItem = class(TCollectionItem)
  private
    FLargura: Integer;
    FCampo: string;
    FMoeda: Boolean;
    FLarguraAuto: Boolean;
    procedure SetCampo(const Value: string);
    procedure SetLargura(const Value: Integer);
    procedure SetMoeda(const Value: Boolean);
    procedure SetLarguraAuto(const Value: Boolean);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    property LarguraAuto: Boolean read FLarguraAuto write SetLarguraAuto default True;
    property Campo: string read FCampo write SetCampo;
    property Largura: Integer read FLargura write SetLargura;
    property Moeda: Boolean read FMoeda write SetMoeda;
  end;

  TColunas = class(TCollection)
  private
    FOwner: TComponent;
    function GetColumnItem(Index: Integer): TColunaItem;
    procedure SetColumnItem(Index: Integer; const Value: TColunaItem);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TComponent);
    function Add: TColunaItem;
    function Insert(Index: Integer): TColunaItem;
    property Items[Index: Integer]: TColunaItem read GetColumnItem write SetColumnItem;
  end;

  TCamposBuscaItem = class(TCollectionItem)
  private
    FUsarLike: Boolean;
    FCampo: string;
    procedure SetCampo(const Value: string);
    procedure SetUsarLike(const Value: Boolean);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Campo: string read FCampo write SetCampo;
    property UsarLike: Boolean read FUsarLike write SetUsarLike;
  end;

  TCamposBusca = class(TCollection)
  private
    FOwner: TComponent;
    function GetCamposBuscaItem(Index: Integer): TCamposBuscaItem;
    procedure SetCamposBuscaItem(Index: Integer; const Value: TCamposBuscaItem);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TComponent);
    function Add: TCamposBuscaItem;
    function Insert(Index: Integer): TCamposBuscaItem;
    property Items[Index: Integer]: TCamposBuscaItem read GetCamposBuscaItem
      write SetCamposBuscaItem;
  end;

  TDWFSearch = class(TDWInput)
  private
  //  FTopLabel: String;
    FFormatData: TFormatSettings;
   // FTopLabelCss: string;
 //   FDesignLabel: ThackGraphic;
    FZebrarLista: Boolean;
    FOnBtnClick: TNotifyEvent;
    FonSelectErro: TNotifyEvent;
    FAlturaLinhaLista: Integer;
    FonSelectOk: TNotifyEvent;
    FResultBusca: String;
    FCampoOrdenacao: string;
    FConnecion: TFDConnection;
    FAlturaLista: Integer;
    FCampoChave: string;
    FColunas: TColunas;
    FonKeyDown: TKeyEvent;
  //  FListtxt:TIWBSTable;
    FSelecionaAoClicar: Boolean;
    FKeyAtalho: Word;
    FCamposBusca: TCamposBusca;
    FTableOrigem: string;
    FQryBusca: TFDQuery;      // Qry que mostrará os resultados no dropdown
 //   FDsQryBusca: TDataSource; // dataset que mostrara os resultados no dropdown
    // FTimerBuscaLista:TIwSrpTimer; //Timer que atrasa a busca da lista quando em digitação
    FBtnAdd: TDWButton; // Botão personalizavel lateral direita
    FKey: Word;           // ----------v
    FShift: TShiftState;
    FDropDownVisible: Boolean;
    FSqlSearchParameters: string;
    // Usados para armazenar a key pressionada para a busca da lista atrazada pelo timer
  //  procedure SetTopLabel(const Value: string);
  //  procedure SetTopLabelCss(const Value: string);
    // function GetDrawColumnCell: TDrawColumnCellEvent;
    function GetResultBusca: TFDQuery;
    procedure SetAlturaLinhaLista(const Value: Integer);
    procedure SetAlturaLista(const Value: Integer);
    procedure SetCamposBusca(const Value: TCamposBusca);
    procedure SetColunas(const Value: TColunas);
    // procedure SetDrawColumnCell(const Value: TDrawColumnCellEvent);
    procedure SetSelecionaAoClicar(const Value: Boolean);
    procedure AguardaTerminarDigitacao(Sender: TObject);
    Procedure BuscaLista(Sender: TObject; var Key: Word; Shift: TShiftState);
    // busca a lista de resultados
    procedure BtnAddOnClick(Sender: TObject; aParams: TStringList);
    function FindParentForm: TDWCustomForm;
    procedure PosicionaLista;
    procedure AjustaLarguraColunasDbGrid(Grid: TDWTable);
    procedure VoltaParaFedittxt(Sender: TObject; aParams: TStringList);
   // procedure SelecionarDropdown(Sender: TObject; aParams: TStringList);
   // procedure SaiDaLista(Sender: TObject; aParams: TStringList);
    procedure OnClickLista(Sender: TObject; aParams: TStringList); // ao clicar no botão add
    procedure FakeKeyUp(Sender: TObject; aParams: TStringList);
  //  procedure SetTopLabel(const Value: string);
 //   procedure SetTopLabelCss(const Value: string);
    // to update Dropdown script table options when Component options are changed
    procedure UpdateDropOptions;
    procedure DoDropDownData(aParams: TStrings; var aReply:string);
    procedure DoOnAsyncSelect(aParams:TStringList);
    procedure SetDropDownVisible(const Value: Boolean);
    procedure SetSqlSearchParameters(const Value: string);
  protected
    procedure DoAsyncKeyUp(aParams: TStringList);  override;
    procedure InternalRenderScript(const AHTMLName: string; AScript: TStringList); override;
    procedure InternalRenderHTML(var AHTMLTag: TDWElementTag); override;
  public
    function RenderHTML: TDWElementTag; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
   // property TopLabel: string read FTopLabel write SetTopLabel;
   // property TopLabelCss: string read FTopLabelCss write SetTopLabelCss;
    Property TableNameOrigem: string read FTableOrigem write FTableOrigem;
    // Nome da tabela de origem no banco
    property Connection: TFDConnection read FConnecion Write FConnecion;
    // TzConnection do banco de dados
    property CamposBusca: TCamposBusca read FCamposBusca Write SetCamposBusca;
    // Campo da tabela de origem a ser buscado
    property CampoOrdenacao: string read FCampoOrdenacao write FCampoOrdenacao;
    property CampoChave: string read FCampoChave Write FCampoChave;
    // campo chave para localizar o resultado selecionado na TZquery  Resultados
    property KeyAtalho: Word read FKeyAtalho write FKeyAtalho;
    // Key de atalho para inserir registro
    property ResultBusca: String read FResultBusca write FResultBusca;
    property onSelectOk: TNotifyEvent read FonSelectOk write FonSelectOk;
    property onSelectErro: TNotifyEvent read FonSelectErro write FonSelectErro;
    property OnAsyncSelect;
    // property Text:TCaption read GetText write SetText;
    // property SelStart:Integer read GetSelStart write SetSelStart;
    // property SelLength:Integer read GetSelLength write SetSelLength;
    // property Constraints;
    // property Enabled;
    // property Visible;
    // property Align;
    property OnkeyDown: TKeyEvent read FonKeyDown write FonKeyDown;
    // property Images:TCustomImageList read GetImages write SetImages;
    // property ImageIndex:Integer read GetImageIndex write SetImageIndex;
    property OnBtnClick: TNotifyEvent read FOnBtnClick write FOnBtnClick;
    // property ParentShowHint;
    // property ShowHint;
    // property TabOrder;
    // property OnEnter;
    // property OnExit;
    // property SkinData : TsCtrlSkinData read GetSkinData write SetSkinData;
    // property SelText:string read GetSelText write SetSelText;
    property QryResultBusca: TFDQuery read GetResultBusca;
    // property ParentColor: Boolean read GetParentColor write SetParentColor;
    // property Focused : Boolean read GetFocused;
    // property CanFocus : Boolean read GetCanFocus;
    // property Color : TColor read GetColor write SetColor;
    property CamposLista: TColunas read FColunas write SetColunas;
    // property OnDrawColumnCell:TDrawColumnCellEvent read GetDrawColumnCell write SetDrawColumnCell;
    property ZebrarLista: Boolean read FZebrarLista write FZebrarLista;
  //  property GridLista: TIWBSTable read FListtxt write FListtxt;
    property AlturaLinhaLista: Integer read FAlturaLinhaLista write SetAlturaLinhaLista;
    property AlturaLista: Integer read FAlturaLista write SetAlturaLista default 265;
    property SelecionaAoClicar: Boolean read FSelecionaAoClicar write SetSelecionaAoClicar
      default False;
    property DropDownVisible:Boolean read FDropDownVisible write SetDropDownVisible;
    //parameters to add in where clause, sample: 'AGE = 25' or  'NAME = ''john'' and AGE = 21'
    property SqlSearchParameters:string read FSqlSearchParameters write SetSqlSearchParameters;
    property ScriptInsideTag default True;
  end;



implementation
    uses
      DWFiredacUtils, DWUtils, DW.VCL.Common, DWGlobal;



procedure TDWFSearch.FakeKeyUp(Sender: TObject; aParams: TStringList);
begin
  //
end;

function TDWFSearch.FindParentForm: TDWCustomForm;
var
  CompTest: TControl;
begin
  Result := nil;
  try
    CompTest := self;
    while Assigned(CompTest) and (not(CompTest is TDWCustomForm)) do
      CompTest := CompTest.Parent;
    if CompTest = nil then
      begin
        if TComponent(self).Owner is TFrame then
          begin
            CompTest := TControl(TComponent(self).Owner.Owner);
            while (not(CompTest is TDWCustomForm)) and (CompTest <> nil) do
              CompTest := TControl(TComponent(CompTest).Owner);
          end;
      end;
    if CompTest <> nil then
      if CompTest is TDWCustomForm then
        Result := CompTest as TDWCustomForm;
  except

  end;
end;

procedure TDWFSearch.AguardaTerminarDigitacao(Sender: TObject);
begin
  BuscaLista(self, FKey, FShift); // executa busca lista antes do evento da classe pai]
end;

procedure TDWFSearch.BtnAddOnClick(Sender: TObject; aParams: TStringList);
begin
  if Assigned(FOnBtnClick) then
    FOnBtnClick(self);
end;

procedure TDWFSearch.BuscaLista(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  CamposBuscaSql: string;
  i, L: Integer;
  SearchString: string;
begin
  // FTimerBuscaLista.Enabled:=False;
  // Modified by Cleomar 29/04/2016 14:48:47
  // se pressionado esc sai da caixa
  if Key = VK_ESCAPE then
    begin
      DropDownVisible:=False;
      Abort;
    end;
  // se pressionado o atalho KeyInserir, abre o Form FrmInserir
  if (Key = FKeyAtalho) then
    begin
      BtnAddOnClick(FBtnAdd, nil);
      Abort;
    end;
  // se pressionado seta para baixo
  if Key = VK_DOWN then
    begin
    //  if FListtxt.Visible then
        begin
     //     FListtxt.SetFocus;
          { ParentForm:= FindParentForm;
            if FindParentForm <> nil then
            ParentForm.ActiveControl:= FListtxt; }
          Abort;
        end;
    end;
  // somente para não executar a busca para seta para cima dentro do edit
  if Key = VK_UP then
    Abort;
  // busca por clientes de nome parecido com o digitado
 // FListtxt.Visible := False;
  if (FTableOrigem = '') then // verifica se foi especificaca a tabela
    raise Exception.Create('Não foi especificada a propriedade TableNameOrigem no componente ' +
      self.Name);
  // Se não fôr especificado o Campo Busca, adiciona
  if (FCamposBusca.Count = 0) and (FColunas.Count > 0) then // Se o CampoBusca estiver vazio...
    with FCamposBusca.Add do
      begin // será a primeira coluna.
        FCampo    := FColunas.Items[0].Campo;
        FUsarLike := True;
      end;
  CamposBuscaSql := ''; // campos buscados na sql
  // Se fôr especificado o campo Busca e não o campo Chave, o campo chave será o campo Busca
  if (FCampoChave = '') and (FCamposBusca.Count > 0) then
    FCampoChave := FCamposBusca.Items[0].Campo;
  if (FCampoChave = '') and (FCamposBusca.Count = 0) then
    raise Exception.Create
      ('Não foram especidficados CampoBusca, CampoChave ou CamposLista no componente ' + self.Name);
  // Adiciona as Colunas a Sql de busca
  for i := 0 to FColunas.Count - 1 do // adiciona as colunas a SQL
    begin
      if CamposBuscaSql <> '' then
        CamposBuscaSql := CamposBuscaSql + ',' + FColunas.Items[i].Campo
      else
        CamposBuscaSql := FColunas.Items[i].Campo;
    end;
  // Se não forem Setadas colunas, os CamposBusca serão as Colunas
  if CamposBuscaSql = '' then // se não foram setadas colunas...
    begin
      for i := 0 to FCamposBusca.Count - 1 do
        begin // adiciona os CamposBusca como colunas.
          if CamposBuscaSql <> '' then
            CamposBuscaSql := CamposBuscaSql + ', ' + FCamposBusca.Items[i].Campo
          else
            CamposBuscaSql := FCamposBusca.Items[i].Campo;
        end;
    end;
  // adiciona o Campo Chave no primeiro campo da SQL
  if FCampoChave <> '' then
    begin
      if (pos(',' + FCampoChave, CamposBuscaSql) = 0)
      and (pos(FCampoChave + ',', CamposBuscaSql) = 0) then
        begin
          if CamposBuscaSql <> '' then
            CamposBuscaSql := FCampoChave + ',' + CamposBuscaSql
          else
            CamposBuscaSql := FCampoChave
        end;
    end;
  // remove caracteres indesejados
  SearchString := self.Text;
  SearchString := ReplaceSpecialChars(SearchString);
  // efetua a busca
  FQryBusca.Connection        := FConnecion;
  FQryBusca.FetchOptions.Mode := TfdFetchMode.fmAll;
 // FDsQryBusca                 := TDataSource.Create(self);
//  FDsQryBusca.DataSet         := FQryBusca;
  FQryBusca.Close;
  FQryBusca.SQL.Clear;
  FQryBusca.SQL.Add('select ' + CamposBuscaSql + ' from ' + FTableOrigem);
  if FCamposBusca.Items[0].UsarLike then
    begin
      FQryBusca.SQL.Add('where ' + FCamposBusca.Items[0].Campo + ' like');
      FQryBusca.SQL.Add(QuotedStr('%' + SearchString + '%'));
    end
  else
    begin
      FQryBusca.SQL.Add('where ' + FCamposBusca.Items[0].Campo + ' like');
      FQryBusca.SQL.Add(QuotedStr(SearchString));
    end;
  if FCamposBusca.Count > 1 then
    begin
      for i := 1 to FCamposBusca.Count - 1 do
        begin
          if FCamposBusca.Items[i].UsarLike then
            begin
              FQryBusca.SQL.Add('or ' + FCamposBusca.Items[i].Campo + ' like');
              FQryBusca.SQL.Add(QuotedStr('%' + SearchString + '%'));
            end
          else
            begin
              FQryBusca.SQL.Add('or ' + FCamposBusca.Items[i].Campo + ' like');
              FQryBusca.SQL.Add(QuotedStr(SearchString));
            end;
        end;
    end;
  if Trim(FSqlSearchParameters) <> '' then
    FQryBusca.SQL.Add(FSqlSearchParameters);
  if FCampoOrdenacao = '' then
    FQryBusca.IndexFieldNames := FCamposBusca.Items[0].Campo
  else
    FQryBusca.IndexFieldNames := FCampoOrdenacao;
  // FQryBusca.SortType:= stAscending;
  FQryBusca.Open;
  if FQryBusca.RecordCount > 0 then
    begin
      // formata campos marcados como moeda
      for i := 0 to FColunas.Count - 1 do
        begin
          if FColunas.Items[i].Moeda then
            (FQryBusca.FieldByName(FColunas.Items[i].Campo) as TCurrencyField).currency := True;
        end;

      PosicionaLista;
      DropDownVisible:=True;
      DWApplication.CallBackResp.AddScriptToExecuteFirst('$("#'+HTMLName+'DROP").bootstrapTable(''refresh'');', False);

     { if Not FListtxt.Visible then
        FListtxt.Visible := True; }
      { ParentForm:= FindParentForm;
        if ParentForm <> nil then
        ParentForm.WebApplication.CallBackResponse.AddJavaScriptToExecuteAsCDATA('document.getElementById("REGIONBUSCAFRAMETFRAMECLIENTES").reload();'); }
      // Self.RepaintControl;
     // FListtxt.AsyncRefreshControl;
      // TiwControl(Self.Parent).RepaintControl;
      { if FAlturaLinhaLista > 0 then //altera a altura das linhas somente se    Implementar
        FListtxt.DefaultRowHeight := FAlturaLinhaLista; //especificada uma altura }
    end
  else
    begin
      DropDownVisible:=False;
    end;
end;

procedure TDWFSearch.PosicionaLista;
var
  Posicao: TWinControl;
  i: Integer;
  GridWidth: Integer;
begin
  try
    { if (Flisttxt.Parent.InheritsFrom(TCustomForm)) then
      begin }

    { end; }
  //  AjustaLarguraColunasDbGrid(FListtxt);
    { //enquanto o parent da lista não for o form
      while not (Flisttxt.Parent.InheritsFrom(TCustomForm)) do
      begin
      Flisttxt.Top:= Flisttxt.Top + Flisttxt.Parent.Top;  //adiciona a posição do parent a lista
      Flisttxt.Left:= Flisttxt.Left + Flisttxt.Parent.Left;
      Flisttxt.Parent:= Flisttxt.Parent.Parent; //define o parent da lista como o parent do parent atual da lista
      end; }

  (*  GridWidth := 0;
    for i     := 0 to FListtxt.Columns.Count - 1 do
      begin
        GridWidth := GridWidth + StrToIntDef(TIWDBGridColumn(FListtxt.Columns.Items[i]).Width, 0);
      end;
    // define a largura da lista de acordo com a largura da lista ou do componente
    // Flisttxt.Width:= TeavDbSearchListSimple(self).Width - TeavDbSearchListSimple(self).Margins.Right;

    if (FListtxt.Width <> GridWidth) and (GridWidth > (self.Width - self.Margins.Right)) then
      FListtxt.Width := GridWidth + 30
    else
      FListtxt.Width := self.Width - self.Margins.Right;
    // define a altura padrão;
    if FAlturaLista > 0 then
      FListtxt.Height := FAlturaLista
    else
      FListtxt.Height := 300; *)
  except

  end;
end;

procedure TDWFSearch.AjustaLarguraColunasDbGrid(Grid: TDWTable);
const
  DEFBORDER = 10;
var
  temp, n: Integer;
  lmax: array [0 .. 30] of Integer;
begin
  (* with Grid do    //implementar
    begin
    Canvas.Font := Font;
    for n := 0 to Columns.Count - 1 do
    //if columns[n].visible then
    lmax[n] := DEFBORDER;//Canvas.TextWidth(Fields[n].FieldName) + DEFBORDER;
    grid.DataSource.DataSet.First;
    while not grid.DataSource.DataSet.EOF do
    begin
    for n := 0 to Columns.Count - 1 do
    begin
    if Self.CamposLista.Items[n].FLarguraAuto then
    begin
    temp := Canvas.TextWidth(trim(Columns[n].Field.DisplayText)) + DEFBORDER;
    if temp > lmax[n] then lmax[n] := temp;
    end
    else
    lmax[n] := Self.CamposLista.Items[n].Largura;
    //end; { if }
    end; {for}
    grid.DataSource.DataSet.Next;
    end; { while }
    grid.DataSource.DataSet.First;
    for n := 0 to Columns.Count - 1 do
    if lmax[n] > 0 then
    Columns[n].Width := lmax[n];
    end; { With } *)
end; { SetGridColumnWidths }

constructor TDWFSearch.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited Create(AOwner);
  // Self.Parent:= (Aowner as TIWControl);
  self.AutoSize     := False;
  self.FZebrarLista := True;
  // Self.TabStop:=True;
  FAlturaLista       := 265;
  FSelecionaAoClicar := False;
  FSqlSearchParameters:= '';
  ScriptInsideTag:=True;

  FQryBusca := TFDQuery.Create(self);
  FQryBusca.FetchOptions.RecsMax:= 10;

  FColunas     := TColunas.Create(self);
  FCamposBusca := TCamposBusca.Create(self);

  FFormatData           := TFormatSettings.Create('en-US');


  (* FTimerBuscaLista:= TIwSrpTimer.Create;

    FTimerBuscaLista.Interval:= 500;
    FTimerBuscaLista.Enabled:=False;
    FTimerBuscaLista.OnTimer:= AguardaTerminarDigitacao; *)


  // cria o botão
  FBtnAdd := TDWButton.Create(self);
  // FBtnAdd.Parent:= TWinControl(Self);
  FBtnAdd.Align        := alRight;
  FBtnAdd.Width        := 21;
  FBtnAdd.OnAsyncClick := BtnAddOnClick;

  FDropDownVisible:=False;

end;

procedure TDWFSearch.DoAsyncKeyUp(aParams: TStringList);
var
  ParentFormRec: TDWCustomForm;
  KeyReceived: Word;
  ShiftReceived: TShiftState;
begin
  if Assigned(OnAsyncKeyUp) then
    inherited;
  (* //BuscaOnTabPress:=True; implementar
    ParentFormRec:= FindParentForm;
    if ParentFormRec <> nil then
    ParentFormRec.WebApplication.ShowMessage('DoOnAsyncKeyUp Params:' + aParams.Text); *)

  KeyReceived   := GetKeyFromParams(aParams);
  ShiftReceived := GetShiftStateFromParams(aParams);
  if (KeyReceived = VK_DOWN) or (KeyReceived = VK_UP) then
  // teclas que não são de busca não precisar aguardar
    begin
      BuscaLista(self, KeyReceived, ShiftReceived);
      // FTimerBuscaLista.Enabled:=False;
      if Assigned(FonKeyDown) then
        FonKeyDown(self, KeyReceived, ShiftReceived);
      Exit;
    end;
  FKey   := KeyReceived;
  FShift := ShiftReceived;
  // FListtxt.RepaintControl;
  BuscaLista(nil, KeyReceived, ShiftReceived);
  self.RepaintControl;
  // FTimerBuscaLista.Enabled:=False;
  // FTimerBuscaLista.Enabled:=True;
  if Assigned(FonKeyDown) then
    FonKeyDown(self, KeyReceived, ShiftReceived);
end;

procedure TDWFSearch.DoOnAsyncSelect(aParams: TStringList);
begin
   DropDownVisible:=False;
  if FResultBusca <> aParams.Values['keyvalue'] then
     FResultBusca := aParams.Values['keyvalue'];
  if Assigned(OnAsyncSelect) then
    OnAsyncSelect(Self, aParams);
end;

destructor TDWFSearch.Destroy;
begin
  // FQryBusca.Free;
  // Fedit.Free;
  // FQryBusca.Free;
  // Flisttxt.Free;
  // FBtnAdd.Free;
  // FCamposBusca.Free;
  // FColunas.Free;
  (* FTimerBuscaLista.WaitFor;
    FTimerBuscaLista.Terminate;
    FTimerBuscaLista.Free; *)

  inherited Destroy;
end;

procedure TDWFSearch.OnClickLista(Sender: TObject; aParams: TStringList);
var
  Enter: Word;
begin
  if FSelecionaAoClicar then
    begin
      if FQryBusca.RecordCount > 0 then
        begin
          Enter := VK_RETURN;
          //SelecionarDropdown(Sender, aParams);
        end;
    end;
end;


procedure TDWFSearch.VoltaParaFedittxt(Sender: TObject; aParams: TStringList);
var
  ParentForm: TDWCustomForm;
begin
  ParentForm := FindParentForm;
  if ParentForm <> nil then
    DWApplication.ShowMessage('VoltaParaFedittxt Params:' + aParams.Text);
  {
    if (key = VK_UP) and (FQryBusca.RecNo = 1) then
    begin                                                 Implementar
    Self.SetFocus;
    FListtxt.Visible:=False;
    end; }
end;

function TDWFSearch.GetResultBusca: TFDQuery;
begin
  Result := FQryBusca;
end;

procedure TDWFSearch.InternalRenderHTML(var AHTMLTag: TDWElementTag);
var
  DropTag:TDWElementTag;
  DivTag:TDWElementTag;
begin
  inherited;
  // Render Dropdown
   DropTag := TDWElementTag.CreateHTMLTag('table');
   DropTag.AddStringParam('id', HTMLName + 'DROP');
   if not FDropDownVisible then
     DropTag.AddStringParam('style', 'visibility:hidden; display:none;');

   if Caption <> '' then
     AHTMLTag.Contents.AddElemetAsObject(DropTag)
   else
    begin
      DivTag:= TDWElementTag.CreateHTMLTag('div');
      DivTag.Contents.AddElemetAsObject(AHTMLTag);
      DivTag.Contents.AddElemetAsObject(DropTag);
      AHTMLTag:= DivTag;
    end;
end;

procedure TDWFSearch.InternalRenderScript(const AHTMLName: string; AScript: TStringList);
begin
  inherited;
  UpdateDropOptions;
  AScript.Add('$("#{%htmlname%}DROP").bootstrapTable({%dropoptions%});');
  //to correct comp div height
  AScript.Add('$(".bootstrap-table:has(#' + HTMLName +'DROP)").css("display","'+'none'+'");');
  AScript.Add('$("#' + HTMLName +'_FG > .clearfix").css("display","'+'none'+'");');


  //To fire Server OnSelect Event
  AScript.Add('$("#{%htmlname%}DROP").bootstrapTable().off("click-row.bs.table")' +
             '.on("click-row.bs.table", function(e, row, $element){ executeAjaxEvent("&keyvalue="+ row[''' + FCampoChave +'''], null, "'+AHTMLName+'.DoOnAsyncSelect", true, null, true); });');

  DWApplication.RegisterCallBack(Self, AHTMLName+'.DoOnAsyncSelect', DoOnAsyncSelect);

end;

function TDWFSearch.RenderHTML: TDWElementTag;
var
  DropTag: TDWElementTag;
  //LabelStyle: string;
  ajaxcall: string;
begin
  ajaxcall := '';
  if Not Assigned(OnAsyncKeyUp) then
    begin
      (* ajaxcall := '<Script Language="JavaScript"> processAjaxEvent(''onKeyUp'', '
        + HTMLControlImplementation.IWCLName
        + ',''' + HTMLName + '.' + 'DoOnAsyncKeyUp' + ''','
        + 'true' + ', null, '
        + 'true' + ');'
        + 'function '+HTMLName+'KeyUp(ctrl){'
        + ' FindElem(''' + HTMLName + '_INPUT'').value = ' + HTMLName + 'obj.ItemIndex + "~" + ctrl.value;'#13
        + ajaxcall
        + '   }'#13 + '</script>' ;

        Acontext.WebApplication.RegisterCallBack(HTMLName+'.DoOnAsyncKeyUp', DoOnAsyncKeyUp); *)
      OnAsyncKeyUp := FakeKeyUp;
    end;

  // render original component
  Result := inherited RenderHTML;
  Result.AddStringParam('autocomplete', 'off');
  (* //Add Script KeuUp
    if Not Assigned(FOnAsyncKeyUp) then
    begin
    Result.Contents.AddText(ajaxcall);
    Result.AddStringParam('onkeyup',  HTMLName + 'KeyUp(this);');//Contents.AddText(ajaxcall);
    end; *)
  // Add CSS Classes(jQuery Compatible)
  if Css = '' then
    begin
      Result.AddClassParam('ui-widget');
      Result.AddClassParam('ui-widget-content');
      Result.AddClassParam('ui-corner-all');
    end;

end;

procedure TDWFSearch.SetAlturaLinhaLista(const Value: Integer);
begin
  if FAlturaLinhaLista <> Value then
    begin
      FAlturaLinhaLista := Value;
    end;
end;

procedure TDWFSearch.SetAlturaLista(const Value: Integer);
begin
  if FAlturaLista <> Value then
    FAlturaLista := Value;
end;

procedure TDWFSearch.SetCamposBusca(const Value: TCamposBusca);
begin
  FCamposBusca.Assign(Value);
end;

procedure TDWFSearch.SetColunas(const Value: TColunas);
begin
  FColunas.Assign(Value);
end;

procedure TDWFSearch.SetDropDownVisible(const Value: Boolean);
var
  DropCss:string;
begin
  if Value <> FDropDownVisible then
    begin
      TDWBSCommon.SetAsyncVisible(HTMLName + 'DROP', Value, FDropDownVisible);
      DropCss:= '{display:'+iif(Value,'""','"none"')+',position:"absolute",backgroundColor:"#f0f8ff",zIndex:"100" }';  // +',position:"absolute",z-index:"100",background-color:"aliceblue"

      DWApplication.CallBackResp.AddScriptToExecute('var styles =' + DropCss +';  $(".bootstrap-table:has(#' + HTMLName +'DROP)").css(styles);', False);
    end;
end;

procedure TDWFSearch.SetSelecionaAoClicar(const Value: Boolean);
begin
  FSelecionaAoClicar := Value;
end;



procedure TDWFSearch.SetSqlSearchParameters(const Value: string);
begin
  FSqlSearchParameters := Value;
end;

procedure TDWFSearch.UpdateDropOptions;
var
  OptColumns: string;
  OptTxt: TStrings;
  J: integer;
  KeyInclude:Boolean;
begin
  KeyInclude:=False;
  if FCampoChave = '' then
    raise Exception.Create('CampoChave precisa ser setado com o nome do campo chave primária');

  OptColumns := '[';
  for J      := 0 to FColunas.Count - 1 do
    begin
      with FColunas.Items[J] do
        begin
          if Campo = FCampoChave then
            KeyInclude:=True;
          if J > 0 then
            OptColumns := OptColumns + ',';
          OptColumns := OptColumns + '{"field":"' + Campo+ '","title":"' + Campo;
          (*
          if FCSSclass <> '' then
            OptColumns := OptColumns + '","class":"' + FCSSclass;
          case FDataAlign of
            bscAlLeft:
              OptColumns := OptColumns + '","align":"left';
            bscAlRight:
              OptColumns := OptColumns + '","align":"right';
            bscAlCenter:
              OptColumns := OptColumns + '","align":"center';
          end;
          case FDataVertAlign of
            bscAlVerTop:
              OptColumns := OptColumns + '","valign":"top';
            bscAlVerBottom:
              OptColumns := OptColumns + '","valign":"bottom';
            bscAlVerMiddle:
              OptColumns := OptColumns + '","valign":"middle';
          end;
          case FHeaderAlign of
            bscAlLeft:
              OptColumns := OptColumns + '","halign":"left';
            bscAlRight:
              OptColumns := OptColumns + '","halign":"right';
            bscAlCenter:
              OptColumns := OptColumns + '","halign":"center';
          end;
          case FFooterAlign of
            bscAlLeft:
              OptColumns := OptColumns + '","falign":"left';
            bscAlRight:
              OptColumns := OptColumns + '","falign":"right';
            bscAlCenter:
              OptColumns := OptColumns + '","falign":"center';
          end;  *)
          if (not FLarguraAuto) and (FLargura > 0) then
            OptColumns := OptColumns + '",width:' + IntToStr(FLargura);
          OptColumns := OptColumns + '",sortable:false';
         // OptColumns := OptColumns + IfThen(FSortable, '","sortable":"true', '","sortable":"false');
          OptColumns := OptColumns + '}';
        end;
    end;
  //if not exists Key Column, add as Invisible
  if Not KeyInclude then
    begin
      if OptColumns <> '[' then
        OptColumns:= OptColumns + ',';
      OptColumns := OptColumns + '{"field":"' + FCampoChave
                              + '","title":"' + FCampoChave
                              + '",sortable:false'
                              + ',visible:false}';
    end;
  //Close columns array
  OptColumns := OptColumns + ']';

  OptTxt := TStringList.Create;
  try
    OptTxt.NameValueSeparator := ':';
    OptTxt.Delimiter          := ',';
    OptTxt.QuoteChar          := ' ';
    OptTxt.StrictDelimiter    := True;

    OptTxt.Values['url']              := '"'+ DWApplication.RegisterRestCallBack(Self, HTMLName +'.dataurl', DoDropDownData) + '"';
    OptTxt.Values['columns']          := OptColumns;
    OptTxt.Values['pagination']       := 'false';
    OptTxt.Values['sidePagination']   := '"server"';
    OptTxt.Values['mobileResponsive'] := 'true';
     OptTxt.Values['paginationVAlign'] := '"top"';

   //OptTxt.Values['showHeader']  := 'false';
    OptTxt.Values['showFooter']  := 'false';
    OptTxt.Values['showRefresh'] := 'false';

    OptTxt.Values['clickToSelect'] := 'true';
    OptTxt.Values['singleSelect']  := 'true';

    ScriptParams.Values['dropoptions'] := '{' + OptTxt.DelimitedText + '}';



  finally
    OptTxt.Free;
  end;

end;

procedure TDWFSearch.DoDropDownData(aParams: TStrings; var aReply:string);
  function GetFieldForColumn(aColumnIndex: integer): TField;
  begin
    try
    Result := QryResultBusca.FieldByName(FColunas.Items[aColumnIndex].FCampo);
    except
      Result:=nil;
    end;
  end;

var
  data: string;
  line: string;
  bmrk: TBookmark;
  r, i, f, t: integer;
  KeyInclude:Boolean;
begin
  // here we return the data in json format
  // see format on: http://bootstrap-table.wenzhixin.net.cn/getting-started/#usage-via-javascript
  f := StrToIntDef(aParams.Values['offset'], 0);
  t := Min(f + StrToIntDef(aParams.Values['limit'], 10),
    QryResultBusca.RecordCount);

  QryResultBusca.DisableControls;
  bmrk := QryResultBusca.Bookmark;
  try
    KeyInclude:=False;
    data  := '';
    for r := f + 1 to t do
      begin
        QryResultBusca.RecNo := r;

        line  := '';
        for i := 0 to FColunas.Count - 1 do
          begin
            if FColunas.Items[i].FCampo = FCampoChave then
              KeyInclude:=True;
            if i > 0 then
              line := line + ',';
            if (GetFieldForColumn(i) is TNumericField) then
              line := line + '"' + GetFieldForColumn(i).FieldName + '":"' +
                FloatToStr(GetFieldForColumn(i).AsFloat, FFormatData) + '"'
            else if (GetFieldForColumn(i) is TStringField) or
              (GetFieldForColumn(i) is TMemoField) then
              line := line + '"' + GetFieldForColumn(i).FieldName + '":"' +
                EscapeJsonString(GetFieldForColumn(i).AsString) + '"'
            else if GetFieldForColumn(i) <> nil then
               line := line + '"' + GetFieldForColumn(i).FieldName + '":"' +
                GetFieldForColumn(i).AsString + '"'
            else
              line := line + '"' + GetFieldForColumn(i).FieldName + '":""';
          end;
        //if not exists Key Column, add as Invisible
        if Not KeyInclude then
          begin
            line := line + ',';
            line := line + '"' + QryResultBusca.FieldByName(FCampoChave).FieldName + '":"' +
              QryResultBusca.FieldByName(FCampoChave).AsString + '"';
          end;
         if data <> '' then
          data := data + ',';
        data   := data + '{' + line + '}';
      end;


    aReply:= '{"total": ' + IntToStr(QryResultBusca.RecordCount) + ', "rows": [' +
      data + ']}';
  finally
    QryResultBusca.GotoBookmark(bmrk);
    QryResultBusca.EnableControls;
  end;
end;

{ TColunaItem }

procedure TColunaItem.Assign(Source: TPersistent);
begin
  inherited;

end;

constructor TColunaItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FLarguraAuto := True;
  FLargura     := 30;
  FMoeda       := False;
end;

function TColunaItem.GetDisplayName: string;
begin
  if FCampo <> '' then
    Result := FCampo
  else
    Result := Format('Coluna %d', [Index]);
end;

procedure TColunaItem.SetCampo(const Value: string);
begin
  FCampo := Value;
end;

procedure TColunaItem.SetLargura(const Value: Integer);
begin
  FLargura := Value;
end;

procedure TColunaItem.SetLarguraAuto(const Value: Boolean);
begin
  FLarguraAuto := Value;
end;

procedure TColunaItem.SetMoeda(const Value: Boolean);
begin
  FMoeda := Value;
end;

{ TColunas }

function TColunas.Add: TColunaItem;
begin
  Result := TColunaItem(inherited Add);
end;

constructor TColunas.Create(AOwner: TComponent);
begin
  inherited Create(TColunaItem);
  FOwner := AOwner;
end;

function TColunas.GetColumnItem(Index: Integer): TColunaItem;
begin
  Result := TColunaItem(inherited Items[Index]);
end;

function TColunas.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TColunas.Insert(Index: Integer): TColunaItem;
begin
  Result := TColunaItem(inherited Insert(Index));
end;

procedure TColunas.SetColumnItem(Index: Integer; const Value: TColunaItem);
begin
  Items[Index].Assign(Value);
end;

{ TCamposBuscaItem }

procedure TCamposBuscaItem.Assign(Source: TPersistent);
begin
  inherited;

end;

constructor TCamposBuscaItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FUsarLike := False;
end;

function TCamposBuscaItem.GetDisplayName: string;
begin
  if FCampo <> '' then
    Result := FCampo
  else
    Result := Format('Coluna %d', [Index]);
end;

procedure TCamposBuscaItem.SetCampo(const Value: string);
begin
  FCampo := Value;
end;

procedure TCamposBuscaItem.SetUsarLike(const Value: Boolean);
begin
  FUsarLike := Value;
end;

{ TCamposBusca }

function TCamposBusca.Add: TCamposBuscaItem;
begin
  Result := TCamposBuscaItem(inherited Add);
end;

constructor TCamposBusca.Create(AOwner: TComponent);
begin
  inherited Create(TCamposBuscaItem);
  FOwner := AOwner;
end;

function TCamposBusca.GetCamposBuscaItem(Index: Integer): TCamposBuscaItem;
begin
  Result := TCamposBuscaItem(inherited Items[Index]);
end;

function TCamposBusca.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TCamposBusca.Insert(Index: Integer): TCamposBuscaItem;
begin
  Result := TCamposBuscaItem(inherited Insert(Index));
end;

procedure TCamposBusca.SetCamposBuscaItem(Index: Integer; const Value: TCamposBuscaItem);
begin
  Items[Index].Assign(Value);
end;

{ TsMyListGrid }
(*
function TsMyListGrid.RenderHTML(Acontext: TIWCompContext): TDWElementTag;
begin
  if Visible then
    Result := inherited RenderHTML(Acontext)
  else
    begin
      Result := TDWElementTag.CreateTag('div');
      Result.AddStringParam('style', 'position: absolute;' + 'left: 6px;' + 'top: 38px;' +
        'z-index: 10100;' + 'width: 374px;' + 'height: 300px;' + 'padding: 0;' +
        'font-weight: normal;' + 'font-style: normal;' + 'text-decoration: none;' +
        'font-size: 13px;' + 'border-style: inset;' + 'border-width: 2px;' + 'overflow-x: auto;' +
        'overflow-y: auto;' + 'padding: 0;' + 'visibility: hidden;');
      Result.AddStringParam('id', self.HTMLName);
    end;
end;

procedure TsMyListGrid.SetLineHeight(const Value: Integer);
begin
  { TODO 1 -oDELCIO -cMELHIRIAS : aDICIONAR ALTURA DA LINHA DA LISTA }

end; *)

initialization

// Enable CSS and JS for Table Plugin
if DebugHook <> 0 then
  begin
    IWBSAddGlobalLinkFile('/<dwlibpath>/bstable/bootstrap-table.css');
    IWBSAddGlobalLinkFile('/<dwlibpath>/bstable/bootstrap-table.js');
  end
else
  begin
    IWBSAddGlobalLinkFile('/<dwlibpath>/bstable/bootstrap-table.min.css');
    IWBSAddGlobalLinkFile('/<dwlibpath>/bstable/bootstrap-table.min.js');
  end;


end.
