unit DW.VCL.CustomForm;

interface
  uses Classes, Forms, DW.HTML.Page;
type

  TDWCustomForm = class(TForm)
   private
     FHTMLPage:TDWHTMLPage;
   public
     constructor Create(AOwner: TComponent); override;
     property HTMLPage:TDWHTMLPage read FHTMLPage;
  end;


implementation

{ TDWCustomForm }

constructor TDWCustomForm.Create(AOwner: TComponent);
begin
  inherited;
  FHTMLPage:= TDWHTMLPage.Create(Self);
end;

end.
