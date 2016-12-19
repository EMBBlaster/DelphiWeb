{ DelphiWeb is an VCL internet Framework for  Delphi
| https://github.com/DrHank/DelphiWeb
| Developped by Delcio Sbeghen @SRP Sistemas
| Under MIT Licence: https://opensource.org/licenses/MIT
|
| Credits:
|   * The Server Core is based in OverByte ICS: http://www.overbyte.be/
|   * The Json Library is based in JsonDataObjects:  https://github.com/ahausladen/JsonDataObjects
|and changes by IWBootstrap Framework
|   * The DW VCL is based on Bootstrap Framework: https://github.com/kattunga/IWBootstrapFramework
}

unit uWebMainForm;

interface

uses
  DWForm, System.Classes, Vcl.Controls, DW.VCL.Control, DW.VCL.DBControl,
  DW.VCL.Labels;
type
  TWebMainForm = class(TDWForm)
    DWLabel1: TDWLabel;
  private
    { private declarations }
  public
    { public declarations }
  end;

{ !!! Do Not Declare Global Variables !!! }

implementation

{$R *.dfm}

end.