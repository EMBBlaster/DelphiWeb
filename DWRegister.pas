unit DWRegister;

interface

uses
  System.Classes;

procedure register;

implementation

uses DW.Server, DWUserSessionUnit, DW.VCL.Labels, DW.VCL.Edit;

procedure register;
begin
  RegisterComponents('DelphiWeb', [TDWServer]);
  RegisterComponents('DelphiWeb', [TDWUserSession]);
  RegisterComponents('DelphiWeb', [TDWLabel]);
  RegisterComponents('DelphiWeb', [TDWEdit]);
end;

end.
