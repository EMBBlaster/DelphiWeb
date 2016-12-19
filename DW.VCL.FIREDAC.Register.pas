unit DW.VCL.FIREDAC.Register;

interface
  uses Classes;

  procedure Register;


implementation
  uses
    DW.VCL.FIREDAC.Search;


procedure Register;
begin
  RegisterComponents('DelphiWeb Firedac', [TDWFSearch]);
end;


end.
