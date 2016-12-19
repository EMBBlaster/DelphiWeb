unit uDWSessionData;

interface

uses
  DW.CORE.UserSession;
type
  TDWSessionData = class(TDWUserSession)
  private
    { private declarations }
  public
    { public declarations }
  end;

{ !!! Do Not Declare Global Variables !!! }

implementation

  uses
    DWGlobal;

{$R *.dfm}



initialization
  gUserSessionClass :=  TDWSessionData;

end.