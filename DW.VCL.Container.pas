unit DW.VCL.Container;

interface
   uses Classes, Controls, DWTypes;

   type

   //TDWContainer is the Base for DW Containers
   TDWContainer = class (TWinControl)

   protected

   published
    property OnAsyncLoad:TDWAsyncProcedure read FOnAsyncLoad write SetOnAsyncLoad;
    property OnAsyncUnLoad:TDWAsyncProcedure read FOnAsyncUnLoad write SetOnAsyncUnLoad;
   end;

  //Helper to access private Field  FTabList in TWinControl,
  //is used in TDWControl Class
  TWinControlHelper = class helper for TWinControl
    public
    function GetTabList:TList;
  end;

implementation

{ TDWContainer }



{ TWinControlHelper }

function TWinControlHelper.GetTabList: TList;
begin
  Result:= Self.FTabList;
end;

end.
