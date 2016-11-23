unit DW.VCL.Container;

interface
   uses Classes, Controls, DWTypes;

   type

   //TDWContainer is the Base for DW Containers
   TDWContainer = class (TWinControl)
   private
     FOnAsyncLoad:TDWAsyncProcedure;
     FOnAsyncUnLoad:TDWAsyncProcedure;
    procedure SetOnAsyncLoad(const Value: TDWAsyncProcedure);
    procedure SetOnAsyncUnLoad(const Value: TDWAsyncProcedure);
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



{ TDWContainer }

procedure TDWContainer.SetOnAsyncLoad(const Value: TDWAsyncProcedure);
begin
  FOnAsyncLoad := Value;
end;

procedure TDWContainer.SetOnAsyncUnLoad(const Value: TDWAsyncProcedure);
begin
  FOnAsyncUnLoad := Value;
end;

{ TWinControlHelper }

function TWinControlHelper.GetTabList: TList;
begin
  Result:= Self.FTabList;
end;

end.
