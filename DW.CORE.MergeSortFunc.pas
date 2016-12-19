unit DW.CORE.MergeSortFunc;

interface

uses Classes, Controls;

function ControlRenderingSort(AItem1: Pointer; AItem2: Pointer): Integer;
function TabIndexSort(AItem1: Pointer; AItem2: Pointer): Integer;

implementation

uses DWGlobal, DWTypes, DW.VCL.TabControl;

function TabIndexSort(AItem1: Pointer; AItem2: Pointer): Integer;
var
  LTab1, LTab2: Integer;
begin
  if TComponent(AItem1) is TDWTabPage then
    begin
      LTab1 := TDWTabPage(AItem1).TabIndex;
    end
  else
    begin
      LTab1 := -1;
    end;
  if TComponent(AItem2) is TDWTabPage then
    begin
      LTab2 := TDWTabPage(AItem2).TabIndex;
    end
  else
    begin
      LTab2 := -1;
    end;
  Result := LTab1 - LTab2;
end;

function ControlRenderingSort(AItem1: Pointer; AItem2: Pointer): Integer;
var
  LTop1, LLeft1, LTop2, LLeft2, LIdx1, LIdx2: Integer;
begin
  if TComponent(AItem1) is TControl then
    begin
      LTop1  := TControl(AItem1).Top;
      LLeft1 := TControl(AItem1).Left;
      LIdx1  := TControl(AItem1).ComponentIndex;
    end
  else
    begin
      LTop1  := -1;
      LLeft1 := -1;
      LIdx1  := -1;
    end;
  if TComponent(AItem2) is TControl then
    begin
      LTop2  := TControl(AItem2).Top;
      LLeft2 := TControl(AItem2).Left;
      LIdx2  := TControl(AItem2).ComponentIndex;
    end
  else
    begin
      LTop2  := -1;
      LLeft2 := -1;
      LIdx2  := -1;
    end;

  if gIWBSRenderingSortMethod = bsrmSortYX then
    begin
      Result := LTop1 - LTop2;
      if Abs(Result) < gIWBSRenderingGridPrecision then
        Result := LLeft1 - LLeft2;
    end
  else
    begin
      Result := LLeft1 - LLeft2;
      if Abs(Result) < gIWBSRenderingGridPrecision then
        Result := LTop1 - LTop2;
    end;

  if Result = 0 then
    Result := LIdx1 - LIdx2;
end;

end.
