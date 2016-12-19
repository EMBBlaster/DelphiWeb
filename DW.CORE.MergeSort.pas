unit DW.CORE.MergeSort;
//try to implement this:
//https://rosettacode.org/wiki/Sorting_algorithms/Merge_sort#Pascal

{******************************************************************************

Merge Sort for Delphi Lists

Filename..........: uMergeSort.pas
Version...........: 1.1
Author............: Alexandre C. Machado
Target compilers..: Delphi 2009 to XE7. Should probably work with older versions and newer Delphi versions as well
Date..............: Feb-02-2015
Description.......: Merge Sort for Delphi TList and descendants.
                    http://alexandrecmachado.blogspot.com.br/2015/02/merge-sort-for-delphi.html
                    Adapted from Julian Bucknall's algorithm published in "The Tomes of Delphi Algorithms and Data Structures" book.
                    IMPORTANT NOTE:
                    ***** The original algorithm in the book uses an "improved" insertion sort, but this algorithm breaks the merge sort stability.
                    ***** We are using a standard insertion sort instead.
                    ***** Thanks to dangph for pointing this and writing a test case

                    Merge Sort algorithm is STABLE and this implementation performs even better than
                    standard TList quick sort algorithm (~10% faster in most cases).
					          Also implements replacements for TList and TObjectList, using this algorithm for sorting,
					          instead of standard quick sort.
Licensing stuff...: You may use this software in any kind of development,
                    including commercial, redistribute and modify it freely.
                    This software is provided as it is, without any kind of
                    warranty given. Use it at Your own risk.

                    Original test case unit written by dangph
******************************************************************************}


interface

uses
  SysUtils, Classes, Contnrs;

type
  TListEx = class(TList)
  public
    procedure Sort(Compare: TListSortCompare);
    procedure MergeSort(Compare: TListSortCompare);
    procedure QuickSort(Compare: TListSortCompare);
  end;

  TObjectListEx = class(TObjectList)
  public
    procedure Sort(Compare: TListSortCompare);
    procedure MergeSort(Compare: TListSortCompare);
    procedure QuickSort(Compare: TListSortCompare);
  end;

// this function may be used to merge sort any TList descendant, including TObjectList
procedure MergeSortList(List: TList; CompareFunc: TListSortCompare);

implementation

type
  TPointerArray = array of Pointer;
  PPointerArray = ^TPointerArray;

procedure DoInsertionSort(ptrList: PPointerList; FirstIndex: Integer; LastIndex: Integer;
  CompareFunc: TListSortCompare);
var
  i, j: Integer;
  Temp: Pointer;
begin
  for i := succ(FirstIndex) to LastIndex do begin
    Temp := ptrList^[i];
    j := i;
    while (j > FirstIndex) and (CompareFunc(Temp, ptrList^[j-1]) < 0) do begin
      ptrList^[j] := ptrList^[j-1];
      dec(j);
    end;
    ptrList^[j] := Temp;
  end;
end;

procedure DoMergeSort(ptrList: PPointerList; FirstIndex: Integer; LastIndex: Integer;
  CompareFunc: TListSortCompare; TempList: PPointerArray);
const
  // When the list is smaller than this we use InsertionSort instead of calling MergeSort recursively.
  // 8 and 64 seem to be the lower and upper limits where the performance degrades, so
  // something between 16 and 32 probably gives the best performance
  MIN_LIST_SIZE = 16;
var
  Mid: Integer;
  i, j: Integer;
  ToInx: Integer;
  FirstCount: Integer;
begin
  // calculate the midpoint
  Mid := (FirstIndex + LastIndex) div 2;
  // sort the 1st half of the list, either with merge sort, or, if there are few enough items, with insertion sort
  if (FirstIndex < Mid) then begin
    if (Mid - FirstIndex) <= MIN_LIST_SIZE then begin
      DoInsertionSort(ptrList, FirstIndex, Mid, CompareFunc)
    end else begin
      DoMergeSort(ptrList, FirstIndex, Mid, CompareFunc, TempList);
    end;
  end;
  // sort the 2nd half of the list likewise
  if (succ(Mid) < LastIndex) then begin
    if (LastIndex - succ(Mid)) <= MIN_LIST_SIZE then begin
      DoInsertionSort(ptrList, succ(Mid), LastIndex, CompareFunc);
    end else begin
      DoMergeSort(ptrList, succ(Mid), LastIndex, CompareFunc, TempList);
    end;
  end;
  // copy the first half of the list to our temporary list
  FirstCount := succ(Mid - FirstIndex);
  System.Move(ptrList^[FirstIndex], TempList^[0], FirstCount * SizeOf(Pointer));
  // set up the indexes: i is the index for the temporary list (i.e., the
  //  first half of the list), j is the index for the second half of the
  //  list, ToInx is the index in the merged where items will be copied
  i := 0;
  j := succ(Mid);
  ToInx := FirstIndex;
  // now merge the two lists
  // repeat until one of the lists empties...
  while (i < FirstCount) and (j <= LastIndex) do begin
     // calculate the smaller item from the next items in both lists and copy it over; increment the relevant index
    if (CompareFunc(TempList^[i], ptrList^[j]) <= 0) then begin
      ptrList^[ToInx] := TempList^[i];
      inc(i);
    end else begin
      ptrList^[ToInx] := ptrList^[j];
      inc(j);
    end;
    // there's one more item in the merged list
    inc(ToInx);
  end;
  // if there are any more items in the first list, copy them back over
  if (i < FirstCount) then begin
    System.Move(TempList^[i], ptrList^[ToInx], (FirstCount - i) * SizeOf(Pointer));
  end;
  // if there are any more items in the second list then they're already in place and we're done; if there aren't, we're still done
end;

// Delphi XE2 and up declare TList's FList field as TPointerList. XE1 and below declare as PPointerList
{$IF CompilerVersion > 22}
  {$DEFINE HAS_TPOINTERLIST}
{$IFEND}

procedure MergeSortList(List: TList; CompareFunc: TListSortCompare);
var
  TempList: TPointerArray;
  xTempListSize: Integer;
  aFirst, aLast: Integer;
begin
  if (List = nil) or (List.Count < 2) then begin
    Exit;
  end;
  aFirst := 0;
  aLast := List.Count - 1;
  xTempListSize := (List.Count div 2) + 1;
  SetLength(TempList, xTempListSize);
  DoMergeSort({$IFDEF HAS_TPOINTERLIST}@{$ENDIF}List.List, aFirst, aLast, CompareFunc, @TempList);
end;

{ TListEx }

procedure TListEx.MergeSort(Compare: TListSortCompare);
begin
  MergeSortList(Self, Compare);
end;

procedure TListEx.QuickSort(Compare: TListSortCompare);
begin
  inherited Sort(Compare);
end;

procedure TListEx.Sort(Compare: TListSortCompare);
begin
  MergeSort(Compare);   // Differently from TList, the default Sort() method uses the MergeSort algorightm
end;

{ TObjectListEx }

procedure TObjectListEx.MergeSort(Compare: TListSortCompare);
begin
  MergeSortList(Self, Compare);
end;

procedure TObjectListEx.QuickSort(Compare: TListSortCompare);
begin
  inherited Sort(Compare);
end;

procedure TObjectListEx.Sort(Compare: TListSortCompare);
begin
  MergeSort(Compare);   // Differently from TList, the default Sort() method uses the MergeSort algorightm
end;

end.

