{ *******************************************************************************
  DW.CORE.DWApplication contains code from
  Project Overbyte ICS http://www.overbyte.be

  Delphi Web is
  Developped by Delcio Sbeghen @ SRP Sistemas
  delcio @ eavnet.com.br (remove spaces)
  under MIT Licence
}

unit DW.CORE.DWHandlers;

interface

uses Classes, DWUrlHandlerBase, DWTypes, OverbyteIcsHttpSrv;

type

  THttpAllowedElement = class
    Path: String;
    Flags: TDWHttpAllowedFlag;
  end;

  THttpDispatchElement = class
    Path: String;
    Flags: THttpGetFlag;
    Proc: Pointer;
    SObjClass: TDWUrlHandlerBase;
  end;

  TDWHttpHandlerList = class(TStringList)
  protected
    function GetDisp(NItem: Integer): THttpDispatchElement;
  public
    destructor Destroy; override;
    property Disp[NItem: Integer]: THttpDispatchElement read GetDisp;
  end;

  TDWHttpAllowedPath = class(TStringList)
  protected
    function GetElem(NItem: Integer): THttpAllowedElement;
  public
    destructor Destroy; override;
    property Elem[NItem: Integer]: THttpAllowedElement read GetElem;
  end;

implementation

{ TDWHttpHandlerList }

destructor TDWHttpHandlerList.Destroy;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    begin
      if Assigned(Objects[I]) then
        begin
          Objects[I].Free;
          Objects[I] := nil;
        end;
      Self.Delete(I);
    end;
  inherited Destroy;
end;

function TDWHttpHandlerList.GetDisp(NItem: Integer): THttpDispatchElement;
begin
  Result := Objects[NItem] as THttpDispatchElement;
end;

{ TDWHttpAllowedPath }

destructor TDWHttpAllowedPath.Destroy;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    begin
      if Assigned(Objects[I]) then
        begin
          Objects[I].Free;
          Objects[I] := nil;
        end;
      Self.Delete(I);
    end;
  inherited Destroy;
end;

function TDWHttpAllowedPath.GetElem(NItem: Integer): THttpAllowedElement;
begin
  Result := Objects[NItem] as THttpAllowedElement;
end;

end.
