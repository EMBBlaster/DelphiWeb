unit DWUtils;

interface

uses Forms, Controls, DW.Server, System.SysUtils, System.StrUtils, DWUserSessionUnit,
DW.VCL.CustomForm;

function DWServer: TDWServer;
function MakeValidFileUrl(const ARootUrl: String; const AFileUrl: String):string;



implementation

function DWServer: TDWServer;
begin
  Result:= TDWServer.GetDWServer;
end;


function MakeValidFileUrl(const ARootUrl: String; const AFileUrl: String):string;
var
  RootUrl:string;
  FileUrl:string;
begin
  if ARootUrl <> '' then
    RootUrl:= ARootUrl
  else
    RootUrl:= DWServer.UrlBase;
  FileUrl:= ExtractRelativePath(DWServer.DocDir + '\', AFileUrl);
  FileUrl:= StringReplace(FileUrl, '\', '/', [rfReplaceAll]);
  Result:=  {RootUrl + }'/' + FileUrl;
end;


function DWFindParentForm(aControl:TControl): TDWCustomForm;
var
  CompTest: TControl;
begin
  Result := nil;
  try
    CompTest := aControl;
    while Assigned(CompTest) and (not(CompTest.InheritsFrom(TDWCustomForm))) do
      CompTest := CompTest.Parent;
    if CompTest = nil then
      begin
        if TControl(aControl).Owner is TFrame then
          begin
            CompTest := TControl(aControl.Owner.Owner);
            while (not(CompTest.InheritsFrom(TDWCustomForm))) and (CompTest <> nil) do
              CompTest := TControl(CompTest.Owner);
          end;
      end;
    if CompTest <> nil then
      if CompTest.InheritsFrom(TDWCustomForm) then
        Result := CompTest as TDWCustomForm;
  except

  end;
end;

end.
