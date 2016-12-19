unit DW.VCL.Image;

interface

uses Classes, SysUtils, VCL.Controls, StrUtils, System.IOUtils, Graphics, Db,
  DW.VCL.DBControl, DWElementTag;

type
  TDWImageOption  = (bsimResponsive, bsimCircle, bsimRounded, bsimThumbnail);
  TDWImageOptions = set of TDWImageOption;

  TDWImage = class(TDWCustomDbControl)
  private
    FActiveSrc: string;
    FOldSrc: string;

    FAltText: string;
    FAutoFormGroup: boolean;
    FEmbedBase64: boolean;
    FImageFile: string;
    FImageOptions: TDWImageOptions;
    FimageSrc: string;
    FMimeType: string;
    FPicture: TPicture;
    FUseSize: boolean;

    procedure SetImageFile(const AValue: string);
    procedure SetImageSrc(const AValue: string);
    function GetPicture: TPicture;
    procedure SetPicture(AValue: TPicture);
    procedure SetUseSize(const AValue: boolean);
  protected
    procedure CheckData; override;
    procedure PictureChanged(ASender: TObject);
    procedure InternalRenderAsync(const AHTMLName: string); override;
    procedure InternalRenderCss(var ACss: string); override;
    procedure InternalRenderHTML(var AHTMLTag: TDWElementTag); override;
    procedure InternalRenderStyle(AStyle: TStringList); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ActiveSrc: string read FActiveSrc;
    procedure Refresh;
    function GetFixedFilePath: string;
  published
    property AltText: string read FAltText write FAltText;
    property AutoFormGroup: boolean read FAutoFormGroup write FAutoFormGroup default False;
    property BSImageOptions: TDWImageOptions read FImageOptions write FImageOptions
      default [bsimResponsive];
    { TODO 1 -oDELCIO -cIMPLEMENT :  change EmbedBase64 default to false after implement cache }
    property EmbedBase64: boolean read FEmbedBase64 write FEmbedBase64 default True;
    property Enabled default True;
    property ImageFile: string read FImageFile write SetImageFile;
    property ImageSrc: string read FimageSrc write SetImageSrc;
    property MimeType: string read FMimeType write FMimeType;
    property Picture: TPicture read GetPicture write SetPicture;
    property UseSize: boolean read FUseSize write SetUseSize default False;
  end;

implementation

uses
  DWUtils, DW.VCL.CustomForm, OverbyteIcsMimeUtils, DW.CORE.Cache, DW.VCL.Common,
  DW.VCL.InputForm;

{$REGION 'TIWBSImage'}

constructor TDWImage.Create(AOwner: TComponent);
begin
  inherited;
  FAltText      := '';
  ImageFile     := '';
  FImageOptions := [bsimResponsive];
  ImageSrc      := '';
  FMimeType     := '';
  FPicture      := nil;
  FUseSize      := False;
  Width         := 89;
  Height        := 112;
  FEmbedBase64  := True;
end;

destructor TDWImage.Destroy;
begin
  FreeAndNil(FPicture);
  inherited;
end;

function TDWImage.GetPicture: TPicture;
begin
  if not Assigned(FPicture) then
    begin
      FPicture          := TPicture.Create;
      FPicture.OnChange := PictureChanged;
    end;
  Result := FPicture;
end;

procedure TDWImage.SetImageFile(const AValue: string);
begin
  FImageFile := AValue;
  FActiveSrc := '';
  Invalidate;
end;

procedure TDWImage.SetImageSrc(const AValue: string);
begin
  FimageSrc  := AValue;
  FActiveSrc := '';
  Invalidate;
end;

procedure TDWImage.SetPicture(AValue: TPicture);
begin
  Picture.Assign(AValue);
  FActiveSrc := '';
end;

procedure TDWImage.PictureChanged(ASender: TObject);
begin
  if not IsLoading then
    begin
      FActiveSrc := '';
      Invalidate;
    end;
end;

procedure TDWImage.SetUseSize(const AValue: boolean);
begin
  if FUseSize <> AValue then
    begin
      FUseSize := AValue;
      Invalidate;
    end;
end;

procedure TDWImage.Refresh;
begin
  FActiveSrc := '';
  Invalidate;
end;

function TDWImage.GetFixedFilePath: string;
begin
  if TPath.IsDriveRooted(FImageFile) then
    begin
      Result := TPath.Combine(DWServer.DocDir, FImageFile);
      if not FileExists(Result) then
        Result := FImageFile;
    end
  else
    Result := FImageFile;
end;

procedure TDWImage.CheckData;
var
  LField: TField;
  LMimeType: string;
  LFile: string;
  LStream: TStream;
  // LFileStream: TMemoryStream;
  // LParentForm: TDWCustomForm;
begin
  LFile := '';

  if FMimeType <> '' then
    LMimeType := FMimeType
  else
    LMimeType := 'image';

  // if there is field data we show it, if not we fallback to other sources
  if CheckDataSource(DataSource, DataField, LField) then
    begin
      FActiveSrc := '';
      if Assigned(FPicture) then
        FPicture.Graphic := nil;
      if (LField is TBlobField) and not LField.IsNull then
        begin
          LStream := DWGetFieldBlobStream(DataSource.DataSet, TBlobField(LField));
          try
            if FEmbedBase64 then
              { TODO 1 -oDELCIO -cIMPLEMENT :  !!!! Need to implement this }
              // FActiveSrc := 'data:image;base64, '+ DoFileEncBase64(LStream, False)
            else
              begin
                { TODO 1 -oDELCIO -cIMPLEMENT : Need to implement TIWBSImage.CheckData for non EmbedBase64 }
                // FActiveSrc := TDWCache.AddFileToCache(AContext.WebApplication, LFile, LMimeType);
                // FActiveSrc := TDWCache.AddStreamToSessionFileCache(Self, LStream,     AContext.WebApplication, LFile, LMimeType);
              end;
          finally
            LStream.Free;
          end;
        end;
    end;

  if FActiveSrc = '' then
    begin

      if Assigned(FPicture) and Assigned(FPicture.Graphic) and (not FPicture.Graphic.Empty) then
        begin
          if FEmbedBase64 then
            begin
              LStream := TMemoryStream.Create;
              try
                FPicture.Graphic.SaveToStream(LStream);
                LStream.Position := 0;
                { TODO 1 -oDELCIO -cIMPLEMENT :  !!!! Need to implement this }
                // FActiveSrc := 'data:image;base64, '+ DoFileEncBase64(LStream, False);
              finally
                LStream.Free;
              end;
            end
          else
            begin
              { TODO 1 -oDELCIO -cIMPLEMENT : Need to implement TIWBSImage.CheckData for non EmbedBase64 }
              // LFile := TIWAppCache.NewTempFileName;
              // FPicture.SaveToFile(LFile);
            end;
        end

      else if FImageFile <> '' then
        LFile := GetFixedFilePath

      else if FimageSrc <> '' then
        begin
          { TODO 1 -oDELCIO -cIMPLEMENT : Need to implement TIWBSImage.CheckData for ImageSrc }
          (* if AnsiStartsStr('//', FImageSrc) or AnsiContainsStr('://', FImageSrc) then
            FActiveSrc := FImageSrc
            else
            FActiveSrc := TURL.MakeValidFileUrl(AContext.WebApplication.AppUrlBase, FImageSrc); *)
        end;

      if LFile <> '' then
        begin
          { TODO 1 -oDELCIO -cIMPLEMENT : Need to implement TIWBSImage.CheckData for File }
          (*
            LParentForm := TIWForm.FindParentForm(Self);
            if LParentForm <> nil then
            FActiveSrc := TIWAppCache.AddFileToCache(LParentForm, LFile, LMimeType, ctForm)
            else
            FActiveSrc := TIWAppCache.AddFileToCache(AContext.WebApplication, LFile, LMimeType); *)
        end;
    end;
end;

procedure TDWImage.InternalRenderAsync(const AHTMLName: string);
begin
  inherited;
  if FActiveSrc <> FOldSrc then
    begin
      DWApplication.CallBackResp.AddScriptToExecuteFirst('$("#' + AHTMLName + '").attr("src","' +
        FActiveSrc + '");', False);
      FOldSrc := FActiveSrc;
    end;
end;

procedure TDWImage.InternalRenderCss(var ACss: string);
begin
  if bsimResponsive in FImageOptions then
    TDWBSCommon.AddCssClass(ACss, 'img-responsive');
  if bsimCircle in FImageOptions then
    TDWBSCommon.AddCssClass(ACss, 'img-circle');
  if bsimRounded in FImageOptions then
    TDWBSCommon.AddCssClass(ACss, 'img-rounded');
  if bsimThumbnail in FImageOptions then
    TDWBSCommon.AddCssClass(ACss, 'img-thumbnail');
  inherited;
end;

procedure TDWImage.InternalRenderHTML(var AHTMLTag: TDWElementTag);
begin
  inherited;
  FOldSrc := FActiveSrc;

  AHTMLTag := TDWElementTag.CreateHTMLTag('img');
  AHTMLTag.AddClassParam(ActiveCss);
  AHTMLTag.AddStringParam('id', HTMLName);
  AHTMLTag.AddStringParam('style', ActiveStyle);
  AHTMLTag.AddStringParam('src', FActiveSrc);
  if AltText <> '' then
    AHTMLTag.AddStringParam('alt', AltText, True)
  else
    AHTMLTag.AddStringParam('alt', FActiveSrc, True);
  if not AutoSize then
    begin
      AHTMLTag.AddIntegerParam('width', Width);
      AHTMLTag.AddIntegerParam('height', Height);
    end;
  { TODO 1 -oDELCIO -cIMPLEMENT : SetEnabled False for  TIWBSImage.InternalRenderHTML }
  // if not Enabled then
  // AContext.AddToInitProc('setEnabled("' + HTMLName + '", false);');

  if FAutoFormGroup and not(Parent is TDWInputGroup) then
    AHTMLTag := DWCreateInputFormGroup(Self, Parent, AHTMLTag, Caption, HTMLName);
end;

procedure TDWImage.InternalRenderStyle(AStyle: TStringList);
begin
  inherited;
  if Assigned(OnAsyncClick) and (Cursor = crDefault) then
    AStyle.Values['cursor'] := 'pointer';
end;
{$ENDREGION}

end.
