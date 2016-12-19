unit DW.CORE.Cache;

interface

uses
  Classes, System.SysUtils, DB, DWUrlHandlerBase, System.IOUtils;

type

  TCacheItemBase = class(TObject)
  protected
    // FMemoryCacheStream:TMemoryStream;
    FFilename: string;
    FOwner: TComponent;
    FContentType: string;
    // FFileCachePath:string;
  public
    constructor Create(aOwner: TComponent; aFileName: string; aContentType: string);
    destructor Destroy; override;
    function URL: string;
    function FileName: string;
  end;

  TCacheItemMemory = class(TCacheItemBase)
  protected
    FStream: TMemoryStream;
  public
    constructor Create(aOwner: TComponent; aStream: TStream; aFileName: string;
      aContentType: string);
    destructor Destroy; override;
  end;

  TCacheItemFile = class(TCacheItemBase)
  private
  protected
    FPath: string;
  public
    constructor Create(aOwner: TComponent; aStream: TStream; aFileName: string;
      aContentType: string);
    destructor Destroy; override;
    class function GetCacheFilePath(aOwner: TComponent; aFileName: string): string;
  end;

  TDWCacheList = class(TStringList)

  end;

  TDWUrlHandlerCache = class(TDWUrlHandlerBase)
  public
    procedure Execute; override;
  end;

  TDWCache = class
  public
    class function AddStreamToGlobalMemoryCache(aOwner: TComponent; aStream: TStream;
      aFileName: string; aContentType: string): string;
    class function AddStreamToSessionMemoryCache(aOwner: TComponent; aStream: TStream;
      aFileName: string; aContentType: string): string;
    class function AddStreamToSessionFileCache(aOwner: TComponent; aStream: TStream;
      aFileName: string; aContentType: string): string;
  end;

implementation

uses DWUtils, DW.CORE.DWApplication, DW.CORE.DWClientConnection, OverbyteIcsStreams;

type
  THack = class(TDWApplication);

  { TDWCache }

class function TDWCache.AddStreamToGlobalMemoryCache(aOwner: TComponent; aStream: TStream;
  aFileName: string; aContentType: string): string;
begin
  raise Exception.Create('Need to implement Global Cache. You can contribute?');
end;

class function TDWCache.AddStreamToSessionFileCache(aOwner: TComponent; aStream: TStream;
  aFileName, aContentType: string): string;
var
  LItem: TCacheItemFile;
  LUrl: string;
begin
  // Create the cache item Object
  LItem := TCacheItemFile.Create(aOwner, aStream, aFileName, aContentType);
  // Get url for get item
  LUrl := LItem.URL;
  // add item to DWApplication cache list
  THack(DWApplication).FCacheList.AddObject(LUrl, LItem);
  // Register get handler class for item url
  DWApplication.RegisterGetHandler(aOwner, LUrl, nil, TDWUrlHandlerCache);
  // return the url for get item
  Result := LUrl;
end;

class function TDWCache.AddStreamToSessionMemoryCache(aOwner: TComponent; aStream: TStream;
  aFileName: string; aContentType: string): string;
var
  LItem: TCacheItemMemory;
  LUrl: string;
begin
  // Create the cache item Object
  LItem := TCacheItemMemory.Create(aOwner, aStream, aFileName, aContentType);
  // Get url for get item
  LUrl := LItem.URL;
  // add item to DWApplication cache list
  THack(DWApplication).FCacheList.AddObject(LUrl, LItem);
  // Register get handler class for item url
  DWApplication.RegisterGetHandler(aOwner, LUrl, nil, TDWUrlHandlerCache);
  // return the url for get item
  Result := LUrl;
end;

{ TCacheItem }

constructor TCacheItemBase.Create(aOwner: TComponent; aFileName: string; aContentType: string);
begin
  FOwner       := aOwner;
  FContentType := aContentType;
  FFilename    := aFileName;
end;

destructor TCacheItemBase.Destroy;
begin
  inherited;
end;

function TCacheItemBase.FileName: string;
begin
  Result := FFilename;
end;

function TCacheItemBase.URL: string;
begin
  if FOwner <> nil then
    Result := FOwner.Name + FileName
  else
    Result := 'NIL.' + FileName;
end;

{ TDWUrlHandlerCache }

procedure TDWUrlHandlerCache.Execute;
var
  Status: string;
  LHeader: String;
  LPath: string;
  LCacheList: TDWCacheList;
  Lindex: Integer;
  LCacheItem: TCacheItemBase;
begin
  Status     := '';
  LPath      := TDWClientConnection(Client).Path;
  LCacheList := THack(DWApplication).FCacheList;
  if LPath <> '' then
    begin
      Lindex := LCacheList.IndexOf(LPath);
      if Lindex > -1 then
        begin
          LCacheItem := TCacheItemBase(LCacheList.GetObject(Lindex));
          LHeader    := 'Content-Disposition: attachment; filename="' + LCacheItem.FileName + '"';
          // if is in memory cache
          if LCacheItem is TCacheItemMemory then
            begin
              // load buffer from memory
              TDWClientConnection(Client).DocStream := TCacheItemMemory(LCacheItem).FStream;
              // send
              AnswerStream(Status, LCacheItem.FContentType, LHeader);
            end
            // else if is in file cache
          else if LCacheItem is TCacheItemFile then
            begin
              // load buffer from File
              TDWClientConnection(Client).DocStream :=
                TIcsBufferedFileStream.Create(TCacheItemFile(LCacheItem).FPath,
                fmOpenRead + fmShareDenyWrite, MAX_BUFSIZE);
              // send
              AnswerStream(Status, LCacheItem.FContentType, LHeader);
            end
        end;
    end;
  Finish;
end;

{ TCacheItemMemory }

constructor TCacheItemMemory.Create(aOwner: TComponent; aStream: TStream;
  aFileName, aContentType: string);
begin
  inherited Create(aOwner, aFileName, aContentType);
  FStream := TMemoryStream.Create;
  FStream.CopyFrom(aStream, 0);
end;

destructor TCacheItemMemory.Destroy;
begin
  FStream.Free;
  inherited;
end;

{ TCacheItemFile }

constructor TCacheItemFile.Create(aOwner: TComponent; aStream: TStream;
  aFileName, aContentType: string);
var
  LStream: TMemoryStream;
begin
  inherited Create(aOwner, aFileName, aContentType);
  LStream.Create;
  try
    FPath := GetCacheFilePath(aOwner, aFileName);
    LStream.CopyFrom(aStream, 0);
    LStream.SaveToFile(FPath);
  finally
    LStream.Free;
  end;
end;

destructor TCacheItemFile.Destroy;
begin

  inherited;
end;

class function TCacheItemFile.GetCacheFilePath(aOwner: TComponent; aFileName: string): string;
begin
  // if is global cache
  if (aOwner <> nil) and (aOwner = DWServer) then
    // path is in base cache dir
    Result := DWServer.DocDir + 'Cache\' + aFileName
    // else if is Session Cache with Component Owner
  else if (aOwner <> nil) then // path is in Session dir and owner dir
    Result := DWServer.DocDir + 'Cache\' + DWApplication.DWAppID + '\' + aOwner.Name + '\' +
      aFileName
    // else is Session Cache without component Owner
  else if aOwner = nil then
    Result := DWServer.DocDir + 'Cache\' + DWApplication.DWAppID + '\' + aFileName;
end;

end.
