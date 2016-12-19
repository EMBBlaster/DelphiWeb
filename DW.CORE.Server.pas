unit DW.CORE.Server;

interface

uses Classes, windows, Forms, System.SysUtils, OverbyteIcsHttpAppServer, DW.CORE.DWHttpServer,
  DWForm, System.SyncObjs, Vcl.StdCtrls, DW.CORE.UserSession, JclDebug;

type
  (* TDWServerClient = class(THttpAppSrvConnection)
    private
    FResponseHeader: string;
    procedure SetResponseHeader(const Value: string);

    protected

    public
    CStartTick: longword ;
    CLastRead: int64 ;
    CLastWrite: int64 ;
    constructor Create(AOwner: TComponent); override;
    //header of response
    property ReplyHeader:string read FResponseHeader write SetResponseHeader;
    end; *)

  TDatamoduleClass = class of TDataModule;

  TDWServer = class(TComponent)
  private
    FHttpSrv: TDWHttpServer;
    FMainForm: TDWFormClass;
    FLibDir: string;
    FUrlBase: string;
    FRefreshCacheParam: string;
    FCookieParam: string;
    FComInitilization: Boolean;
    FCriticalLog: TCriticalSection;
    FLogMemo: TMemo;
    FAllowedPaths: TStrings;
    FOnNewSession: TNotifyEvent;
    FOnDWAppTerminate: TNotifyEvent;
    FOnSessionClose: TNotifyEvent;
    procedure SetDocDir(const Value: string);
    procedure SetPort(const Value: String);
    procedure SetTemplateDir(const Value: string);
    function GetDocDir: string;
    function GetPort: String;
    function GetTemplateDir: string;
    procedure SetMainForm(const Value: TDWFormClass);
    procedure SetLibDir(const Value: string);
    procedure SetUrlBase(const Value: string);
    function GetUrlBase: string;
    procedure HttpAppSrvBeforeProcessRequest(Sender, Client: TObject);
    procedure SetCookieParam(const Value: string);
    procedure SetComInitilization(const Value: Boolean);
    procedure SetLogMemo(const Value: TMemo);
    procedure CreateLogfile;
    // Getting the current filename for the logfile
    function GetCurrentLogName: string;
    procedure WriteToLogFile(aLogMessage: String);
    function PrepareLogText(aMsg: string): string;
    procedure DoThreadSyncException(Thread: TJclDebugThread);
    procedure DoThreadRegistered(ThreadID: DWORD);
    procedure DoThreadUnregistered(ThreadID: DWORD);
    procedure SetAllowedPaths(const Value: TStrings);
    procedure SetOnNewSession(const Value: TNotifyEvent);
    procedure SetOnDWAppTerminate(const Value: TNotifyEvent);
    procedure SetOnSessionClose(const Value: TNotifyEvent);
  protected
    procedure ProcessException(e: Exception);
    procedure DoNewSession(Sender: TObject);
    procedure DoSessionClose(Sender: TObject);
    procedure DoDWAppTerminate(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    function AppPath: string;
    class function GetDWServer: TDWServer;
  published
    property Port: String read GetPort write SetPort;
    property DocDir: string read GetDocDir write SetDocDir;
    property TemplateDir: string read GetTemplateDir write SetTemplateDir;
    property LibDir: string read FLibDir write SetLibDir;
    property MainForm: TDWFormClass read FMainForm write SetMainForm;
    property UrlBase: string read GetUrlBase write SetUrlBase;
    property RefreshCacheParam: string read FRefreshCacheParam;
    // class used to handle data of session need to be an TDWSession descendant
    property HttpServer: TDWHttpServer read FHttpSrv;
    property CookieParam: string read FCookieParam write SetCookieParam;
    property ComInitilization: Boolean read FComInitilization write SetComInitilization
      default False;
    property LogMemo: TMemo read FLogMemo write SetLogMemo;
    property AllowedPaths: TStrings read FAllowedPaths write SetAllowedPaths;
    property OnNewSession: TNotifyEvent read FOnNewSession write SetOnNewSession;
    property OnSessionClose: TNotifyEvent read FOnSessionClose write SetOnSessionClose;
    property OnDWAppTerminate: TNotifyEvent read FOnDWAppTerminate write SetOnDWAppTerminate;
  end;

implementation

uses DWUrlHandlerForms, System.IOUtils;

var
  gDWServer: TDWServer;

const
  BreakingLine = '//----------------------------------------------------------------------------//';

  { TDWServer }

function TDWServer.AppPath: string;
begin
  Result := ExtractFilePath(Application.ExeName);
end;

constructor TDWServer.Create(AOwner: TComponent);
begin
  inherited;
  FCriticalLog := TCriticalSection.Create;
  with JclDebugThreadList do
    begin
      OnSyncException      := DoThreadSyncException;
      OnThreadRegistered   := DoThreadRegistered;
      OnThreadUnregistered := DoThreadUnregistered;
    end;
  FHttpSrv := TDWHttpServer.Create(Self);
  with FHttpSrv do
    begin
      // ClientClass:= TDWServerClient;
      Port        := '80';
      DocDir      := AppPath + 'wwwroot\';
      TemplateDir := AppPath + 'wwwroot\templates\';
      ForceDirectories(AppPath + 'wwwroot');
      ForceDirectories(AppPath + 'wwwroot\templates');
      OnBeforeProcessRequest := HttpAppSrvBeforeProcessRequest;
    end;
  gDWServer         := Self;
  FLibDir           := AppPath + 'wwwroot\dwlib\';
  FUrlBase          := '';
  FComInitilization := False;
  FAllowedPaths     := TStringList.Create;
end;

destructor TDWServer.Destroy;
begin
  FHttpSrv.Stop;
  FHttpSrv.Free;
  FCriticalLog.Free;
  FAllowedPaths.Free;
  inherited;
end;

class function TDWServer.GetDWServer: TDWServer;
begin
  Result := gDWServer;
end;

function TDWServer.GetCurrentLogName: string;
begin
  Result := AppPath + TPath.GetFileNameWithoutExtension(Application.ExeName) +
    FormatDateTime('_yyyy_mm_dd', now) + '.log';
end;

function TDWServer.GetDocDir: string;
begin
  Result := FHttpSrv.DocDir;
end;

function TDWServer.GetPort: String;
begin
  Result := FHttpSrv.Port;
end;

function TDWServer.GetTemplateDir: string;
begin
  Result := FHttpSrv.TemplateDir;
end;

function TDWServer.GetUrlBase: string;
begin
  if FUrlBase <> '' then
    Result := FUrlBase
  else
    begin
      if FHttpSrv.Addr <> '' then
        begin
          Result := 'http://' + FHttpSrv.Addr + ':' + FHttpSrv.Port;
        end;
    end;
end;

procedure TDWServer.SetAllowedPaths(const Value: TStrings);
begin
  FAllowedPaths.Assign(Value);
end;

procedure TDWServer.SetComInitilization(const Value: Boolean);
begin
  FComInitilization := Value;
end;

procedure TDWServer.SetCookieParam(const Value: string);
begin
  if (FCookieParam <> Value) then
    begin
      FCookieParam := Value;
      FHttpSrv.SetDWAppCookieParam(Value);
    end;
end;

procedure TDWServer.SetDocDir(const Value: string);
begin
  if FHttpSrv.DocDir <> Value then
    FHttpSrv.DocDir := Value;
end;

procedure TDWServer.SetLibDir(const Value: string);
begin
  FLibDir := Value;
end;

procedure TDWServer.SetLogMemo(const Value: TMemo);
begin
  if FLogMemo <> Value then
    FLogMemo := Value;
  // FLogMemo.Assign(Value);
end;

procedure TDWServer.SetMainForm(const Value: TDWFormClass);
begin
  FMainForm := Value;
end;

procedure TDWServer.SetOnDWAppTerminate(const Value: TNotifyEvent);
begin
  FOnDWAppTerminate := Value;
end;

procedure TDWServer.SetOnNewSession(const Value: TNotifyEvent);
begin
  FOnNewSession := Value;
end;

procedure TDWServer.SetOnSessionClose(const Value: TNotifyEvent);
begin
  FOnSessionClose := Value;
end;

procedure TDWServer.SetPort(const Value: String);
begin
  if FHttpSrv.Port <> Value then
    FHttpSrv.Port := Value;
end;

procedure TDWServer.SetTemplateDir(const Value: string);
begin
  if FHttpSrv.TemplateDir <> Value then
    FHttpSrv.TemplateDir := Value;
end;

procedure TDWServer.SetUrlBase(const Value: string);
begin
  FUrlBase := Value;
end;

{ procedure TDWServer.SetUserDataModule(const Value: TDatamoduleClass);
  begin
  FUserDataModule := Value;
  end; }

procedure TDWServer.Start;
begin
  FRefreshCacheParam := FormatDateTime('yyyymmddhhnnsszzz', now);
  FHttpSrv.Start;
end;

procedure TDWServer.Stop;
begin
  FHttpSrv.Stop;
end;

procedure TDWServer.HttpAppSrvBeforeProcessRequest(Sender, Client: TObject);
// var
// RemoteClient: TDWServerClient;
begin
  // RemoteClient := TDWServerClient(Client) ;
  // RemoteClient.CStartTick := IcsGetTickCountX;
  // RemoteClient.CLastWrite := RemoteClient.WriteCount;
end;

function TDWServer.PrepareLogText(aMsg: string): string;
begin
  Result := DateTimeToStr(now) + ': ' + aMsg + #13#10 + BreakingLine;
end;

procedure TDWServer.ProcessException(e: Exception);
begin
  { TODO 1 -oDELCIO -cBUG: !!!!! NEDD TO BE SYNCRONIZE THIS BECAUSE MEMO IS VCL AND NOT THREAD SAFE THOUGH ON CRITICAL SECTION }
  FCriticalLog.Acquire;
  try
    if Assigned(FLogMemo) then
      FLogMemo.Lines.Add(PrepareLogText(e.Message))
    else
      WriteToLogFile(PrepareLogText(e.Message));
  finally
    FCriticalLog.Release;
  end;
end;

{ TAppHttpConnection }

(* constructor TDWServerClient.Create(AOwner: TComponent);
  begin
  inherited Create(AOwner);
  { keep alive means connection may be used for multiple requests so we must track how much
  data is sent before and after each request }
  CLastRead := 0 ;
  CLastWrite := 0 ;
  FResponseHeader:='';
  end;

  procedure TDWServerClient.SetResponseHeader(const Value: string);
  begin
  FResponseHeader := Value;
  end; *)

// ** This procedure just creates a new Logfile an appends when it was created **
// credits: http://delphi.cjcsoft.net//viewthread.php?tid=47526
procedure TDWServer.CreateLogfile;
var
  F: TextFile;
  FN: String;
begin
  FN := GetCurrentLogName;
  // Assigns Filename to variable F
  AssignFile(F, FN);
  // Rewrites the file F
  Rewrite(F);
  // Open file for appending
  Append(F);
  // Write text to Textfile F
  WriteLn(F, BreakingLine);
  WriteLn(F, 'This Logfile was created on ' + FormatDateTime('yyyy/mm/dd', now));
  WriteLn(F, BreakingLine);
  WriteLn(F, '');
  // finally close the file
  CloseFile(F);
end;

// Procedure for appending a Message to an existing logfile with current Date and Time **
// credits http://delphi.cjcsoft.net//viewthread.php?tid=47526
procedure TDWServer.WriteToLogFile(aLogMessage: String);
var
  F: TextFile;
  FN: String;
begin
  // Getting the current filename for the logfile
  FN := GetCurrentLogName;

  // Checking for file
  if (not FileExists(FN)) then
    begin
      // if file is not available then create a new file
      CreateLogfile;
    end;

  // Assigns Filename to variable F
  AssignFile(F, FN);
  // start appending text
  Append(F);
  // Write a new line with current date and message to the file
  WriteLn(F, aLogMessage);
  // Close file
  CloseFile(F)
end;

procedure TDWServer.DoDWAppTerminate(Sender: TObject);
begin
  if Assigned(FOnDWAppTerminate) then
    FOnDWAppTerminate(Sender);
end;

procedure TDWServer.DoNewSession(Sender: TObject);
begin
  if Assigned(FOnNewSession) then
    FOnNewSession(Sender);
end;

procedure TDWServer.DoSessionClose(Sender: TObject);
begin
  if Assigned(FOnSessionClose) then
    FOnSessionClose(Sender);
end;

procedure TDWServer.DoThreadRegistered(ThreadID: DWORD);
begin
  //
end;

procedure TDWServer.DoThreadSyncException(Thread: TJclDebugThread);
var
  LMsg: TStrings;
begin
  // MessageRichEdit.Lines.Add(Format('Exception in thread: %s', [Thread.ThreadInfo]));
  // Note: JclLastExceptStackList always returns list for *current* thread ID. To simplify getting the
  // stack of thread where an exception occured JclLastExceptStackList returns stack of the thread instead
  // of current thread when called *within* the JclDebugThreadList.OnSyncException handler. This is the
  // *only* exception to the behavior of JclLastExceptStackList described above.
  LMsg := TStringList.Create;
  try
    JclLastExceptStackList.AddToStrings(LMsg, False, True, True);
    if Assigned(FLogMemo) then
      FLogMemo.Lines.Add(PrepareLogText(LMsg.Text))
    else
      WriteToLogFile(PrepareLogText(LMsg.Text));
  finally
    LMsg.Free;
  end;
end;

procedure TDWServer.DoThreadUnregistered(ThreadID: DWORD);
begin
  //
end;

initialization

Include(JclStackTrackingOptions, stRawMode);
JclStartExceptionTracking;

end.
