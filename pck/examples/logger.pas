unit logger;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections;

type
  TLogLevel = (llTrace, llDebug, llInfo, llWarning, llError);

  TLogEvent = record
    Level: TLogLevel;
    Msg: string;
    TimeStamp: TDateTime;
    Identation: uint16;
  end;

  { ILogLayout }
  ILogLayout = interface
    ['{A2B3C4D5-E6F7-4A8B-9C0D-E1F2A3B4C5D6}']
    function FormatMessage(var event: TLogEvent): string;
  end;

  { TBaseLogLayout }

  TBaseLogLayout = class(TInterfacedObject, ILogLayout)
  protected
    function getIndentation(deep: uint16): string;
  public
    function FormatMessage(var event: TLogEvent): string; virtual; abstract;
  end;

  { TSimpleLogLayout }
  TSimpleLogLayout = class(TBaseLogLayout, ILogLayout)
  public
    function FormatMessage(var event: TLogEvent): string; override;
  end;

  { TTimestampLogLayout }
  TTimestampLogLayout = class(TBaseLogLayout, ILogLayout)
  private
    FDateTimeFormat: string;
  public
    constructor Create(const DateTimeFormat: string = 'yyyy-mm-dd hh:nn:ss');
    function FormatMessage(var event: TLogEvent): string; override;
  end;

  { TAppender }
  TAppender = class
  private
    FLayout: ILogLayout;
    procedure SetLayout(const Value: ILogLayout);
  public
    constructor Create; virtual;
    procedure Log(var event: TLogEvent); virtual; abstract;
    property Layout: ILogLayout read FLayout write SetLayout;
  end;

  { TConsoleAppender }
  TConsoleAppender = class(TAppender)
  private
    FLevel: TLogLevel;
    FDeepLevel: uint32;
  public
    constructor Create(Level: TLogLevel = llInfo); reintroduce;
    procedure Log(var event: TLogEvent); override;
  end;

  { TFileAppender }
  TFileAppender = class(TAppender)
  private
    FFileNamePattern: string; // Pattern with %d placeholder, e.g., 'log-%d.log'
    FDateFormat: string; // Date format for file name, e.g., 'yyyyMMdd'
    FLevel: TLogLevel;
    function GetCurrentFileName: string; // Resolve pattern with current date
  public
    constructor Create(const FileNamePattern: string; Level: TLogLevel = llDebug; const DateFormat: string = 'yyyyMMdd'); reintroduce;
    procedure Log(var event: TLogEvent); override;
  end;

  { TRollingFileAppender }
  TRollingFileAppender = class(TAppender)
  private
    FBaseFileName: string;
    FExtension: string;
    FMaxFileSize: int64;
    FMaxBackupIndex: integer;
    FLevel: TLogLevel;
    function GetCurrentFileName: string;
    function GetBackupFileName(Index: integer): string;
    procedure RollOver;
    function GetFileSize(const FileName: string): int64;
  public
    constructor Create(const BaseFileName: string; Level: TLogLevel = llTrace; MaxFileSize: int64 = 10485760; MaxBackupIndex: integer = 5); reintroduce;
    destructor Destroy; override;
    procedure Log(var event: TLogEvent); override;
  end;

  { TAppenderList }
  TAppenderList = specialize TList<TAppender>;

  { TLogger }
  TLogger = class
  private
    FDeepLevel: integer;
    FAppenders: TAppenderList;
  protected
    procedure Log(Level: TLogLevel; const Msg: string; const Args: array of const);
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddAppender(Appender: TAppender);
    procedure Debug(const Msg: string); overload;
    procedure Debug(const Msg: string; const Args: array of const); overload;
    procedure Info(const Msg: string); overload;
    procedure Info(const Msg: string; const Args: array of const); overload;
    procedure Warning(const Msg: string); overload;
    procedure Warning(const Msg: string; const Args: array of const); overload;
    procedure Error(const Msg: string); overload;
    procedure Error(const Msg: string; const Args: array of const); overload;
    procedure Enter(const Msg: string); overload;
    procedure Enter(const Msg: string; const Args: array of const); overload;
    procedure Leave(const Msg: string); overload;
    procedure Leave(const Msg: string; const Args: array of const); overload;
  end;

implementation




const
  LevelName: array[TLogLevel] of string =
    (    // 0
    ' TRACE ', // 0
    ' DEBUG ',  // 0
    '  INFO ',  // 0
    'WARNING',// 0
    ' ERROR '// 0
    );

function LevelToStr(Level: TLogLevel): string;
begin
  Result := LevelName[level];
end;

{ TBaseLogLayout }

function TBaseLogLayout.getIndentation(deep: uint16): string;
begin
  Result := Result.PadRight(deep);
end;

{ TSimpleLogLayout }

function TSimpleLogLayout.FormatMessage(var event: TLogEvent): string;
var
  Ident: string;
  PrefixLine: string;
  FormattedMessage: string;
begin
  Ident := getIndentation(event.Identation * 2);
  PrefixLine := Format('|%s|%s ', [LevelToStr(event.Level), Ident]);
  FormattedMessage := event.Msg.Replace(LineEnding, LineEnding + PrefixLine);
  Result := PrefixLine + FormattedMessage;
end;

{ TTimestampLogLayout }

constructor TTimestampLogLayout.Create(const DateTimeFormat: string);
begin
  FDateTimeFormat := DateTimeFormat;
end;

function TTimestampLogLayout.FormatMessage(var event: TLogEvent): string;
var
  Ident: string;
  PrefixLine: string;
  FormattedMessage: string;
  FormattedDate: string;
begin
  FormattedDate := FormatDateTime(FDateTimeFormat, event.TimeStamp);
  Ident := getIndentation(event.Identation * 2 + LevelToStr(event.Level).Length + FormattedDate.Length + 2);
  PrefixLine := Format('|%s|%s|%8x|%s ', [LevelToStr(event.Level), FormattedDate, GetCurrentThreadId, Ident]);
  FormattedMessage := event.Msg.Replace(LineEnding, LineEnding + PrefixLine);
  Result := PrefixLine + FormattedMessage;
end;

{ TAppender }

constructor TAppender.Create;
begin
  FLayout := nil;
end;

procedure TAppender.SetLayout(const Value: ILogLayout);
begin
  FLayout := Value;
end;

{ TConsoleAppender }

constructor TConsoleAppender.Create(Level: TLogLevel);
begin
  inherited Create;
  FLayout := TSimpleLogLayout.Create;
  FLevel := Level;
  FDeepLevel := 0;
end;

procedure TConsoleAppender.Log(var event: TLogEvent);
begin
  if event.Level >= FLevel then
  begin
    if FLayout = nil then
      Writeln(StdOut, Format('|%s| %s', [LevelToStr(event.Level), event.Msg]))
    else
      Writeln(StdOut, FLayout.FormatMessage(event));
  end;
end;

{ TFileAppender }

constructor TFileAppender.Create(const FileNamePattern: string; Level: TLogLevel; const DateFormat: string);
begin
  inherited Create;
  FLayout := TTimestampLogLayout.Create;
  FFileNamePattern := FileNamePattern;
  FDateFormat := DateFormat;
  FLevel := Level;
end;

function TFileAppender.GetCurrentFileName: string;
begin
  Result := StringReplace(FFileNamePattern, '%d', FormatDateTime(FDateFormat, Now), [rfReplaceAll]);
end;

procedure TFileAppender.Log(var event: TLogEvent);
var
  F: TextFile;
  FileName: string;
begin
  if event.Level >= FLevel then
  begin
    FileName := GetCurrentFileName;
    AssignFile(F, FileName);
    try
      if FileExists(FileName) then
        Append(F)
      else
        Rewrite(F);
      if FLayout = nil then
        Writeln(F, Format('|%s| %s', [LevelToStr(event.Level), event.Msg]))
      else
        Writeln(F, FLayout.FormatMessage(event));
      CloseFile(F);
    except
      on E: Exception do
        Writeln(StdErr, Format('Error writing to log file %s: %s', [FileName, E.Message]));
    end;
  end;
end;

{ TRollingFileAppender }

constructor TRollingFileAppender.Create(const BaseFileName: string; Level: TLogLevel; MaxFileSize: int64; MaxBackupIndex: integer);
begin
  inherited Create;
  FLayout := TTimestampLogLayout.Create; // Default layout with timestamp
  FBaseFileName := BaseFileName;
  FExtension := '.log';
  FMaxFileSize := MaxFileSize;
  FMaxBackupIndex := MaxBackupIndex;
  FLevel := Level;
end;

destructor TRollingFileAppender.Destroy;
begin
  inherited Destroy;
end;

function TRollingFileAppender.GetCurrentFileName: string;
begin
  Result := FBaseFileName + FExtension;
end;

function TRollingFileAppender.GetBackupFileName(Index: integer): string;
begin
  Result := Format('%s.%d%s', [FBaseFileName, Index, FExtension]);
end;

function TRollingFileAppender.GetFileSize(const FileName: string): int64;
var
  F: file;
begin
  Result := 0;
  if not FileExists(FileName) then
    Exit;
  AssignFile(F, FileName);
  try
    Reset(F, 1); // Open in read mode with record size 1 (byte)
    Result := FileSize(F);
    CloseFile(F);
  except
    on E: Exception do
    begin
      Writeln(StdErr, Format('Error getting file size for %s: %s', [FileName, E.Message]));
      Result := 0;
    end;
  end;
end;

procedure TRollingFileAppender.RollOver;
var
  I: integer;
  CurrentSize: int64;
  BackupFile: string;
begin
  CurrentSize := GetFileSize(GetCurrentFileName);
  if CurrentSize < FMaxFileSize then
    Exit; // No rollover needed

  // Roll over existing backups
  for I := FMaxBackupIndex downto 2 do
  begin
    BackupFile := GetBackupFileName(I);
    if FileExists(GetBackupFileName(I - 1)) then
    begin
      // Rename backup I-1 to backup I
      DeleteFile(BackupFile);
      RenameFile(GetBackupFileName(I - 1), BackupFile);
    end;
  end;

  // Roll over current file to backup.1
  if FileExists(GetCurrentFileName) then
  begin
    DeleteFile(GetBackupFileName(1));
    RenameFile(GetCurrentFileName, GetBackupFileName(1));
  end;
end;

procedure TRollingFileAppender.Log(var event: TLogEvent);
var
  F: TextFile;
  FileName: string;
  FormattedMsg: string;
begin
  if event.Level >= FLevel then
  begin
    RollOver;
    FileName := GetCurrentFileName;
    AssignFile(F, FileName);
    try
      if not FileExists(FileName) then
        Rewrite(F)
      else
        Append(F);
      if FLayout = nil then
        FormattedMsg := Format('|%s| %s', [LevelToStr(event.Level), event.Msg])
      else
        FormattedMsg := FLayout.FormatMessage(event);
      Writeln(F, FormattedMsg);
      Flush(F); // Ensure immediate write
      CloseFile(F);
    except
      on E: Exception do
        Writeln(StdErr, Format('Error writing to rolling log file %s: %s', [FileName, E.Message]));
    end;
  end;
end;

var
  DebugInfoCritSect: TRTLCriticalSection;

  { TLogger }

constructor TLogger.Create;
begin
  FAppenders := TAppenderList.Create;
  FDeepLevel := 0;
end;

destructor TLogger.Destroy;
begin
  FAppenders.Free;
  inherited Destroy;
end;

procedure TLogger.AddAppender(Appender: TAppender);
begin
  FAppenders.Add(Appender);
end;

procedure TLogger.Log(Level: TLogLevel; const Msg: string; const Args: array of const);
var
  Appender: TAppender;
  LogMsg, FormattedMsg: string;
  event: TLogEvent;
begin
  FormattedMsg := Format(Msg, Args);
  LogMsg := FormattedMsg;
  event.Level := level;
  event.Identation := FDeepLevel;
  event.Msg := LogMsg;
  event.TimeStamp := now;
  for Appender in FAppenders do
    Appender.Log(event);
end;

procedure TLogger.Debug(const Msg: string);
begin
  Debug(Msg, []);
end;

procedure TLogger.Debug(const Msg: string; const Args: array of const);
begin
  Log(llDebug, Msg, Args);
end;

procedure TLogger.Info(const Msg: string);
begin
  Info(Msg, []);
end;

procedure TLogger.Info(const Msg: string; const Args: array of const);
begin
  Log(llInfo, Msg, Args);
end;

procedure TLogger.Warning(const Msg: string);
begin
  Warning(Msg, []);
end;

procedure TLogger.Warning(const Msg: string; const Args: array of const);
begin
  Log(llWarning, Msg, Args);
end;

procedure TLogger.Error(const Msg: string);
begin
  Error(Msg, []);
end;

procedure TLogger.Error(const Msg: string; const Args: array of const);
begin
  Log(llError, Msg, Args);
end;

procedure TLogger.Enter(const Msg: string);
begin
  Enter(msg, []);
end;


procedure TLogger.Enter(const Msg: string; const Args: array of const);
begin
  FDeepLevel += 1;
  Log(llTrace, '>>' + msg, Args);
end;

procedure TLogger.Leave(const Msg: string);
begin
  Leave(msg, []);
end;

procedure TLogger.Leave(const Msg: string; const Args: array of const);
begin
  Log(llTrace, '<<' + msg, Args);
  FDeepLevel -= 1;
end;

initialization

  InitCriticalSection(DebugInfoCritSect);

finalization
  DoneCriticalSection(DebugInfoCritSect);

end.
