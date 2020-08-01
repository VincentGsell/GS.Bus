unit GS.Bus.Services;
///-------------------------------------------------------------------------------
/// Title      : GS.Bus.Services
/// Short Desc : Service definition : Bus supervising Bus, which could be task process.
/// Source     : https://github.com/VincentGsell
/// Aim        : ServiceManager will be used as base for GridServer.
///              Basically, Services is a thread worker / survey implementation.
/// Notes      : Written to replace GS.Tasks, which is not usefull in GridServer
///              current implementation
///              See GS.LocalMemCached for "from ground" thread task dedicated bus.
///              Services is just a more abstract implementation.
///              Never forget that  a bus is firstly a thread,
///              An high commmunication skill thread. And when you work,
///              as a thread, more important thing in not what you done,
///              it is the way of you communicate between thread. ;)

{$I GSCore.inc}

interface

Uses
{$IFDEF FPC}
  Classes,
  SysUtils,
  {$IFDEF USE_GENERIC}
  Generics.Collections,
  {$ENDIF}
  SyncObjs,
{$ELSE}
  System.Classes,
  System.SysUtils,
  {$IFDEF USE_GENERIC}
  System.Generics.Collections,
  {$ENDIF}
  System.SyncObjs,
{$ENDIF}
  GS.Common,
  GS.Bus,
  GS.Threads,
  GS.Stream,
  GS.System.CPU;


const
  CST_CUSTOMSERVICEAWAITINGTIMEOUT = 250; //ms
  CST_THREAD_SERVICE_NOT_RIGHT = 'Service has not a TThreadService class Thread or Thread not assigned.';
  CST_FIVE_SEC = 5000; //Thread life on waitfor (win lock)
  cst_ThreadServiceStatus_string : Array[0..4] of String = ('None', 'Exception', 'Waiting for data', 'Processing', 'Finished');


  //Thread's Sending channel. (Service receive)
  cThreadHeardBeatChannel        = 'HBC';
  cThreadProgressChannel         = 'PRG';
  cThreadChangeStatus            = 'CHS';
  cThreadResultChannel           = 'RES';
  cThreadLogChannel              = 'LOG';
  cThreadReceiveChannel          = 'RCV';
  cThreadProcessDuration         = 'PDU';

Type
TCustomServiceManager = Class;   //Management level. container. Bus.
TCustomService = Class;    //Single Service level : Container.
TServiceClass = class of TCustomService;
TThreadServiceStatus = (None, ExceptionInvoke, AWaitingForData, Processing, Terminated);

TThreadServiceStat = Record
  StartDateTime : TDateTime;
  FinishDateTime : TDateTime;
  DurationInMs : NativeUInt;
  FinishedNormaly : Boolean;
  FinishedAnormalyInfo : String;
  InternalStatus : TThreadServiceStatus;
  SystemEventCount : NativeUInt;
  LogProcessed : NativeUInt;

  Function AsString : String;
  Function Header : String;
End;
pTThreadServiceStat = ^TThreadServiceStat;


//Message def.
TThreadServiceMessage_changeStatus = Packed Record
  ServiceContext : UInt64;
  ThreadID : UInt64;
  NewStatus : TThreadServiceStatus;
  AdditionalData : String;

  Function GetAsStream : TMemoryStream; //Cannot use delphi serialization : Multiplatform set.
  Procedure LoadFromStream(aStream : TMemoryStream);
end;

TThreadServiceMessage_Progress = Packed Record
  ServiceContext : UInt64;
  ThreadID : UInt64;
  Progress : Double;
  AdditionalData : String;

  Function GetAsStream : TMemoryStream; //Idem as previous...
  Procedure LoadFromStream(aStream : TMemoryStream);
end;

TThreadServiceMessage_HeartBeat = Packed Record
  ServiceContext : UInt64;
  ThreadID : UInt64;

  Function GetAsStream : TMemoryStream; //And idem...
  Procedure LoadFromStream(aStream : TMemoryStream);
end;

TThreadServiceMessage_LogVerbosityLevel = (all, info, warning, exception);
TThreadServiceMessage_Log = Packed Record
  ServiceContext : UInt64;
  ThreadID : UInt64;
  VerbosityLevel : TThreadServiceMessage_LogVerbosityLevel;
  LogText : string;

  Function GetAsStream : TMemoryStream; //...
  Procedure LoadFromStream(aStream : TMemoryStream);
end;

TThreadServiceMessage_Result = Packed Record
  ServiceContext : UInt64;
  ThreadID : UInt64;
  PartialResult : Boolean;
  ResultAsStream : TMemoryStream;

  Function GetAsStream : TMemoryStream;
  Procedure LoadFromStream(aStream : TMemoryStream);
end;

TThreadServiceMessage_ProcessDuration = Packed Record
  ServiceContext : UInt64;
  ThreadID : UInt64;
  Duration : UInt64;

  Function GetAsStream : TMemoryStream;
  Procedure LoadFromStream(aStream : TMemoryStream);
end;

TCustomServiceThread = Class;
TServiceTask = Class
  MasterThread : TCustomServiceThread;
  Procedure Initialize; Virtual; //Use this as a constructor, in this method, you had acces to MasterThread. Not in the constructor.
  Procedure Execute; Virtual; Abstract;
  Procedure Finalize; Virtual;
End;

//Internal resident Thread of a service.
//This thread take task as an entry and execute it, as an engine.
//This thread's aim is to supervise "Run" the task, while TServiceManager supervise the run.
TServiceStatus = (NotActive, Active);

  TGSProtectedServiceStatus = Class(TGSProtectedItem)
  private
    FStatus : TServiceStatus;
    function GetValue: TServiceStatus;
    procedure SetValue(const Value: TServiceStatus);
  public
    Constructor Create(aStatus : TServiceStatus); Reintroduce;

    Property Value : TServiceStatus read GetValue Write SetValue;
  End;


TCustomServiceThread = Class(TThread)
Protected
  FStatus : TGSProtectedServiceStatus;

  //Thread signal runner.
  FGo : TEvent;
  //Communication Bus Instance.
  FBus : TBus; //Pointer.
  //Master service
  FService : TCustomService; //Pointer.
  //Current Task
  FTask : TServiceTask;

  Procedure DoProcessTask;
  Procedure DoChangeStatus(aNewStatus : TThreadServiceStatus; Const AdditionalInfo : String = ''); Virtual;
  Procedure DoReportDuration(aDurationInMs : UInt64); Virtual;

Public
  Constructor Create; reintroduce;
  Destructor Destroy; Override;

  Procedure Go;
  Procedure Execute; Override;

  //Should not be called. Used by manager to setup internal tools.
  Procedure SetUpThread(Const aService : TCustomService; aBus : GS.Bus.TBus);

  Procedure SubmitTask(aTask : TServiceTask);

  //This method is available to be used inside the "run" Procedure, in order to
  //notify information to master ServiceManager.
  Procedure DoHeartBeat; Virtual;
  Procedure DoProgress(aPercent : Double); Virtual;
  Procedure DoResult(aStream : TMemoryStream; Const IsAFinalResult : Boolean = False); Virtual;
  Procedure DoLog(aLogText : String; const Verbosity : TThreadServiceMessage_LogVerbosityLevel = TThreadServiceMessage_LogVerbosityLevel.info); Virtual;

  Property Bus : TBus read FBus;
  Property Terminated; //Prop. Visibility.
End;

  TGSProtectedServiceStatsPointer = Class(TGSProtectedItem)
  private
    FServiceStat : pTThreadServiceStat;
    function GetValue: pTThreadServiceStat;
    procedure SetValue(const Value: pTThreadServiceStat);
    function GetValueCopy: TThreadServiceStat;
  public
    Constructor Create(aServiceStat : pTThreadServiceStat); Reintroduce;

    Property Value : pTThreadServiceStat read GetValue Write SetValue;
    Property Copy : TThreadServiceStat read GetValueCopy;
  End;


TCustomService = Class
private
protected
  FWaitForCompletion : TEvent;
  FThread :  TCustomServiceThread;
  FBus : Gs.Bus.TBus;
  FInternalStats : TGSProtectedServiceStatsPointer;
  FTask : TServiceTask;
  function GetServiceStats: TThreadServiceStat;
  Function GetServiceName : String; Virtual;
  function GetServiceStatus: TServiceStatus; Virtual;

  function GetTask: TServiceTask;
  procedure SetTask(const Value: TServiceTask);

public
  constructor Create; Virtual;
  Destructor Destroy; Override;
  Property ServiceName : String read GetServiceName;

  Procedure StartService(const aWaitForStarting : Boolean = true);
  Procedure WaitForStarting;
  Procedure StopService;
  Procedure WaitFor;

  //Should Not called : Should be used only by underlying thread.
  //When user call a a WaitFor from outside, thread must call SetTaskFinished for
  //liberate the user call.
  Procedure SetTaskFinished;

  Property ServiceStatus : TServiceStatus read GetServiceStatus;
  Property ServiceStats :  TThreadServiceStat read GetServiceStats;
  Property Task : TServiceTask read GetTask Write SetTask;
End;

TService = Class(TCustomService)
End;

{$IFDEF USE_GENERIC}
TServiceList = TObjectList<TCustomService>;
{$ELSE}
TServiceList = Class(TList_ObjectArray)
Private
  function GetCustomServiceItem(Index: Uint32): TCustomService;
  procedure SetCustomServiceItem(Index: Uint32; const Value: TCustomService);
Public
  constructor Create; Reintroduce;
  Procedure Add(aBusChannel : TCustomService);
  Property Items[Index : Uint32] : TCustomService read GetCustomServiceItem Write SetCustomServiceItem; Default;
End;
{$ENDIF}

TGSProtectedServiceList = class(TGSProtectedObject)
public
  Constructor Create; Reintroduce;
  Function Lock : TServiceList; reintroduce;
end;

TCustomServiceManager = Class(GS.Bus.TBus)
private
  function GetServiceCount: Uint32;
protected
  FServices : TGSProtectedServiceList; //TProtectedObject<TServiceList>;

  function GetServices(Index: UInt32): TCustomService; Virtual;
  Procedure EnsureAllThreadStoped; Virtual;

  //Bus processing
  Procedure OnServiceChangeStatus(Var aMessage : TBusEnvelop);
  Procedure OnServiceLog(Var aMessage : TBusEnvelop);
  Procedure OnServiceHearBeat(Var aMessage : TBusEnvelop);
  Procedure OnServiceResult(Var aMessage : TBusEnvelop);
  Procedure OnServiceProgress(Var aMessage : TBusEnvelop);
  Procedure OnServiceDuration(Var aMessage : TBusEnvelop);

  //Tools
  Function ServiceFromContext(aContext : UInt64) : TCustomService;
Public
  Constructor Create; Override;
  Destructor Destroy; Override;

  Procedure StartAllServices; Virtual;
  Procedure StopAllServices; Virtual;
  Procedure StopService(aService : TCustomService); Virtual;

  Procedure RegisterService(aService : TCustomService); Virtual;
  procedure UnregisterService(aService : TCustomService); Virtual;
  Procedure UnregisterAllServices(Const aFreeTask : Boolean = False); Virtual;

  Function ServiceIndex(aService : TCustomService) : Integer; Virtual;

  Function StatsTask : String;

  Function StatsServices : String;

  Function ServicesLock : TServiceList; //Call with try..
  Procedure ServicesUnlock;             //...finally.

  Property Services[Index : UInt32] : TCustomService read getServices;
  Property ServiceCount : Uint32 read GetServiceCount;
End;

TServiceManager = Class(TCustomServiceManager)
End;



implementation


{ TThreadService }

Function NullThreadStat : pTThreadServiceStat;
var la : pTThreadServiceStat;
begin
  new(la);
  la.StartDateTime :=  0;
  la.FinishDateTime :=  0;
  la.DurationInMs := 0;
  la.FinishedNormaly := True; //We are proudly Optimistic.
  la.FinishedAnormalyInfo := EmptyStr;
  la.InternalStatus := TThreadServiceStatus.None;
  la.SystemEventCount := 0;
  la.LogProcessed := 0;
  Result := la;
end;


procedure TCustomServiceThread.DoChangeStatus( aNewStatus: TThreadServiceStatus;
                                         Const AdditionalInfo : String = '');
var lMes : TBusMessage;
    lMem : TMemoryStream;
    lFormatedMessage : TThreadServiceMessage_changeStatus;
begin
  if Assigned(FBus) then
  Begin
    lFormatedMessage.ServiceContext := UInt64(FService);
    lFormatedMessage.ThreadID := ThreadID;
    lFormatedMessage.NewStatus := aNewStatus;
    lFormatedMessage.AdditionalData := AdditionalInfo;
    lMem := lFormatedMessage.GetAsStream;
    try
      lMes.FromStream(lMem);
      FBus.Send(lMes,cThreadChangeStatus,IntToStr(ThreadID));
    finally
      FreeAndNil(lMem);
    end;
  End;
end;

procedure TCustomServiceThread.DoResult(aStream: TMemoryStream; Const IsAFinalResult : Boolean = False);
var lMes : TBusMessage;
    lMem : TMemoryStream;
    lFormatedMessage : TThreadServiceMessage_Result;
begin
  if Assigned(FBus) then
  Begin
    lFormatedMessage.ServiceContext := UInt64(FService);
    lFormatedMessage.ThreadID := ThreadID;
    lFormatedMessage.PartialResult := Not(IsAFinalResult);
    lFormatedMessage.ResultAsStream := aStream;
    lMem := lFormatedMessage.GetAsStream;
    try
      lMes.FromStream(lMem);
      FBus.Send(lMes,cThreadResultChannel,IntToStr(ThreadID));
    finally
      FreeAndNil(lMem);
    end;
  End;
end;

procedure TCustomServiceThread.DoHeartBeat;
var lMes : TBusMessage;
    lMem : TMemoryStream;
    lFormatedMessage : TThreadServiceMessage_HeartBeat;
begin
  if Assigned(FBus) then
  Begin
    lFormatedMessage.ServiceContext := UInt64(FService);
    lFormatedMessage.ThreadID := ThreadID;
    lMem := lFormatedMessage.GetAsStream;
    try
      lMes.FromStream(lMem);
      FBus.Send(lMes,cThreadHeardBeatChannel,IntToStr(ThreadID));
    finally
      FreeAndNil(lMem);
    end;
  End;
end;

procedure TCustomServiceThread.DoLog(aLogText: String; const Verbosity : TThreadServiceMessage_LogVerbosityLevel = TThreadServiceMessage_LogVerbosityLevel.info);
var lMes : TBusMessage;
    lMem : TMemoryStream;
    lFormatedMessage : TThreadServiceMessage_Log;
begin
  if Assigned(FBus) then
  Begin
    lFormatedMessage.ServiceContext := UInt64(FService);
    lFormatedMessage.ThreadID := ThreadID;
    lFormatedMessage.LogText := aLogText;
    lFormatedMessage.VerbosityLevel := Verbosity;
    lMem := lFormatedMessage.GetAsStream;
    try
      lMes.FromStream(lMem);
      FBus.Send(lMes,cThreadLogChannel,IntToStr(ThreadID));
    finally
      FreeAndNil(lMem);
    end;
  End;
end;

procedure TCustomServiceThread.DoProcessTask;
begin
  Assert(Assigned(FService));
  if Assigned(FTask) then
  begin
    FTask.Initialize;
    FTask.Execute;
    FTask.Finalize;
  end;
end;

procedure TCustomServiceThread.DoProgress(aPercent: Double);
var lMes : TBusMessage;
    lMem : TMemoryStream;
    lFormatedMessage : TThreadServiceMessage_Progress;
begin
  if Assigned(FBus) then
  Begin
    lFormatedMessage.ServiceContext := UInt64(FService);
    lFormatedMessage.ThreadID := ThreadID;
    lFormatedMessage.Progress := aPercent;
    lMem := lFormatedMessage.GetAsStream;
    try
      lMes.FromStream(lMem);
      FBus.Send(lMes,cThreadProgressChannel,IntToStr(ThreadID));
    finally
      FreeAndNil(lMem);
    end;
  End;
end;


procedure TCustomServiceThread.DoReportDuration(aDurationInMs: UInt64);
var lMes : TBusMessage;
    lMem : TMemoryStream;
    lFormatedMessage : TThreadServiceMessage_ProcessDuration;
begin
  if Assigned(FBus) then
  Begin
    lFormatedMessage.ServiceContext := UInt64(FService);
    lFormatedMessage.ThreadID := ThreadID;
    lFormatedMessage.Duration := aDurationInMs;
    lMem := lFormatedMessage.GetAsStream;
    try
      lMes.FromStream(lMem);
      FBus.Send(lMes,cThreadProcessDuration,IntToStr(ThreadID));
    finally
      FreeAndNil(lMem);
    end;
  End;
end;

procedure TCustomServiceThread.SetUpThread(Const aService : TCustomService; aBus : GS.Bus.TBus);
begin
  Assert(Assigned(aService));
  Assert(Assigned(aBus));
  FService := AService;
  FBus := aBus;
  if Suspended then
    Start;
end;


procedure TCustomServiceThread.SubmitTask(aTask: TServiceTask);
begin
  if FStatus.Value = TServiceStatus.NotActive then
  begin
    FTask := aTask;
    FTask.MasterThread := Self;
  end;
end;

{ TCustomServiceManager }

constructor TCustomServiceManager.Create;
begin
  inherited Create;
  FServices := TGSProtectedServiceList.Create;
  ChannelSetOnBeforeDeliverMessageEvent(cThreadChangeStatus,OnServiceChangeStatus);
  ChannelSetOnBeforeDeliverMessageEvent(cThreadResultChannel,OnServiceResult);
  ChannelSetOnBeforeDeliverMessageEvent(cThreadProgressChannel,OnServiceProgress);
  ChannelSetOnBeforeDeliverMessageEvent(cThreadLogChannel,OnServiceLog);
  ChannelSetOnBeforeDeliverMessageEvent(cThreadHeardBeatChannel,OnServiceHearBeat);
  ChannelSetOnBeforeDeliverMessageEvent(cThreadProcessDuration,OnServiceDuration);
end;

destructor TCustomServiceManager.Destroy;
begin
  BusShutDown;
  EnsureAllThreadStoped;
  UnregisterAllServices;
  FreeAndNil(FServices);
  inherited;
end;

procedure TCustomServiceManager.EnsureAllThreadStoped;
var i : Integer;
    lServices : TServiceList;
begin
  lServices := FServices.Lock;
  try
    for i := 0 to lServices.Count-1 do
    begin
      lServices[i].StopService;
    end;
  finally
    FServices.Unlock;
  end;
end;

function TCustomServiceManager.GetServiceCount: Uint32;
var lServices : TServiceList;
begin
  lServices := FServices.Lock;
  try
    result := lServices.Count;
  finally
    FServices.Unlock;
  end;
end;

function TCustomServiceManager.getServices(Index: UInt32): TCustomService;
var lServices : TServiceList;
begin
  lServices := FServices.Lock;
  try
    result := lServices[Index];
  finally
    FServices.Unlock;
  end;
end;

procedure TCustomServiceManager.OnServiceChangeStatus(
  var aMessage: TBusEnvelop);
var lcc : TCustomService;
    lFormatedMessage : TThreadServiceMessage_changeStatus;
    lMem : TMemoryStream;
begin
  lMem := aMessage.ContentMessage.AsStream;
  try
    lFormatedMessage.LoadFromStream(lMem);
  finally
    FreeAndNil(lMem);
  end;
  lcc := ServiceFromContext(lFormatedMessage.ServiceContext);

  if assigned(lcc) then
  begin
    lcc.FInternalStats.Value^.InternalStatus := lFormatedMessage.NewStatus;
    lcc.FInternalStats.Value^.SystemEventCount := lcc.FInternalStats.Value^.SystemEventCount+1;
    case lFormatedMessage.NewStatus of
      TThreadServiceStatus.None: ;
      TThreadServiceStatus.ExceptionInvoke :
      begin
        lcc.FInternalStats.Value^.FinishedNormaly := False;
        lcc.FInternalStats.Value^.FinishedAnormalyInfo := lFormatedMessage.AdditionalData;
      end;
      TThreadServiceStatus.AWaitingForData:
      begin
      end;
      TThreadServiceStatus.Processing:
      begin
        lcc.FInternalStats.Value^.StartDateTime := Now;
      end;
      TThreadServiceStatus.Terminated:
      begin
        lcc.FInternalStats.Value^.FinishDateTime := Now;
      end;
    end;
    //Event ?
  end;
end;

procedure TCustomServiceManager.OnServiceDuration(var aMessage: TBusEnvelop);
var lcc : TCustomService;
    lFormatedMessage : TThreadServiceMessage_ProcessDuration;
    lMem : TMemoryStream;
begin
  lMem := aMessage.ContentMessage.AsStream;
  try
    lFormatedMessage.LoadFromStream(lMem);
  finally
    FreeAndNil(lMem);
  end;
  lcc := ServiceFromContext(lFormatedMessage.ServiceContext);

  if assigned(lcc) then
  begin
    lcc.FInternalStats.Value^.SystemEventCount := lcc.FInternalStats.Value^.SystemEventCount+1;
    lcc.FInternalStats.Value^.DurationInMs := lFormatedMessage.Duration;
    //Event ?
  end;
end;

procedure TCustomServiceManager.OnServiceHearBeat(var aMessage: TBusEnvelop);
var lcc : TCustomService;
    lFormatedMessage : TThreadServiceMessage_HeartBeat;
    lMem : TMemoryStream;
begin
  lMem := aMessage.ContentMessage.AsStream;
  try
    lFormatedMessage.LoadFromStream(lMem);
  finally
    FreeAndNil(lMem);
  end;
  lcc := ServiceFromContext(lFormatedMessage.ServiceContext);

  if assigned(lcc) then
  begin
    lcc.FInternalStats.Value^.SystemEventCount := lcc.FInternalStats.Value^.SystemEventCount+1;
    //Event ?
  end;
end;

procedure TCustomServiceManager.OnServiceLog(var aMessage: TBusEnvelop);
var lcc : TCustomService;
    lFormatedMessage : TThreadServiceMessage_Log;
    lMem : TMemoryStream;
begin
  lMem := aMessage.ContentMessage.AsStream;
  try
    lFormatedMessage.LoadFromStream(lMem);
  finally
    FreeAndNil(lMem);
  end;
  lcc := ServiceFromContext(lFormatedMessage.ServiceContext);

  if assigned(lcc) then
  begin
    lcc.FInternalStats.Value^.SystemEventCount := lcc.FInternalStats.Value^.SystemEventCount+1;
    lcc.FInternalStats.Value^.LogProcessed := lcc.FInternalStats.Value^.LogProcessed+1;
    //Event ?
  end;

end;

procedure TCustomServiceManager.OnServiceProgress(var aMessage: TBusEnvelop);
var lcc : TCustomService;
    lFormatedMessage : TThreadServiceMessage_Progress;
    lMem : TMemoryStream;
begin
  lMem := aMessage.ContentMessage.AsStream;
  try
    lFormatedMessage.LoadFromStream(lMem);
  finally
    FreeAndNil(lMem);
  end;
  lcc := ServiceFromContext(lFormatedMessage.ServiceContext);

  if assigned(lcc) then
  begin
    lcc.FInternalStats.Value^.SystemEventCount := lcc.FInternalStats.Value^.SystemEventCount+1;
    //Event ?
  end;
end;

procedure TCustomServiceManager.OnServiceResult(var aMessage: TBusEnvelop);
var lcc : TCustomService;
    lFormatedMessage : TThreadServiceMessage_Result;
    lMem : TMemoryStream;
begin
  lMem := aMessage.ContentMessage.AsStream;
  try
    lFormatedMessage.LoadFromStream(lMem);
  finally
    FreeAndNil(lMem);
  end;
  lcc := ServiceFromContext(lFormatedMessage.ServiceContext);

  if assigned(lcc) then
  begin
    lcc.FInternalStats.Value^.SystemEventCount := lcc.FInternalStats.Value^.SystemEventCount+1;
    //Event ?
  end;
end;

procedure TCustomServiceManager.RegisterService(aService: TCustomService);
var lServices : TServiceList;
begin
  Assert(Assigned(aService));
  lServices := FServices.Lock;
  try
  if lServices.IndexOf(aService) = -1 then
  begin
    lServices.Add(aService);
    aService.FBus := TBus(Self); //Only bus part is visible. But it is a TCustomServiceManager.
  end;
  finally
    FServices.Unlock;
  end;
end;

function TCustomServiceManager.ServiceFromContext(
  aContext: UInt64): TCustomService;
var i : integer;
    lServices : TServiceList;

begin
  Result := nil;
  lServices := FServices.Lock;
  try
    for i := 0 to lServices.Count-1 do
    begin
      if Uint64(lServices[i]) = aContext  then
      begin
        Result := lServices[i];
        Exit;
      end;
    end;

    if Not(Assigned(Result)) then
    begin
      //Thread could respond after service removing.
      //raise Exception.Create(ClassName+'.ServiceFromContext : Not found');
    end;
  finally
    Fservices.Unlock;
  end;
end;

function TCustomServiceManager.ServiceIndex(aService: TCustomService): Integer;
var lServices : TServiceList;
begin
  lServices := FServices.Lock;
  try
    Result := lServices.IndexOf(aService);
  finally
    FServices.Lock;
  end;
end;

function TCustomServiceManager.ServicesLock: TServiceList;
begin
  Result := FServices.Lock;
end;

procedure TCustomServiceManager.ServicesUnlock;
begin
  FServices.Unlock;
end;

procedure TCustomServiceManager.StartAllServices;
var i : integer;
    lServices : TServiceList;
begin
  lServices := FServices.Lock;
  try
    for i := 0 to lServices.Count-1 do
    begin
      lServices[i].StartService;
    end;
  finally
    FServices.Unlock;
  end;
end;

function TCustomServiceManager.StatsServices: String;
var ls : TStringList;
    i : integer;
    lServices : TServiceList;
begin
  lServices := FServices.Lock;
  try
    ls := TStringList.Create;
    try
      if lServices.Count>0 then
      begin
        ls.Add('ServiceName,'+lServices[0].ServiceStats.Header);
        for i := 0 to lServices.Count-1 do
        begin
          ls.Add(lServices[i].ServiceName+','+lServices[i].ServiceStats.AsString);
        end;
      end;
      Result := ls.Text;
    finally
      FreeAndNil(ls);
    end;
  finally
    FServices.UnLock;
  end;
end;

function TCustomServiceManager.StatsTask: String;
var ls : TStringList;
    i : integer;
    lServices : TServiceList;
begin
  lservices := FServices.Lock;
  try
    ls := TStringList.Create;
    try
      if lServices.Count>0 then
      begin
        for i := 0 to lServices.Count-1 do
        begin
          if Assigned(lServices[i].Task) then
          begin
            ls.Add(lServices[i].ServiceName+' - '+lServices[i].Task.ClassName);
          end
          else
          begin
            ls.Add(lServices[i].ServiceName+' - No task.' );
          end;
        end;
      end;
      Result := ls.Text;
    finally
      FreeAndNil(ls);
    end;
  finally
    FServices.Unlock;
  end;
end;

procedure TCustomServiceManager.StopAllServices;
begin
  EnsureAllThreadStoped;
end;

procedure TCustomServiceManager.StopService(aService: TCustomService);
begin
  if Assigned(aService) then
  begin
    aService.StopService;
  end;
end;

procedure TCustomServiceManager.UnregisterAllServices(Const aFreeTask : Boolean = False);
var i : integer;
    lServices : TServiceList;
begin
  lServices := FServices.Lock;
  try
    for i := 0 to lServices.Count-1 do
    begin
      if Assigned(lServices[i].Task) And aFreeTask then
      begin
        lServices[i].Task.Free;
      end;
    end;
  finally
    FServices.Unlock;
  end;
end;

procedure TCustomServiceManager.UnregisterService(aService: TCustomService);
var lServices : TServiceList;
begin
  if Assigned(aService) then
  begin
    StopService(aService);
    lServices := FServices.Lock;
    try
      lServices.Remove(aService); //Service freed.
    Finally
      FServices.Unlock;
    end;
  end;
end;


{ TCustomService }

constructor TCustomService.Create;
begin
  Inherited Create;
  FInternalStats := TGSProtectedServiceStatsPointer.Create(NullThreadStat);
  FThread := TCustomServiceThread.Create;
  FWaitForCompletion :=  TEvent.Create(nil,False,False,EmptyStr);
end;

destructor TCustomService.Destroy;
begin
  if Not(FThread.Terminated) And Not(FThread.Suspended) then
  begin
    FThread.Terminate;
    FThread.WaitFor;
  end;
  FreeAndNil(FThread);
  Dispose(FInternalStats.Value);
  FreeAndNil(FInternalStats);
  FreeAndNil(FWaitForCompletion);
  inherited;
end;

function TCustomService.GetServiceName: String;
begin
  if Assigned(FTask) then
  begin
    Result :=  FTask.ClassName;
  end
  else
  if Assigned(FThread) then
  begin
    Result := '"'+FThread.ClassName+'" based Service';
  end
  else
  begin
    Result := 'Noname Service';
  end;
end;

function TCustomService.GetServiceStats: TThreadServiceStat;
begin
  Result := FInternalStats.Value^; //Get a copy.
end;

function TCustomService.GetServiceStatus: TServiceStatus;
begin
  Result := TServiceStatus.NotActive;
  if Not(FThread.Suspended) and Not(FThread.Finished) then
    Result := TServiceStatus.Active;
end;


function TCustomService.GetTask: TServiceTask;
begin
  Result := FTask;
end;

procedure TCustomService.SetTask(const Value: TServiceTask);
begin
  if FThread.FStatus.Value = TServiceStatus.NotActive then
  begin
    FTask := Value;
    FThread.SubmitTask(Ftask);
  end
  else
  begin
    raise Exception.Create(ClassName+'.SetTask : Cannot apply a Task during Task processing.');
  end;
end;

procedure TCustomService.SetTaskFinished;
begin
  FWaitForCompletion.SetEvent;
end;

procedure TCustomService.StartService(const aWaitForStarting : Boolean = true);
begin
  Fthread.SetUpThread(Self, FBus); //Will thread.Start.
  FThread.Go;                      //Will Run the Task process.
  if aWaitForStarting then
    WaitForStarting;
end;

procedure TCustomService.StopService;
begin
  if FThread.Suspended then
    Exit;
  if Not FThread.Terminated then
  begin
    if Not(FThread.Suspended) then //Was Not(FThread.Started)
      FThread.Go;
    FThread.Terminate;
    FThread.WaitFor;
  end;
end;

procedure TCustomService.WaitFor;
begin
  if TThread.CurrentThread.ThreadID <> FThread.ThreadID then
    FWaitForCompletion.WaitFor(CST_FIVE_SEC); //INFINITE);
end;

procedure TCustomService.WaitForStarting;
begin
  While Not(ServiceStatus = TServiceStatus.Active) do;
end;

{ TThreadServiceStat }

function TThreadServiceStat.AsString: String;
var ls : TStringList;
begin
  ls := TStringList.Create;
  try
    ls.Delimiter := ',';
    ls.Add(IntToStr(DurationInMs));
    ls.Add(DateTimeToStr(StartDateTime));
    ls.Add(DateTimeToStr(FinishDateTime));
    ls.Add(IntToStr(Byte(FinishedNormaly)));
    ls.Add(FinishedAnormalyInfo);
    ls.Add(cst_ThreadServiceStatus_string[Byte(InternalStatus)]);
    ls.Add(IntToStr(SystemEventCount));
    ls.Add(IntToStr(LogProcessed));
    Result := ls.DelimitedText;
  finally
    FreeAndNil(ls);
  end;
end;

function TThreadServiceStat.Header: String;
var ls : TStringList;
begin
  ls := TStringList.Create;
  try
    ls.Delimiter := ',';
    ls.Add('DurationInMs');
    ls.Add('StartDateTime');
    ls.Add('FinishDateTime');
    ls.Add('FinishedNormaly');
    ls.Add('FinishedAnormalyInfo');
    ls.Add('Status');
    ls.Add('SystemEventCount');
    ls.Add('LogProcessed');
    Result := ls.DelimitedText;
  finally
    FreeAndNil(ls);
  end;
end;

{ TThreadServiceMessage_changeStatus }

function TThreadServiceMessage_changeStatus.GetAsStream: TMemoryStream;
begin
  Result :=  TMemoryStream.Create;
  WriteUInt64(Result, ServiceContext);
  WriteUInt64(Result, ThreadID);
  WriteByte(Result, Byte(NewStatus));
  WriteString(Result, AdditionalData);
end;

procedure TThreadServiceMessage_changeStatus.LoadFromStream(
  aStream: TMemoryStream);
begin
  Assert(assigned(aStream));
  ServiceContext := ReadUInt64(aStream);
  ThreadID := ReadUInt64(aStream);
  NewStatus := TThreadServiceStatus(ReadByte(aStream));
  AdditionalData := ReadString(aStream);
end;

{ TThreadServiceMessage_Progress }

function TThreadServiceMessage_Progress.GetAsStream: TMemoryStream;
begin
  Result :=  TMemoryStream.Create;
  WriteUInt64(Result, ServiceContext);
  WriteUInt64(Result, ThreadID);
  WriteDouble(Result, Progress);
  WriteString(Result, AdditionalData);
end;

procedure TThreadServiceMessage_Progress.LoadFromStream(aStream: TMemoryStream);
begin
  Assert(assigned(aStream));
  ServiceContext := ReadUInt64(aStream);
  ThreadID := ReadUInt64(aStream);
  Progress := ReadDouble(aStream);
  AdditionalData := ReadString(aStream);
end;

{ TThreadServiceMessage_HeartBeat }

function TThreadServiceMessage_HeartBeat.GetAsStream: TMemoryStream;
begin
  Result :=  TMemoryStream.Create;
  WriteUInt64(Result, ServiceContext);
  WriteUInt64(Result, ThreadID);
end;

procedure TThreadServiceMessage_HeartBeat.LoadFromStream(
  aStream: TMemoryStream);
begin
  Assert(assigned(aStream));
  ServiceContext := ReadUInt64(aStream);
  ThreadID := ReadUInt64(aStream);
end;

{ TThreadServiceMessage_Log }

function TThreadServiceMessage_Log.GetAsStream: TMemoryStream;
begin
  Result :=  TMemoryStream.Create;
  WriteUInt64(Result, ServiceContext);
  WriteUInt64(Result, ThreadID);
  WriteByte(result,Byte(VerbosityLevel));
  WriteString(Result, LogText);
end;

procedure TThreadServiceMessage_Log.LoadFromStream(aStream: TMemoryStream);
begin
  Assert(assigned(aStream));
  ServiceContext := ReadUInt64(aStream);
  ThreadID := ReadUInt64(aStream);
  VerbosityLevel := TThreadServiceMessage_LogVerbosityLevel(readByte(aStream));
  LogText := ReadString(aStream)
end;

{ TThreadServiceMessage_Result }

function TThreadServiceMessage_Result.GetAsStream: TMemoryStream;
begin
  Result :=  TMemoryStream.Create;
  WriteUInt64(Result, ServiceContext);
  WriteUInt64(Result, ThreadID);
  WriteBoolean(Result, PartialResult);
  ResultAsStream.Position := 0;
  WriteStream(Result, ResultAsStream);
end;

procedure TThreadServiceMessage_Result.LoadFromStream(aStream: TMemoryStream);
begin
  Assert(assigned(aStream));
  ServiceContext := ReadUInt64(aStream);
  ThreadID := ReadUInt64(aStream);
  PartialResult := ReadBoolean(aStream);
  ResultAsStream := TMemoryStream.Create;
  ReadStream(aStream,TStream(ResultAsStream));
end;

{ TCustomServiceThread }

constructor TCustomServiceThread.Create;
begin
  Inherited Create(True); //run immediately.
  FreeOnTerminate := false;
  FStatus := TGSProtectedServiceStatus.Create(TServiceStatus.NotActive);
  FGo := TEvent.Create(nil,False,False,EmptyStr);
end;

destructor TCustomServiceThread.Destroy;
begin
  Terminate;
  if Not(Terminated) then
  begin
    //FGo.SetEvent;
    WaitFor;
  end;
  FreeAndNil(FGo);
  FreeAndNil(FStatus);
  inherited;
end;

procedure TCustomServiceThread.Execute;
var lt : Int64;
begin
  while not(terminated) do
  begin
    DoChangeStatus(TThreadServiceStatus.AWaitingForData);
    if FStatus.Value<>TServiceStatus.NotActive then
      FStatus.Value := TServiceStatus.NotActive;
    case FGo.WaitFor(CST_CUSTOMSERVICEAWAITINGTIMEOUT) of
      wrSignaled :
      begin
        try
          if Terminated then
            Exit;
          FStatus.Value := TServiceStatus.Active;
          DoChangeStatus(TThreadServiceStatus.Processing);
          FTask.MasterThread := self;
          lt :=  gsGetTickCount;
          DoProcessTask;
          if Terminated then
            Exit;
          lt := gsGetTickCount - lt;
          DoReportDuration(lt);
          DoChangeStatus(TThreadServiceStatus.Terminated);
          FService.SetTaskFinished;
        Except
          On E : Exception do
          begin
            DoChangeStatus(TThreadServiceStatus.ExceptionInvoke, E.Message);
          end;
        end;
      end;
      wrTimeout:
      begin
        if Terminated then
          Exit;
        DoHeartBeat; //TODO : Heard beath only on a givent HeatBeat frequency time (Usualy, 1 sec.)
      end;
      wrAbandoned, wrError{$IFDEF DELPHI}, wrIOCompletion {$ENDIF} :
      begin
        if Terminated then
          Break;
        DoTerminate;
      end;
    end;
  end;
  //DOterminated will be called.
end;

procedure TCustomServiceThread.Go;
begin
  FGo.SetEvent;
end;

{ TThreadServiceMessage_ProcessDuration }

function TThreadServiceMessage_ProcessDuration.GetAsStream: TMemoryStream;
begin
  Result :=  TMemoryStream.Create;
  WriteUInt64(Result, ServiceContext);
  WriteUInt64(Result, ThreadID);
  WriteUInt64(Result, Duration);
end;

procedure TThreadServiceMessage_ProcessDuration.LoadFromStream(
  aStream: TMemoryStream);
begin
  Assert(assigned(aStream));
  ServiceContext := ReadUInt64(aStream);
  ThreadID := ReadUInt64(aStream);
  Duration := ReadUInt64(aStream);
end;

{ TServiceTask }

procedure TServiceTask.Finalize;
begin
  //None here. Can be override if needed.
end;

procedure TServiceTask.Initialize;
begin
  //None here. Can be override if needed.
end;

{ TGSProtectedServiceStatus }

constructor TGSProtectedServiceStatus.Create(aStatus: TServiceStatus);
begin
  Inherited Create;
  FStatus := aStatus;
end;

function TGSProtectedServiceStatus.GetValue: TServiceStatus;
begin
  Lock;
  try
    result := FStatus;
  finally
    Unlock;
  end;
end;

procedure TGSProtectedServiceStatus.SetValue(const Value: TServiceStatus);
begin
  Lock;
  try
    FStatus := Value;
  finally
    Unlock;
  end;
end;

{ TGSProtectedServiceStatsPointer }

constructor TGSProtectedServiceStatsPointer.Create(
  aServiceStat: pTThreadServiceStat);
begin
  Inherited Create;
  FServiceStat := aServiceStat;
end;

function TGSProtectedServiceStatsPointer.GetValue: pTThreadServiceStat;
begin
  Lock;
  try
    Result := FServiceStat;
  finally
    Unlock
  end;
end;

function TGSProtectedServiceStatsPointer.GetValueCopy: TThreadServiceStat;
begin
  Lock;
  try
    Result := FServiceStat^;
  finally
    Unlock
  end;
end;

procedure TGSProtectedServiceStatsPointer.SetValue(
  const Value: pTThreadServiceStat);
begin
  Lock;
  try
    FServiceStat := Value;
  finally
    Unlock
  end;
end;

{$IFNDEF USE_GENERIC}

{ TServiceList }

procedure TServiceList.Add(aBusChannel: TCustomService);
begin
  ManagedAdd(aBusChannel);
end;

constructor TServiceList.Create;
begin
  Inherited Create(true);
end;

function TServiceList.GetCustomServiceItem(Index: Uint32): TCustomService;
begin
  Result:= TCustomService(FArray[Index]);
end;

procedure TServiceList.SetCustomServiceItem(Index: Uint32;
  const Value: TCustomService);
begin
  ManagedSet(Index,Value);
end;

{$ENDIF}

{ TGSProtectedServiceList }

constructor TGSProtectedServiceList.Create;
begin
  Inherited Create(TServiceList.Create);
end;


function TGSProtectedServiceList.Lock: TServiceList;
begin
  result := TServiceList(Inherited Lock)
end;

end.
