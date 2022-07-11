///-------------------------------------------------------------------------------
/// Title      : GS.LocalMemCached
/// Short Desc : Mimic MemCached server locally.
/// Source     : https://github.com/VincentGsell
/// Aim        : MemCched helper in a task : Ressouce shared in all thread easely.
/// Notes      :
/// Did you know memcached ?  https://memcached.org/
///
/// MemCached is a client server program : You have to get the memcache server to get it work.
/// This one (GS.localmemcached) is the same stuff but inside a process.
/// you can share easely string or stream oriented data between all your thread ! It is a reasonably fast and light solution
/// to achieve an simple ressource sharing.
///
/// Design Note : We decided to inherited from TBus for performance reason : The Bus thread dispatch the message AND, in this version, process
/// the memcached duty (all in the same thread) : It could take a certain amount of time, during it, none other message is dispatched !
/// --> In a performance way, It is not a realy good idea to use this TLocalMemcached as a local memcached AND as a Bus.
/// Take care to use a pure TBus for communication, and when wou specilize a TBus,
/// such here, keep in mind to take care about the process cost by the inside processing event.
unit GS.LocalMemcached;

{$I GSCore.inc}

interface

Uses
{$IFDEF FPC}
  Classes,
  SysUtils,
  SyncObjs,
{$ELSE}
  System.Classes,
  System.SysUtils,
  System.SyncObjs,
  System.Threading,
{$ENDIF}
  GS.Common,
  GS.Bus,
  GS.Threads,
  GS.Stream,
  GS.Reference,
  GS.Reference.Persister,
  GS.Reference.Persister.SingleFile;


Const
  CST_CHANNELPROCESS = 'MemcachedProcess';
  CST_CHANNELOAD = 'MemcachedProcessLoad';
Type

TLocalMCOrder = (lmoUnknown, lmoGET, lmoSET);
TLocalMCtype = (lmtInknown, lmtString, lmtStream);

TOnWarmEvent = Procedure(Sender : TObject; PercentProgress : Double) of Object;
TLocalMemcached = Class(GS.Bus.TBus)
private
  FInternalDataCS : TCriticalSection;
  FInternalData : TofReference;
  FFileName: TGSProtectedString;
  FOnWarm: TOnWarmEvent;
  FOnWarmTrigEvery : Integer;
  FOnReady: TNotifyEvent;
  FReferenceClass: TofReferenceClass;
  function GetItemCount: Int64;
  function GetFileName: String;
  procedure SetFileName(const Value: String);
  function GetInMemory: Boolean;
  function GetIsOpen: Boolean;
  procedure SetReferenceClass(const Value: TofReferenceClass);
Protected
 //This event will be trig when channel will receive a message. (See constructor)
 Procedure InternalEventOnChannelBeforeDeliverMessage(Var aMessage : TBusEnvelop); Virtual;
 Procedure InternalEventLoadData(Var aMessage : TBusEnvelop); Virtual;
 Procedure InternalEventUnLoad(Var aMessage : TBusEnvelop); Virtual;

 Procedure InternalOnWarmLoad(Sender : TObject; PercentProgress : Double);
 Procedure InternalOnReady;
Public
  Constructor Create; Virtual;
  Destructor Destroy; Override;

  Function GetCVSSnapShot : String; Overload;
  Function GetCVSSnapShot(aFrom, aTo : Int64) : String; Overload;
  Function GetCVSSnapShotAsString(aFrom, aTo : Int64) : String;

  Procedure Open;
  Procedure Close;

  Property ItemCount : Int64 read GetItemCount;
  Property FileName : String read GetFileName Write SetFileName;

  //All fellowing event are Threaded event : Use syncho or queue in other thread such as GUI.
  Property OnWarm : TOnWarmEvent read FOnWarm Write FonWarm;
  Property OnReady : TNotifyEvent read FOnReady Write FOnReady;

  //Memcached Memory only (no write on disk) - Put FileName to empty string ("") to achieve.
  Property InMemory : Boolean read GetInMemory;
  Property IsOpen : Boolean read GetIsOpen;
  Property PersisterClass : TofReferenceClass read FReferenceClass Write SetReferenceClass;
End;


TLocalMemcachedClient = Class
Private
  FMaster : TLocalMemcached; //pointer.
  FClient : TBusClientReader;
  FInternalResponseChannel : String;
  FResponseString : string;
  FLastCallForType : TLocalMCtype;
  FLastCallOrder : TLocalMCOrder;
  FResponseStream : TMemoryStream;
  //This object in not really itended to be share by different thread.
  //But it can theorically. Twpycal use is to build a client for each thread, or x client in each thread.
  FResponseStringProtection : TCriticalSection;

  Procedure InternalBusMemCachedProcessResponse(Sender : TBusSystem; aReader : TBusClientReader; Var Packet : TBusEnvelop);

  Procedure InternalWaitSendAndReceive;

  Function BuildCmdSet(aKey : String; aValue : string) : TMemoryStream; Overload;
  Function BuildCmdSet(aKey : String; aValue : TmemoryStream) : TMemoryStream; Overload;
  Function BuildCmdGetString(aKey : String) : TMemoryStream; Virtual;
  Function BuildCmdGetStream(aKey : String) : TMemoryStream; Virtual;

Public
  Constructor Create(aMemcached : TLocalMemcached); Reintroduce;
  Destructor Destroy; Override;

  Procedure SetValue(Key : String; Value : String); Overload;
  Function GetValue(Key : String) : String; Overload;

  Procedure SetValue(Key : String; Value : TMemoryStream); Overload;
  Procedure GetValue(Key : String; Var aStream : TMemoryStream); Overload;
End;


implementation

{ TLocalMemcached }

procedure TLocalMemcached.Close;
begin
  Open; // ;) -> Open and close is, behind the scene, just a swith. Open twice is like open/Close
end;

constructor TLocalMemcached.create;
begin
  Inherited Create;
  FReferenceClass := TofReferenceSingleFile;
  FOnWarm := nil;
  FOnWarmTrigEvery := 0;
  FFileName := TGSProtectedString.Create('LocalMemCached.data');
  FInternalDataCS := TCriticalSection.Create;
  //Set event on our channel : This event let us a chance to modify the message.
  //Step 1 :  Client --- Send message --> Bus [Event executed IN Bus thread.];
  ChannelSetOnBeforeDeliverMessageEvent(CST_CHANNELPROCESS, InternalEventOnChannelBeforeDeliverMessage);
  ChannelSetOnBeforeDeliverMessageEvent(CST_CHANNELOAD, InternalEventLoadData);
  //Step 2 :  Client ---> Call Bus.ProcessMessage(...) : We deliver the new message throught InternalBusMemCachedProcessResponse
end;

destructor TLocalMemcached.Destroy;
begin
  FreeAndNil(FInternalData);
  FreeAndNil(FInternalDataCS);
  FreeAndNil(FFileName);
  inherited;
end;

function TLocalMemcached.GetCVSSnapShot: String;
var i : integer;
begin
  FInternalDataCS.Acquire;
  Result := 'number;key;type';
  try
    if Not(Assigned(FInternalData)) then
    begin
      Exit;
    end;
    for I := 0 to FInternalData.EntryCount-1 do
    begin
      result := result +
                sLineBreak + IntToStr(i) +
                ';' + FInternalData.KeyByIndex[i]+';'+Cst_ContentTypeStr[Ord(FInternalData.DataTypeByIndex[i])];
    end;
  finally
    FInternalDataCS.Release;
  end;
end;

function TLocalMemcached.GetCVSSnapShot(aFrom, aTo: Int64): String;
var i : integer;
begin
  FInternalDataCS.Acquire;
  Result := 'number;key;type';
  try
    if Not(Assigned(FInternalData)) then
    begin
      Exit;
    end;
    for I := aFrom to aTo do
    begin
      if (i < FInternalData.EntryCount-1) then
      begin
        result := result +
                  sLineBreak + IntToStr(i) +
                  ';' + FInternalData.KeyByIndex[i]+';'+Cst_ContentTypeStr[Ord(FInternalData.DataTypeByIndex[i])];
      end;
    end;
  finally
    FInternalDataCS.Release;
  end;
end;

function TLocalMemcached.GetFileName: String;
begin
  Result := FFileName.Value;
end;

function TLocalMemcached.GetInMemory: Boolean;
begin
  Result := False;
  FInternalDataCS.Acquire;
  try
    if Not(Assigned(FInternalData)) then
    begin
      Exit;
    end;
    if FInternalData is TofReferenceSingleFile then
    begin
      Result := TofReferenceSingleFile(FInternalData).InMemory;
    end;
  finally
    FInternalDataCS.Release;
  end;
end;

function TLocalMemcached.GetIsOpen: Boolean;
begin
  result:= Not(Assigned(FInternalData));
end;

function TLocalMemcached.GetItemCount: Int64;
begin
  FInternalDataCS.Acquire;
  try
    if Not(Assigned(FInternalData)) then
    begin
      Result := 0;
      Exit;
    end;
    Result := FInternalData.EntryCount;
  finally
    FInternalDataCS.Release;
  end;
end;

procedure TLocalMemcached.InternalEventLoadData(var aMessage: TBusEnvelop);
begin
  ChannelSetOnBeforeDeliverMessageEvent(CST_CHANNELOAD,InternalEventUnLoad); //Switch the event to Unlod for next message (It is a "Switch").
  FInternalDataCS.Acquire;
  try
    if FReferenceClass = TofReferenceSingleFile then
      FInternalData := TofReferenceSingleFile.Create
    else
      raise Exception.Create('Error Message');

    if FInternalData is TofReferenceSingleFile then
    begin
      TofReferenceSingleFile(FInternalData).FileName := FFileName.Value;
      TofReferenceSingleFile(FInternalData).TypeConversionAllowedWriteTime := True; //Case by default, but specified in order to be explicit.
      TofReferenceSingleFile(FInternalData).OnInitialLoading := InternalOnWarmLoad;
    end;

    FInternalData.Open;

  Finally
    FInternalDataCS.Release;
  end;
  InternalOnReady;
end;

procedure TLocalMemcached.InternalEventOnChannelBeforeDeliverMessage(
  var aMessage: TBusEnvelop);
  var lMsg : TMemoryStream;
      lcmd : TLocalMCOrder;
      lkey : string;
      ltype : TLocalMCtype;
      lstream : TMemoryStream;
      ltemp : tofContentType;
begin
  lmsg := aMessage.ContentMessage.AsStream;
  SetLength(aMessage.ContentMessage.Buffer,0);
  lmsg.Position := 0;
  lcmd := TLocalMCOrder(ReadByte(lmsg));
  ltype := TLocalMCtype(ReadByte(lMsg));
  lkey := ReadString(lMsg);

  FInternalDataCS.Acquire;
  try
    case lcmd of
      TLocalMCOrder.lmoUnknown: ;
      TLocalMCOrder.lmoGET:
      begin
        if FInternalData.IsKeyExists(lkey) then
        begin
          //Value exists.
          ltemp := FInternalData.DataType[lkey];

          case ltemp of
            ctUnknow: ;
            ctString:
            begin
              case ltype of
                TLocalMCtype.lmtInknown: ;
                TLocalMCtype.lmtString:
                begin
                  aMessage.ContentMessage.FromString(FInternalData.GetEntryAsString(lkey));
                end;
                TLocalMCtype.lmtStream:
                begin
                  //Convert : Send the string in a stream.
                  lstream := TMemoryStream.Create;
                  try
                    WriteString(lStream,FInternalData.GetEntryAsString(lkey));
                    lstream.Position := 0;
                    aMessage.ContentMessage.FromStream(lstream);
                  finally
                    FreeAndNil(lstream);
                  end;
                end
              end;
            end;
            ctStream:
            begin
              case ltype of
                TLocalMCtype.lmtInknown: ;
                TLocalMCtype.lmtString:
                begin
                  //Convert : Stream and string are not compatible : Send string with explicit content !
                  lstream := FInternalData.GetEntryAsStream(lkey);
                  try
                    aMessage.ContentMessage.FromString('DataStreamLength='+IntToStr(lstream.Size));
                  finally
                    FreeAndNil(lstream)
                  end;
                end;
                TLocalMCtype.lmtStream:
                begin
                  lstream := FInternalData.GetEntryAsStream(lkey);
                  try
                    lStream.Position := 0;
                    aMessage.ContentMessage.FromStream(lStream);
                  finally
                    FreeAndNil(lStream);
                  end;
                end
              end;
            end;
            ctInteger: ;
            ctUINT32: ;
            ctDouble: ;
          end;
        end
        else
        begin
          //Value not exists : In a get case, we create empty shell.
          case ltype of
            TLocalMCtype.lmtInknown: ;
            TLocalMCtype.lmtString:
            begin
              aMessage.ContentMessage.FromString(EmptyStr);
            end;
            TLocalMCtype.lmtStream:
            begin
              lstream := TMemoryStream.Create;
              try
                aMessage.ContentMessage.FromStream(lStream); //Empty : normal.
              finally
                FreeAndNil(lStream);
              end;
            end
          end;
        end;
      end;
      TLocalMCOrder.lmoSET:
      begin;
        if FInternalData.IsKeyExists(lkey) then
        begin
          //Value exists.
          ltemp := FInternalData.DataType[lkey];
          case ltemp of
            ctUnknow: ;
            ctString:
            begin
              case ltype of
                TLocalMCtype.lmtInknown: ;
                TLocalMCtype.lmtString:
                begin
                  FInternalData.WriteEntryString(lkey,ReadString(lMsg));
                end;
                TLocalMCtype.lmtStream:
                begin
                  //Conversion
                  lstream := TMemoryStream.Create;
                  try
                    WriteStream(lstream,lMsg);
                    lstream.Position := 0;
                    FInternalData.WriteEntryStream(lkey,lstream);
                  finally
                    FreeAndNil(lstream);
                  end;
                end
              end;
            end;
            ctStream:
            begin
              case ltype of
                TLocalMCtype.lmtInknown: ;
                TLocalMCtype.lmtString:
                begin
                  //Conversion
                  FInternalData.WriteEntryString(lkey,ReadString(lMsg));
                end;
                TLocalMCtype.lmtStream:
                begin
                  lstream := TMemoryStream.Create;
                  try
                    WriteStream(lstream,lMsg);
                    lstream.Position := 0;
                    FInternalData.WriteEntryStream(lkey,lStream);
                  finally
                    FreeAndNil(lStream);
                  end;
                end;
              end;
            end;
            ctInteger: ;
            ctUINT32: ;
            ctDouble: ;
          end;
        end
        else
        begin
          //Value not exists : Create.
          case ltype of
            TLocalMCtype.lmtInknown: ;
            TLocalMCtype.lmtString:
            begin
              FInternalData.WriteEntryString(lkey,ReadString(lMsg));
            end;
            TLocalMCtype.lmtStream:
            begin
              lstream := TMemoryStream.Create;
              try
                WriteStream(lstream,lMsg);
                lstream.Position := 0;
                FInternalData.WriteEntryStream(lkey,lstream)
              finally
                FreeAndNil(lstream);
              end;;
            end;
          end;
        end;
      end;
    end;
  finally
    FreeAndNil(lMsg);
    FInternalDataCS.Release;
    //Now, we send this content on the asked Response channel.
    Send(aMessage.ContentMessage,aMessage.ResponseChannel);
  end;
end;

procedure TLocalMemcached.InternalEventUnLoad(var aMessage: TBusEnvelop);
begin
  ChannelSetOnBeforeDeliverMessageEvent(CST_CHANNELOAD,InternalEventLoadData); //Switch the event to Unlod for next message (It is a "Switch").
  FInternalDataCS.Acquire;
  try
    FreeAndNil(FInternalData);
  Finally
    FInternalDataCS.Release;
  end;
  InternalOnReady;
end;

procedure TLocalMemcached.InternalOnReady;
begin
  if Assigned(FOnReady) then
  begin
    FOnReady(Self);
  end;
end;

procedure TLocalMemcached.InternalOnWarmLoad(Sender: TObject;
  PercentProgress: Double);
begin
  if Assigned(FOnWarm) then
  begin
    if (FOnWarmTrigEvery = 0) or (PercentProgress = 100.00) then
    begin
      FOnWarm(Self,PercentProgress);
      FOnWarmTrigEvery := 10000;
    end;
    Dec(FOnWarmTrigEvery);
  end;
end;

procedure TLocalMemcached.Open;
var am : TBusMessage;
begin
  FInternalDataCS.Acquire;
  try
    if not(Assigned(FInternalData)) then
    begin
      {$IFNDEF FPC}
      if Not(Started) then
      {$ENDIF}
      begin
        Start;
      end;
      Send(am,CST_CHANNELOAD);
    end;
  finally
    FInternalDataCS.Release;
  end;
end;

procedure TLocalMemcached.SetFileName(const Value: String);
begin
  if not(Assigned(FInternalData)) then
  begin
    FFileName.Value := Value;
  end
  else
  begin
    raise Exception.Create(ClassName + ' FileName cannot be set on active data');
  end;
end;


procedure TLocalMemcached.SetReferenceClass(const Value: TofReferenceClass);
begin
  if (IsOpen) then
  begin
    raise Exception.Create('To change persistence class, component must be close.');
  end;
  FReferenceClass := Value
end;

function TLocalMemcached.GetCVSSnapShotAsString(aFrom, aTo: Int64): String;
var i : integer;
    m : TMemoryStream;
begin
  FInternalDataCS.Acquire;
  Result := 'number;key;type;Value';
  try
    if Not(Assigned(FInternalData)) then
    begin
      Exit;
    end;
    for I := aFrom to aTo do
    begin
      if (i < FInternalData.EntryCount) then
      begin

        case FInternalData.DataTypeByIndex[i] of
          ctStream :
          begin
            m := FInternalData.GetEntryAsStream(FInternalData.KeyByIndex[i]);
            try
              result := result +
                        sLineBreak + IntToStr(i) +
                        ';' + FInternalData.KeyByIndex[i]+
                        ';'+Cst_ContentTypeStr[Ord(FInternalData.DataTypeByIndex[i])]+
                        ';Stream size : '+IntTostr(m.Size);
            finally
              FreeAndNil(m);
            end;
          end;
          ctString :
          begin
            result := result +
                      sLineBreak + IntToStr(i) +
                        ';' + FInternalData.KeyByIndex[i]+
                        ';'+Cst_ContentTypeStr[Ord(FInternalData.DataTypeByIndex[i])]+
                      ';'+FInternalData.GetEntryAsString(FInternalData.KeyByIndex[i]);
          end;
          else
          begin
            raise Exception.Create(ClassName + '.GetCVSSnapShotAsString : Type not supported.');
          end;
        end;
      end;
    end;
  finally
    FInternalDataCS.Release;
  end;
end;

{ TLocalMemcachedClient }

function TLocalMemcachedClient.BuildCmdGetStream(aKey: String): TMemoryStream;
begin
  Assert(length(aKey)>0);
  Result := TMemoryStream.Create;
  WriteByte(Result,Byte(TLocalMCOrder.lmoGET));
  WriteByte(Result,Byte(TLocalMCType.lmtStream));
  WriteString(Result,aKey);
  Result.Position :=0;
end;

function TLocalMemcachedClient.BuildCmdGetString(aKey: String): TMemoryStream;
begin
  Assert(length(aKey)>0);
  Result := TMemoryStream.Create;
  WriteByte(Result,Byte(TLocalMCOrder.lmoGET));
  WriteByte(Result,Byte(TLocalMCType.lmtString));
  WriteString(Result,aKey);
  Result.Position :=0;
end;

function TLocalMemcachedClient.BuildCmdSet(aKey: String;
  aValue: TmemoryStream): TMemoryStream;
begin
  Assert(Assigned(aValue));
  Assert(length(aKey)>0);
  Result := TMemoryStream.Create;
  WriteByte(Result,Byte(TLocalMCOrder.lmoSET));
  WriteByte(Result,Byte(TLocalMCType.lmtStream));
  WriteString(Result,aKey);
  aValue.Position := 0;
  WriteStream(Result,aValue);
  Result.Position :=0;
end;

function TLocalMemcachedClient.BuildCmdSet(aKey, aValue: string): TMemoryStream;
begin
  Assert(length(aKey)>0);
  Result := TMemoryStream.Create;
  WriteByte(Result,Byte(TLocalMCOrder.lmoSET));
  WriteByte(Result,Byte(TLocalMCType.lmtString));
  WriteString(Result,aKey);
  WriteString(Result,aValue);
  Result.Position :=0;
end;

constructor TLocalMemcachedClient.Create(aMemcached : TLocalMemcached);
begin
  Inherited Create;
  Assert(Assigned(aMemcached));
  FMaster := aMemCached;
  FResponseStringProtection := TCriticalSection.Create;
  FResponseStream := TMemoryStream.Create;
  FInternalResponseChannel := CST_CHANNELPROCESS+IntToStr(UINT64(Self));

  FClient := aMemcached.Subscribe( FInternalResponseChannel,
                                   InternalBusMemCachedProcessResponse);
  FClient.Event := aMemcached.GetNewEvent;
end;

destructor TLocalMemcachedClient.Destroy;
begin
  FMaster.UnSubscribe(FClient);
  FreeAndNil(FResponseStringProtection);
  FreeAndNil(FResponseStream);
  FreeAndNil(FClient);
  inherited;
end;

function TLocalMemcachedClient.GetValue(Key: String): String;
var aM : TBusMessage;
    lss : TMemoryStream;
begin
  FResponseStringProtection.Acquire;
  try
    FLastCallOrder := TLocalMCOrder.lmoGET;
    FLastCallForType := TLocalMCtype.lmtString;
    lss := BuildCmdGetString(Key);
    try
      am.FromStream(lss);
    finally
      FreeAndNil(lss);
    end;
    FMaster.Send(aM,CST_CHANNELPROCESS,EmptyStr,FInternalResponseChannel);
    InternalWaitSendAndReceive;
    Result := FResponseString;
  finally
    FResponseStringProtection.Leave;
  end;
end;


Procedure TLocalMemcachedClient.GetValue(Key: String;
  var aStream: TMemoryStream);
var aM : TBusMessage;
    lss : TMemoryStream;
begin
  Assert(Assigned(aStream));

  FResponseStringProtection.Acquire;
  try
    FLastCallOrder := TLocalMCOrder.lmoGET;
    FLastCallForType := TLocalMCtype.lmtStream;
    lss := BuildCmdGetStream(Key);
    try
      am.FromStream(lss);
    finally
      FreeAndNil(lss);
    end;
    FMaster.Send(aM,CST_CHANNELPROCESS,EmptyStr,FInternalResponseChannel);
    InternalWaitSendAndReceive;
    aStream.LoadFromStream(FResponseStream);
    aStream.Position := 0;
  finally
    FResponseStringProtection.Leave;
  end;
end;

procedure TLocalMemcachedClient.InternalBusMemCachedProcessResponse(Sender : TBusSystem; aReader : TBusClientReader; Var Packet : TBusEnvelop);
var lStream : TMemoryStream;
begin
  if FLastCallOrder = TLocalMCOrder.lmoSET then
    Exit;

  if FLastCallForType = TLocalMCType.lmtString then
    FResponseString := Packet.ContentMessage.AsString
  else
  if FLastCallForType = TLocalMCType.lmtStream then
  begin
    lStream := Packet.ContentMessage.AsStream; //Copy
    try
      lStream.Position := 0;
      FResponseStream.Clear;
      FResponseStream.LoadFromStream(lStream);
      FResponseStream.Position := 0;
    finally
      FreeAndNil(lStream);
    end;
  end
  else
  begin
    raise Exception.Create('Error Message');
  end;
end;

procedure TLocalMemcachedClient.InternalWaitSendAndReceive;
begin
  while Not(FMaster.Terminated) do
  begin
    Case FClient.Event.WaitFor(CST_BUSTIMER) of
    wrSignaled :
    begin
      BusProcessMessages(FClient);
      Break;
    end;
    wrTimeout :
    begin
      //log..
    end;
    End;
  end;
end;

Procedure TLocalMemcachedClient.SetValue(Key, Value: String);
var aM : TBusMessage;
    lss : TMemoryStream;
begin
  FResponseStringProtection.Acquire;
  try
    FLastCallOrder := TLocalMCOrder.lmoSET;
    FLastCallForType := TLocalMCtype.lmtString;
    lss := BuildCmdSet(Key,Value);
    try
      am.FromStream(lss);
    finally
      FreeAndNil(lss);
    end;
    FMaster.Send(aM,CST_CHANNELPROCESS,EmptyStr,FInternalResponseChannel);
    InternalWaitSendAndReceive;
  finally
    FResponseStringProtection.Leave;
  end;
end;

Procedure TLocalMemcachedClient.SetValue(Key: String; Value: TMemoryStream);
var aM : TBusMessage;
    lss : TMemoryStream;
begin
  Assert(assigned(Value));
  FResponseStringProtection.Acquire;
  try
    FLastCallOrder := TLocalMCOrder.lmoSET;
    FLastCallForType := TLocalMCtype.lmtStream;
    Value.Position := 0;
    lss := BuildCmdSet(Key,Value);
    try
      am.FromStream(lss);
    finally
      FreeAndNil(lss);
    end;
    FMaster.Send(aM,CST_CHANNELPROCESS,EmptyStr,FInternalResponseChannel);
    InternalWaitSendAndReceive;
  finally
    FResponseStringProtection.Leave;
  end;
end;


end.
