///-------------------------------------------------------------------------------
/// Title      : GS.Bus
/// Short Desc : "central" threaded in memory mini bus.
/// Source     : https://github.com/VincentGsell
/// Aim        : - This Bus implementation provide classic Subscription/Write
///                model to a "channel" (Queue or Topic).
///              - It permit Inter thread communication safely and easely.
///              - It work in its own thread for distribution.
///              - One of the basic usage should to separate GUI and
///                Model/Controler, and let a chance to swap easely from
///                a standalone app to a middleware (network) one.
///
/// History
/// 20170924 - VGS - Create
/// 20180524 - VGS - Added Wilcard "message dispatch" capabilities
///                  (i.e a message launch on "ABC\DEF\GHI" channel, will be
///                  delivered on "ABC\DEF\GHI", "ABC\DEF" and "ABC".
/// 20181124 - VGS - Added Data Repo side feature, in order to exchange easely
///                  stream data between client thread in a simple way.
///                  this is an exemple to how implement rpc style call in
///                  a bus environmment.
/// 20181204 - VGS - Conditional directive on Generic. (Reason : Optimization,
///                  FPC Embeded compatibility, portability on
///                  other Pascal compiler (jsFPC...)
/// 20190211 - VGS - Adding Exception handing option.
///                  Massive architecture redesign : Channel centric
///                  (Could put all channel processing into another thread).
///                  Generic and base list refacto and relocation.
/// 20190220 - VGS - Change channel Creation option. Improve Queue.
///                - Introduce Message count limitation per channel.
///                  -> this could be usefull for heavy trafic source, such as object position data.
/// 20190403 - VGS - Remove Wildcard features. After thinking, It is an Applevel
///                  feature, as it break completely an app if you enabled it.
///                  Moreover, it is on root bus level, it should be on channel level.
///                  So it a bit "tricky" and too "app analytics level" to be confortable here.
/// 20190606 - VGS - Introduce AppFilter for channel privacy : Two client with same AppFilter can
///                  communicate directly.
///                - Introduce ClientId, to follow the author of a message on a reader point of view.
///                - Introduce ChannelEchoEnabled (Default:False) : To allow or not "echo" effect.
/// 20190717 - VGS - remove FWaitingMessage list, this liste permited 0 wait on sending message, but
///                  introduce memory problem when message producer send an heavy amount of messages.
///                  those messages were not processed fast enough. With this new version,
//                   since it is delivered directly to relevant channel, there are no more  memory problem.
///-------------------------------------------------------------------------------
unit GS.Bus;

{$I GSCore.inc}

interface

Uses
{$IFDEF FPC}
  Classes,
  SysUtils,
  {$IFDEF USE_GENERIC}
  Generics.Collections, //Starting 3.1.1 only.
  {$ENDIF}
  SyncObjs,
{$ELSE}
  System.Classes,
  System.SysUtils,
  {$IFDEF USE_GENERIC}
  System.Generics.Collections,
  {$ENDIF}
  System.SyncObjs,
  System.Threading,
{$ENDIF}
  GS.Stream,
  GS.Common,
  GS.CPUUsage,
  GS.Threads.Pool;

Const
  CST_BUSTIMER = 250; //MilliSec.
  CST_THREAD_COOLDOWN = 1;
  CST_DATAREPO_DELIMITER = ';';


Type
TBusSystem = Class;
TBusChannel = Class;
TBusClient = Class;
TBusClientReader = Class;
TBusChannelList = Class;
TBusClientReaderList = Class;


//Write in it whatever you want.
TBusMessage = Packed Record
  Buffer : TBytes;

  procedure FromDouble(aD : Double);
  function AsDouble : Double;
  Procedure FromString(const aText : String);
  Function AsString : String;
  Procedure FromStream(aStream : TStream);
  Procedure ToStream(var aTargetStream : TStream); //No need transfert's stream.
  Function AsStream : TMemoryStream; //(!) Got new object.
  Procedure FromByte(aByte : Byte);
  Function AsByte : Byte;
  Function Size : Uint64;
End;
pTBusMessage = ^TBusMessage;

//This is the envelop for a message to the bus
TBusEnvelop = packed Record
  EnvelopId : UINT64;
  TargetChannel : String;
  ResponseChannel : String;
  ContentMessage : TBusMessage;
  AdditionalData : String;
  Persistent : Boolean;
  ClientSourceId : String;
  AppFilter : String;
  CreateTag : Uint64;
End;

TBusMessageNotify = Procedure(Sender : TBusSystem; aReader : TBusClientReader; Var Packet : TBusEnvelop) Of Object;

PTBusEnvelop = ^TBusEnvelop;
{$IFDEF USE_GENERIC}
TList_PTBusEnvelop = TList<PTBusEnvelop>;
{$ELSE}

TList_PTBusEnvelop = Class
Private
  FArray : Array of PTBusEnvelop;
  FIndex : UInt32;
  FInitialized : Boolean;

  function GetPTBusEnvelop(Index: Uint32): PTBusEnvelop;
  function GetPTBusEnvelopCount: Uint32;
  procedure SetPTBusEnvelop(Index: Uint32; const Value: PTBusEnvelop);
Public
  constructor Create; Virtual;
  procedure Add(aPTBusEnvelop : PTBusEnvelop);
  procedure Clear;
  property Items[Index : Uint32] : PTBusEnvelop read GetPTBusEnvelop Write SetPTBusEnvelop; default;
  property Count : Uint32 read GetPTBusEnvelopCount;
end;
{$ENDIF}

TBusEnvelopList = Class
Private
  FList : TList_PTBusEnvelop;
  FLock : TCriticalSection;
Public
  Constructor Create; Virtual;
  Destructor Destroy; Override;


  function Lock : TList_PTBusEnvelop;
  function TryLock(var aList : TList_PTBusEnvelop) : boolean;
  procedure Unlock;

  Property Items : TList_PTBusEnvelop read FList; //Direct Access, beware (!).
end;

TBusChannelAdditionalInformation = class
private
  FMasterChannel: TBusChannel;
Public
  Constructor Create(aMasterChannel : TBusChannel); reintroduce; Virtual;
  Property MasterChannel : TBusChannel read FMasterChannel;
end;

//Restricted : Manual mode. Only user having same keypass where user give what "session can read or write
TBusChannelPrivacy = Class(TBusChannelAdditionalInformation)
private
  FKeyPass: String;
Public
  Constructor Create; reintroduce;
  property KeyPass : String read FKeyPass Write FKeyPass;
End;

TBusChannelBehaviourTopic = Class(TBusChannelAdditionalInformation)
Public
End;

TBusChannelBehaviourQueueSpecific = (cbqQueueFaultTolerant, cbqQueueDistributed);
///cbqQueueFaultTolerant :
///   - Serve first client, as long as it is connected, if this client disconnect, server the second one and so on.
///cbqQueueDistributed :
///   - Serve each client one after the other.
TBusChannelBehaviourQueue = Class(TBusChannelAdditionalInformation)
private
  FQueueBehaviour: TBusChannelBehaviourQueueSpecific;
  FLastClientIndexServed: UInt32;
Public
  Constructor Create(aMasterChannel : TBusChannel); Override;
  Property QueueBehaviour : TBusChannelBehaviourQueueSpecific read FQueueBehaviour write FQueueBehaviour;
  Property LastClientIndexServed : UInt32 read FLastClientIndexServed write FLastClientIndexServed;
End;

TBusChannelBehaviour = (bcbTopic, bcbQueue);

TBusChannelData = class
protected
  FLock : TCriticalSection;
  FChannel: String;
  FReceivedMessageCount : Int64;
  FConsumedMessageCount : Int64;
  FDeliveredMessageCount : Int64;

  FMessageInThisChannelWillBeSetAsPersistent : Boolean;
public
  Procedure Lock;
  Procedure Unlock;

  Constructor Create; Virtual;
  Destructor Destroy; Override;

end;

//WARNING : All TBUSChannel Event (included event) is processed within a thread.
TOnBusChannelBeforeDeliverMessage = Procedure(Var aMessage : TBusEnvelop) of Object;

//TBusChannel object manage all channel and client message distribution.
//Optionaly, all the channel centric processing could be sub threaded.
TBusChannel = Class
private
Protected
  FProtect : TCriticalSection;
  FDataProtect : TCriticalSection;
  FBusChannelData : TBusChannelData;
  FOnBeforeDeliverMessage: TOnBusChannelBeforeDeliverMessage;
  FSubscribters : TBusClientReaderList;
  FBehaviour: TBusChannelBehaviour;
  FBehaviourInfo: TBusChannelAdditionalInformation;

  //Temporary message list.
  FProcessingMessages : TBusEnvelopList;

  //Here will be save all the message marked as persitent.
  //they will be delivered to new subscripters as is.
  FPersistentMessages : TBusEnvelopList;

  FEventDeliveryDesactivateOnException: Boolean;
  FEventDelivery_ExceptionEnabled: Boolean;
  FChannelMessageCountLimitation : Integer;
  FChannelEchoEnabled: Boolean;
  FMaster : TBusSystem;

  function GetConsumedMessageCount: Int64;
  function GetReceivedMessageCount: Int64;
  function GetDeliveredMessageCount: Int64;

  procedure SetChannelBehaviour(const Value: TBusChannelBehaviour);
  function GetPersistentMessageCount: Int64;
  function GetMessageInThisChannelWillBeSetAsPersistent: Boolean;
  procedure SettMessageInThisChannelWillBeSetAsPersistent(const Value: Boolean);
  function GetChannel: string;
  function GetChannelMessageCountLimitation: Integer;
  procedure SetChannelMessageCountLimitation(const Value: Integer);
  function GetCurrentSubscribterCount: Int64;

  Function ProcessingMessageLock : TList_PTBusEnvelop;
  Procedure ProcessingMessageMessageUnlock;
  Function PersistentMessageLock : TList_PTBusEnvelop;
  Procedure PersistentMessageUnlock;

  procedure IncReceivedMessageCount;
  procedure IncConsumedMessageCount;
  procedure IncDeliveredMessageCount;
Public
  procedure Lock;
  procedure Unlock;

  Constructor Create(aBus : TBusSystem; aChannelName : string); Reintroduce;
  Destructor Destroy; Override;

  Procedure DoProcessing; //All channel centric processing stuff is doing here, and could be sub threaded.

  Property ChannelName : string read GetChannel;

  Property CurrentSubscribterCount : Int64 read GetCurrentSubscribterCount;

  Property ReceivedMessageCount : Int64 read GetReceivedMessageCount;
  Property ConsumedMessageCount : Int64 read GetConsumedMessageCount;
  Property DeliveredMessageCount : Int64 read GetDeliveredMessageCount;

  Property PersistentMessageCount : Int64 read GetPersistentMessageCount;

  Property Subscribters : TBusClientReaderList read FSubscribters;

  Property ChannelBehaviour : TBusChannelBehaviour read FBehaviour write SetChannelBehaviour;
  Property ChannelBehaviourInfo : TBusChannelAdditionalInformation read FBehaviourInfo;

  //ChannelEchoEnabled : If false, there are no more "echo" effect
  //(i.e. client's subscribted to "x" chan will not receive message they have just sent on the "x" chan.)
  // -> It used ClientID propoerty of ClientReader and ClientWriter.
  Property ChannelEchoEnabled : Boolean read FChannelEchoEnabled Write FChannelEchoEnabled;

  //- This will "limit" the number of message in channel
  //(Only the newest is taking account if there is no more room)
  // 2 effects :
  //- With "0", this will completely desactivate the channel. No message will be processed (arriving is still detected).
  //- if more message than the limitation incoming, only the lastest will be take, in accordance of room available.
  //  --> On non-loaded system : It will decrease the Message "Burst" effect - i.e. many message incomming and possible saturation.
  //                             but you can have many message : Because the bus is fast, the number of pending message will be low.
  //                             And this parameter work on the channel's pending message count.
  //  --> On loaded system you'll obtain good result : You will be near of this number, because pending message will increase,
  //      this parameter will be very efficient in that case.
  property ChannelMessageCountLimitation : Integer read GetChannelMessageCountLimitation Write SetChannelMessageCountLimitation;

  Property MessageInThisChannelWillBeSetAsPersistent : Boolean read GetMessageInThisChannelWillBeSetAsPersistent
                                                               write SettMessageInThisChannelWillBeSetAsPersistent;


  Property OnBeforeDeliverMessage : TOnBusChannelBeforeDeliverMessage read FOnBeforeDeliverMessage Write FOnBeforeDeliverMessage;
  Property EventDelivery_DesactivateOnException : Boolean read FEventDeliveryDesactivateOnException Write FEventDeliveryDesactivateOnException;
  Property EventDelivery_ExceptionEnabled : Boolean read FEventDelivery_ExceptionEnabled Write FEventDelivery_ExceptionEnabled;
End;

//CLIENT SUBSCRIBTION OBJECT
TBusClient = Class abstract
private
Protected
  FBus : TBusSystem;
  function GetClientPendingMessageCount: Int64;
  function GetClientProcessMessageCount: Int64; Virtual; Abstract;
Public
  ClientMessageStack : TBusEnvelopList;
  Event : TEvent; //Pointer ! Use it for thread use only, *NOT* managed by bus.

  Constructor Create(aBus : TBusSystem); Virtual;
  Destructor Destroy; Override;

  Property PendingMessageCount : Int64 read GetClientPendingMessageCount;
  Property ProcessMessageCount : Int64 read GetClientProcessMessageCount;
  Property Bus : TBusSystem read FBus;
End;

//Used to "sign" message.
//Usage : - Determine who is writen from a receiver point of view (ID remain in envelop, not passkey).
//        - Permit to make the channel private : The channel.passkey must be identical with Writer.passkey to allow to the message to be processed.
//        - In cunjunction with TChannel.ChannelDuplex, you can avoid "echo" effect (Seet TChannel) and then build "one channel dupplex communication" over multi client.
TBusClientReader = Class(TBusClient)
private
Protected
  FProcessMessageCount : Int64;
  FCallBack: TBusMessageNotify;
  FChannel: String;
  FData: TObject;
  FClientBusID: String;
  FAppFilter: String;

  function GetClientProcessMessageCount: Int64; Override;
Public
  Procedure IncProcessMessageCount;

  //Not aimed to be call directly : Call by Bus.Subscribt.
  Constructor Create(aBus : TBusSystem; aChannelName : String; aCallBack : TBusMessageNotify); Reintroduce;

  Property ChannelListening : String read FChannel;
  Property CallBack : TBusMessageNotify read FCallBack write FCallBack;
  property Data : TObject read FData Write FData;
  property ClientBusID : String read FClientBusID write FClientBusID;
  property AppFilter : String read FAppFilter Write FAppFilter;
End;
PTBusClientReader = ^TBusClientReader;

TObjectDictionary_BusChannel = class(TObjectDictionary_StringObject)
  private
    function GetBusChannelItem(Index: UInt32): TBusChannel;
public
  procedure AddIfNotAlready(aKey : String; aBusChannel : TBusChannel);
  property Items[Index : UInt32] : TBusChannel read GetBusChannelItem; Default;
end;

TBusChannelList = Class
Private
  FList : TObjectDictionary_BusChannel;
  FListProcessing : TObjectDictionary_BusChannel;
  FLock : TCriticalSection;
  FMaster : TBusSystem;
Public
  Constructor Create(aBus : TBusSystem); reintroduce;
  Destructor Destroy; Override;

  //Flush all message in waiting, delete the channel.
  Procedure DeleteChannel(aChannelName : string);
  //Create or Set channel to achieve advanced behaviour (Memory Persitance, Queue)
  Procedure CreateOrSetChannel( aChannelName : String;
                                aChannelBehaviourType : TBusChannelBehaviour;
                                const aMessageWillBePersistent : Boolean = False;
                                const EchoEnabled : Boolean = True);
  //Set an event, which will trig when a message is delivered on this channel.
  //Warning : It it the thread of the bus whitch will process the event ! Keep it thread safe and beware
  //          to not take too many time in this event : Other message will not be dispached during this time.
  Procedure SetChannelOnBeforeDeliverEvent(aChannelName : String; aChannelProc : TOnBusChannelBeforeDeliverMessage);

  Function IsChannelExists(Const aChannelName : String) : Boolean;

  procedure Lock;
  function GetLockedList : TObjectDictionary_BusChannel;
  Procedure Unlock;

  //Multithread iterator : Given channel is locked.
  function GetLockChannel(index: Uint32; var channel : TBusChannel) : Boolean;
end;

{$IFDEF USE_GENERIC}
TList_TBusClientReader = TList<TBusClientReader>;
TObjectList_TEvent = TObjectList<TEvent>;
{$ELSE}
TList_TBusClientReader = Class(TList_ObjectArray)
  private
    function GetBusClientReaderItem(Index: Uint32): TBusClientReader;
    procedure SetBusClientReaderItem(Index: Uint32;
      const Value: TBusClientReader);
public
  Procedure Add(aBusClientReader : TBusClientReader);
  Property Items[Index : Uint32] : TBusClientReader read GetBusClientReaderItem Write SetBusClientReaderItem; Default;
End;

TObjectList_TEvent = Class(TList_ObjectArray)
private
  function GetEventItem(Index: Uint32): TEvent;
  procedure SetEventItem(Index: Uint32; const Value: TEvent);
Public
  constructor Create; Reintroduce;
  Procedure Add(aEvent : TEvent);
  Property Items[Index : Uint32] : TEvent read GetEventItem Write SetEventItem;
End;
{$ENDIF}

TBusClientReaderArray = Array of TBusClientReader;
TBusClientReaderList = Class
Private
  FList : TList_TBusClientReader;
  FListProcessing : TList_TBusClientReader;
  FLock : TCriticalSection;
Public
  Constructor Create; Virtual;
  Destructor Destroy; Override;

  Function ToArray : TBusClientReaderArray;

  Function IsChannelAlreadyRepresented(aChan : String) : Boolean;

  procedure Lock;
  function GetLockedList : TList_TBusClientReader;
  Procedure Unlock;
end;

TBusSystem = Class
Private
  FLockStat : TCriticalSection;
  FLockPropertyBasic : TCriticalSection;
  FEventListProtect : TCriticalSection;

  FTotalMessageSend : Int64;
  FTotalMessagePending : Int64;
  FTotalMessagePendingMemory : Int64;
  FTotalMessageProcessed : Int64;
  FTotalMessagePersistent : Int64;

  FCallBack_DisabledOnException : Boolean;
  FCallBack_ExceptionEnabled : Boolean;
  FLog_ChannelName : String;
  FLog_Enabled : Boolean;

  FInternalMessageIdGenerator : Int64;               //Message generator ID. Not used today (Just a incremental), certainly better system when needed.
  FWaitMessageList : TBusEnvelopList;                //Message pending. (Where clients post)
protected
  FChannels : TBusChannelList;                       //List of all channels.
private
  FDoWork : TEvent;                                  //Event signaled when "Send" is called : It start message processing (Execute).
  FAllBusSubscribters : TBusClientReaderList;              //Raw Subscripter list. Reference. : WARNING : In channel object, there is shortcut to content object. Keep it synchro.
  FDataRepo : TObjectDictionary_StringStream;
  FInternalEventList : TObjectList_TEvent;

  FIdle : Boolean;

  function GetStats: String;
  function GetCallBack_DisabledOnException: Boolean;
  function GetLog_ChannelName: String;
  function GetLog_Enabled: Boolean;
  procedure SetCallBack_DisableOnException(const Value: Boolean);
  procedure SetLog_ChannelName(const Value: String);
  procedure SetLog_Enabled(const Value: Boolean);
  function GetCallBack_ExceptionEnabled: Boolean;
  procedure SetCallBack_ExceptionEnabled(const Value: Boolean);
  function GetBusIdle: Boolean;
  function GetMessageWaitingCount: Uint64;
Protected

  Procedure BusShutDown; Virtual;

  //Data repo end point internal process.
  Procedure InternalDataRepoChannelCallBack(Var aMessage : TBusEnvelop);
Public
  constructor Create; overload; Virtual;
  Destructor Destroy; Override;

  //Call this only if you have to pulse inner logic of Bus : in a timer, in OnIdle
  //(For those 2, modify CST_TIMER Value to a very low value), or in a thread Execute (And here CST_TIMER is ok).
  //this will : consume awaiting message, dispatch messages folowing channel rules.
  //You have than to call ProcessMessages to consume dispatched message.
  Procedure BusExecute; Virtual;

  //WARNING : In current design, client is not owned by BusSystem : client responsability to free it.
  Function Subscribe(aChannelName : String; CallBack : TBusMessageNotify) : TBusClientReader;
  Function UnSubscribe(aClient : TBusClientReader) : Boolean;

  // Send a message on "aTargetChannel" channel.
  // Out : Id of message.
  // In : aMessage : The Message payload
  //      aTargetChannel : The channel where the message will be sent.
  //      aResponsechannel : Used for indicate on wich channel an (eventual) response must be sent.
  //      clientIdSignature : You can put a clientID here to indicate the message origin. Use with TChannel.ChannelEchoEnabled
  //      AppFilter : if the AppFilter in not empty (''), message will be delivered *only* to the client which have the same Appfilter.
  //                  If the AppFilter is empty, the nessage will be delivered to all cient, regardless to their own AppFilter.
  //      aSomeAdditionalData : this data will be carried in envelop, not in the message, usage on your own (used in keyvalue process, for example).
  //      IsPersistent : If True, the message will be marked as MemoryPersistent in the channel and will be delivered to new subscribter.
  Function Send( var aMessage : TBusMessage;
                 const aTargetChannel : String;
                 const aSomeAdditionalData : String = '';
                 const aResponseChannel : string = '';
                 const IsPersistent : Boolean = False;
                 const ClientIDSignature : String = '';
                 const AppFilter : String = '') : Int64;

  // Retrieve messages
  // In :  aClientReadeds : Client previously subscribted to channel.
  // out : Messages : List of envelop.
  Function Recv( const aClientReaders : Array of TBusClientReader;
                 const Messages : TBusEnvelopList) : UInt32; Overload;

  //This one with only one client : It will test the client only.
  //if the client have a system event, it will be used, else, as above, it will
  // be the FWorkOn system event (More intensive)
  Function Recv( const aClientReader : TBusClientReader;
                 const Messages : TBusEnvelopList;
                 const WaitForMessageWithTimeOut : Boolean = true) : UInt32; Overload;

  // Retrieve messages
  // In :  aClientReadeds : Client previously subscribted to channel.
  // out : Array with message count in waiting
  //Note : It use process message internally. So, if aClientReaders have
  //      receive event associate, the event will *NOT* be fired
  //      with this methods. For event based, Use ProcesMessage instead.
{  Procedure MessageStatus( const aClientReaders : Array of TBusClientReader;
                          const aClientMessagesCount : Array of UInt32;
                          const aClientMessagesBytes : Array of UInt64);
}

  /// This use bus capability and ChannelSetOnBeforeDeliverMessageEvent to make an
  /// easy way to exchange information between threads.
  /// When you call that, the bus install on a channel a "data repo" where you can,
  /// thought a TClientDataRepo instance, set and get data directy (synchrone in code point of view)
  Procedure DeclareDataRepository(aRepoName : String);
  function IsDataRepositoryExists(aRepoName : String) : Boolean;


  Procedure GetChannelsConfigurationAsCSV(var aStr : TStringList);
  Procedure GetSubscribtersConfigurationAsCSV(var aStr : TStringList);

  Function GetNewEvent : TEvent; //Managed (owned) by bus.

  procedure ChannelDelete(aChannelName : string);
  procedure ChannelSet( aChannelName : String;
                        aChannelBehaviourType : TBusChannelBehaviour;
                        Const aMessageWillBePersistent : Boolean = False;
                        Const EchoEnabled : Boolean = true);
  procedure ChannelSetAsTopic( aChannelName : String;
                        Const aMessageWillBePersistent : Boolean = False;
                        Const aMessageCountLimit : Integer = -1);
  procedure ChannelSetAsQueue( aChannelName : String;
                        aQueueType : TBusChannelBehaviourQueueSpecific;
                        Const aMessageWillBePersistent : Boolean = False;
                        Const aMessageCountLimit : Integer = -1);
  Procedure ChannelSetOnBeforeDeliverMessageEvent( aChannelName: String;
                                                   aChannelProc: TOnBusChannelBeforeDeliverMessage);

  procedure BusLog(aText : String); //Plain text is sended on internal bus's log. (See porperties Log_xxxx).
                                    //you can listen this log with standart BusClient. Usefull when you desactivate exception/callback.


  property Stats : String read GetStats;
  property CallBack_DisabledOnException : Boolean read GetCallBack_DisabledOnException Write SetCallBack_DisableOnException;
  property CallBack_ExceptionEnabled : Boolean read GetCallBack_ExceptionEnabled Write SetCallBack_ExceptionEnabled;
  property Log_Enabled : Boolean read GetLog_Enabled write SetLog_Enabled;
  property Idle : Boolean read GetBusIdle;
  property Log_ChannelName : String read GetLog_ChannelName Write SetLog_ChannelName;

  property MessageWaitingCount : Uint64 read GetMessageWaitingCount;
End;


//Thread bus wrapper.
TBus = class(TThread)
private
  FWaitIdlingForShutdown: Boolean;
  function GetStats: String;
  function GetIdle: Boolean;
protected
  Sys : TBusSystem;

  procedure WaitIdle;
  Procedure Execute; Override;
Public
  constructor Create; Reintroduce; virtual;
  Destructor Destroy; Override;


  Procedure BusShutDown; Virtual;

  ///All the bellow method is only "shortcut" method to the BusSystem.
  ///
  ///  TBusSystem object is fully Multi-threaded friendly.
  ///  you can share it between many thread. It is done for that.
  ///
  Function Subscribe(aChannelName : String; CallBack : TBusMessageNotify) : TBusClientReader;
  Function UnSubscribe(aClient : TBusClientReader) : Boolean;

  Function Send( var aMessage : TBusMessage;
                 const aTargetChannel : String;
                 const aSomeAdditionalData : String = '';
                 const aResponseChannel : string = '';
                 const IsPersistent : Boolean = False;
                 const ClientIDSignature : String = '';
                 const AppFilter : String = '') : Int64;

  Function Recv( const aClientReaders : Array of TBusClientReader;
                 const Messages : TBusEnvelopList) : UInt32; Overload;
  Function Recv( const aClientReader : TBusClientReader;
                 const Messages : TBusEnvelopList;
                 const WaitForMessageWithTimeOut : Boolean = false) : UInt32; Overload;

{  Procedure MessageStatus( const aClientReaders : Array of TBusClientReader;
                          const aClientMessagesCount : Array of UInt32;
                          const aClientMessagesBytes : Array of UInt64);
}


  // Send and retrieve message in a synchrone way.
  // In : aClient : a TClientReader previously subscribted to a channel.
  // In : aTargetChannel : The channel where the message will be send.
  // In : aMessage : the message to send.
  // Out : aResponse : The response message. If there are more than one message,
  // first only will be presented.
  // Note : This method use above's Send and Recv (With WaitForMessage = true) methods.
  Function SendAndRecv( const aClient : TBusClientReader;
                        aMessage : TBusEnvelop;
                        var aResponse : TBusEnvelop) : UInt32;


  Procedure GetChannelsConfigurationAsCSV(var aStr : TStringList);
  Procedure GetSubscribtersConfigurationAsCSV(var aStr : TStringList);

  procedure DeclareDataRepository(aRepoName : String);
  Function IsDataRepositoryExists(aRepoName : String) : Boolean;


  Function GetNewEvent : TEvent;

  Procedure ChannelDelete(aChannelName : string);
  Procedure ChannelSet( aChannelName : String;
                        aChannelBehaviourType : TBusChannelBehaviour;
                        Const aMessageWillBePersistent : Boolean = False;
                        Const EchoEnabled : Boolean = true);
  procedure ChannelSetAsTopic( aChannelName : String;
                        Const aMessageWillBePersistent : Boolean = False;
                        Const aMessageCountLimit : Integer = -1);
  procedure ChannelSetAsQueue( aChannelName : String;
                        aQueueType : TBusChannelBehaviourQueueSpecific;
                        Const aMessageWillBePersistent : Boolean = False;
                        Const aMessageCountLimit : Integer = -1);

  Procedure ChannelSetOnBeforeDeliverMessageEvent( aChannelName: String;
                                                   aChannelProc: TOnBusChannelBeforeDeliverMessage);

  Property Stats : String read GetStats;

  //Access to bus object.
  Property SubSystem : TBusSystem read Sys;

  //If true, it means that there are no activity on the bus.
  property Idle : Boolean read GetIdle;

  //If true, this will wait that "idle" property take "true" value. (in order to shutdown properly)
  property WaitIdlingForShutdown : Boolean read FWaitIdlingForShutdown write FWaitIdlingForShutdown;
end;

//This is used for TBusClientDataRepoSetValueStamped API.
//It allow to write data, and associate a timestamp to it.
TBCDRStampedStringItem = record //= TBusClientDataRepoFormatStamped :/
  DateTime : TDateTime;
  Data : String;

  procedure SaveToStream(aStream : TStream);
  procedure LoadFromStream(aStream : TStream);
End;
TBCDRStampedStringItems = Array of TBCDRStampedStringItem;

//DataRepo specialized client.
TBusClientDataRepo = Class
private
  FBus : TBus;
  FRepo : String;
  FClient : TBusClientReader;
  FInternalChannelResponse : String;
  FInternalChannelToDataRepo : String;

  function GetRepoStr : String;

  procedure InternalSetValue(const aKey : string; const aValue : string; const aSetOrder : string = 'SET;'); Overload;
  procedure InternalSetValue(const aKey : string; aValue : TStream; const aSetOrder : string = 'SET;'); Overload;
public
  Constructor Create(Const aBus : TBus; Const aRepoName : String); Reintroduce; Virtual;
  Destructor Destroy; Override;

  Procedure SetValue(const aKey : String; aValue : TStream); Overload; Virtual;
  function GetValue(const aKey : String; aValue : TStream) : Boolean; Overload; Virtual; //true if find...
  Procedure SetValue(const aKey : String; Const aValue : String); Overload; Virtual;
  function GetValue(const aKey : String; var aValue : String) : Boolean; Overload; Virtual;  //...Idem...
  Procedure SetValue(const aKey : String; Const aValue : Double); Overload; Virtual;
  function GetValue(const aKey : String; var aValue : Double) : Boolean; Overload; Virtual; //...

  function isKeyEntryExits(aKey : String) : boolean;

  ///
  /// All the previous API Get/Set keep in memory the last "single" value setted. (as it is KEY/Value system).
  /// The above one, write data into a LIST : aValue is appending to the previous ones.
  /// As a result, you get a list of TBCDRStamped<TYPE>Item.
  Procedure SetValueStamped(const aKey : String; Const aValue : String); Overload; Virtual;
  function GetValueStamped(const aKey : String; var aResult : TBCDRStampedStringItems) : Boolean; Overload; Virtual;

  procedure ClearValue(const aKey : string);

  Property RepositoryName : String read FRepo;
End;

TStackTaskChannel = Class(TStackTask)
private
  fchan : TBusChannel;
public
  constructor create(aChannel : TBusChannel); reintroduce;
  procedure execute; Override;
End;


var Bus : TBus;

Procedure StartStandartBus;
Procedure ReleaseStandartBus;

Function AtomicIncrement64(var a : Int64) : Int64; Overload;
Function AtomicIncrement64(var a : Int64; const Value : Int64) : Int64; Overload;
Function AtomicDecrement64(var a : Int64) : Int64;


  //Key methods : Call that in your coin of world (in another thread or not) to check your message box ;)
  //If aMailBoy is present, message will not be summonint in a ClientReaders callbacks, but pass to the aMailBox (as a copy).
  //if NonBlocks is true (Default : False), ProcessMesages will not wait Bus thread : It exit fastly.
  //Usage : Use True of you call ProcessMessages in a loop, periodically.
  Procedure BusProcessMessages( Const aClientReaders : Array of TBusClientReader;
                                      Const aMailBox : TBusEnvelopList = Nil);

function StreamToStampedStringItems(aStream : TStream) : TBCDRStampedStringItems;
function StampedStringItemsToString(const aStamptedString : TBCDRStampedStringItems; const WithStamp : Boolean = false) : String;

//remove forbidden char
function KeyStrNormalize(const aStrToProcess : String) : String;

implementation

var FBusGL : TCriticalSection;

function KeyStrNormalize(const aStrToProcess : String) : String;
var i : integer;
{$IFDEF FPC}
    StringsToRemove : array of string;
{$ELSE}
const StringsToRemove : array of string = [CST_DATAREPO_DELIMITER,' '];
{$ENDIF}
begin
  {$IFDEF FPC}
  SetLength(StringsToRemove,2);
  StringsToRemove[0] := CST_DATAREPO_DELIMITER;
  StringsToRemove[1] := ' ';
  {$ENDIF}
  for i := low(StringsToRemove) to high(StringsToRemove) do
  begin
    result := StringReplace(aStrToProcess,StringsToRemove[i],'',[rfReplaceAll, rfIgnoreCase]);
  end;
end;

{ TBusSystem }
Function AtomicIncrement64(var a : Int64) : Int64;
begin
  {$IFDEF FPC}
    {$IF DEFINED(CPUARM) OR DEFINED(CPU386)}
  FBusGL.Lock; try result := a+1; finally FBusGL.Unlock; end; ///!!!
    {$ELSE}
  result := InterLockedIncrement64(a);
    {$ENDIF}
  {$ELSE}
  result := AtomicIncrement(a);
  {$ENDIF}
end;

Function AtomicIncrement64(var a : Int64; const Value : Int64) : Int64; Overload;
begin
  {$IFDEF FPC}
    {$IF DEFINED(CPUARM) OR DEFINED(CPU386)}
  FBusGL.Lock; try result := a+Value; finally FBusGL.Unlock; end; ///!!!
    {$ELSE}
  result := InterLockedExchangeAdd64(a,Value);
    {$ENDIF}
  {$ELSE}
  result := AtomicIncrement(a,Value);
  {$ENDIF}
end;

Function AtomicDecrement64(var a : Int64) : Int64;
begin
  {$IFDEF FPC}
  {$IF DEFINED(CPUARM) OR DEFINED(CPU386)}
  FBusGL.Lock; try result := a-1; finally FBusGL.Unlock; end; ///!!!
    {$ELSE}
  result := InterLockedDecrement64(a);
    {$ENDIF}
  {$ELSE}
  result := AtomicDecrement(a);
  {$ENDIF}
end;

function StreamToStampedStringItems(aStream : TStream) : TBCDRStampedStringItems;
var i, h : UInt64;
    m : TBCDRStampedStringItem;
begin
  Assert(assigned(aStream));
  Result := nil;
  //Index saved in stream.
  aStream.Position := aStream.Size - SizeOf(UInt64);
  h := ReadUInt64(aStream);
  SetLength(Result,h);
  i := 0;
  aStream.Position := 0;
  while (aStream.Position < aStream.Size) do
  begin
    Result[i].loadFromStream(aStream);
    h := ReadUInt64(aStream);
    inc(i);
  end;
end;

function StampedStringItemsToString(const aStamptedString : TBCDRStampedStringItems; const WithStamp : Boolean = false) : String;
var l : TStringList;
    i : integer;
begin
  l := TStringList.Create;
  try
    if WithStamp then
      for I := Low(aStamptedString) to High(aStamptedString) do
        l.Add(DateTimeToStr(aStamptedString[i].DateTime)+' '+aStamptedString[i].Data)
    else
      for I := Low(aStamptedString) to High(aStamptedString) do
        l.Add(aStamptedString[i].Data);
    result := trim(l.Text);
  finally
    FreeAndNil(l);
  end;
end;



Procedure StartStandartBus;
begin
  if Not(assigned(Bus)) then
  begin
    Bus := TBus.Create;
    Bus.Start;
  end;
end;
Procedure ReleaseStandartBus;
begin
  if Assigned(bus) then
  begin
    FreeAndNil(Bus);
  end;
end;

Procedure BusProcessMessages( Const aClientReaders : Array of TBusClientReader;
                              Const aMailBox : TBusEnvelopList = Nil);
var
    mcl : TList_PTBusEnvelop;
    mcl2 : TList_PTBusEnvelop;
    mpPacketMailBox : PTBusEnvelop;
    i,k,q : integer;
    aReader : TBusClientReader;
    t : String;
begin
  //VGS : Above assert : Removed once more... Let this comment to ***not turn it back !!!***.
  //      Bus.processMessages is attended to be called by everywhere, in all thread we want.
  //  Assert(aClientReader.CreatorThreadId = GetThreadID);

  if length(aClientReaders)=0 then
    Exit;

  mcl2 := TList_PTBusEnvelop.Create;
  try

    for k := Low(aClientReaders) to High(aClientReaders) do
    begin
      if not(assigned(aClientReaders[k])) then
        continue;

      aReader := aClientReaders[k];

      //aReader.ClientMessageStack *must* be stoped shorter than possible !
      //--> We make a quick transfert of message's pointer form the locked list to another new one.
      mcl := aReader.ClientMessageStack.Lock;
      try
        mcl2.Clear;
        for i := 0 to mcl.Count-1 do
          mcl2.Add(mcl[i]); // Pointer copy only !
        mcl.Clear;
      finally
        aReader.ClientMessageStack.Unlock;
      end;


      if mcl2.Count>0 then
      begin
        if aMailBox=Nil then
        begin
          //Classic Event oriented.
          q := 0;
          try
            for i := 0 to mcl2.Count-1 do
            begin
              q := i; //For Exception.
              aReader.IncProcessMessageCount;
              if Assigned(aReader.CallBack) then
                aReader.CallBack(aReader.Bus,aReader,mcl2[i]^);
              dispose(mcl2[i]);
            end;
          Except
            On E : Exception do
            begin
              t := 'Client CallBack from Bus : Exception in Callback for channel "'+
                                      aReader.ChannelListening+'" Message '+IntToStr(q)+' of '+IntToStr(mcl2.Count)+
                                      ' : ['+E.Message+']';
              aReader.Bus.BusLog(t);
              if (aReader.Bus.CallBack_DisabledOnException) then
                aReader.CallBack := Nil; //Avoid further Exception ?

              if aReader.Bus.CallBack_ExceptionEnabled then
                raise Exception.Create(t);
            end;
          end;
        end
        else
        begin
          //Mail box.
          for i := 0 to mcl2.Count-1 do
          begin
            q := i; //For Exception.
            aReader.IncProcessMessageCount;

            New(mpPacketMailBox);
            mpPacketMailBox^.EnvelopId := mcl2[i]^.EnvelopId;
            mpPacketMailBox^.TargetChannel := mcl2[i]^.TargetChannel;
            mpPacketMailBox^.ResponseChannel := mcl2[i]^.ResponseChannel;
            mpPacketMailBox^.AdditionalData := mcl2[i]^.AdditionalData;
            mpPacketMailBox^.ContentMessage := mcl2[i]^.ContentMessage; //Deep copy;
            mpPacketMailBox^.ClientSourceId := mcl2[i]^.ClientSourceId;
            mpPacketMailBox^.AppFilter := mcl2[i]^.AppFilter;
            mpPacketMailBox^.Persistent := mcl2[i]^.Persistent;
            mpPacketMailBox^.CreateTag := mcl2[i]^.CreateTag;
            //MailBox is local (parameter) no need to lock/unlock for access.
            aMailBox.Items.Add(mpPacketMailBox);

            dispose(mcl2[i]);
          end;
        end;
      end;
    end;

  finally
    FreeAndNil(mcl2);
  end;
end;

procedure TBusSystem.ChannelSet( aChannelName : String;
                        aChannelBehaviourType : TBusChannelBehaviour;
                        Const aMessageWillBePersistent : Boolean = False;
                        Const EchoEnabled : Boolean = true);
begin
  FChannels.Lock;
  try
    FChannels.CreateOrSetChannel( aChannelName,
                                  aChannelBehaviourType,
                                  aMessageWillBePersistent,
                                  EchoEnabled);
  finally
    FChannels.Unlock;
  end;
end;

procedure TBusSystem.ChannelSetAsQueue( aChannelName : String;
                        aQueueType : TBusChannelBehaviourQueueSpecific;
                        Const aMessageWillBePersistent : Boolean = False;
                        Const aMessageCountLimit : Integer = -1);
var l : TObjectDictionary_BusChannel;
    chan : TBusChannel;
begin
  FChannels.Lock;
  try
    l := FChannels.GetLockedList;
    if l.TryGetValue(aChannelName,TObject(chan)) then
    begin
      if chan.ChannelBehaviourInfo is TBusChannelBehaviourTopic then
      begin
        chan.ChannelBehaviour := TBusChannelBehaviour.bcbQueue;
      end;
      chan.ChannelMessageCountLimitation := aMessageCountLimit;
      chan.MessageInThisChannelWillBeSetAsPersistent := aMessageWillBePersistent;
      TBusChannelBehaviourQueue(chan.ChannelBehaviourInfo).QueueBehaviour := aQueueType;
    end;
  finally
    FChannels.Unlock;
  end;
end;

procedure TBusSystem.ChannelSetAsTopic( aChannelName : String;
                        Const aMessageWillBePersistent : Boolean = False;
                        Const aMessageCountLimit : Integer = -1);
var l : TObjectDictionary_BusChannel;
    chan : TBusChannel;
begin
  FChannels.Lock;
  try
    l := FChannels.GetLockedList;
    if l.TryGetValue(aChannelName,TObject(chan)) then
    begin
      if chan.ChannelBehaviourInfo is TBusChannelBehaviourQueue then
      begin
        chan.ChannelBehaviour := TBusChannelBehaviour.bcbTopic;
      end;
      chan.ChannelMessageCountLimitation := aMessageCountLimit;
      chan.MessageInThisChannelWillBeSetAsPersistent := aMessageWillBePersistent;
      //TBusChannelBehaviourTopic(chan.ChannelBehaviourInfo). Here Topic specifi info channel...
    end;
  finally
    FChannels.Unlock;
  end;
end;

procedure TBusSystem.ChannelSetOnBeforeDeliverMessageEvent(aChannelName: String;
  aChannelProc: TOnBusChannelBeforeDeliverMessage);
begin
  FChannels.SetChannelOnBeforeDeliverEvent(aChannelName,aChannelProc);
end;

constructor TBusSystem.Create;
begin
  Inherited Create;
  FLockStat := TCriticalSection.Create;
  FLockPropertyBasic := TCriticalSection.Create;
  FEventListProtect := TCriticalSection.Create;
  FWaitMessageList := TBusEnvelopList.Create;
  FChannels := TBusChannelList.Create(self);
  FAllBusSubscribters := TBusClientReaderList.Create;
  FDoWork := TEvent.Create(nil,False,False,EmptyStr);
  FInternalMessageIdGenerator := 0;
  FTotalMessageSend := 0;
  FTotalMessagePending := 0;
  FTotalMessageProcessed := 0;
  FTotalMessagePersistent := 0;
  FDataRepo := TObjectDictionary_StringStream.Create;
  FInternalEventList := TObjectList_TEvent.Create;
  FCallBack_DisabledOnException := true; //If exception in user's callback, the event attached to the ClientReader will be stopped.
  FCallBack_ExceptionEnabled := false;
  FLog_Enabled := False; //No log by default.
  FLog_ChannelName := ClassName+'_InternalExceptionLog'; //If log activated, it will be generated on this channel.
end;

procedure TBusSystem.BusLog(aText: String);
var m :  TBusMessage;
begin
  if FLog_Enabled then
  begin
    assert(Length(FLog_ChannelName)>0);
    m.FromString(aText);
    Send(m,FLog_ChannelName)
  end;
end;

procedure TBusSystem.BusShutDown;
begin
  FDoWork.ResetEvent;
  FDoWork.SetEvent;
end;

procedure TBusSystem.ChannelDelete(aChannelName: string);
begin
  FChannels.DeleteChannel(aChannelName);
end;

Procedure TBusSystem.DeclareDataRepository(aRepoName: String);
begin
  FChannels.Lock;
  try
    if not(FChannels.IsChannelExists(CST_DATAREPONAME_PREFIX+aRepoName+CST_DATAREPONAME_SUFFIX)) then
      ChannelSetOnBeforeDeliverMessageEvent(CST_DATAREPONAME_PREFIX+aRepoName+CST_DATAREPONAME_SUFFIX, InternalDataRepoChannelCallBack);
  finally
    FChannels.Unlock;
  end;
end;

destructor TBusSystem.Destroy;
var i : Integer;
begin
  BusShutdown;
  FreeAndNil(FDoWork);
  FreeAndNil(FWaitMessageList);
  FreeAndNil(FAllBusSubscribters);
  FreeAndNil(FChannels);
  FreeAndNil(FLockStat);
  FreeAndNil(FLockPropertyBasic);
  {$IFDEF USE_GENERIC}
  FreeAndNil(FDataRepo);
  {$ELSE}
  for i := 0 to FDataRepo.Count-1 do
    FDataRepo[i].Value.Free;
  FreeAndNil(FDataRepo);
  {$ENDIF}
  FreeAndNil(FInternalEventList);
  FreeAndNil(FEventListProtect);
  inherited;
end;

procedure TBusSystem.BusExecute;
var
    aMes : PTBusEnvelop;
    lMasterMessageList,
    lTempML : TList_PTBusEnvelop;
    lchans : TObjectDictionary_StringObject;
    channel : TBusChannel;

    Procedure LocalInternalChannelDispatch;
    var i,j : Integer;
        t : String;
    begin
      //STEP ONE : All message in processing list are now dispached to the channels.
      lMasterMessageList := FWaitMessageList.Lock;
      try

        //Pass 1 : Create all needed channel.
        For i := 0 to lMasterMessageList.Count-1 do
        begin
           FChannels.Lock;
          try
            lchans := FChannels.GetLockedList;
            channel := Nil;
            if Not(lchans.TryGetValue(lMasterMessageList[i].TargetChannel,TObject(channel))) then
            begin
              { TODO -oVGS -cNiceToHave : AutoCreate channel option + according exception }
              channel := TBusChannel.Create(Self, lMasterMessageList[i].TargetChannel);
              lchans.Add(lMasterMessageList[i].TargetChannel,channel);
            end;
          finally
            FChannels.Unlock;
          End;

          lTempML := channel.ProcessingMessageLock;
          try
            lTempML.Add(lMasterMessageList[i]); //Pointer only.
            channel.IncReceivedMessageCount;
          finally
            channel.ProcessingMessageMessageUnlock;
          end;

        end;
        lMasterMessageList.Clear;
      finally
        FWaitMessageList.Unlock;
      end;
    end;

    procedure LocalInternalChannelProcess;
    var i,j : integer;
        l : TStackTaskChannel;
        ll : TList_TStackTask;
        f : boolean;

        chanIndex : Integer;
        chanCount : Integer;
    begin
        FChannels.Lock;
        try
          lchans := FChannels.GetLockedList;
          chanCount := lchans.Count;
        finally
          FChannels.Unlock;
        End;

        for chanIndex := 0 to chanCount-1 do
        begin
          if FChannels.GetLockChannel(chanIndex,channel) then
          begin
            try
              channel.DoProcessing;
            finally
              channel.Unlock;
            end;
          end;
        end;
    end;

    procedure LocalInternalIdleUpdate;
    begin
      FLockPropertyBasic.Acquire;
      try
        FIdle := FTotalMessagePending = FTotalMessagePendingMemory;
        FTotalMessagePendingMemory := FTotalMessagePending;
      finally
        FLockPropertyBasic.Release;
      end;
    end;

    procedure MessageProcess;
    begin
      LocalInternalChannelDispatch;
      LocalInternalChannelProcess;
      LocalInternalIdleUpdate;
    end;

begin
  if GetMessageWaitingCount>0 then
    MessageProcess
  else
    case FDoWork.WaitFor(CST_BUSTIMER) of
      wrSignaled :
      begin
        MessageProcess;
      end;
      wrTimeout :
      begin
        //Todo : Generate Idle message ?;
        LocalInternalIdleUpdate;
      end;
      wrAbandoned, wrError {$IFNDEF FPC}, wrIOCompletion {$ENDIF} :
      begin
        //Todo : Exception message. Stop ?
      end;
    end;
end;


function TBusSystem.GetBusIdle: Boolean;
begin
  FLockPropertyBasic.Acquire;
  try
    result := FIdle;
  finally
    FLockPropertyBasic.Release;
  end;
end;

function TBusSystem.GetCallBack_DisabledOnException: Boolean;
begin
  FLockPropertyBasic.Enter;
  try
    result := FCallBack_DisabledOnException;
  finally
    FLockPropertyBasic.Leave;
  end;
end;

function TBusSystem.GetCallBack_ExceptionEnabled: Boolean;
begin
  FLockPropertyBasic.Enter;
  try
    result := FCallBack_ExceptionEnabled;
  finally
    FLockPropertyBasic.Leave;
  end;
end;

procedure TBusSystem.GetChannelsConfigurationAsCSV(var aStr: TStringList);
var i : Integer;
    s2 : TStringList;
    c : TBusChannel;
    cl : TObjectDictionary_BusChannel;

    Function GetChannelType : String;
    begin
      case c.ChannelBehaviour of
        TBusChannelBehaviour.bcbTopic: result := 'Topic';
        TBusChannelBehaviour.bcbQueue: result := 'Queue';
      end;
    end;

    Function BoolToPlainEnglish(ab : Boolean) : string;
    begin
      result := 'No';
      if ab then
        result:= 'Yes';
    end;
begin
//  if Terminated then
//    Exit;
  Assert(Assigned(aStr));
  FChannels.Lock;
  try
    aStr.Clear;
    s2 := TStringList.Create;
    cl := FChannels.GetLockedList;
    s2.Add('ChannelName');
    s2.Add('ChannelType');
    s2.Add('IsChannelPersistent');
    s2.Add('EchoEnabled');
    s2.Add('ReceivedMessageCount');
    s2.Add('ConsumedMessageCount');
    s2.Add('DeliveredMessageCount');
    s2.Add('PersistentMessageCount');
    s2.Add('SubscribterCount');
    aStr.Add(s2.DelimitedText);
    for I := 0 to cl.Count-1 do
    begin
      c := cl[i];
      s2.Clear;
      s2.Add(c.ChannelName);
      s2.Add(GetChannelType);
      s2.Add(BoolToPlainEnglish(c.MessageInThisChannelWillBeSetAsPersistent));
      s2.Add(BoolToPlainEnglish(c.ChannelEchoEnabled));
      s2.Add(IntToStr(c.ReceivedMessageCount));
      s2.Add(IntToStr(c.ConsumedMessageCount));
      s2.Add(IntToStr(c.DeliveredMessageCount));
      s2.Add(IntToStr(c.PersistentMessageCount));
      s2.Add(IntToStr(c.CurrentSubscribterCount));
      aStr.Add(s2.DelimitedText);
    end;
  finally
    FChannels.Unlock;
    FreeAndNil(s2);
  end;
end;

function TBusSystem.GetLog_ChannelName: String;
begin
  FLockPropertyBasic.Enter;
  try
    result := FLog_ChannelName;
  finally
    FLockPropertyBasic.Leave;
  end;
end;

function TBusSystem.GetLog_Enabled: Boolean;
begin
  FLockPropertyBasic.Enter;
  try
    result := FLog_Enabled;
  finally
    FLockPropertyBasic.Leave;
  end;
end;

function TBusSystem.GetMessageWaitingCount: Uint64;
begin
  try
    result := FWaitMessageList.Lock.Count;
  finally
    FWaitMessageList.Unlock;
  end;
end;

function TBusSystem.GetNewEvent: TEvent;
begin
  FEventListProtect.Acquire;
  try
    Result := TEvent.Create(nil,False,False,EmptyStr);
    FInternalEventList.add(Result);
  finally
    FEventListProtect.Release;
  end;
end;

function TBusSystem.GetStats: String;
begin
//---CRITICAL !!!
  //TODO : Jsonify ?
  Result := 'Messages - Send : ' +IntTostr(FTotalMessageSend) +
            ' Pending : ' +IntTostr(FTotalMessagePending) +
            ' Processed : ' +IntTostr(FTotalMessageProcessed) +
            ' Current Message Wait : '+IntToStr(MessageWaitingCount);
end;

procedure TBusSystem.GetSubscribtersConfigurationAsCSV(var aStr: TStringList);
var i : Integer;
    s2 : TStringList;
    sub : TList_TBusClientReader;

    Function AssignedOrNot(b : boolean) : String;
    begin
      Result := 'Assigned';
      if Not(b) then
        Result := 'Unassigned';
    end;

begin
//  if Terminated then
//    Exit;
  Assert(Assigned(aStr));
  aStr.Clear;
  s2 := TStringList.Create;
  s2.Add('SubscribterID');
  s2.Add('SubscribtedToChannel');
  s2.Add('PendingMessageCount');
  s2.Add('ProcessMessageCount');
  aStr.Add(s2.DelimitedText);
  FAllBusSubscribters.Lock;
  try
    sub := FAllBusSubscribters.GetLockedList;
    for I := 0 to sub.Count-1 do
    begin
      s2.Clear;
      s2.Add(IntToStr(i));
      s2.Add(sub[i].ChannelListening);
      s2.Add(IntToStr(sub[i].PendingMessageCount));
      s2.Add(IntToStr(sub[i].ProcessMessageCount));
      aStr.Add(s2.DelimitedText);
    end;
  finally
    FAllBusSubscribters.Unlock;
    FreeAndNil(s2);
  end;
end;

procedure TBusSystem.InternalDataRepoChannelCallBack(var aMessage: TBusEnvelop);
var s : TStringList;
    l : TStream;
    Response : TBusMessage;
    c : Uint64;
begin
  ///Here data retrieval through very simple key/value.

  s :=  TStringList.Create;
  s.Delimiter := CST_DATAREPO_DELIMITER;
  s.DelimitedText := aMessage.AdditionalData;
  try
    if s[0] = 'SET' then
    begin
      if FDataRepo.TryGetValue(s[1],l) then
      begin
        l.Size := 0;
        aMessage.ContentMessage.ToStream(l);
      end
      else
      begin
        FDataRepo.Add(s[1],aMessage.ContentMessage.AsStream);
      end;
    end
    else
    if s[0] = 'GET' then
    begin
      try
        Response.Buffer := nil;
        if FDataRepo.TryGetValue(s[1],l) then
        begin
          l.Position := 0;
          Response.FromStream(TMemoryStream(l)); //Load data into response stream.
        end;
      finally
        Send(Response,aMessage.ResponseChannel);
      end;
    end
    else
    if s[0] = 'SETSTAMPED' then
    begin
      if FDataRepo.TryGetValue(s[1],l) then
      begin
        //l.Size := 0; No ! Stamped is a list. :) It is added.
        l.Position := l.Size-sizeOf(Uint64);
        c := ReadUInt64(l);
        l.Position := l.Size;
        aMessage.ContentMessage.ToStream(TStream(l));
        WriteUInt64(l,c+1); //we add an index at the end.
      end
      else
      begin
        l := aMessage.ContentMessage.AsStream;
        l.Position := l.Size;
        WriteUInt64(l,1); //we add the number of item systematically, at the end.
        FDataRepo.Add(s[1],l);
      end;
    end
    else
    if s[0] = 'CLEAR' then
    begin
      FDataRepo.Remove(s[1]);
    end
    else
    if s[0] = 'EXISTS' then
    begin
      try
        if FDataRepo.TryGetValue(s[1],l) then
          Response.FromString('Y')
        else
          Response.FromString('');
      finally
        Send(Response,aMessage.ResponseChannel);
      end;
    end;
  finally
    FreeAndNil(s);
  end;
end;

function TBusSystem.IsDataRepositoryExists(aRepoName: String): Boolean;
begin
  FChannels.Lock;
  try
    result := FChannels.IsChannelExists(CST_DATAREPONAME_PREFIX+aRepoName+CST_DATAREPONAME_SUFFIX);
  finally
    FChannels.Unlock;
  end;
end;

//------------------------------------------------------------------------------
//W A R N I N G
// TBusSystem.ProcessMessages([aobj...]) will be called by different thread :
// It build a "execution list" of the clientreaders (for speed) and execute it
// for the caller thread context.
//------------------------------------------------------------------------------
function TBusSystem.Recv(const aClientReader: TBusClientReader;
  const Messages: TBusEnvelopList; const WaitForMessageWithTimeOut: Boolean): UInt32;
var aOb : THandleObject;
begin
  Assert(assigned(aClientReader));
  if WaitForMessageWithTimeOut then
    if Assigned(aClientReader.Event) then
      aClientReader.Event.WaitFor(CST_BUSTIMER)
    else
      FDoWork.WaitFor(CST_BUSTIMER);
  BusProcessMessages([aClientReader],Messages);
  result := Messages.Items.Count;
end;

function TBusSystem.Recv(const aClientReaders: array of TBusClientReader;
  const Messages: TBusEnvelopList): UInt32;
var i : integer;
    ldetect : boolean;
begin
  BusProcessMessages(aClientReaders,Messages);
  result := Messages.Items.Count;
end;

Function TBusSystem.Send(  var aMessage : TBusMessage;
                 const aTargetChannel : String;
                 const aSomeAdditionalData : String = '';
                 const aResponseChannel : string = '';
                 const IsPersistent : Boolean = False;
                 const ClientIDSignature : String = '';
                 const AppFilter : String = '') : Int64;
var aPacket : PTBusEnvelop;
    L : TList_PTBusEnvelop;
    lTempChannel : String;
    ltempstr : TStringList;
    i : integer;
    FWchr : char;

    Procedure InternalSendMessage(aChannel : String);
    begin
      New(aPacket);
      FLockStat.Enter;
      try
        aPacket^.EnvelopId := FInternalMessageIdGenerator; //For later use (ack ?)
        FInternalMessageIdGenerator := FInternalMessageIdGenerator + 1;
      finally
        FLockStat.Leave;
      end;
      aPacket^.TargetChannel := aChannel;
      aPacket^.AdditionalData := aSomeAdditionalData;
      aPacket^.ResponseChannel := aResponseChannel;
      aPacket^.ContentMessage := aMessage; //Deep copy;
      aPacket^.Persistent := IsPersistent;
      aPacket^.ClientSourceId := ClientIDSignature;
      aPacket^.AppFilter := AppFilter;
      aPacket^.CreateTag := gsGetTickCount;
      Result := aPacket^.EnvelopId;
      L := FWaitMessageList.Lock;
      try
        L.Add(aPacket);
      finally
        FWaitMessageList.Unlock;
        FDoWork.SetEvent;
        AtomicIncrement64(FTotalMessageSend);
      end;
    end;
begin
  Assert(aTargetchannel<>'');
  Result := 0;
  //Send message with full targetname.
  InternalSendMessage(aTargetChannel);
end;


procedure TBusSystem.SetCallBack_DisableOnException(const Value: Boolean);
begin
  FLockPropertyBasic.Enter;
  try
    FCallBack_DisabledOnException := Value;
  finally
    FLockPropertyBasic.Leave;
  end;
end;

procedure TBusSystem.SetCallBack_ExceptionEnabled(const Value: Boolean);
begin
  FLockPropertyBasic.Enter;
  try
    FCallBack_ExceptionEnabled := Value;
  finally
    FLockPropertyBasic.Leave;
  end;
end;

procedure TBusSystem.SetLog_ChannelName(const Value: String);
begin
  FLockPropertyBasic.Enter;
  try
    FLog_ChannelName := Value;
  finally
    FLockPropertyBasic.Leave;
  end;
end;

procedure TBusSystem.SetLog_Enabled(const Value: Boolean);
begin
  FLockPropertyBasic.Enter;
  try
    FLog_Enabled := Value;
  finally
    FLockPropertyBasic.Leave;
  end;
end;


Function TBusSystem.Subscribe(aChannelName : String; CallBack : TBusMessageNotify) : TBusClientReader;
var aNewChannel : TBusChannel;
var i : integer;
    cl : TObjectDictionary_BusChannel;
    c : TList_TBusClientReader;
    cd : TList_TBusClientReader;

    ll,llo : TList_PTBusEnvelop;
    aPacket : PTBusEnvelop;
    tm : Int64;
begin
  //Assert(Assigned(CallBack));
  Assert(aChannelName <> EmptyStr);
  Result := TBusClientReader.Create(Self, aChannelName, CallBack);
  //Channel : Find or create it.
  aNewChannel := Nil;
  FChannels.Lock;
  try
    cl := FChannels.GetLockedList;
    if not cl.TryGetValue(aChannelName,TObject(aNewChannel)) then
    begin  //Channel does not exists : Exit ;)
      aNewChannel := TBusChannel.Create(Self, aChannelName);
      cl.Add(aChannelName,aNewChannel);
    end;

    //Add the subscribter to the channel (ShortCut).
    aNewChannel.Subscribters.Lock;
    try
      c := aNewChannel.Subscribters.GetLockedList;
      c.Add(Result);
    finally
      aNewChannel.Subscribters.Unlock;
    end;

    //Add new client to the reference subscibter list (Reference).
    FAllBusSubscribters.Lock;
    try
      cd := FAllBusSubscribters.GetLockedList;
      cd.Add(Result);
    finally
      FAllBusSubscribters.Unlock;
    end;

    //Finally, deliver all potentiel Persistent message of the channel to the new subscribter
    FLockPropertyBasic.Enter;
    try
      tm := FTotalMessagePersistent;
    finally
      FLockPropertyBasic.Leave;
    end;

    if tm>0 then
    begin
      ll := Result.ClientMessageStack.Lock;
      llo := aNewChannel.PersistentMessageLock;
      try
        for I := 0 to llo.Count-1 do
        begin
          New(aPacket);
          aPacket^.EnvelopId := llo[i]^.EnvelopId;
          aPacket^.AdditionalData := llo[i]^.AdditionalData;
          aPacket^.TargetChannel := aNewChannel.ChannelName;
          aPacket^.ResponseChannel := llo[i]^.ResponseChannel;
          aPacket^.ContentMessage := llo[i]^.ContentMessage; //Deep copy;
          aPacket^.Persistent := llo[i]^.Persistent;
          aPacket^.ClientSourceId := llo[i]^.ClientSourceId;
          aPacket^.AppFilter := llo[i]^.AppFilter;
          aPacket^.CreateTag := llo[i]^.CreateTag;
          ll.Add(aPacket);
        end;
      finally
        Result.ClientMessageStack.Unlock;
        aNewChannel.PersistentMessageUnlock;
      end;
    end;
  finally
    FChannels.Unlock;
  end;
end;

function TBusSystem.Unsubscribe(aClient: TBusClientReader): Boolean;
var i,h : integer;
    lChannel : TBusChannel;
    lChannelName : String;
    cl : TObjectDictionary_BusChannel;

    c : TList_TBusClientReader;
    cd : TList_TBusClientReader;
begin
  Result := false;
  Assert(Assigned(aClient));
  lChannelName := aClient.ChannelListening;
  Assert(lChannelName <> EmptyStr);
  //Channel : Find it.
  lChannel := Nil;
  FChannels.Lock;
  try
    cl := FChannels.GetLockedList;
    if not cl.TryGetValue(lChannelName,TObject(lChannel)) then
    begin  //Channel does not exists : Exit ;)
      Exit;
    end;

    //we do not forget to remove that from Channel.Subscripters list, which is a mirror of FSubscripter reference list.
    lChannel.Subscribters.Lock;
    try
      c := lChannel.Subscribters.GetLockedList;
      i := c.IndexOf(aClient);
      if i>-1 then
        c.Delete(i); //It a shortcut list : not the real object owned.
    finally
      lChannel.Subscribters.Unlock;
    end;

    //find the client to subscribter reference list, and remove it.
    FAllBusSubscribters.Lock;
    try
      cd := FAllBusSubscribters.GetLockedList;
      i := cd.IndexOf(aClient);
      if i>-1 then
      begin
        cd.Delete(i); //aClient is NOT destroy : As documented, it is CLIENT responsability to free its client access.
        Result := True;
      end;
    finally
      FAllBusSubscribters.Unlock;
    end;

    //TODO : Historize ?
    for I := 0 to cl.Count-1 do
    begin
      if cl[i].ChannelName = lChannelName then
      begin
        cl[i].Subscribters.Lock;
        try
          cd := cl[i].Subscribters.GetLockedList;
          h := cd.Count; //Subscribters remaining.
        finally
          cl[i].Subscribters.Unlock;
        end;

        if h=0 then //No Subscribters.
        begin
          cl[i].Lock;
          cl[i].ProcessingMessageLock;
          cl.Remove(i); //Delete channel
        end;

        Break;
      end;
    end;

  finally
    FChannels.Unlock;
  end;
end;

{ TBusMessage }

{ TBusEnvelopList }

constructor TBusEnvelopList.Create;
begin
  FList := TList_PTBusEnvelop.Create;
  FLock := TCriticalSection.Create;
end;

destructor TBusEnvelopList.Destroy;
var i : integer;
    ap : pTBusEnvelop;
begin
  FLock.Acquire;
  try
    for I := 0 to FList.Count-1 do
    begin
      ap := FList[i];
      Dispose(ap);
    end;
  finally
    FLock.Release;
  end;
  FreeAndNil(flist);
  FreeAndNil(FLock);
  inherited;
end;

function TBusEnvelopList.Lock: TList_PTBusEnvelop;
begin
  FLock.Enter;
  Result := FList;
end;

function TBusEnvelopList.TryLock(var aList: TList_PTBusEnvelop): boolean;
begin
  result := FLock.TryEnter;
  if result then
   aList := FList;
end;

procedure TBusEnvelopList.Unlock;
begin
  FLock.Release;
end;

{ TBusChannel }

constructor TBusChannel.Create(aBus : TBusSystem; aChannelName : String);
begin
  Inherited Create;
  Assert(Assigned(aBus));
  Assert(aChannelName<>EmptyStr);
  FMaster := aBus;
  FProtect := TCriticalSection.Create;
  FBusChannelData := TBusChannelData.Create;
  FBusChannelData.FChannel := aChannelName;
  FBusChannelData.FReceivedMessageCount := 0;
  FBusChannelData.FConsumedMessageCount := 0;
  FBusChannelData.FMessageInThisChannelWillBeSetAsPersistent:= false;
  FChannelEchoEnabled := False; //Bus behaviour (MQTT, KissBe...)
  FDataProtect := TCriticalSection.Create;
  FChannelMessageCountLimitation := -1; //No limit.
  FSubscribters := TBusClientReaderList.Create;
  FBehaviour := TBusChannelBehaviour.bcbTopic;
  FBehaviourInfo := TBusChannelBehaviourTopic.Create(Self);
  FPersistentMessages := TBusEnvelopList.Create;
  FProcessingMessages := TBusEnvelopList.Create;
  FEventDeliveryDesactivateOnException := True; //If exception, event is desactivated.
  FEventDelivery_ExceptionEnabled := False; //If false (Default), exception will be muzzled : Considere to let Event desactivated on this case. (FEventDeliveryDesactivateOnException)
end;

destructor TBusChannel.Destroy;
begin
  FreeAndNil(FBusChannelData);
  FreeAndNil(FSubscribters);
  FreeAndNil(FBehaviourInfo);
  FreeAndNil(FPersistentMessages);
  FreeAndNil(FProcessingMessages);
  FreeAndNil(FDataProtect);
  FreeAndNil(FProtect);
  inherited;
end;

procedure TBusChannel.DoProcessing;
var packet : PTBusEnvelop;
    mes : PTBusEnvelop;
    lclientList : TList_TBusClientReader;
    lclientIndex : Integer;
    lc : TBusClientReader;
    lmp, ll : TList_PTBusEnvelop;
    i,j : Integer;
    proceed : boolean;


    procedure Duplicate;
    begin
      New(packet);
      packet^.EnvelopId := mes^.EnvelopId;
      packet^.AdditionalData := mes^.AdditionalData;
      packet^.TargetChannel := FBusChannelData.FChannel; // ChannelName;
      packet^.ResponseChannel := mes^.ResponseChannel;
      packet^.Persistent := mes^.Persistent;
      packet^.ContentMessage := mes^.ContentMessage; //Deep copy;
      packet^.ClientSourceId := mes^.ClientSourceId;
      packet^.AppFilter := mes^.AppFilter;
      packet^.CreateTag := mes^.CreateTag;
    end;

    procedure BeforeDeliver;
    var t : String;
    begin

      if Assigned(OnBeforeDeliverMessage) then
      begin
        try
          OnBeforeDeliverMessage(mes^);
        Except
          On E : Exception do
          begin
            if EventDelivery_DesactivateOnException then
              OnBeforeDeliverMessage := Nil;
            t := 'Error processed onBeforeDelivered : "'+e.Message+'"';
            FMaster.BusLog(t);
            if  EventDelivery_ExceptionEnabled then
              raise Exception.Create(t);
          end;
        end;

        AtomicIncrement64(FMaster.FTotalMessageProcessed);
      end;

    end;

    procedure DeliverSingleMessage(aMessage : PTBusEnvelop);
    var i : integer;
    begin
      ll := lc.ClientMessageStack.Lock;
      try
        mes := aMessage;
        proceed := true;

        if mes^.AppFilter<>'' then
          proceed := mes^.AppFilter = lc.AppFilter;

        if proceed and Not ChannelEchoEnabled then
          proceed := (mes.ClientSourceId <> lc.ClientBusID);

        if proceed then
        begin
          Duplicate;
          ll.Add(packet);
          IncDeliveredMessageCount;

          if assigned(lc.Event) then
            lc.Event.SetEvent; //If this reader is waiting somewhere in a thread, it will be trig.
        end;
      finally
        lc.ClientMessageStack.Unlock;
      end;
    end;

    procedure DeliverMessage(var aList :  TList_PTBusEnvelop);
    var i : integer;
    begin
      ll := lc.ClientMessageStack.Lock;
      try
        for i := 0 to aList.Count-1 do
        begin
          mes := aList[i];
          proceed := true;

          if mes^.AppFilter<>'' then
            proceed := mes^.AppFilter = lc.AppFilter;

          if proceed and Not ChannelEchoEnabled then
            proceed := (mes.ClientSourceId <> lc.ClientBusID);

          if proceed then
          begin
            Duplicate;
            ll.Add(packet);
            IncDeliveredMessageCount;
          end;
        end;

        if assigned(lc.Event) then
        begin
          lc.Event.SetEvent; //If this reader is waiting somewhere in a thread, it will be trig.
        end;

        begin
          //TODO : We can here evaluate if there are to many message in waiting for a client...
          // OR : Implement message time limitation existance within client message list. (Memory improvement)
          // OR : Minimum client activity timer if many message are stored on its side. (Memory improvement)
          // OR : Start a notification procedure (To the client, or, to a "memory manager" with "problemetic" client as parameter.
        end;

      finally
        lc.ClientMessageStack.Unlock;
      end;
    end;

    Procedure InternalPrepareAndSavePersistentMessage;
    begin
      Duplicate;
      ll := PersistentMessageLock;
      try
        ll.Add(packet);
        AtomicIncrement64(FMaster.FTotalMessagePersistent);
      finally
        PersistentMessageUnlock;
      end;
    end;

    Procedure PersistanceManagement;
    begin
      if MessageInThisChannelWillBeSetAsPersistent then
      begin
        //Even if the message is not persitant, the channel is set as auto Persistent.
        //We turn the message as persitant one. It will be copied in private list just after.
        mes^.Persistent := True;
      end;
      if mes^.Persistent then
      begin
        InternalPrepareAndSavePersistentMessage;
      end;
    end;

    function NoClient : boolean;
    begin
      result := lclientList.Count=0;
      if result then
      begin
        if (MessageInThisChannelWillBeSetAsPersistent) then
        begin
          InternalPrepareAndSavePersistentMessage;
        end;
      end;
    end;

var lbPersistantChannel : Boolean;
    lmem : UInt32;
    lp : TList_PTBusEnvelop;
    idx : integer;
    lcl : Integer;
begin
  lbPersistantChannel := MessageInThisChannelWillBeSetAsPersistent;

  lcl := ChannelMessageCountLimitation; //Defalt value = -1
  if lcl=0 then //Note : ChannelMessageCountLimitation = 0 disable the channel. (feature.)
    Exit;

  lp := TList_PTBusEnvelop.Create;
  lmp := ProcessingMessageLock;
  try
    if lmp.Count=0 then
      Exit;

    idx := 0;
    if ChannelMessageCountLimitation>0 then
    begin
      idx := integer(lmp.Count) - ChannelMessageCountLimitation;
      if idx<0 then
        idx := 0;
    end;

    //We deliver, on max, the "lmp.Count - ChannelMessageCountLimitation" messages. (burst effect)
    for j := idx to lmp.Count-1 do
    begin
      mes := lmp[j];
      BeforeDeliver;
    end;

    Subscribters.Lock;
    try
      lclientList := Subscribters.GetLockedList;
      if NoClient then
        Exit; //Stop ! No further treatment : No client in this channel.

      //In Memory Persistant message process.
      for j := idx to lmp.Count-1 do
      begin
        IncConsumedMessageCount;
        mes := lmp[j];
        if (mes.Persistent) Or (lbPersistantChannel)  then
        begin
          PersistanceManagement;
        end;
      end;

      case ChannelBehaviour of
        TBusChannelBehaviour.bcbTopic:
        begin
          for i := 0 to lclientList.Count-1 do
          begin
            lc := lclientList[i];
            if idx>0 then
            begin
              lp.Clear;
              for j := idx to lmp.Count-1 do
                lp.Add(lmp[j]);
              DeliverMessage(lp);
            end
            else
              DeliverMessage(lmp);
          end;
        end;

        TBusChannelBehaviour.bcbQueue:
        begin
          case TBusChannelBehaviourQueue(ChannelBehaviourInfo).QueueBehaviour of
            ///cbqQueueFaultTolerant :
            ///   - Serve first client, as long as it is connected, if this client disconnect, server the second one and so on.
            TBusChannelBehaviourQueueSpecific.cbqQueueFaultTolerant :
            begin
              lc := lclientList[0]; //Always a client (NoClient condition, top of proc.)
              if idx>0 then
              begin
                lp.Clear;
                for j := idx to lmp.Count-1 do
                  lp.Add(lmp[j]);
                DeliverMessage(lp);
              end
              else
                DeliverMessage(lmp);
            end;
            ///cbqQueueDistributed :
            ///   - Serve each client one after the other.
            TBusChannelBehaviourQueueSpecific.cbqQueueDistributed :
            begin
              lclientIndex := TBusChannelBehaviourQueue(ChannelBehaviourInfo).LastClientIndexServed;
              for j := idx to lmp.Count-1 do
              begin
                lc := lclientList[lclientIndex];
                DeliverSingleMessage(lmp[j]);
                Inc(lclientIndex);
                if lclientIndex > integer(lclientList.Count)-1 then
                  lclientIndex := 0;
              end;
              TBusChannelBehaviourQueue(ChannelBehaviourInfo).LastClientIndexServed := lclientIndex;
            end;
          end;
        end;
      end;

    finally
      Subscribters.Unlock;
    end;

  finally
    for i := 0 to lmp.Count-1 do
    begin
      mes := lmp[i];
      dispose(mes);
    end;
    lmp.Clear;
    FreeAndNil(lp);
    ProcessingMessageMessageUnlock;
  end;
end;


function TBusChannel.GetChannel: string;
begin
  FBusChannelData.Lock;
  try
    result := FBusChannelData.FChannel;
  finally
    FBusChannelData.Unlock;
  end;
end;

function TBusChannel.GetChannelMessageCountLimitation: Integer;
begin
  FDataProtect.Enter;
  try
    result := FChannelMessageCountLimitation;
  finally
    FDataProtect.Leave;
  end;
end;

function TBusChannel.GetConsumedMessageCount: Int64;
begin
  FBusChannelData.Lock;
  try
    Result := FBusChannelData.FConsumedMessageCount;
  finally
    FBusChannelData.Unlock;
  end;
end;

function TBusChannel.GetCurrentSubscribterCount: Int64;
begin
  Subscribters.Lock;
  try
    Result := Subscribters.GetLockedList.Count;
  finally
    Subscribters.Unlock;
  end;
end;

function TBusChannel.GetDeliveredMessageCount: Int64;
begin
  FBusChannelData.Lock;
  try
    Result := FBusChannelData.FDeliveredMessageCount;
  finally
    FBusChannelData.Unlock;
  end;
end;

function TBusChannel.GetMessageInThisChannelWillBeSetAsPersistent: Boolean;
begin
  FBusChannelData.Lock;
  try
    result := FBusChannelData.FMessageInThisChannelWillBeSetAsPersistent;
  finally
    FBusChannelData.Unlock
  end;
end;

function TBusChannel.GetPersistentMessageCount: Int64;
var ll : TList_PTBusEnvelop;
begin
  ll := FPersistentMessages.Lock;
  try
    Result := ll.Count;
  finally
    FPersistentMessages.Unlock;
  end;
end;

function TBusChannel.GetReceivedMessageCount: Int64;
begin
  FBusChannelData.Lock;
  try
  Result := FBusChannelData.FReceivedMessageCount;
  finally
    FBusChannelData.Unlock;
  end;
end;

procedure TBusChannel.IncConsumedMessageCount;
begin
  AtomicIncrement64(FBusChannelData.FConsumedMessageCount);
end;

procedure TBusChannel.IncDeliveredMessageCount;
begin
  AtomicIncrement64(FBusChannelData.FDeliveredMessageCount);
end;

procedure TBusChannel.IncReceivedMessageCount;
begin
  AtomicIncrement64(FBusChannelData.FReceivedMessageCount);
end;

procedure TBusChannel.Lock;
begin
  FProtect.Enter;
end;

function TBusChannel.PersistentMessageLock: TList_PTBusEnvelop;
begin
  result := FPersistentMessages.Lock;
end;

procedure TBusChannel.PersistentMessageUnlock;
begin
  FPersistentMessages.Unlock;
end;

function TBusChannel.ProcessingMessageLock: TList_PTBusEnvelop;
begin
  result := FProcessingMessages.Lock;
end;

procedure TBusChannel.ProcessingMessageMessageUnlock;
begin
  FProcessingMessages.Unlock;
end;

procedure TBusChannel.SetChannelBehaviour(const Value: TBusChannelBehaviour);
begin
  if FBehaviour<>Value then
  begin
    if Assigned(FBehaviourInfo) then
      FreeAndNil(FBehaviourInfo);
    FBehaviour := Value;
    case FBehaviour of
      TBusChannelBehaviour.bcbTopic: FBehaviourInfo := TBusChannelBehaviourTopic.Create(Self);
      TBusChannelBehaviour.bcbQueue:
      begin
        FBehaviourInfo := TBusChannelBehaviourQueue.Create(Self);
        TBusChannelBehaviourQueue(FBehaviourInfo).QueueBehaviour := TBusChannelBehaviourQueueSpecific.cbqQueueFaultTolerant;
      end;
    end;
  end;
end;

procedure TBusChannel.SetChannelMessageCountLimitation(const Value: Integer);
begin
  FChannelMessageCountLimitation := value;
end;

procedure TBusChannel.SettMessageInThisChannelWillBeSetAsPersistent(
  const Value: Boolean);
begin
  FBusChannelData.Lock;
  try
    FBusChannelData.FMessageInThisChannelWillBeSetAsPersistent := Value;
  finally
    FBusChannelData.Unlock;
  end;
end;

procedure TBusChannel.Unlock;
begin
  FProtect.Leave;
end;

{ TBusChannelList }

constructor TBusChannelList.Create(aBus : TBusSystem);
begin
  Inherited Create;
  Assert(Assigned(aBus));
  FListProcessing := TObjectDictionary_BusChannel.Create;
  FList := nil;
  FLock := TCriticalSection.Create;
  FMaster := aBus;
end;

procedure TBusChannelList.CreateOrSetChannel(aChannelName : String;
                                aChannelBehaviourType : TBusChannelBehaviour;
                                const aMessageWillBePersistent : Boolean = False;
                                const EchoEnabled : Boolean = True);
var cl : TObjectDictionary_BusChannel;
    i : integer;
    c : TBusChannel;
Begin
  Assert(Assigned(Flist));
  if Not(FList.TryGetValue(aChannelName,TObject(c))) then
  begin
    c := TBusChannel.Create(FMaster,aChannelName);
    FList.Add(aChannelName,c);
  end;

  c.Lock;
  try
    c.ChannelBehaviour := aChannelBehaviourType;
    c.MessageInThisChannelWillBeSetAsPersistent := aMessageWillBePersistent;
    c.ChannelEchoEnabled := EchoEnabled;
  finally
    c.Unlock;
  end;
end;

procedure TBusChannelList.DeleteChannel(aChannelName: string);
var chan : TbusChannel;
Begin
  Assert(Assigned(Flist));
  if FList.TryGetValue(aChannelName, TObject(chan)) then
  begin
    chan.Lock;
    FList.Remove(aChannelName);
  end;
end;

destructor TBusChannelList.Destroy;
var i : integer;
begin
  Lock;
  for i := 0 to FList.Count-1 do
    FList[i].Free;
  FreeAndNil(FList);
  FreeAndNil(FLock);
  inherited;
end;

function TBusChannelList.GetLockedList: TObjectDictionary_BusChannel;
begin
  Assert(Assigned(Flist));
  result := FList;
end;

function TBusChannelList.IsChannelExists(const aChannelName: String): Boolean;
var
  I: Integer;
  ldummy : TObject;
begin
  Assert(Assigned(Flist));
  Result := FList.TryGetValue(aChannelName,ldummy);
end;

procedure TBusChannelList.Lock;
begin
  FLock.Acquire;
  FList := FListProcessing;
end;

procedure TBusChannelList.SetChannelOnBeforeDeliverEvent(
  aChannelName: String; aChannelProc: TOnBusChannelBeforeDeliverMessage);
var cl : TObjectDictionary_BusChannel;
    i : integer;
    c : TBusChannel;
Begin
  Assert(Assigned(Flist));
  if Not(FList.TryGetValue(aChannelName,TObject(c))) then
  begin
    c := TBusChannel.Create(FMaster,aChannelName);
    FList.Add(aChannelName,c);
  end;
  c.OnBeforeDeliverMessage := aChannelProc;
end;

function TBusChannelList.GetLockChannel(index: Uint32; var channel : TBusChannel) : boolean;
begin
  result := false;
  FLock.Acquire;
  try
    if FListProcessing.Count>index then
    begin
      channel := FListProcessing[index];
      channel.Lock;
      result := true;
    end;
  finally
    FLock.Release;
  end;
end;

procedure TBusChannelList.Unlock;
begin
  FList := Nil;
  FLock.Release;
end;

{ TBusClient }

constructor TBusClient.Create(aBus : TBusSystem);
begin
  Inherited Create;
  Assert(assigned(aBus));
  ClientMessageStack := TBusEnvelopList.Create;
  Event := Nil;
  FBus := aBus;
end;

destructor TBusClient.Destroy;
begin
  FreeAndNil(ClientMessageStack);
  inherited;
end;

function TBusClient.GetClientPendingMessageCount: Int64;
var L : TList_PTBusEnvelop;
begin
  L := ClientMessageStack.Lock;
  try
    Result := L.Count;
  finally
    ClientMessageStack.Unlock;
  end;
end;


{ TBusClientReader }

constructor TBusClientReader.Create(aBus : TBusSystem; aChannelName : String; aCallBack: TBusMessageNotify);
var Fg : TGUID;
begin
  Assert(aChannelName <> EmptyStr);
  Inherited Create(aBus);
  FCallBack := aCallBack;
  FChannel := aChannelName;
  FProcessMessageCount := 0;

  Fg := TGUID.NewGuid;
  FClientBusID := Fg.ToString;
  FAppFilter := '';
end;


function TBusClientReader.GetClientProcessMessageCount: Int64;
begin
  result := FProcessMessageCount;
end;

procedure TBusClientReader.IncProcessMessageCount;
begin
  AtomicIncrement64(FProcessMessageCount);
end;

{ TBusClientReaderList }

constructor TBusClientReaderList.Create;
begin
  FListProcessing := Tlist_TBusClientReader.Create;
  FList := Nil;
  FLock := TCriticalSection.Create;
end;

destructor TBusClientReaderList.Destroy;
begin
  Lock;
  FreeAndNil(FList); //User *must* delete client themself, because of share object (event)
  FreeAndNil(FLock);
  inherited;
end;

function TBusClientReaderList.GetLockedList: TList_TBusClientReader;
begin
  Assert(Assigned(Flist));
  result := FList;
end;

function TBusClientReaderList.IsChannelAlreadyRepresented(
  aChan: String): Boolean;
var i : Integer;
begin
  result := false;
  Lock;
  try
    for I := 0 to FList.Count-1 do
    begin
      result := FList[i].ChannelListening = aChan;
      if result then
        break;
    end;
  finally
    Unlock;
  end;
end;

procedure TBusClientReaderList.Lock;
begin
  FLock.Acquire;
  FList := FListProcessing;
end;

function TBusClientReaderList.ToArray: TBusClientReaderArray;
var i : integer;
begin
  Lock;
  try
    SetLength(Result,FList.Count);
    for i := 0 to FList.Count-1 do
    begin
      Result[i] := FList[i];
    end;
  finally
    Unlock;
  end;
end;

procedure TBusClientReaderList.Unlock;
begin
  FList := Nil;
  FLock.Release;
end;

{ TBusMessage }

function TBusMessage.AsByte: Byte;
begin
  if length(Buffer) >0 then
    Result := Buffer[0]
  else
    raise Exception.Create('TBusMessage.asByte : Read error');
end;

function TBusMessage.AsStream: TMemoryStream;
begin
  Result := TMemoryStream.Create;
  Result.Write(Buffer[0],Length(Buffer));
  Result.Position := 0;
end;

procedure TBusMessage.FromByte(aByte: Byte);
begin
  SetLength(Buffer,1);
  Buffer[0] := aByte;
end;


function TBusMessage.AsDouble: Double;
var i : integer;
begin
  result := 0.0;
  if length(Buffer) = SizeOf(Double) then
  begin
    Move(Buffer[0],Result,SizeOf(Double));
  end;
end;

procedure TBusMessage.FromDouble(aD: Double);
var i : integer;
    t : Double;
begin
  SetLength(Buffer,SizeOf(Double));
  Move(aD,Buffer[0],SizeOf(Double));
  t := asDouble;
  Assert(ad=t);
end;

procedure TBusMessage.FromStream(aStream: TStream);
begin
  aStream.Position := 0;
  SetLength(Buffer,aStream.Size);
  aStream.Read(Buffer[0],aStream.Size);
end;

procedure TBusMessage.FromString(const aText: String);
begin
{ TODO : Manage encoding }
  Buffer := TEncoding.UTF8.GetBytes(aText);
end;

function TBusMessage.AsString: String;
begin
{ TODO : Manage encoding }
  Result := String(TEncoding.UTF8.GetString(Buffer));
end;

function TBusMessage.Size: Uint64;
begin
  result := Length(Buffer);
end;


procedure TBusMessage.ToStream(var aTargetStream: TStream);
begin
  aTargetStream.Write(Buffer[0],Length(Buffer));
end;

{ TBusChannelAdditionalInformation }

constructor TBusChannelAdditionalInformation.Create(
  aMasterChannel: TBusChannel);
begin
  inherited Create;
  Assert(Assigned(AMasterChannel));
  FMasterChannel := aMasterChannel;
end;

{ TBusChannelBehaviourQueue }

constructor TBusChannelBehaviourQueue.Create(aMasterChannel: TBusChannel);
begin
  inherited;
  FLastClientIndexServed := 0;
end;

{ TBus }

procedure TBus.BusShutDown;
begin
  if FWaitIdlingForShutdown then
    WaitIdle;

  if not(suspended) and not(Terminated) then
  begin
    Terminate;
    Sys.BusShutDown;
    Waitfor; //Terminate main bus loop.
  end;
end;

procedure TBus.ChannelDelete(aChannelName: string);
begin
  sys.ChannelDelete(aChannelName);
end;

procedure TBus.ChannelSet( aChannelName : String;
                        aChannelBehaviourType : TBusChannelBehaviour;
                        Const aMessageWillBePersistent : Boolean = False;
                        Const EchoEnabled : Boolean = true);
begin
  sys.ChannelSet(aChannelName,aChannelBehaviourType,aMessageWillBePersistent,EchoEnabled);
end;

procedure TBus.ChannelSetAsQueue( aChannelName : String;
                        aQueueType : TBusChannelBehaviourQueueSpecific;
                        Const aMessageWillBePersistent : Boolean = False;
                        Const aMessageCountLimit : Integer = -1);
begin
  sys.ChannelSetAsQueue(aChannelName,aQueueType,aMessageWillBePersistent,aMessageCountLimit);
end;

procedure TBus.ChannelSetAsTopic( aChannelName : String;
                        Const aMessageWillBePersistent : Boolean = False;
                        Const aMessageCountLimit : Integer = -1);
begin
  sys.ChannelSetAsTopic(aChannelName,aMessageWillBePersistent,aMessageCountLimit);
end;

procedure TBus.ChannelSetOnBeforeDeliverMessageEvent(
  aChannelName: String; aChannelProc: TOnBusChannelBeforeDeliverMessage);
begin
  sys.FChannels.Lock;
  try
    sys.ChannelSetOnBeforeDeliverMessageEvent(aChannelName,aChannelProc);
  finally
    sys.FChannels.Unlock;
  end;
end;

constructor TBus.Create;
begin
  inherited Create(true);
  {$IFDEF DELPHI}
  NameThreadForDebugging(ClassName);
  {$ENDIF}
  Sys := TBusSystem.Create;
  FWaitIdlingForShutdown := true;
end;

procedure TBus.DeclareDataRepository(aRepoName: String);
begin
  Sys.DeclareDataRepository(KeyStrNormalize(aRepoName));
end;

destructor TBus.Destroy;
begin
  BusShutDown;
  FreeAndNil(Sys);
  inherited;
end;

procedure TBus.Execute;
begin
  while not(Terminated) do
    Sys.BusExecute;
end;

procedure TBus.GetChannelsConfigurationAsCSV(var aStr: TStringList);
begin
  sys.GetChannelsConfigurationAsCSV(aStr);
end;

function TBus.GetIdle: Boolean;
begin
  result := sys.Idle;
end;

function TBus.GetNewEvent: TEvent;
begin
  Result := sys.GetNewEvent;
end;

function TBus.GetStats: String;
begin
  result := sys.GetStats;
end;

procedure TBus.GetSubscribtersConfigurationAsCSV(var aStr: TStringList);
begin
  sys.GetSubscribtersConfigurationAsCSV(aStr);
end;

function TBus.IsDataRepositoryExists(aRepoName: String): Boolean;
begin
  result := Sys.IsDataRepositoryExists(aRepoName);
end;

function TBus.Recv(const aClientReader: TBusClientReader;
  const Messages: TBusEnvelopList; const WaitForMessageWithTimeOut: Boolean): UInt32;
begin
  result := Sys.Recv(aClientReader,Messages,WaitForMessageWithTimeOut);
end;

function TBus.Recv(const aClientReaders: array of TBusClientReader;
  const Messages: TBusEnvelopList): UInt32;
begin
  result := Sys.Recv(aClientReaders,Messages);
end;

function TBus.Send( var aMessage : TBusMessage;
                 const aTargetChannel : String;
                 const aSomeAdditionalData : String = '';
                 const aResponseChannel : string = '';
                 const IsPersistent : Boolean = False;
                 const ClientIDSignature : String = '';
                 const AppFilter : String = ''): Int64;
begin
  result := sys.Send( aMessage,
                      aTargetChannel,
                      aSomeAdditionalData,
                      aResponseChannel,
                      IsPersistent,
                      ClientIDSignature,
                      AppFilter);
end;

function TBus.SendAndRecv( const aClient : TBusClientReader;
                        aMessage : TBusEnvelop;
                        var aResponse : TBusEnvelop): UInt32;

var lmb : TBusEnvelopList;
    n : TDateTime;
begin
  Assert(aMessage.TargetChannel<>'');
  result := 0;
  /// WARNING : Dev Note : Why note the 2 lines above is NOT good ?
  /// aClient.Event.WaitFor(INFINITE);
  /// ProcessMessages([aClient],lmb);
  ///
  /// - It lock definetly a thread if bus does not respond. It is not the bus responsability to permit at the other thread to go on.
  /// Please implement on client side the possibility to retry on they level if result is false.
  /// - With this solution, aClient does not need an system event : Economy.
  lmb := TBusEnvelopList.Create;
  try
    n := now;
    Send(aMessage.ContentMessage,aMessage.TargetChannel,aMessage.AdditionalData,aClient.ChannelListening);
    while not(Terminated) And not(TVisibilityThread(CurrentThread).Terminated) do
    begin
      Recv(aclient,lmb,true);
      if lmb.Items.Count>0 then
      begin
        Result := lmb.Items.Count;
        aResponse := lmb.Items[0]^;
        Break;
      end
      else
    end;


{    while not(Terminated)
          And(not( now-n > (1/(3600*24))*5) )
          And not(TVisibilityThread(CurrentThread).Terminated)
          And (Recv(aclient,lmb,true) = 0) do;

    if lmb.Items.Count>0 then
    begin
      Result := lmb.Items.Count;
      aResponse := lmb.Items[0]^;
    end
    else
    begin
      raise Exception.Create('SendAndRecv Error Message');
    end;
}
  finally
    FreeAndNil(lmb);
  end;
end;

function TBus.Subscribe(aChannelName: String;
  CallBack: TBusMessageNotify): TBusClientReader;
begin
  result := sys.Subscribe(aChannelName,CallBack);
end;

function TBus.UnSubscribe(aClient: TBusClientReader): Boolean;
begin
  result := sys.UnSubscribe(aClient);
end;

procedure TBus.WaitIdle;
var l : TDateTime;
    Sec5 : Single;
begin
 //Limit the idling "tentative reaching" to 3 sec.
  Sec5 := (1/24/60/60) * 3;
  l := now;
  While(not(Idle)) do
  begin
    if now-l >Sec5 then
    begin
      Sleep(5);
      Break;
    end;
  end;
end;

{ TBusChannelData }

constructor TBusChannelData.Create;
begin
  Inherited;
  FLock := TCriticalSection.Create;
  FReceivedMessageCount := 0;
  FConsumedMessageCount := 0;
  FDeliveredMessageCount := 0;
end;

destructor TBusChannelData.Destroy;
begin
  FreeAndNil(Flock);
  inherited;
end;

procedure TBusChannelData.Lock;
begin
  FLock.Acquire;
end;

procedure TBusChannelData.Unlock;
begin
  FLock.Release;
end;

{ TBusClientDataRepo }

procedure TBusClientDataRepo.ClearValue(const aKey: string);
var m1 : TBusEnvelop;
begin
  m1.TargetChannel := FInternalChannelToDataRepo;
  m1.AdditionalData := 'CLEAR;'+aKey;
  m1.ContentMessage.Buffer := nil;
  FBus.Send(m1.ContentMessage,m1.TargetChannel,m1.AdditionalData);
end;


constructor TBusClientDataRepo.Create(const aBus: TBus;
  const aRepoName: String);
begin
  Inherited Create;
  Assert(assigned(aBus));
  Assert(Length(trim(aRepoName))>0);
  FRepo := KeyStrNormalize(aRepoName);
  FBus := aBus;
  FInternalChannelResponse := aRepoName+'.Client.'+IntToStr(Uint64(Self));
  FInternalChannelToDataRepo := CST_DATAREPONAME_PREFIX+FRepo+CST_DATAREPONAME_SUFFIX;
  FBus.DeclareDataRepository(FRepo);
  FClient := FBus.Subscribe(FInternalChannelResponse,Nil);
  FClient.Event := FBus.GetNewEvent;
end;

destructor TBusClientDataRepo.Destroy;
begin
  FBus.UnSubscribe(FClient);
  FreeAndNil(FClient);
  inherited;
end;

function TBusClientDataRepo.GetRepoStr: String;
begin
  result :='_'+FRepo;
end;

function TBusClientDataRepo.GetValue(const aKey: String;
  var aValue: Double): Boolean;
var m1,m2 : TBusEnvelop;
begin
  result := false;
  aValue := 0.0;

  m1.TargetChannel := FInternalChannelToDataRepo;
  m1.AdditionalData := 'GET;'+aKey+GetRepoStr;
  result := FBus.SendAndRecv(FClient,m1,m2)>0;
  if result then
  begin
    aValue := m2.ContentMessage.AsDouble;
  end;
end;

function TBusClientDataRepo.GetValueStamped(const aKey: String;
  var aResult: TBCDRStampedStringItems): Boolean;
var l : TMemoryStream;
begin
  result := false;
  l := TMemoryStream.Create;
  try
    if GetValue(aKey,l) then
    begin
      aresult := StreamToStampedStringItems(l);
      result := true;
    end;
  finally
    FreeAndnil(l);
  end;
end;

procedure TBusClientDataRepo.InternalSetValue(const aKey: string;
  aValue: TStream; const aSetOrder: string);
var m1 : TBusEnvelop;
begin
  Assert(Assigned(aValue));
  aValue.Position := 0;
  m1.TargetChannel := FInternalChannelToDataRepo;
  m1.AdditionalData := aSetOrder+KeyStrNormalize(aKey)+GetRepoStr;
  m1.ContentMessage.FromStream(aValue);
  FBus.Send(m1.ContentMessage,m1.TargetChannel,m1.AdditionalData);
end;

function TBusClientDataRepo.isKeyEntryExits(aKey: String): boolean;
var m1,m2 : TBusEnvelop;
begin
  result := false;
  m1.TargetChannel := FInternalChannelToDataRepo;
  m1.AdditionalData := 'EXISTS;'+KeyStrNormalize(aKey)+GetRepoStr;
  if FBus.SendAndRecv(FClient,m1,m2)>0 then
    result := m2.ContentMessage.AsString = 'Y';
end;

procedure TBusClientDataRepo.InternalSetValue(const aKey, aValue,
  aSetOrder: string);
var m1 : TBusEnvelop;
begin
  m1.TargetChannel := FInternalChannelToDataRepo;
  m1.AdditionalData := aSetOrder+KeyStrNormalize(aKey)+GetRepoStr;
  m1.ContentMessage.FromString(aValue);
  FBus.Send(m1.ContentMessage,m1.TargetChannel,m1.AdditionalData);
end;

function TBusClientDataRepo.GetValue(const aKey : String; var aValue : String): Boolean;
var m1,m2 : TBusEnvelop;
begin
  result := false;
  aValue := '';

  m1.TargetChannel := FInternalChannelToDataRepo;
  m1.AdditionalData := 'GET;'+KeyStrNormalize(aKey)+GetRepoStr;

  result := FBus.SendAndRecv(FClient,m1,m2)>0;
  if result then
  begin
    aValue := m2.ContentMessage.AsString;
  end;
end;

function TBusClientDataRepo.GetValue(const aKey: String;
  aValue: TStream): Boolean;
var m1,m2 : TBusEnvelop;
begin
  result := false;
  Assert(Assigned(aValue));
  aValue.Position := 0;

  m1.TargetChannel := FInternalChannelToDataRepo;
  m1.AdditionalData := 'GET;'+KeyStrNormalize(aKey)+GetRepoStr;
  result := FBus.SendAndRecv(FClient,m1,m2)>0;
  if result then //we got a message.
  begin
    result := Length(m2.ContentMessage.Buffer)>0;
    if result then
      m2.ContentMessage.ToStream(aValue);
  end;
end;

procedure TBusClientDataRepo.SetValue(const aKey: String; aValue: TStream);
begin
  InternalSetValue(aKey,aValue);
end;

procedure TBusClientDataRepo.SetValue(const aKey, aValue: String);
begin
  InternalSetValue(aKey,aValue);
end;

procedure TBusClientDataRepo.SetValue(const aKey: String; const aValue: Double);
var m1,m2 : TBusEnvelop;
begin
  m1.TargetChannel := FInternalChannelToDataRepo;
  m1.AdditionalData := 'SET;'+KeyStrNormalize(aKey)+GetRepoStr;
  m1.ContentMessage.FromDouble(aValue);
  FBus.Send(m1.ContentMessage,m1.TargetChannel,m1.AdditionalData);
end;

procedure TBusClientDataRepo.SetValueStamped(const aKey, aValue: String);
var a : TBCDRStampedStringItem;
    l : TMemoryStream;
begin
  a.DateTime := Now;
  a.Data := aValue;
  l := TMemoryStream.Create;
  try
    a.SaveToStream(l);
    InternalSetValue(aKey,l,'SETSTAMPED;');
  finally
    FreeAndNil(l);
  end;
end;

{ TList_PTBusEnvelop }
{$IFNDEF USE_GENERIC}

procedure TList_PTBusEnvelop.Add(aPTBusEnvelop: PTBusEnvelop);
begin
  if FInitialized then
  begin
    if FIndex = UInt32(Length(FArray)) then
    begin
      SetLength(FArray,Length(FArray)*2);
    end;
    FArray[FIndex] := aPTBusEnvelop;
  end
  else
  begin
    FInitialized := true;
    FIndex := 0;
    FArray[FIndex] := aPTBusEnvelop;
  end;
  Inc(FIndex);
end;

procedure TList_PTBusEnvelop.Clear;
begin
  FArray := nil;
  SetLength(Farray,CST_ARRAY_INIT_QTE);
  FIndex := 0;
  FInitialized := False;
end;

constructor TList_PTBusEnvelop.Create;
begin
  Clear;
end;

function TList_PTBusEnvelop.GetPTBusEnvelop(Index: Uint32): PTBusEnvelop;
begin
  result := FArray[Index];
end;

function TList_PTBusEnvelop.GetPTBusEnvelopCount: Uint32;
begin
  result := FIndex;
end;

procedure TList_PTBusEnvelop.SetPTBusEnvelop(Index: Uint32;
  const Value: PTBusEnvelop);
begin
  FArray[Index] := Value;
end;

{ TList_TBusClientReader }

procedure TList_TBusClientReader.Add(aBusClientReader: TBusClientReader);
begin
  ManagedAdd(aBusClientReader);
end;

function TList_TBusClientReader.GetBusClientReaderItem(
  Index: Uint32): TBusClientReader;
begin
 Result := TBusClientReader(Farray[Index]);
end;

procedure TList_TBusClientReader.SetBusClientReaderItem(Index: Uint32;
  const Value: TBusClientReader);
begin
  ManagedSet(Index,Value);
end;


{ TObjectList_TEvent }

procedure TObjectList_TEvent.Add(aEvent: TEvent);
begin
  ManagedAdd(aEvent);
end;

constructor TObjectList_TEvent.Create;
begin
  Inherited Create(True);
end;

function TObjectList_TEvent.GetEventItem(Index: Uint32): TEvent;
begin
  result := TEvent(FArray[Index]);
end;

procedure TObjectList_TEvent.SetEventItem(Index: Uint32;
  const Value: TEvent);
begin
  ManagedSet(Index,Value);
end;


{$ENDIF}

{ TObjectDictionary_TBusChannel }

procedure TObjectDictionary_BusChannel.AddIfNotAlready(aKey: String;
  aBusChannel: TBusChannel);
var lr : TBusChannel;
begin
  if Not(TryGetValue(aKey,TObject(lr))) then
    Inherited Add(aKey,aBusChannel);
end;

function TObjectDictionary_BusChannel.GetBusChannelItem(
  Index: UInt32): TBusChannel;
begin
  result := TBusChannel(inherited Items[Index].Value);
end;

{ TStackTaskChannel }

constructor TStackTaskChannel.create(aChannel: TBusChannel);
begin
  assert(Assigned(aChannel));
  fchan := aChannel;
end;

procedure TStackTaskChannel.execute;
begin
  fchan.DoProcessing;
end;

{ TBCDRStampedStringItem }

procedure TBCDRStampedStringItem.LoadFromStream(aStream: TStream);
begin
  DateTime := ReadDateTime(aStream);
  Data := ReadString(aStream);
end;

procedure TBCDRStampedStringItem.SaveToStream(aStream: TStream);
begin
  WriteDateTime(aStream,Now);
  WriteString(aStream,Data);
end;

{ TBusChannelPrivacy }

constructor TBusChannelPrivacy.Create;
begin
  FKeyPass := '{7FDB0206-5775-492C-B8EF-5D8642C02BF6}'; //Or put whatever you want.
end;


Initialization
FBusGL := TCriticalSection.Create;
Bus := Nil;

Finalization

FreeAndNil(FBusGL);

end.
