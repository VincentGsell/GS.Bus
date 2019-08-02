unit fmain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  GS.Bus, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TFormMain = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    MyClient : TBusClientReader;
    procedure OnMessageIncomingTestChannel(Sender : TBusSystem; aReader : TBusClientReader; Var Packet : TBusEnvelop);
  end;

  TMyLittleSender = class(TThread)
  public
    procedure Execute; Override;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}


procedure TFormMain.Button1Click(Sender: TObject);
var aMessage : TBusMessage;
begin
  aMessage.FromString('Hello ! '+DateTimeToStr(Now)); //build a message.
  bus.Send(aMessage,'test Channel'); //Send it !
end;


procedure TFormMain.FormCreate(Sender: TObject);
begin
  StartStandartBus; //Start a bus... Into "bus" global object.
  MyClient := Bus.Subscribe('test Channel',OnMessageIncomingTestChannel); //MyClient is a client of the bus.
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  ReleaseStandartBus; //Free all...
  FreeAndNil(MyClient);      //...And MyClient, because its owned by your app.
end;

procedure TFormMain.OnMessageIncomingTestChannel(Sender: TBusSystem;
  aReader: TBusClientReader; var Packet: TBusEnvelop);
begin
  //Message incoming
  Memo1.Lines.Add(Packet.ContentMessage.AsString);
end;

procedure TFormMain.Timer1Timer(Sender: TObject);
begin
  //ask to get message in the main thread context :
  //-  Avoid synchro call.
  //- do not overload your main gui thread, or, at least, kept control on it.
  BusProcessMessages([MyClient]);
end;


procedure TFormMain.Button2Click(Sender: TObject);
{$j+}
const threadcount : Integer = 1;
{$j-}
begin
  TMyLittleSender.Create(false);
  caption := 'Bus minimal exemple ('+IntToStr(threadcount)+' thread(s))';
  inc(threadcount);
end;

{ MyLittleSender }

procedure TMyLittleSender.Execute;
var aM : TBusMessage;
    cc : Integer;
begin
  FreeOnTerminate := true;
  cc := 0;
  while not(Terminated) do
  begin
    if Assigned(bus) then
    begin
      Inc(cc);
      aM.FromString('thread #'+IntToStr(ThreadId)+' message '+IntToStr(cc));
      Bus.Send(aM,'test Channel');
      Sleep(500);
      if Terminated then
        exit;
    end
    else
    begin
      exit;
    end;
  end;
end;

end.
