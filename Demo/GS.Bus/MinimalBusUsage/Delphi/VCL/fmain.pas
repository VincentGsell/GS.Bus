unit fmain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls,
  System.Generics.Collections,
  GS.Bus;

type
  TFormMain = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    Timer1: TTimer;
    CheckBox1: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    fpool : TList<TThread>;
    lenv : GS.Bus.TBusEnvelopList;
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

procedure TFormMain.FormCreate(Sender: TObject);
begin
  lenv := GS.Bus.TBusEnvelopList.Create;
  fpool := TList<TThread>.Create;
  StartStandartBus; //Start a bus... Into "bus" global object.
  MyClient := Bus.Subscribe('test Channel',OnMessageIncomingTestChannel); //MyClient is a client of the bus.
end;


procedure TFormMain.Button1Click(Sender: TObject);
var aMessage : TBusMessage;
begin
  aMessage.FromString('Hello ! '+DateTimeToStr(Now)); //build a message.
  bus.Send(aMessage,'test Channel'); //Send it !
end;


procedure TFormMain.FormDestroy(Sender: TObject);
var i : integer;
begin
  for I := 0 to fpool.Count-1 do begin
      fpool[i].Terminate;
      fpool[i].WaitFor;
      fpool[i].Free;
  end;
  FreeAndNil(fpool);

  ReleaseStandartBus; //Free all...
  FreeAndNil(MyClient);      //...And MyClient, because its owned by your app.
  FreeAndNil(lenv);
end;

procedure TFormMain.OnMessageIncomingTestChannel(Sender: TBusSystem;
  aReader: TBusClientReader; var Packet: TBusEnvelop);
begin
  //Message incoming
  Memo1.Lines.Add('[Event]'+Packet.ContentMessage.AsString);
end;

procedure TFormMain.Timer1Timer(Sender: TObject);
var
    lp : GS.Bus.TList_PTBusEnvelop;
    lbo : Boolean;
    i : integer;
begin
  //ask to get message in the main thread context :
  //-  Avoid synchro call.
  //- do not overload your main gui thread, or, at least, kept control on it.

  if CheckBox1.Checked then
    BusProcessMessages([MyClient]) //Event will trigerered.
 else begin
    lbo := BusProcessMessages([MyClient],lenv)>0;
    if lbo then begin
      lp := lenv.Lock;
      try
        for i := 0 to lp.Count-1 do
          Memo1.Lines.Add('[mailbox]'+lp.Items[i].ContentMessage.AsString);
      finally
        lp.ClearAndDispose;
        lenv.Unlock;
      end;
    end;
  end;
end;


procedure TFormMain.Button2Click(Sender: TObject);
{$j+}
const threadcount : Integer = 1;
{$j-}
begin
  fpool.Add(TMyLittleSender.Create(false));
  caption := 'Bus minimal exemple ('+IntToStr(threadcount)+' thread(s))';
  inc(threadcount);
end;

{ MyLittleSender }

procedure TMyLittleSender.Execute;
var aM : TBusMessage;
    cc : Integer;
begin
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
