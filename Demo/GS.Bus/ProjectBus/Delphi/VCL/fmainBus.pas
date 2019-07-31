unit fmainBus;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, Vcl.ExtCtrls,
  Vcl.StdCtrls,Vcl.ComCtrls,
  System.Threading,
  GS.Bus;

type
  TForm2 = class(TForm)
    TimerBusQuery: TTimer;
    Panel1: TPanel;
    PageControl1: TPageControl;
    tsBus: TTabSheet;
    tsDataRepo: TTabSheet;
    pnl2: TPanel;
    Label3: TLabel;
    Label2: TLabel;
    Label1: TLabel;
    lblChannels: TLabel;
    lbl2: TLabel;
    edt1: TEdit;
    btn2: TButton;
    lst2: TListBox;
    btn4: TButton;
    btn3: TButton;
    btn1: TButton;
    lvSubscripters: TListView;
    ListView2: TListView;
    Button1: TButton;
    Edit1: TEdit;
    Memo1: TMemo;
    ListBox1: TListBox;
    Panel2: TPanel;
    Label5: TLabel;
    Button2: TButton;
    Memo2: TMemo;
    Button3: TButton;
    Button4: TButton;
    cbThreadedSend: TCheckBox;
    TimerGUI: TTimer;
    btTest: TButton;
    btTest2: TButton;
    RadioGroup1: TRadioGroup;
    Edit2: TEdit;
    UpDown1: TUpDown;
    Label6: TLabel;
    Label7: TLabel;
    RadioGroup2: TRadioGroup;
    Button5: TButton;
    chkMemPersistant: TCheckBox;
    Button6: TButton;
    Label8: TLabel;
    Edit3: TEdit;
    Label9: TLabel;
    Label10: TLabel;
    Button7: TButton;
    Button8: TButton;
    cbEchoEnabled: TCheckBox;
    GroupBox1: TGroupBox;
    Label4: TLabel;
    procedure btn1Click(Sender: TObject);
    procedure btn2Click(Sender: TObject);
    procedure btn3Click(Sender: TObject);
    procedure btn4Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TimerBusQueryTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure TimerGUITimer(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btTestClick(Sender: TObject);
    procedure btTest2Click(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
  private
    { Private declarations }
    procedure UpdateListView;
  public
    GUIHit : UInt64;
    { Public declarations }
    procedure OnReceived(Sender : TBusSystem; aReader : TBusClientReader;  Var Packet : TBusEnvelop);
  end;


///
///  WARNING : The thread above do not care about Bus var freeing : So, trig them and close app will lead
///  to an AV. (It can lead to an AV only IF you changed Bus.WaitIdlingForShutdown default value.)
///
  TMyThread = Class(TThread)
  private
    counter : UInt32;
    channel : String;
  public
    constructor Create(aCounter : Uint32; aChannel : string); Reintroduce;
    procedure Execute; override;
  End;

  TMyThreadDistri = Class(TThread)
  private
  public
    counter : UInt32;
    List : TStringList;
    constructor Create(aCounter : Uint32); Reintroduce;
    destructor Destroy; Override;
    procedure Execute; override;
  End;

  TMyThreadDataRepoStress = Class(TThread)
  private
    counter : UInt32;
    RepoName : String;
  public
    constructor Create(aCounter : Uint32; const aRepoName : String); Reintroduce;
    procedure Execute; override;
  End;


var
  Form2: TForm2;

implementation


{$R *.dfm}

procedure TForm2.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  Bus.BusShutDown; //If the bus is loaded, this will stop it (Warning, message will ne be all processed !)
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  StartStandartBus;
  GUIHit := 0;
end;

procedure TForm2.FormDestroy(Sender: TObject);
var i : integer;
    lb : TBusClientReader;
begin
  for i := 0 to lst2.Count-1 do
  begin
    lb := TBusClientReader(lst2.Items.Objects[i]);
    try
      Bus.UnSubscribe(lb);
    finally
      freeAndNil(lb);
    end;
  end;
  ReleaseStandartBus;
end;

procedure TForm2.btn1Click(Sender: TObject);
var lb : TBusMessage;
begin
  lb.FromString('This a test at '+DateToStr(Now));
  Bus.Send(lb, edt1.Text);
  UpdateListView;
end;

procedure TForm2.btn2Click(Sender: TObject);
var la : TBusClientReader;
begin
  la := Bus.Subscribe(edt1.Text,OnReceived);   //La owned by your app : you have to manage it.
  lst2.AddItem('Reader on '+la.ChannelListening,la);

  bus.ChannelSet(edt1.Text,TBusChannelBehaviour.bcbTopic,false,cbEchoEnabled.Checked);
end;

procedure TForm2.btn3Click(Sender: TObject);
var lb : TBusMessage;
    i : integer;
    c : String;
    ct : integer;
begin
  c := edt1.Text;
  ct := TButton(Sender).Tag-1;
  if cbThreadedSend.Checked then
  begin
    With TMyThread.Create(ct,c) do
      Start;
  end
  else
  begin
    for I := 0 to ct do
    begin
      lb.FromString('['+IntToStr(i)+'] This a test at '+DateToStr(Now));
      Bus.Send(lb, c);
      if i mod 100 = 0 then
        Application.ProcessMessages;
    end;
  end;
end;

procedure TForm2.btn4Click(Sender: TObject);
var la : TBusClientReader;
begin
  lvSubscripters.Clear; //Will be rebuild by timer.
  if lst2.ItemIndex>-1 then
  begin
     la := TBusClientReader(lst2.Items.Objects[lst2.ItemIndex]);
     if Bus.UnSubscribe(la) then
     begin
       la.Free;
       lst2.DeleteSelected;
     end;
  end
  else
  begin
    ShowMessage('Select a subscribter first.');
  end;
end;



procedure TForm2.btTest2Click(Sender: TObject);
var  i : integer;
     l : TStringList;
begin
  l := TStringList.Create;
  try
    for i := 0 to lst2.Count-1 do
    begin
      l.Add(TBusClientReader(lst2.Items.Objects[i]).ChannelListening);
    end;

    With TMyThreadDistri.Create(10000) do
    begin
      List.Text := l.Text;
      Start;
    end;
  finally
    l.Free;
  end;
end;

procedure TForm2.btTestClick(Sender: TObject);
var la : TBusClientReader;
    i : integer;
    l : TStringList;
begin
  l := TStringList.Create;
  try
  for i := 0 to 200 do
  begin
    la := Bus.Subscribe(edt1.Text+'Test '+IntToStr(i+1),OnReceived);   //La owned by your app : you have to manage it.
    lst2.AddItem('Reader on '+la.ChannelListening,la);
    l.Add(la.ChannelListening);
  end;
  finally
    l.Free;
  end;
end;

procedure TForm2.Button1Click(Sender: TObject);
begin
  Bus.DeclareDataRepository(Edit1.Text);
  if ListBox1.Items.IndexOf(Edit1.Text)=-1 then
    ListBox1.AddItem(Edit1.Text,nil);
end;

procedure TForm2.Button2Click(Sender: TObject);
var a : TBusClientDataRepo;
begin
  if Listbox1.ItemIndex>-1 then
  begin
    a := TBusClientDataRepo.Create(Bus,ListBox1.Items[Listbox1.ItemIndex]);
    try
      a.SetValue(Edit3.Text,Memo2.Text);
    finally
      FreeAndNil(a);
    end;
  end;
end;

procedure TForm2.Button5Click(Sender: TObject);
begin
  case RadioGroup1.ItemIndex of
  0 : bus.ChannelSetAsTopic(edt1.Text,chkMemPersistant.Checked,StrToInt(Edit2.Text));
  1 : Bus.ChannelSetAsQueue(edt1.Text,TBusChannelBehaviourQueueSpecific(RadioGroup2.ItemIndex),chkMemPersistant.Checked,StrToInt(Edit2.Text));
  end;
end;

procedure TForm2.Button6Click(Sender: TObject);
begin
  With TMyThreadDataRepoStress.Create(10000,Edit1.Text) do
    Start;
end;

procedure TForm2.Button7Click(Sender: TObject);
var a : TBusClientDataRepo;
    temp : String;
begin
  if Listbox1.ItemIndex>-1 then
  begin
    a := TBusClientDataRepo.Create(Bus,ListBox1.Items[Listbox1.ItemIndex]);
    try
      a.GetValue(Edit3.Text,temp);
      Memo2.Text := temp;
    finally
      FreeAndNil(a);
    end;
  end;
end;

procedure TForm2.Button8Click(Sender: TObject);
var a : TBusClientDataRepo;
begin
  if Listbox1.ItemIndex>-1 then
  begin
    a := TBusClientDataRepo.Create(Bus,ListBox1.Items[Listbox1.ItemIndex]);
    try
      if a.isKeyEntryExits(Edit3.Text) then
         showMessage('the key "'+edit3.text+'" already exists !')
      else
         showMessage('the key "'+edit3.text+'" does not exist !')
    finally
      FreeAndNil(a);
    end;
  end;
end;

procedure TForm2.UpdateListView;
var astrSub, asub, astrchan, achan : TStringList;
    lv : TListItem;

    Procedure GUIUpdateSubList;
    var i : Integer;
    begin
      if lvSubscripters.Items.Count<>astrSub.Count-1 then
        lvSubscripters.Clear;

      lvSubscripters.Items.BeginUpdate;
      try
        astrSub.Delete(0); //header.
        if astrSub.Count=0 then
          Exit;

        for i := 0 to astrSub.Count-1 do
        begin
          if lvSubscripters.Items.Count<=i then //Create new line if needed.
          begin
            lv := lvSubscripters.Items.Add;
            lv.SubItems.Add(EmptyStr);
            lv.SubItems.Add(EmptyStr);
            lv.SubItems.Add(EmptyStr);
          end;

          asub.DelimitedText := astrSub[i];
          lv := lvSubscripters.Items[i];
          lv.caption := asub[0];
          lv.SubItems[0] := asub[1];
          lv.SubItems[1] := asub[2];
          lv.SubItems[2] := asub[3];
        end;
      finally
        lvSubscripters.Items.EndUpdate;
      end;
    end;

    Procedure GUIUpdateChanList;
    var i : Integer;
    begin
      if ListView2.Items.Count<>astrchan.Count-1 then
        ListView2.Clear;
      ListView2.Items.BeginUpdate;
      try
       astrchan.Delete(0); //Header.
        if astrchan.Count=0 then
          Exit;

        for i := 0 to astrchan.Count-1 do
        begin
          achan.DelimitedText := astrchan[i];
          if ListView2.Items.Count<=i then //Create new line if needed.
          begin
            lv := ListView2.Items.Add;
            lv.SubItems.Add(EmptyStr);
            lv.SubItems.Add(EmptyStr);
            lv.SubItems.Add(EmptyStr);
            lv.SubItems.Add(EmptyStr);
            lv.SubItems.Add(EmptyStr);
            lv.SubItems.Add(EmptyStr);
            lv.SubItems.Add(EmptyStr);
            lv.SubItems.Add(EmptyStr);
          end;

          ListView2.Items[i].caption := achan[0];
          ListView2.Items[i].SubItems[0] := achan[1];
          ListView2.Items[i].SubItems[1] := achan[2];
          ListView2.Items[i].SubItems[2] := achan[3];
          ListView2.Items[i].SubItems[3] := achan[4];
          ListView2.Items[i].SubItems[4] := achan[5];
          ListView2.Items[i].SubItems[5] := achan[6];
          ListView2.Items[i].SubItems[6] := achan[7];
          ListView2.Items[i].SubItems[7] := achan[8];
        end;
      finally
        ListView2.Items.EndUpdate;
      end;
    end;

begin
  astrSub := TStringList.Create;
  astrChan := TStringList.Create;
  aSub := TStringList.Create;
  aChan := TStringList.Create;
  try
    Bus.GetChannelsConfigurationAsCSV(astrChan);
    Bus.GetSubscribtersConfigurationAsCSV(astrSub);

    //Update list view's GUI.
    GUIUpdateSubList;
    GUIUpdateChanList;

    Label4.Caption := Bus.Stats;

  finally
    FreeAndNil(astrsub);
    FreeAndNil(astrchan);
    FreeAndNil(asub);
    FreeAndNil(achan);
  end;
end;

procedure TForm2.OnReceived(Sender: TBusSystem; aReader : TBusClientReader; var Packet: TBusEnvelop);
begin
  inc(GUIHit);
end;

procedure TForm2.RadioGroup1Click(Sender: TObject);
begin
  RadioGroup2.Visible :=  RadioGroup1.ItemIndex = 1;
  RadioGroup2.ItemIndex := 0;
end;

procedure TForm2.TimerBusQueryTimer(Sender: TObject);
var lb : array of TBusClientReader;
    i : integer;
begin
  //Build array for submiting to Bus Query...
  SetLength(lb,lst2.Count);
  for i := 0 to lst2.Count-1 do
  begin
    lb[i] := TBusClientReader(lst2.Items.Objects[i]);
  end;
  //ask bus if we get message.
  BusProcessMessages(lb);
end;

procedure TForm2.TimerGUITimer(Sender: TObject);
begin
  UpdateListView;
  Label1.Caption := IntToStr(GUIHit);
  if bus.Idle then
    Label8.Caption := 'Bus is idling...'
  else
    Label8.Caption := 'Bus is working.'
end;

{ TMyThread }

constructor TMyThread.Create(aCounter: Uint32; aChannel: string);
begin
  inherited Create(true);
  FreeOnTerminate := true;
  counter :=  aCounter;
  channel := aChannel;
end;

procedure TMyThread.Execute;
var lbp : TBusMessage;
    j : integer;
begin
  for j := 0 to Counter do
  begin
    if Application.Terminated then //°|°
      Exit;
    lbp.FromString('['+IntToStr(j)+'] This a test at '+DateToStr(Now));
    Bus.Send(lbp, Channel);
  end;
end;

{ TMyThreadDistri }

constructor TMyThreadDistri.Create(aCounter: Uint32);
begin
  inherited Create(true);
  FreeOnTerminate := true;
  counter :=  aCounter;
  List :=  TStringList.Create;
end;

destructor TMyThreadDistri.Destroy;
begin
  FreeAndNil(list);
  inherited;
end;

procedure TMyThreadDistri.Execute;
var lbp : TBusMessage;
    i,j : Integer;
begin
  for j := 0 to Counter do
  begin
    for i:= 0 to List.Count-1 do
    begin
      if Application.Terminated then //°|°
        Exit;
      lbp.FromString('['+IntToStr(i)+'/'+intToStr(j)+'] This a test at '+DateToStr(Now));
      Bus.Send(lbp, list[i]);
    end;
  end;
end;

{ TMyThreadDataRepoStress }

constructor TMyThreadDataRepoStress.Create(aCounter: Uint32; const aRepoName : String);
begin
  Inherited Create(true);
  FreeOnTerminate := true;
  counter := aCounter;
  RepoName := aRepoName;
  Assert(trim(aRepoName)<>'');
end;


procedure TMyThreadDataRepoStress.Execute;
var a : TBusClientDataRepo;
    i : integer;
    t,temp : string;
begin
  if counter>0 then
  begin
    for I := 0 to counter-1 do
    begin
      if Application.Terminated then //°|°
        Exit;
      a := TBusClientDataRepo.Create(Bus,RepoName);
      try
        t := 'test value...'+inttostr(i);
        a.SetValue('test',t);
        a.GetValue('test',temp);
        //here t and temp could be different, if we launch many thread.
      finally
        FreeAndNil(a);
      end;
    end;
  end;
end;

end.
