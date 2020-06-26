unit ProjectRaytracerTestFMX.fmain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Surfaces,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects, FMX.Platform,
  System.Diagnostics,
  raytrace,
  System.Threading,

  GS.Bus.Services,
  GS.Threads.Pool;

Const
  BMPSIZE = 512; //1200; //400; //2400;
  GTHREADCOUNT = 8;  //Thread count and BMPSIZE should be multiple of 2 :  For rebuliding final image without mantissa issue.

type
  TForm3 = class(TForm)
    Image1: TImage;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Panel1: TPanel;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Panel2: TPanel;
    Button7: TButton;
    Panel3: TPanel;
    Button8: TButton;
    Panel4: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    gsw  : TStopwatch;
    BenchLoop : NativeInt;
    aThreadPool : TStackThreadPool;


    procedure Log(text : string);
    procedure LogClear;
    procedure transfertRawToImg(RawBmp  : TRawBitmap);
    procedure ConvertRawToBitmapAndDrawIt(Index: Integer; Raw: TRawBitmap);
  end;

var
  Form3: TForm3;
  PlatformTimer : IFMXTimerService;

implementation

Uses
  RayTraceThread.ClassicThread,
  RayTraceThread.GSBusService,
  RayTraceThread.GSThreadsPool;

Function GetTickCount : Cardinal;
begin
  Result := Round(PlatformTimer.GetTick * 1000);
end;


{$R *.fmx}

procedure TForm3.transfertRawToImg(RawBmp  : TRawBitmap);
var
  bb : TBitmapSurface;
  p     :  ^Int32;
begin
  bb := TBitmapSurface.Create;
  try
    bb.SetSize(BMPSIZE,BMPSIZE,TPixelFormat.RGBA);
    p := bb.Scanline[0];
    Move(RawBmp.Buffer[0], p^, bb.Width*bb.Height*SizeOf(Int32));
    bb.Flip;
    Image1.Bitmap.Assign(bb);
  finally
    FreeAndNil(bb);
  end;
end;

procedure TForm3.ConvertRawToBitmapAndDrawIt(Index: Integer; Raw: TRawBitmap);
var
  bb : TBitmapSurface;
  b : TBitmap;
  p     :  ^Int32;
  coffset :   Integer;
  c : Integer;
begin
  c := Round(BMPSIZE / GTHREADCOUNT);
  coffset := ((Index-1) * (c));

  bb := TBitmapSurface.Create;
  bb.SetSize(Raw.Width,Raw.Height,TPixelFormat.RGBA);

  p := bb.Scanline[0];
  Move(Raw.Buffer[0], p^, bb.Width*bb.Height*SizeOf(Int32));
  bb.Flip;

  b := TBitmap.Create;
  b.Assign(bb);
  //b.SaveToFile('t'+inttostr(index)+'.jpg');

  if (Image1.Bitmap.Width<>BMPSIZE) Or
     (Image1.Bitmap.Height<>BMPSIZE) then
  begin
    Image1.Bitmap.SetSize(BMPSIZE,BMPSIZE);
  end;

  Image1.Bitmap.Canvas.BeginScene;
  image1.Bitmap.Canvas.DrawBitmap(b,Rect(0,0,bb.Width,bb.Height),Rect(coffset,0,coffset+c,BMPSIZE),1.0);
  Image1.Bitmap.Canvas.EndScene;
  b.Free;
  bb.Free;
end;


{ TForm3 }

procedure TForm3.Button1Click(Sender: TObject);
var
  sc  : Scene;
  rt  : RayTracerEngine;
  s   : String;

  RawBmp  : TRawBitmap;
  p     :  ^Int32;
  i : integer;
begin
  Button1.Enabled := false;
  LogClear;

  sc := Scene.Create(Sin(Random(360)));
  rt := RayTracerEngine.Create();

  RawBmp := TRawBitmap.Create;
  RawBmp.SetSize(BMPSIZE,BMPSIZE);

  gsw.Reset; gsw.Start;
  rt.render(sc,BMPSIZE,BMPSIZE,0,BMPSIZE,RawBmp);
  gsw.Stop;

  transfertRawToImg(RawBmp);

  sc.Free;
  rt.Free;
  RawBmp.Free;

  Log('[NO THREAD] Completed in: ' + IntToStr(gsw.ElapsedMilliseconds) + ' ms');
  Button1.Enabled := true;
end;

procedure TForm3.Button2Click(Sender: TObject);
var l,ll : NativeInt;
begin
  l := GetTickCount;
  try
    For var i := 0 to (BenchLoop-1) do
    begin
      Button3Click(Sender);
    end;
    ll := GetTickCount;
    label1.Text := FloaTtoStr((ll-l)/1000)+' sec.';
  except
    On E : Exception do
    begin
      Label1.Text := 'Failed';
      log(E.Message);
    end;
  end;
  Application.ProcessMessages;

  l := GetTickCount;
  try
    For var i := 0 to (BenchLoop-1) do
    begin
      Button6Click(Sender);
    end;
    ll := GetTickCount;
    label2.Text := FloaTtoStr((ll-l)/1000)+' sec.';
  except
    On E : Exception do
    begin
      Label2.Text := 'Failed';
      log(E.Message);
    end;
  end;
  Application.ProcessMessages;

  l := GetTickCount;
  try
    For var i := 0 to (BenchLoop-1) do
    begin
      Button7Click(Sender);
    end;
    ll := GetTickCount;
    Label3.Text := FloaTtoStr((ll-l)/1000)+' sec.';
  except
    On E : Exception do
    begin
      label3.Text := 'Failed';
      log(E.Message);
    end;
  end;
  Application.ProcessMessages;

  l := GetTickCount;
  try
    For var i := 0 to (BenchLoop-1) do
    begin
      Button8Click(Sender);
    end;
    ll := GetTickCount;
    label4.Text := FloaTtoStr((ll-l)/1000)+' sec.';
  except
    On E : Exception do
    begin
      label4.Text := 'Failed';
      log(E.Message);
    end;
  end;
  Application.ProcessMessages;
end;

procedure TForm3.Button3Click(Sender: TObject);
var i : integer;
    lt : Array of TraytracerThread;
    lv : Double;
begin
//  Button4.Enabled := false;
  LogClear;

  lv := (Sin(Random(360)));
  SetLength(lt,GTHREADCOUNT-1);
  for i := 0 to GTHREADCOUNT-1 do
  begin
    lt[i] := TRaytracerthread.Create(true);
    lt[i].FreeOnTerminate := false;
    lt[i].Id := i+1;
    lt[i].X1 := (i) * (BMPSIZE div GTHREADCOUNT);
    lt[i].X2 := (i+1) * (BMPSIZE div GTHREADCOUNT);
    lt[i].Engine := RayTracerEngine.Create;
    lt[i].Model := Scene.Create(lv);
    lt[i].Bitmap := TRawBitmap.Create;
    lt[i].Bitmap.SetSize(BMPSIZE div GTHREADCOUNT,BMPSIZE);
    lt[i].BmpSideSize := BMPSIZE;
  end;

  gsw.Reset;
  gsw.Start;
  for i := 0 to GTHREADCOUNT-1 do
  begin
    lt[i].Start;
  end;

  for i := 0 to GTHREADCOUNT-1 do
  begin
    lt[i].WaitFor;
  end;
  gsw.Stop;

  for i := 0  to GTHREADCOUNT-1 do
  begin
    ConvertRawToBitmapAndDrawIt(lt[i].id,lt[i].Bitmap);
    Application.ProcessMessages;
  end;

  for i := 0  to GTHREADCOUNT-1 do
  begin
    lt[i].WaitFor;
    lt[i].Model.Free;
    lt[i].Bitmap.Free;
    lt[i].Engine.Free;
    lt[i].free;
  end;

  log('[Classic TThread] Completed in: ' + IntToStr(gsw.ElapsedMilliseconds) + ' ms');
  //Button4.Enabled := true;
end;

procedure TForm3.Button4Click(Sender: TObject);
begin
 Inc(BenchLoop);
 Button2.Text := Format('Bench (%d)',[BenchLoop]);
end;

procedure TForm3.Button5Click(Sender: TObject);
begin
 Dec(BenchLoop);
 if BenchLoop<5 then
   BenchLoop := 5;
 Button2.Text := Format('Bench (%d)',[BenchLoop]);
end;

procedure TForm3.Button6Click(Sender: TObject);
var i : Integer;
    b : array of TRawBitmap;
    lv : Double;
begin
  Button6.Enabled := false;
  LogClear;

  SetLength(b,GTHREADCOUNT);
  for I := 0 to GTHREADCOUNT-1 do
  begin
    b[i] := TRawBitmap.Create;
    b[i].SetSize(BMPSIZE div GTHREADCOUNT,BMPSIZE);
  end;

  lv := Sin(Random(360));
  gsw.reset;
  gsw.Start;
  TParallel.&For(0, GTHREADCOUNT-1,

    Procedure(AIndex : Integer)
    var a : Double;
        r : RayTracerEngine;
        m : Scene;
    begin
      m := Scene.Create(lv);
      r := RayTracerEngine.Create;
      try
        r.render( m,
                BMPSIZE,
                BMPSIZE,
                (AIndex) * BMPSIZE div GTHREADCOUNT,
                (AIndex+1) * BMPSIZE div GTHREADCOUNT,
                b[AIndex]);
      finally
        FreeAnDNil(r);
        FreeAndNil(m);
      end;
    end

    );
  gsw.Stop;

  for I := 0 to GTHREADCOUNT-1 do
  begin
    ConvertRawToBitmapAndDrawIt(i+1,b[i]);
    b[i].Free;
  end;


  Log('[Delphi ITASK] Completed in: ' + IntToStr(gsw.ElapsedMilliseconds) + ' ms');
  Button6.Enabled := true;
end;

procedure TForm3.Button7Click(Sender: TObject);
var lb : TCustomServiceManager;
    ls :  TCustomService;
    i : Integer;
    lt : TRaytraceTaskService;
    lta : TRaytracerThread;

    lStats : TThreadServiceStat;
    lv : Double;
begin
  Button7.Enabled := false;
  LogClear;

  lb := TCustomServiceManager.Create;
  lb.Start;

  lv := (Sin(Random(360)));
  for I := 0 to GTHREADCOUNT-1 do
  begin
    ls := TcustomService.Create;
    lb.RegisterService(ls);

    lt := TRaytraceTaskService.Create(lv);
    lt.Id := i+1;
    lt.X1 := (i) * (BMPSIZE div GTHREADCOUNT);
    lt.X2 := (i+1) * (BMPSIZE div GTHREADCOUNT);
    lt.Bitmap.SetSize(BMPSIZE div GTHREADCOUNT,BMPSIZE);
    lt.BmpSideSize := BMPSIZE;
    ls.Task := lt;
  end;

  gsw.Reset;
  gsw.Start;
  for i := 0  to GTHREADCOUNT-1 do
    lb.Services[i].StartService;
  for i := 0  to GTHREADCOUNT-1 do
    lb.Services[i].WaitFor;
  gsw.Stop;

  for i := 0  to GTHREADCOUNT-1 do
  begin
    log(lb.Services[i].ServiceStats.AsString);
    ConvertRawToBitmapAndDrawIt( TRaytraceTaskService(lb.Services[i].Task).id,
                                 TRaytraceTaskService(lb.Services[i].Task).Bitmap);
    lb.Services[i].Task.Free;
  end;
  FreeAndNil(lb);

  log('[GS.Bus.Services] Completed in: ' + IntToStr(gsw.ElapsedMilliseconds) + ' ms');

  Button7.Enabled := true;
end;

procedure TForm3.Button8Click(Sender: TObject);
var ltak : Array of TRaytracerStackTask;
    i : integer;
    lv : Double;
begin
  if not button8.enabled then
    exit;
  Button8.Enabled := false;
  LogClear;

  if not Assigned(AthreadPool) then
  begin
    AThreadPool := TStackThreadPool.Create(GTHREADCOUNT);
    AthreadPool.FreeTaskOnceProcessed := false;
  end;

  lv := (Sin(Random(360)));
  SetLength(ltak,GTHREADCOUNT);
  for i := 0 to GTHREADCOUNT-1 do
  begin
    ltak[i] := TRaytracerStackTask.Create(lv,5);
    ltak[i].Id := i+1;
    ltak[i].X1 := (i) * (BMPSIZE div GTHREADCOUNT);
    ltak[i].X2 := (i+1) * (BMPSIZE div GTHREADCOUNT);
    ltak[i].Bitmap.SetSize(BMPSIZE div GTHREADCOUNT,BMPSIZE);
    ltak[i].BmpSideSize := BMPSIZE;
  end;

  gsw.Reset;
  gsw.Start;

  for i := 0  to GTHREADCOUNT-1 do
  begin
    aThreadPool.Submit(ltak[i]); //Start immediatly, the stack is consumed by a pool of GTRHREADCOUNT threads.
  end;
  AThreadPool.await;
  gsw.Stop;

  //transfert result.
  for i := 0  to GTHREADCOUNT-1 do
  begin
    ConvertRawToBitmapAndDrawIt(i+1,ltak[i].Bitmap);
    ltak[i].free;
    ltak[i] := nil;
  end;

  Log('[GS.Threads.Pool] Completed in: ' + IntToStr(gsw.ElapsedMilliseconds) + ' ms');
  Button8.Enabled := true;
end;

procedure TForm3.FormCreate(Sender: TObject);
begin
 gsw  := TStopwatch.Create;
 BenchLoop := 10;
 Button2.Text := Format('Bench (%d)',[BenchLoop]);
end;

procedure TForm3.FormDestroy(Sender: TObject);
begin
  if Assigned(aThreadPool) then
    FreeAndNil(aThreadPool);
end;

procedure TForm3.Log(text: string);
begin
  //...
end;

procedure TForm3.LogClear;
begin
  //...
end;

Initialization
  if Not(TPlatformServices.Current.SupportsPlatformService(IFMXTimerService,IInterface(PlatformTimer)) ) then
  begin
    Raise  Exception.Create('Timer not supported on this plateform. Abort.');
  end;

end.
