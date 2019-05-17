unit dmData;

interface

uses
  System.SysUtils, System.Classes, VCL.Controls,
  GS.MVVM, Vcl.ExtCtrls;
{$M+}
type
  TAnotherThread = Class;
  //TDataModule1 stay in the same threAD than formmain. (But it could be located in another thread)
  TDataModule1 = class(TDataModule)
    Timer1: TTimer;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FInputString: String;
    FMouseThread : TAnotherThread;
    FModelTime: String;
    procedure SetInputString(const Value: String);
    procedure SetModelTime(const Value: String);
    { Private declarations }
  public
    MyLocalMVVMComponent : TGSMVVM;
    { Public declarations }
  Published
    Property InputString : String read FInputString Write SetInputString;
    Property ModelTime : String read FModelTime Write SetModelTime;
  end;

  //this thread is a custom datamodule with its own powered system (thread.execute) ;)
  TAnotherThread = class(TThread)
  private
    function GetMouseCoordAsString: String;
    function GetMouseX: Integer;
    function GetMouseY: Integer;
  public
    MyMVVMThreaded : TGSMVVM;
    Procedure Execute; Override;

    property MouseX : Integer read GetMouseX;
    property MouseY : Integer read GetMouseY;
    property MouseCoordAsString : String read GetMouseCoordAsString;
  end;

var
  DataModule1: TDataModule1;

implementation


{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

procedure TDataModule1.DataModuleCreate(Sender: TObject);
begin
  MyLocalMVVMComponent := TGSMVVM.Create;
  //Here, We declare simply the DM property as a published ressource of the MVVM.
  //Note that the property behind the ressource have a setter, where precessing is done.
  MyLocalMVVMComponent.Declare('MyMainDataModule',Self,'InputString','InputEngineString');
  MyLocalMVVMComponent.Declare('MyMainDataModule',Self,'ModelTime','MyModelTime');

  FMouseThread := TAnotherThread.Create(false);
end;

procedure TDataModule1.DataModuleDestroy(Sender: TObject);
begin
  MyLocalMVVMComponent.Free;
  FMouseThread.Terminate;
  FMouseThread.WaitFor;
  FreeAndNil(FMouseThread);
end;

procedure TDataModule1.SetInputString(const Value: String);
begin
  FInputString := 'Hello World '+UpperCase(Value)+' !!!';
  MyLocalMVVMComponent.Activity(Self);
end;

procedure TDataModule1.SetModelTime(const Value: String);
begin
  FModelTime := Value;
  MyLocalMVVMComponent.Activity(Self);
end;

procedure TDataModule1.Timer1Timer(Sender: TObject);
begin
  ModelTime := DateTimeToStr(Now);
end;

{ TAnotherThread }

procedure TAnotherThread.Execute;
var lfx, lfy : Integer;
begin
  MyMVVMThreaded := TGSMVVM.Create;
  MyMVVMThreaded.Declare('MyMouseThread',Self,'MouseCoordAsString','MyMouseCoordAsString');
  MyMVVMThreaded.Declare('MyMouseThread',Self,'MouseX','MyMouseX');
  MyMVVMThreaded.Declare('MyMouseThread',Self,'MouseY','MyMouseY');
  lfx := -1; //Will trig once the update.
  while Not(Terminated) do
  begin
    if (lfx<>GetMouseX) or (lfy<>GetMouseY) then
    begin
      MyMVVMThreaded.Activity(Self);
      lfx := GetMouseX;
      lfy := GetMouseY;
    end;
    Sleep(1);
  end;
  FreeandNil(MyMVVMThreaded);
end;


function TAnotherThread.GetMouseCoordAsString: String;
begin
  result := Format('(%d,%d)',[Mouse.CursorPos.X,Mouse.CursorPos.Y]);
end;

function TAnotherThread.GetMouseX: Integer;
begin
  result := Mouse.CursorPos.X;
end;

function TAnotherThread.GetMouseY: Integer;
begin
  result := Mouse.CursorPos.Y;
end;


end.
