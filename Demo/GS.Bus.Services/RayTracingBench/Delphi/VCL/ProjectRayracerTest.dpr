program ProjectRayracerTest;

uses
  ScaleMM2,
  Vcl.Forms,
  fmainRTProject in 'fmainRTProject.pas' {Form1},
  raytrace in '..\..\raytrace.pas',
  RayTraceThread.ClassicThread in '..\..\RayTraceThread.ClassicThread.pas',
  RayTraceThread.GSBusService in '..\..\RayTraceThread.GSBusService.pas',
  RayTraceThread.GSThreadsPool in '..\..\RayTraceThread.GSThreadsPool.pas',
  GS.Bus in '..\..\..\..\..\GS.Bus.pas',
  GS.Bus.Services in '..\..\..\..\..\GS.Bus.Services.pas',
  GS.Threads in '..\..\..\..\..\..\GS.Core\GS.Threads.pas',
  GS.Threads.Pool in '..\..\..\..\..\..\GS.Core\GS.Threads.Pool.pas',
  GS.Common in '..\..\..\..\..\..\GS.Core\GS.Common.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
