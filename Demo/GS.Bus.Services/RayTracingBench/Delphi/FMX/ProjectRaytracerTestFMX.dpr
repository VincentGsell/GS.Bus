program ProjectRaytracerTestFMX;

uses
  {$IFDEF MSWINDOWS}
  ScaleMM2,
  {$ENDIF}
  System.StartUpCopy,
  FMX.Forms,
  ProjectRaytracerTestFMX.fmain in 'ProjectRaytracerTestFMX.fmain.pas' {Form3},
  RayTraceThread.GSThreadsPool in '..\..\RayTraceThread.GSThreadsPool.pas',
  RayTraceThread.GSBusService in '..\..\RayTraceThread.GSBusService.pas',
  RayTraceThread.ClassicThread in '..\..\RayTraceThread.ClassicThread.pas',
  raytrace in '..\..\raytrace.pas';

{$R *.res}

begin
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := true;
  {$ENDIF}
  Application.Initialize;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
