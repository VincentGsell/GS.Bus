program ProjectMinimalBus;

uses
  Vcl.Forms,
  fmain in 'fmain.pas' {FormMain};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := true;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
