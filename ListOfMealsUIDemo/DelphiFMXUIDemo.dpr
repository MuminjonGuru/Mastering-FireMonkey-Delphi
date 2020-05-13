program DelphiFMXUIDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain.Demo in 'uMain.Demo.pas' {FrmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFrmMain, FrmMain);
  Application.Run;
end.
