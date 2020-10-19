program CarRacing;

uses
  System.StartUpCopy,
  FMX.Forms,
  CarRacing.uMain in 'CarRacing.uMain.pas' {FormMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
