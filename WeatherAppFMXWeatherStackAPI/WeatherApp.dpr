program WeatherApp;

uses
  System.StartUpCopy,
  FMX.Forms,
  WeatherAppDelphi.MainUnit in 'WeatherAppDelphi.MainUnit.pas' {FormMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
