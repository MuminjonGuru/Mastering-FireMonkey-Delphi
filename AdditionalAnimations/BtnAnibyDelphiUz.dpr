program BtnAnibyDelphiUz;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FrmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFrmMain, FrmMain);
  Application.Run;
end.
