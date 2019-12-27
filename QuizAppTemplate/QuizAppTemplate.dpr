program QuizAppTemplate;

uses
  System.StartUpCopy,
  FMX.Forms,
  QuizAppTemplate.uMain in 'QuizAppTemplate.uMain.pas' {FrmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFrmMain, FrmMain);
  Application.Run;
end.
