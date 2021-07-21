program MailboxLayerAPIwithDelphiFireMonkey;

uses
  System.StartUpCopy,
  System.SysUtils,
  View.Main in 'View.Main.pas' {MainFrame: TFrame},
  Model.Constants in 'Model.Constants.pas',
  Model.Types in 'Model.Types.pas',
  Model.Utils in 'Model.Utils.pas',
  View.Menu in 'View.Menu.pas' {MenuFrame: TFrame},
  View.Menu.Item in 'View.Menu.Item.pas' {MenuItemFrame: TFrame},
  Model in 'Model.pas',
  ViewModel in 'ViewModel.pas',
  View.Home in 'View.Home.pas' {HomeFrame: TFrame},
  View.Home.DataRect in 'View.Home.DataRect.pas' {DataRectFrame: TFrame},
  View.NewForm in 'View.NewForm.pas' {NewFormFrame: TFrame},
  FMX.Forms,
  View in 'View.pas' {ViewForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TViewForm, ViewForm);
  Application.Run;
end.
