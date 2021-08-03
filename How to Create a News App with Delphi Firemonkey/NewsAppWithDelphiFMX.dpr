program NewsAppWithDelphiFMX;

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
  View.SignIn in 'View.SignIn.pas' {SignInFrame: TFrame},
  View.SignUp in 'View.SignUp.pas' {SignUpFrame: TFrame},
  View.Profile in 'View.Profile.pas' {ProfileFrame: TFrame},
  View.Home in 'View.Home.pas' {HomeFrame: TFrame},
  View.Home.DataRect in 'View.Home.DataRect.pas' {DataRectFrame: TFrame},
  View.Settings.Item in 'View.Settings.Item.pas' {SettingsItemFrame: TFrame},
  View.Settings in 'View.Settings.pas' {SettingsFrame: TFrame},
  View.Placeholder in 'View.Placeholder.pas' {PlaceholderFrame: TFrame},
  View.TermsOfUse in 'View.TermsOfUse.pas' {TermsOfUseFrame: TFrame},
  View.PrivacyPolicy in 'View.PrivacyPolicy.pas' {PrivacyPolicyFrame: TFrame},
  View.About in 'View.About.pas' {AboutFrame: TFrame},
  View.ContactsList in 'View.ContactsList.pas' {ContactsListFrame: TFrame},
  View.ContactsList.Item in 'View.ContactsList.Item.pas' {ContactListItemFrame: TFrame},
  View.NewForm in 'View.NewForm.pas' {NewFormFrame: TFrame},
  FMX.Forms,
  View in 'View.pas' {ViewForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TViewForm, ViewForm);
  Application.Run;
end.
