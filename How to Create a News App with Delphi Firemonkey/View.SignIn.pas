unit View.SignIn;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Layouts, FMX.Controls.Presentation, FMX.Edit,
  Data.Bind.EngExt, Fmx.Bind.DBEngExt, System.Rtti, Model.Utils,
  Model.Constants, Model.Types;

type
  TSignInFrame = class(TFrame, IActivityInfo)
    MainGridPanelLayout: TGridPanelLayout;
    LogoImg: TImage;
    GridPanelEdits: TGridPanelLayout;
    EmailEdit: TEdit;
    PassEdit: TEdit;
    GridPanelAdds: TGridPanelLayout;
    RememberMeCheckBox: TCheckBox;
    SignInBtn: TButton;
    ShowPassImg: TImage;
    EmailHiddenLbl: TLabel;
    PassHiddenLbl: TLabel;
    HitBox: TLayout;
    lbl1: TLabel;
    lbl2: TLabel;
    pnlSignUpLabels: TGridPanelLayout;
    pnlTermsOfUSeAndPrivacy: TLayout;
    grdTermsOfUSeAndPrivacy: TGridPanelLayout;
    lblTermsAndPolicy1: TLabel;
    lblTermsOfUse: TLabel;
    lblTermsAndPolicy2: TLabel;
    lblPrivacyPolicy: TLabel;
    VertScrollBox: TVertScrollBox;
    procedure ShowPassImgClick(Sender: TObject);
    procedure PassEditTyping(Sender: TObject);
    procedure SignUpClick(Sender: TObject);
    procedure SignInBtnClick(Sender: TObject);
    procedure lblClick(Sender: TObject);
  private
    FActivityName: string;
    function GetActivityName: string;
    procedure SetActivityName(AActivityName: string);
    function GetPrevActivityName: string;
    procedure SetPrevActivityName(AActivityName: string);
    { Private declarations }
  public
    property ActivityName: string read GetActivityName write SetActivityName;
    property PrevActivityName: string read GetPrevActivityName write SetPrevActivityName;
    constructor Create(Ownrer: TComponent); override;
  end;

implementation

uses
  View;

{$R *.fmx}

// Creating frame
constructor TSignInFrame.Create(Ownrer: TComponent);
begin
  inherited;
  pnlSignUpLabels.Visible := (GetMainForm as IViewInfo).IsActivityPresent(sActivitySignUp);
{$IFNDEF MSWINDOWS}
  var VI: IViewInfo := GetMainForm as IViewInfo;
  MainGridPanelLayout.Align := TAlignLayout.Top;
  MainGridPanelLayout.Height := GetMainForm.Height;
  if Assigned(VI)then
  begin
    if VI.GetScreenOrientation =  TScreenOrientation.Landscape then
      MainGridPanelLayout.Height := GetMainForm.Width;
  end;
{$ENDIF}
end;

// Event handler for entering text into the Edit.
function TSignInFrame.GetActivityName: string;
begin
  Result := FActivityName;
end;

function TSignInFrame.GetPrevActivityName: string;
begin
 //
end;

procedure TSignInFrame.lblClick(Sender: TObject);
begin
  GetMainForm.ShowActivity(ExtractActivityName(TControl(Sender).Name), True);
end;

procedure TSignInFrame.PassEditTyping(Sender: TObject);
begin
  DoEditTyping(Sender);
end;

// Event handler to control which hiding/showing the password.
procedure TSignInFrame.SetActivityName(AActivityName: string);
begin
  FActivityName := AActivityName;
end;

procedure TSignInFrame.SetPrevActivityName(AActivityName: string);
begin
//
end;

procedure TSignInFrame.ShowPassImgClick(Sender: TObject);
begin
  DoShowPassImgClick(Sender)
end;

// Event handler for SingIn button
procedure TSignInFrame.SignInBtnClick(Sender: TObject);
begin
  // Checking if MainForm support UserInfo interface.
  var UserInfo: IUserInfo := GetMainForm as IUserInfo;
  if not Assigned(UserInfo) then
     Exit;
//  var UserSettings := UserInfo.GetUserSettings;
//  if not Assigned(UserSettings) then
//    Exit;

  // Skip checkig email & password if values is epmty.
  if EmailEdit.Text.IsEmpty and PassEdit.Text.IsEmpty then
    GetMainForm.ShowActivity(sActivityHomeName, True)
  else
    ValidateEmail(EmailEdit);

//  if not ValidateEmail(EmailEdit.Text) then
//  begin
//    EmailEdit.Text := EmptyStr;
//    EmailHiddenLbl.StyledSettings := [];
//    EmailHiddenLbl.Text := sInvalidEmailFormat;
//    EmailHiddenLbl.TextSettings.FontColor := TAlphaColorRec.Red;
//    DoEditTyping(EmailEdit);
//  end;
  // SignIn.
//  if UserSettings.SignIn(EmailEdit.Text, PassEdit.Text) then
//  begin
//    GetMainForm.ShowActivity(sActivityHomeName, True);
//    ViewForm.MenuFrame.UserImg.Bitmap.Assign(UserSettings.CurrentUser.UserPicture);
//    ViewForm.MenuFrame.lblUsername.Text := UserSettings.CurrentUser.FirstName;
//    ViewForm.MenuFrame.UserImg.MakeCircleMask;
//  end
//  else
//    ShowMessage(sUseSignInError);
end;

// Event handler for SignUp button
procedure TSignInFrame.SignUpClick(Sender: TObject);
begin
  GetMainForm.ShowActivity(sActivitySignUp, True);
end;

initialization
  // Register frame
  RegisterClass(TSignInFrame);
finalization
  // Unregister frame
  UnRegisterClass(TSignInFrame);

end.
