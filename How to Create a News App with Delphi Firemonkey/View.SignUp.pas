//---------------------------------------------------------------------------

// This software is Copyright (c) 2021 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit View.SignUp;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Controls.Presentation, FMX.Edit, FMX.Layouts, Model.Utils,
  Model.Types, System.Actions, FMX.ActnList, FMX.StdActns, Model.Constants,
{$IFDEF ANDROID}
  Androidapi.JNI.Os,
  Androidapi.JNI.JavaTypes,
{$ENDIF}
  FMX.MediaLibrary.Actions;

type
  // SignUp frame.
  TSignUpFrame = class(TFrame, IActivityInfo)
    MainGridPanelLayout: TGridPanelLayout;
    GridPanelEdits: TGridPanelLayout;
    FirstnameEdit: TEdit;
    EmailHiddenLbl: TLabel;
    LastnameEdit: TEdit;
    PassHiddenLbl: TLabel;
    GridPanelAdds: TGridPanelLayout;
    btnSignUp: TButton;
    EmailEdit: TEdit;
    Label2: TLabel;
    PassEdit: TEdit;
    ShowPassImg: TImage;
    PassLbl: TLabel;
    ConfPassEdit: TEdit;
    ShowConfPassImg: TImage;
    ConfPassLbl: TLabel;
    AccountSVG: TPath;
    lblSignIn1: TLabel;
    OpenDialog: TOpenDialog;
    hitboxAccountSVG: TLayout;
    pnlSignIn: TLayout;
    GridPanelLayout1: TGridPanelLayout;
    lblSignIn2: TLabel;
    grdTermsOfUSeAndPrivacy: TGridPanelLayout;
    lblTermsAndPolicy1: TLabel;
    lblTermsOfUse: TLabel;
    lblTermsAndPolicy2: TLabel;
    lblPrivacyPolicy: TLabel;
    pnlTermsOfUSeAndPrivacy: TLayout;
    svgPassNotEqal: TPath;
    VertScrollBox: TVertScrollBox;
    HitsBypassLayout: TLayout;
    procedure lblSignIn1Click(Sender: TObject);
    procedure FirstnameEditTyping(Sender: TObject);
    procedure ShowPassImgClick(Sender: TObject);
    procedure AccountSVGClick(Sender: TObject);
    procedure btnSignUpClick(Sender: TObject);
    procedure lblClick(Sender: TObject);
    procedure lblSignIn2ApplyStyleLookup(Sender: TObject);
    procedure svgPassNotEqalClick(Sender: TObject);
  private
    FUserImage: TImage;
    FActivityName: string;
    FMultiplatformImageLoader: TAppWizardImageLoader;
    function GetActivityName: string;
    procedure SetActivityName(AActivityName: string);
    function GetPrevActivityName: string;
    procedure SetPrevActivityName(AActivityName: string);
    // Handler for Gallery access.
    procedure DoOnDidFinishTaking(Image: TBitmap);
  public
    property ActivityName: string read GetActivityName write SetActivityName;
    property PrevActivityName: string read GetPrevActivityName write SetPrevActivityName;
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{$R *.fmx}

// Event handler fires after picked image from gallery.
procedure TSignUpFrame.DoOnDidFinishTaking(Image: TBitmap);
begin
  if Assigned(Image) then
  begin
    AccountSVG.AddObject(FUserImage);
    AccountSVG.Data.Data := EmptyStr;
    FUserImage.Bitmap.Assign(Image);
    FUserImage.MakeCircleMask;
  end
  else
  begin
    FUserImage.DisposeOf;
    AccountSVG.Data.Data := AccountSVG.TagString
  end;
end;

// Event handler for AccountSVG control.
procedure TSignUpFrame.AccountSVGClick(Sender: TObject);
begin
  FUserImage := TImage.Create(AccountSVG);
  FUserImage.Align := TAlignLayout.Client;
  FUserImage.OnClick := AccountSVG.OnClick;
  AccountSVG.TagString := AccountSVG.Data.Data;
{$IFDEF MSWINDOWS}
  OpenDialog.Execute;
  if FileExists(OpenDialog.FileName) then
  begin
    FUserImage.Bitmap.LoadFromFile(OpenDialog.FileName);
    FUserImage.MakeCircleMask;
    AccountSVG.AddObject(FUserImage);
    AccountSVG.Data.Data := EmptyStr;
  end
  else
  begin
    FUserImage.DisposeOf;
    AccountSVG.Data.Data := AccountSVG.TagString
  end;
{$ELSE}
  FMultiplatformImageLoader.Exec;
{$ENDIF}
end;

// Creating frame.
constructor TSignUpFrame.Create(AOwner: TComponent);
begin
  inherited;
  // Creating Image loading routine.
  // Assigning necessary event handler.
  var VI: IViewInfo := GetMainForm as IViewInfo;
  FMultiplatformImageLoader := TAppWizardImageLoader.Create(DoOnDidFinishTaking);
  pnlTermsOfUSeAndPrivacy.Visible := not ((GetMainForm as IViewInfo).IsActivityPresent(sActivityTermsOfUse) xor
    (GetMainForm as IViewInfo).IsActivityPresent(sActivityPrivacy));
  HitsBypassLayout.Visible := not ((Vi.GetScreenOrientation = TScreenOrientation.Portrait) or (TOSVersion.Platform = TOSVersion.TPlatform.pfWindows));
{$IFDEF ANDROID}
  GrantAndroidPermission(EnumPermissions.WRITE_EXTERNAL_STORAGE);
{$ENDIF}
{$IFNDEF MSWINDOWS}
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
procedure TSignUpFrame.FirstnameEditTyping(Sender: TObject);
begin
  svgPassNotEqal.Visible := (((TEdit(Sender).Name = PassEdit.Name) or (TEdit(Sender).Name = ConfPassEdit.Name))
    and (ConfPassEdit.Text.Length > 0)) and (PassEdit.Text <> ConfPassEdit.Text);
  DoEditTyping(Sender);
end;

function TSignUpFrame.GetActivityName: string;
begin
  Result := FActivityName;
end;

function TSignUpFrame.GetPrevActivityName: string;
begin
  //
end;

// Event handler for control which hiding/showing the password.
procedure TSignUpFrame.SetActivityName(AActivityName: string);
begin
  FActivityName := AActivityName;
end;

procedure TSignUpFrame.SetPrevActivityName(AActivityName: string);
begin
  //
end;

procedure TSignUpFrame.ShowPassImgClick(Sender: TObject);
begin
  DoShowPassImgClick(Sender);
end;

procedure TSignUpFrame.svgPassNotEqalClick(Sender: TObject);
begin
  ShowMessage(svgPassNotEqal.Hint);
end;

// Adding a user. After adding, you can SignIn.
procedure TSignUpFrame.btnSignUpClick(Sender: TObject);
begin
  if not EmailEdit.Text.IsEmpty then
    ValidateEmail(EmailEdit);
  // Temporary disabled SignUp functionality
  Exit;
  // Checking if MainForm support UserInfo interface.
  var UserInfo: IUserInfo := GetMainForm as IUserInfo;
  if not Assigned(UserInfo) then
    Exit;
  // Checking if UserSettings object exist.
  var UserSettings := UserInfo.GetUserSettings;
  if not Assigned(UserSettings) then
    Exit;
  // SignUp routines.
  UserSettings.SignUp(EmailEdit.Text, FirstnameEdit.Text, LastnameEdit.Text, PassEdit.Text, FUserImage.Bitmap)
end;

// Event handler for lblSignIn button(to get back to SignIn screen).
procedure TSignUpFrame.lblSignIn1Click(Sender: TObject);
begin
  GetMainForm.ShowActivity(sActivitySignInName, True);
end;

procedure TSignUpFrame.lblSignIn2ApplyStyleLookup(Sender: TObject);
begin
  if TLabel(Sender).GetStyledColor <> TAlphaColorRec.Black then
    AccountSVG.Fill.Color := MATERIAL_UI_GREY_100
  else
    AccountSVG.Fill.Color := MATERIAL_UI_GREY_400
end;

procedure TSignUpFrame.lblClick(Sender: TObject);
begin
  GetMainForm.ShowActivity(ExtractActivityName(TControl(Sender).Name), True);
end;

initialization
  // Register frame
  RegisterClass(TSignUpFrame);
finalization
  // Register frame
  UnRegisterClass(TSignUpFrame);

end.
