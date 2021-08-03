//---------------------------------------------------------------------------

// This software is Copyright (c) 2021 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit View.Profile;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Layouts, View.Main, System.ImageList, FMX.ImgList, FMX.Ani,
  FMX.Controls.Presentation, FMX.Edit, FMX.DateTimeCtrls, Model.Utils, Model.Constants,
  FMX.ListBox, FMX.Effects, System.Actions, FMX.ActnList, FMX.StdActns, Model.Types,
  FMX.MediaLibrary.Actions;

type
  // Application profile frame.
  TProfileFrame = class(TMainFrame)
    DataPanel: TGridPanelLayout;
    UserAddInfoPanel: TGridPanelLayout;
    ProfileSvg: TPath;
    UsernameLbl: TLabel;
    UserInfoPanel: TGridPanelLayout;
    EmailPanel: TGridPanelLayout;
    EmailLbl: TLabel;
    EmailEdit: TEdit;
    GenderPanel: TGridPanelLayout;
    GenderLbl: TLabel;
    BirthdayPanel: TGridPanelLayout;
    BirthdyayLbl: TLabel;
    BirthdayDateEdit: TDateEdit;
    SignOutBtn: TButton;
    Line1: TLine;
    Line2: TLine;
    Line3: TLine;
    Line4: TLine;
    Line5: TLine;
    Line6: TLine;
    ProfileEditSvg: TPath;
    SignOutHitBox: TLayout;
    GenderComboBox: TComboBox;
    OpenDialog: TOpenDialog;
    hitboxProfileSvg: TLayout;
    procedure SignOutBtnClick(Sender: TObject);
    procedure SignOutHitBoxClick(Sender: TObject);
    procedure ProfileSvgClick(Sender: TObject);
    procedure CaptionLblApplyStyleLookup(Sender: TObject);
  private
    FUserImage: TImage;
    FMultiplatformImageLoader: TAppWizardImageLoader;
    // Handler for Gallery access.
    procedure DoOnDidFinishTaking(Image: TBitmap);
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{$R *.fmx}

procedure TProfileFrame.CaptionLblApplyStyleLookup(Sender: TObject);
begin
  inherited;
  if TLabel(Sender).GetStyledColor <> TAlphaColorRec.Black then
    ProfileSvg.Fill.Color := MATERIAL_UI_GREY_100
  else
    ProfileSvg.Fill.Color := MATERIAL_UI_GREY_400
end;

constructor TProfileFrame.Create(AOwner: TComponent);
begin
  inherited;
  // Creating Image loading routine.
  // Assigning necessary event handler.
  var UserImage: TBitmap :=  (GetMainForm as IUserInfo).GetUserImage;
  if Assigned(UserImage) then
  begin
    FUserImage := TImage.Create(nil);
    FUserImage.Bitmap := (GetMainForm as IUserInfo).GetUserImage;
    if Assigned(FUserImage) and (FUserImage.Bitmap.Height > 0) then
      ProfileSvg.Data.Data := EmptyStr;
    FUserImage.Align := TAlignLayout.Client;
    FUserImage.OnClick := ProfileSvg.OnClick;
    ProfileSvg.AddObject(FUserImage);
  end;
{$IFDEF ANDROID}
  GrantAndroidPermission(EnumPermissions.WRITE_EXTERNAL_STORAGE);
{$ENDIF}
  FMultiplatformImageLoader := TAppWizardImageLoader.Create(DoOnDidFinishTaking);
end;

// Event handler fires after picked image from gallery.
procedure TProfileFrame.DoOnDidFinishTaking(Image: TBitmap);
begin
  if Assigned(Image) then
  begin
    ProfileSvg.Data.Data := EmptyStr;
    FUserImage.Bitmap.Assign(Image);
    FUserImage.MakeCircleMask;
    ProfileSvg.AddObject(FUserImage);
    (GetMainForm as IUserInfo).SetUserImage(FUserImage.Bitmap);
  end
  else
  begin
    ProfileSvg.Data.Data := ProfileSvg.TagString;
    FUserImage.DisposeOf;
  end;
end;

procedure TProfileFrame.ProfileSvgClick(Sender: TObject);
begin
  ProfileSvg.TagString := ProfileSvg.Data.Data;
  FUserImage := TImage.Create(ProfileSvg);
  FUserImage.Align := TAlignLayout.Client;
  FUserImage.OnClick := ProfileSvg.OnClick;
{$IFDEF MSWINDOWS}
  OpenDialog.Execute;
  if FileExists(OpenDialog.FileName) then
  begin
    FUserImage.Bitmap.LoadFromFile(OpenDialog.FileName);
    ProfileSvg.Data.Data := EmptyStr;
    FUserImage.MakeCircleMask;
    ProfileSvg.AddObject(FUserImage);
    (GetMainForm as IUserInfo).SetUserImage(FUserImage.Bitmap);
  end
  else
  begin
    ProfileSvg.Data.Data := ProfileSvg.TagString;
    FUserImage.DisposeOf;
  end;
{$ELSE}
  FMultiplatformImageLoader.Exec;
{$ENDIF}
end;

procedure TProfileFrame.SignOutBtnClick(Sender: TObject);
begin
  if ValidateEmail(EmailEdit.Text) or EmailEdit.Text.IsEmpty then
    GetMainForm.ShowActivity(sActivitySignInName, true)
  else
  begin
    ShowMessage(sInvalidEmailFormat);
    EmailEdit.Text := EmptyStr;
  end;
  // Hide Main Menu just by applying Portrait mode for controls alignment.
  var VU: IViewUtils := GetMainForm as IViewUtils;
  if Assigned(VU) then
    VU.PortraitRealign;
end;

procedure TProfileFrame.SignOutHitBoxClick(Sender: TObject);
begin
  GetMainForm.ShowActivity(sActivitySignInName, true);
end;

initialization
  // Register profile frame class
  RegisterClass(TProfileFrame);
finalization
  // Unregister profile frame class
  UnRegisterClass(TProfileFrame);

end.
