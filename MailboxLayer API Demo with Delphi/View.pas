//---------------------------------------------------------------------------

// This software is Copyright (c) 2021 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit View;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Layouts, FMX.Controls.Presentation, FMX.MultiView,
  FMX.MultiView.Types, FMX.Ani, FMX.Effects, Data.Bind.EngExt, Fmx.Bind.DBEngExt,
  Data.Bind.Components, Model.Types, Model.Constants,
  View.Main, View.Menu, Model.Utils, DateUtils,
  FMX.AddressBook.Types, FMX.AddressBook, ViewModel,
{$IFDEF ANDROID}
  FMX.DialogService,
  Androidapi.NativeActivity,
  Androidapi.Configuration,
  Androidapi.JNI.GraphicsContentViewText,
{$ENDIF}
  System.IOUtils;

type
  TViewForm = class(TForm, IPreloadAddressBook, IUserInfo, IBackgroundTask, IViewUtils, IViewInfo, ICommonUserSettings)
    MultiView: TMultiView;
    MenuFrame: TMenuFrame;
    ViewStyleBook: TStyleBook;
    procedure FormCreate(Sender: TObject);
    procedure FormSaveState(Sender: TObject);
    procedure FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
  private
    FAddressBookLoader: TAddressBookLoader;
    FUserName: string;
    FUserSettings: TUserSettings;
    FCommonUserSettings: TCommonUserSettings;
    FBackgroundTask: TThread;
    FViewModel: TViewModel;
    FPortraitOrientationMonitor: TScreenOrientationMonitor;
    FLandscapeOrientationMonitor: TScreenOrientationMonitor;
    FIsOrientationChanged: Boolean;
    FActivities: TStringList;
    procedure InitView;
    procedure ReadCommonUserSettings;
    procedure WriteCommonUserSetting;
{$IFDEF ANDROID}
    procedure CloseDialogEvent(Sender: TObject; const AResult: TModalResult);
{$ENDIF}
    { IViewInfo }
    // Check for Activity present
    function IsActivityPresent(AActivityName: string): Boolean;
  public
    procedure DoOnPortraitOrientation;
    procedure DoOnLandscapeOrientation;
    { IPreloadAddressBook }
    function GetPreloadAddressBookContacts: TAddressBookContacts;
    { IUserInfo }
    function GetUserName: string;
    procedure SetUserName(AValue: string);
    function GetUserSettings: TUserSettings;
    function GetUserImage: TBitmap;
    procedure SetUserImage(AUserImage: TBitmap);
    { IBackgroundTask }
    function GetBackgroundTask: TThread;
    procedure SetBackgroundTask(ATask: TThread);
    { IViewUtils }
    procedure RecreateView;
    procedure SetViewDarkMode(AValue: Boolean);
    procedure SetDefaultTheme;
    procedure PortraitRealign;
    procedure LandscapeRealign;
    { IViewInfo }
    function GetScreenOrientation: TScreenOrientation;
    function GetMultiView: TMultiView;
    function IsOrientationChanged: Boolean;
    { ICommonUserSettings }
    function GetCommonUserSettings: TCommonUserSettings;

    property ViewModel: TViewModel read FViewModel write FViewModel;
  end;

var
  ViewForm: TViewForm;

implementation

{$R *.fmx}

{$IFDEF MSWINDOWS}
{$R \Res\Styles\Styles_Win.res}
{$ENDIF}
{$IFDEF ANDROID}
{$R \Res\Styles\Styles_Android.res}
{$ENDIF}
{$IFDEF IOS}
{$R \Res\Styles\Styles_iOS.res}
{$ENDIF}

{ TViewForm }
{$IFDEF ANDROID}
procedure TViewForm.CloseDialogEvent(Sender: TObject;
  const AResult: TModalResult);
begin

  case AResult of
    mrOk:
      begin;
        GrantAndroidPermission(EnumPermissions.READ_EXTERNAL_STORAGE);
        GrantAndroidPermission(EnumPermissions.WRITE_EXTERNAL_STORAGE);
      end;
    mrCancel: ShowMessage(sCancelStoragePermissisonsText);
  end;
end;
{$ENDIF}

// Fires if Landscape orientation detected.
procedure TViewForm.DoOnLandscapeOrientation;
begin
  FIsOrientationChanged := True;
  var CurrentAtivity: TFrame := GetCurrentAtivity;
  if not Assigned(CurrentAtivity) or (CurrentAtivity as IActivityInfo).ActivityName.Equals(sActivitySignInName) or
     (CurrentAtivity as IActivityInfo).ActivityName.Equals(sActivitySignUp)
  then
    Exit;
  CurrentAtivity.Align := TAlignLayout.Right;
  MultiView.DrawerOptions.Mode:=  TSlidingMode.PushingDetailView;
  MultiView.Mode := TMultiViewMode.Panel;
  MultiView.RecalcSize;
  CurrentAtivity.Align := TAlignLayout.Client;
  FIsOrientationChanged := False;
end;

// Fires if Portrait orientation detected.
procedure TViewForm.DoOnPortraitOrientation;
begin
  var CurrentAtivity: TFrame := GetCurrentAtivity;
  if not Assigned(CurrentAtivity) or (CurrentAtivity as IActivityInfo).ActivityName.Equals(sActivitySignInName) or
     (CurrentAtivity as IActivityInfo).ActivityName.Equals(sActivitySignUp)
  then
    Exit;
  MultiView.DrawerOptions.Mode:=  TSlidingMode.OverlapDetailView;
  MultiView.Mode := TMultiViewMode.Drawer;
  MultiView.RecalcSize;
  MultiView.HideMaster;
  CurrentAtivity.Align := TAlignLayout.Client;
end;

procedure TViewForm.FormCreate(Sender: TObject);
begin
  FAddressBookLoader := TAddressBookLoader.Create;
{$IFDEF ANDROID}
  if not IsPermissionGranted(EnumPermissions.READ_EXTERNAL_STORAGE) and not IsPermissionGranted(EnumPermissions.WRITE_EXTERNAL_STORAGE) then
    TDialogService.MessageDialog(sGrantStoragePermissionsMsgDlgText, TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOk, TMsgDlgBtn.mbCancel], TMsgDlgBtn.mbOk, 0, CloseDialogEvent);
{$ENDIF}
  ReadCommonUserSettings;
  if not FCommonUserSettings.IsValid then
  begin
    SetViewDarkMode(False);
    FCommonUserSettings.Theme := TThemeMode.tmLight;
  end
  else
  case FCommonUserSettings.Theme of
    TThemeMode.tmNone: SetDefaultTheme;
    TThemeMode.tmLight: SetViewDarkMode(False);
    TThemeMode.tmDark: SetViewDarkMode(True);
  end;
  InitView;
  if Application.IsUnitTestRunning then
    FUserSettings := TUserSettings.Create;
// if Assigned(FUserSettings) then
//   FUserSettings.DeSerialize;
//   Creation Portrait Orientation Monitor.
  FPortraitOrientationMonitor := TScreenOrientationMonitor.Init(DoOnPortraitOrientation, TScreenOrientation.Portrait);
  // Creation Landscape Orientation Monitor.
  FLandscapeOrientationMonitor := TScreenOrientationMonitor.Init(DoOnLandscapeOrientation, TScreenOrientation.Landscape);
  if FLandscapeOrientationMonitor.ScreenOrientation = TScreenOrientation.Landscape then
    DoOnLandscapeOrientation;
end;

procedure TViewForm.FormPaint(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
begin
{$IFDEF MSWINDOWS}
  if not MultiView.IsShowed then
    Invalidate;
{$ENDIF}
end;

procedure TViewForm.FormSaveState(Sender: TObject);
begin
  WriteCommonUserSetting;
//  if Assigned(FUserSettings) then
//    FUserSettings.Serialize;
end;

function TViewForm.GetBackgroundTask: TThread;
begin
  Result := FBackgroundTask;
end;

function TViewForm.GetCommonUserSettings: TCommonUserSettings;
begin
  Result := FCommonUserSettings;
end;

function TViewForm.GetMultiView: TMultiView;
begin
  Result := MultiView;
end;

function TViewForm.GetPreloadAddressBookContacts: TAddressBookContacts;
begin
  Result := nil;
  if Assigned(FAddressBookLoader) then
    Result := FAddressBookLoader.PreloadAddressBookContacts;
end;

// Implements global access to basic View properties.
function TViewForm.GetScreenOrientation: TScreenOrientation;
begin
  Result := FPortraitOrientationMonitor.ScreenOrientation;
end;

procedure TViewForm.InitView;
begin
{$IFDEF MSWINDOWS}
  BorderStyle := TFmxFormBorderStyle.Sizeable;
  Position := TFormPosition.ScreenCenter;
{$ENDIF}
  MultiView.DrawerOptions.DurationSliding := ANIM_DURATION;
  FActivities := EnumRegisteredActivities(False);
  if not Application.IsUnitTestRunning then // Unit testing
  begin
    if IsActivityPresent(sActivitySignInName) then
      ShowActivity(sActivitySignInName, False)
    else
      ShowActivity(sActivityHomeName, False);
  end;
end;

function TViewForm.IsActivityPresent(AActivityName: string): Boolean;
begin
  Result := False;
  if Assigned(FActivities) then
    Result := EnumRegisteredActivities.IndexOf(AActivityName) <> -1;
end;

function TViewForm.IsOrientationChanged: Boolean;
begin
  Result := FIsOrientationChanged;
end;

procedure TViewForm.LandscapeRealign;
begin
  DoOnLandscapeOrientation;
end;

procedure TViewForm.PortraitRealign;
begin
  DoOnPortraitOrientation;
end;

// Recreate view due ScreenOrientation changing.
procedure TViewForm.RecreateView;
begin
  if FLandscapeOrientationMonitor.ScreenOrientation = TScreenOrientation.Landscape then
    DoOnLandscapeOrientation
  else
    DoOnPortraitOrientation
end;

function TViewForm.GetUserName: string;
begin
  Result := FUserName;
end;

function TViewForm.GetUserSettings: TUserSettings;
begin
  Result := FUserSettings;
end;

procedure TViewForm.SetBackgroundTask(ATask: TThread);
begin
  FBackgroundTask := ATask;
end;

procedure TViewForm.SetDefaultTheme;
begin
  StyleBook := nil;
end;

procedure TViewForm.SetUserImage(AUserImage: TBitmap);
begin
  if Assigned(FUserSettings) and FUserSettings.CurrentUser.IsValid then
    FUserSettings.CurrentUser.UserPicture.Assign(AUserImage);
  MenuFrame.UserImg.Bitmap.Assign(AUserImage);
  MenuFrame.AccountSVG.Visible := False;
end;

function TViewForm.GetUserImage: TBitmap;
begin
  Result := nil;
  if Assigned(MenuFrame.UserImg) then
    Result := MenuFrame.UserImg.Bitmap;
end;

procedure TViewForm.SetUserName(AValue: string);
begin
  FUserName := AValue;
end;


procedure TViewForm.SetViewDarkMode(AValue: Boolean);
begin
  var StyleRes: TResourceStream;
  if AValue then
    StyleRes := TResourceStream.Create(HInstance, sDarkThemeResName, RT_RCDATA)
  else
    StyleRes := TResourceStream.Create(HInstance, sLightThemeResName, RT_RCDATA);
  SynchronizedRun(
    procedure
    begin
      try
        StyleRes.Position := 0;
        if Assigned(ViewForm) then
        begin
          ViewForm.StyleBook := nil;
          ViewForm.ViewStyleBook.LoadFromStream(StyleRes);
          ViewForm.StyleBook := ViewForm.ViewStyleBook;
        end;
      finally
        StyleRes.Free;
      end;
    end);
end;

procedure TViewForm.ReadCommonUserSettings;
begin
  var Reader: TBinaryReader;
  SaveState.StoragePath := TPath.GetDocumentsPath;
  if SaveState.Stream.Size > 0 then
  begin
    Reader := TBinaryReader.Create(SaveState.Stream);
    var DataBlockSignature: Integer := Reader.ReadInteger;
    try
      if DataBlockSignature <> dCommonUserSettingsBlockSignature then
        Exit;
      FCommonUserSettings.Theme := TThemeMode(Reader.ReadInteger);
      FCommonUserSettings.TimeZone := Reader.ReadString;
      FCommonUserSettings.StringValue := Reader.ReadString;
      FCommonUserSettings.BoolValue := Reader.ReadBoolean;
    finally
      Reader.Free;
    end;
  end
end;

procedure TViewForm.WriteCommonUserSetting;
begin
  var Writer : TBinaryWriter;
  SaveState.StoragePath := TPath.GetDocumentsPath;
  SaveState.Stream.Clear;
  Writer := TBinaryWriter.Create(SaveState.Stream);
  try
    Writer.Write(dCommonUserSettingsBlockSignature);
    Writer.Write(Integer(FCommonUserSettings.Theme));
    Writer.Write(FCommonUserSettings.TimeZone);
    Writer.Write(FCommonUserSettings.StringValue);
    Writer.Write(FCommonUserSettings.BoolValue);
  finally
    Writer.Free;
  end;
end;

end.
