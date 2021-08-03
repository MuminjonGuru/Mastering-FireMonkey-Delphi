//---------------------------------------------------------------------------

// This software is Copyright (c) 2021 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit View.Settings;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Edit, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.ListBox, View.Main, View.Settings.Item, FMX.Layouts, FMX.Effects, FMX.Ani,
  FMX.Objects, FMX.Controls.Presentation, Model.Constants, Model.Types, Model.Utils;

type
  // Application settings frame.
  TSettingsFrame = class(TMainFrame)
    VertScrollBox: TVertScrollBox;
    ThemeSettingsFrame: TSettingsItemFrame;
    TZSettingsFrame: TSettingsItemFrame;
    StringValueSettingsFrame: TSettingsItemFrame;
    BoolValueSettingsFrame: TSettingsItemFrame;
    procedure SettingsItemFrame1ComboBoxChange(Sender: TObject);
    procedure SettingsItemFrame1ComboBoxPopup(Sender: TObject);
    procedure ThemeSettingsFrameValueComboBoxChange(Sender: TObject);
    procedure StringValueSettingsFrameValueEditChange(Sender: TObject);
    procedure BoolValueSettingsFrameValueSwitchSwitch(Sender: TObject);
  private
    FPrevSeltext: string;
    FPrevSelIdx: Integer;
    //FUserSettings: TUserSettings;
    FCommonUserSettings: ICommonUserSettings;
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{$R *.fmx}

procedure TSettingsFrame.BoolValueSettingsFrameValueSwitchSwitch(
  Sender: TObject);
begin
  inherited;
  FCommonUserSettings.GetCommonUserSettings.BoolValue := TSwitch(Sender).IsChecked;
end;

constructor TSettingsFrame.Create(AOwner: TComponent);
begin
  inherited;
  FCommonUserSettings := GetMainForm as ICommonUserSettings;

  case FCommonUserSettings.GetCommonUserSettings.Theme of
    tmNone: ThemeSettingsFrame.ValueComboBox.ItemIndex := 0;
    tmLight: ThemeSettingsFrame.ValueComboBox.ItemIndex := 1;
    tmDark: ThemeSettingsFrame.ValueComboBox.ItemIndex := 2;
  end;

  TZSettingsFrame.ValueComboBox.Selected.Text := FCommonUserSettings.GetCommonUserSettings.TimeZone;
  StringValueSettingsFrame.ValueEdit.Text := FCommonUserSettings.GetCommonUserSettings.StringValue;
  BoolValueSettingsFrame.ValueSwitch.IsChecked := FCommonUserSettings.GetCommonUserSettings.BoolValue;
//
//  var UserInfo: IUserInfo := GetMainForm as IUserInfo;
//  if Assigned(UserInfo) then
//    FUserSettings := (GetMainForm as IUserInfo).GetUserSettings;
//  if not Assigned(FUserSettings) then
//    Exit;
//  case FUserSettings.Theme of
//    tmNone: ThemeSettingsFrame.ValueComboBox.ItemIndex := 0;
//    tmLight: ThemeSettingsFrame.ValueComboBox.ItemIndex := 1;
//    tmDark: ThemeSettingsFrame.ValueComboBox.ItemIndex := 2;
//  end;
////
//  TZSettingsFrame.ValueComboBox.Selected.Text := FUserSettings.TimeZone;
//  StringValueSettingsFrame.ValueEdit.Text := FUserSettings.StringValue;
//  BoolValueSettingsFrame.ValueSwitch.IsChecked := FUserSettings.BoolValue;
//  ThemeSettingsFrame.ValueComboBox.ItemIndex := -1;
//  ThemeSettingsFrame.ValueComboBox.OnChange := ThemeSettingsFrameValueComboBoxChange;
end;

procedure TSettingsFrame.SettingsItemFrame1ComboBoxChange(Sender: TObject);
begin
  inherited;
  FPrevSeltext := TComboBox(Sender).Selected.Text;
  FPrevSelIdx := TComboBox(Sender).Selected.Index;
  if TComboBox(Sender).Selected.Text.IsEmpty then
    Exit;
  TComboBox(Sender).Selected.Text := Concat(FPrevSeltext.Split([TAB_CHAR])[0], ' (UTC', FPrevSeltext.Split(['UTC'])[1], ')');
  FCommonUserSettings.GetCommonUserSettings.TimeZone :=  TComboBox(Sender).Selected.Text;
end;

procedure TSettingsFrame.SettingsItemFrame1ComboBoxPopup(Sender: TObject);
begin
  inherited;
  if not FPrevSeltext.IsEmpty then
    TComboBox(Sender).Items[FPrevSelIdx] := FPrevSeltext;
end;

procedure TSettingsFrame.StringValueSettingsFrameValueEditChange(
  Sender: TObject);
begin
  inherited;
  FCommonUserSettings.GetCommonUserSettings.StringValue := TEdit(Sender).Text;
end;

procedure TSettingsFrame.ThemeSettingsFrameValueComboBoxChange(Sender: TObject);
begin
  inherited;
  var ViewUtils: IViewUtils := (GetMainForm as IViewUtils);
  if Assigned(ViewUtils) then
    if FCommonUserSettings.GetCommonUserSettings.Theme <> TThemeMode(TComboBox(Sender).Selected.Index) then
      case TComboBox(Sender).Selected.Index of
        0: ViewUtils.SetDefaultTheme;
        1: ViewUtils.SetViewDarkMode(False);
        2: ViewUtils.SetViewDarkMode(True);
      end;
  FCommonUserSettings.GetCommonUserSettings.Theme := TThemeMode(TComboBox(Sender).Selected.Index);
end;

initialization
  // Register frame
  RegisterClass(TSettingsFrame);
finalization
  // Unregister frame
  UnRegisterClass(TSettingsFrame);

end.
