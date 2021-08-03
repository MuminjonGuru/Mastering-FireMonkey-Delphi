//---------------------------------------------------------------------------

// This software is Copyright (c) 2021 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit View.Home;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  View.Main, FMX.ScrollBox, FMX.Memo, FMX.Ani, FMX.Objects,
  FMX.Controls.Presentation, FMX.Layouts, System.ImageList, FMX.ImgList,
  Model.Constants, Model.Utils, FMX.Effects, View.Home.DataRect, Model.Types,
  ViewModel;

type
  // Home/main frame.
  THomeFrame = class(TMainFrame)
    DataPanel: TGridPanelLayout;
    ContactsDataRect: TDataRectFrame;
    DB1SampleDataRect: TDataRectFrame;
    DB2SampleDataRect: TDataRectFrame;
    procedure ContactsDataRectClick(Sender: TObject);
  private
    FCustNo: Integer;
    FViewModel: TViewModel;
    function GetUserName: string;
  public
    { Public declarations }
    constructor Create(Owner: TComponent); override;
  end;

implementation

uses
  View;

{$R *.fmx}

// Calling contacts Screen & Loading contacts.
procedure THomeFrame.ContactsDataRectClick(Sender: TObject);
begin
  GetMainForm.ShowActivity('ContactsList', true);
end;

constructor THomeFrame.Create(Owner: TComponent);
begin
  inherited;
  var UserInfo: IUserInfo := GetMainForm as IUserInfo;
  if UserInfo.GetUserName.IsEmpty then
    UserInfo.SetUserName(DEBUG_DB_USERNAME_1);
  // Create ViewModel object which implements high-level access to DB data.
  var ViewInfo: IViewInfo := GetMainForm as IViewInfo;
  // If Data enbaled.
  if ViewInfo.IsActivityPresent(sActivityData) and IsClassPresent(sModelDataClassName) then
  begin
    if not Assigned(ViewForm.ViewModel) then
    begin
      FViewModel := TViewModel.Create;
      ViewForm.ViewModel := FViewModel;
    end
    else
      FViewModel := ViewForm.ViewModel;
    FCustNo := FViewModel.GetCustNoByUserName(GetUserName);
    DB1SampleDataRect.DataValue := FCustNo.ToString;
    DB2SampleDataRect.DataValue := FViewModel.GetAddressLine1(GetUserName);
  end
  else
  begin
    DB1SampleDataRect.DataCaption := sDBSampleDataRectDisabledCaption;
    DB2SampleDataRect.DataCaption := sDBSampleDataRectDisabledCaption;
    DB1SampleDataRect.DataValue := sDataSampleDataRectDisabledValue;
    DB2SampleDataRect.DataValue := sDataSampleDataRectDisabledValue;
  end;
  // If Contacts enbaled.
  if ViewInfo.IsActivityPresent(sActivityContacts) then
  begin
    var Contacts: IPreloadAddressBook := GetMainForm as IPreloadAddressBook;
    ContactsDataRect.DataValue := sNotDefined;
{$IFDEF ANDROID}
    if Assigned(Contacts) and IsPermissionGranted(EnumPermissions.READ_CONTACTS) and IsPermissionGranted(EnumPermissions.GET_ACCOUNTS) then
        ContactsDataRect.DataValue := Contacts.GetPreloadAddressBookContacts.Count.ToString;
{$ENDIF}
  end
  else
  begin
    ContactsDataRect.DisableEvents;
    ContactsDataRect.DataValue := sDataSampleDataRectDisabledValue;
  end;
  // Applying SVG icon.
  HamburgerImg.Data.Data := MATERIAL_UI_MENU;
end;

// Getting current logged UserName
function THomeFrame.GetUserName: string;
begin
  var UserInfo: IUserInfo := GetMainForm as IUserInfo;
  if Assigned(UserInfo) then
    Result := UserInfo.GetUserName;
end;

initialization
  // Register frame
  RegisterClass(THomeFrame);
finalization
  // Unregister frame
  UnRegisterClass(THomeFrame);

end.
