//---------------------------------------------------------------------------

// This software is Copyright (c) 2021 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit View.Menu;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  View.Main, FMX.Layouts, FMX.Objects, FMX.Ani, FMX.Controls.Presentation,
  Model.Utils, Model.Constants, Model.Types, FMX.Effects, FMX.DialogService.Async,
  FMX.ListBox, System.Generics.Collections, FMX.ListView.Types,
  FMX.ListView.Appearances, FMX.ListView.Adapters.Base, FMX.ListView;

type
  TMenuFrame = class(TMainFrame, IUserInfo)
    BgRect: TRectangle;
    DataLayout: TGridPanelLayout;
    UserInfoLayout: TGridPanelLayout;
    UserImg: TImage;
    VertScrollBox: TVertScrollBox;
    ListBox: TListBox;
    lblUsername: TLabel;
    AccountSVG: TPath;
    procedure HitBoxClick(Sender: TObject);
    procedure MenuItemClick(Sender: TObject);
    procedure lblUsernameApplyStyleLookup(Sender: TObject);
  private
    { Private declarations }
  public
    function GetUserName: string;
    procedure SetUserName(AValue: String);
    function GetUserSettings: TUserSettings;
    procedure SetUserImage(AUserImage: TBitmap);
    function GetUserImage: TBitmap;
    procedure CreateMenu;
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{$R *.fmx}

uses
  View.Menu.Item;

constructor TMenuFrame.Create(AOwner: TComponent);
begin
  inherited;
  CreateMenu;
end;

//  Creating menu accordings registered Frames.
procedure TMenuFrame.CreateMenu;
begin
  ListBox.Height := 0;
  for var ActivityName: string in EnumRegisteredActivities(False) do
  begin
    if not sHiddenMenuActivities.Contains(ActivityName) then
    begin
      var MenuItemFrame := TMenuItemFrame.Create(NameToUserFriendlyName(ActivityName), MATERIAL_UI_GREY_200);
      var MenuItem := TListBoxItem.Create(nil);
      MenuItem.Name := ActivityName;
      MenuItem.Text := EmptyStr;
      MenuItem.OnClick := MenuItemClick;
      MenuItem.Height := MenuItemFrame.Height;
      MenuItem.AddObject(MenuItemFrame);
      MenuItem.ClipParent := true;
      ListBox.AddObject(MenuItem);
    end;
  end;
end;

function TMenuFrame.GetUserImage: TBitmap;
begin
  Result := nil;
  if Assigned(UserImg.Bitmap) then
    Result := UserImg.Bitmap;
end;

function TMenuFrame.GetUserName: string;
begin
  Result := lblUsername.Text;
end;

function TMenuFrame.GetUserSettings: TUserSettings;
begin
  Result := nil;
end;

procedure TMenuFrame.HitBoxClick(Sender: TObject);
begin
  GetMainForm.ShowActivity('Home', True)
end;

procedure TMenuFrame.lblUsernameApplyStyleLookup(Sender: TObject);
begin
  inherited;
  HamburgerImg.Fill.Color := lblUsername.GetStyledColor;
  AccountSVG.Fill.Color := MATERIAL_UI_GREY_400;
  HeaderRect.Stroke.Thickness := 0;
end;

procedure TMenuFrame.MenuItemClick(Sender: TObject);
begin
  try
    GetMainForm.ShowActivity(TControl(Sender).Name , True);
  except
    on E:Exception do
      ShowMessage(E.Message);
  end;
end;

procedure TMenuFrame.SetUserImage(AUserImage: TBitmap);
begin
  UserImg.Bitmap.Assign(AUserImage);
  UserImg.MakeCircleMask;
  AccountSVG.Visible := False;
end;

procedure TMenuFrame.SetUserName(AValue: String);
begin
  lblUsername.Text := AValue;
end;

initialization
  // Register frame
  RegisterClass(TMenuFrame);
finalization
  // Unregister frame
  UnRegisterClass(TMenuFrame);

end.
