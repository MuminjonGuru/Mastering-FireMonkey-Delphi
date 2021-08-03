//---------------------------------------------------------------------------

// This software is Copyright (c) 2021 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit View.ContactsList.Item;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Layouts, FMX.Controls.Presentation, FMX.AddressBook,
  Model.Types, StrUtils, FMX.Effects, Model.Constants, Model.Utils;

type
  // Address book contact item representation frame.
  TContactListItemFrame = class(TFrame)
    ContactInfoPanel: TGridPanelLayout;
    imgUserpic: TImage;
    lblPhone: TLabel;
    lblUsername: TLabel;
    UserInfoPanel: TGridPanelLayout;
    imgEditContact: TPath;
    BgRect: TRectangle;
    GlowEffect: TGlowEffect;
    UserpicGlowEffect: TGlowEffect;
  private
    FContact: TAddressBookContact;
    function GetContactName: string;
    procedure SetContactName(const AName: string);
    function GetPhone: string;
    procedure SetPhone(const APhone: string);
    function GetUserPic: TBitmap;
    procedure SetUserPic(const AUserPic: TBitmap);
    procedure DoOnApplyStyleLookupItem(Sender: TObject);
  public
    constructor Create(AContact: TAddressBookContact); reintroduce;
    property ContactName: string read GetContactName write SetContactName;
    property Phone: string read GetPhone write SetPhone;
    property UserPic: TBitmap read GetUserPic write SetUserPic;
  end;

implementation

{$R *.fmx}

{ TContactListItemFrame }

constructor TContactListItemFrame.Create(AContact: TAddressBookContact);
begin
  inherited Create(nil);
  if Assigned(AContact) then
  begin
    lblUsername.OnApplyStyleLookup := DoOnApplyStyleLookupItem;
    FContact := AContact;
    ContactName := Concat(FContact.FirstName, sSpaceChar, FContact.LastName);
    UserPic.Assign(FContact.PhotoThumbnail);
    imgUserpic.MakeCircleMask;
    if FContact.Phones.Count > 0 then
      Phone := FContact.Phones.Items[0].Number
    else
      Phone := sNotDefined;
    lblUsername.ApplyStyleLookup;
  end;
end;

procedure TContactListItemFrame.DoOnApplyStyleLookupItem(Sender: TObject);
begin
  BgRect.Stroke.Color := lblUsername.GetStyledColor;
end;

function TContactListItemFrame.GetContactName: string;
begin
  Result := lblUsername.Text;
end;

function TContactListItemFrame.GetPhone: string;
begin
  Result := lblPhone.Text;
end;

function TContactListItemFrame.GetUserPic: TBitmap;
begin
  Result := imgUserpic.Bitmap;
end;

procedure TContactListItemFrame.SetContactName(const AName: string);
begin
  lblUsername.Text := AName;
end;

procedure TContactListItemFrame.SetPhone(const APhone: string);
begin
  lblPhone.Text := APhone;
end;

procedure TContactListItemFrame.SetUserPic(const AUserPic: TBitmap);
begin
  imgUserpic.Bitmap.Assign(AUserPic);
end;

initialization
  // Register frame
  RegisterClass(TContactListItemFrame);
finalization
  // Unregister frame
  UnRegisterClass(TContactListItemFrame);

end.
