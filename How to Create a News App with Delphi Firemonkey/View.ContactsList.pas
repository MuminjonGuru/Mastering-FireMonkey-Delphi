//---------------------------------------------------------------------------

// This software is Copyright (c) 2021 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

{
  NOTE: For work application on Android platform you should add next persmissions:
    - Get accounts;
    - Read contacts;
    - Write contacts.
}

unit View.ContactsList;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  View.Main, FMX.Layouts, FMX.Ani, FMX.Objects, FMX.Controls.Presentation,
  FMX.Effects, FMX.AddressBook.Types, FMX.AddressBook,
  FMX.ListBox, FMX.DialogService, Model.Utils, Model.Types, Model.Constants,
  FMX.Gestures, FMX.ListView.Types, FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base, FMX.ListView, Threading;

type
  // Contacts list (Address book) form.
  TContactsListFrame = class(TMainFrame)
    VertScrollBox: TVertScrollBox;
    ContactsList: TListBox;
    lblLoading: TLabel;
    ProgressBar: TProgressBar;
  private
{$IFNDEF MSWINDOWS}
    FBackgroundTask: TThread;
    FPreloadAddressBookContacts: TAddressBookContacts;
    procedure FillContactlist;
    procedure OnHiddenMultiview(Sender: TObject);
    procedure DoOnHiddenMultiview;
{$ENDIF}
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.fmx}

uses
  View.ContactsList.Item;

constructor TContactsListFrame.Create(Owner: TComponent);
begin
  inherited;
{$IFDEF MSWINDOWS}
  lblLoading.Text := sContactsNotSupported;
  ProgressBar.Visible := False;
{$ELSE}
  lblLoading.Text := sPermissionsDenied;
  ProgressBar.Visible := False;
  // Assign an event that triggers when the main menu is hidden.
    GetMultiView.OnHidden := OnHiddenMultiview;
  // Getting instance of preloading contact list.
  FPreloadAddressBookContacts := (GetMainForm as IPreloadAddressBook).GetPreloadAddressBookContacts;
  var VI: IViewInfo := GetMainForm as IViewInfo;
  if not (GetMultiView.IsVisible and HamburgerImg.Visible) or (VI.GetScreenOrientation = TScreenOrientation.Landscape) then
    DoOnHiddenMultiview;
{$ENDIF}
end;

destructor TContactsListFrame.Destroy;
begin
  inherited;
end;

{$IFNDEF MSWINDOWS}
// Event that triggers when the main menu is hidden.
procedure TContactsListFrame.OnHiddenMultiview(Sender: TObject);
begin
  DoOnHiddenMultiview;
end;

procedure TContactsListFrame.DoOnHiddenMultiview;
begin
  if ContactsList.Count = 0 then
   if Assigned(FPreloadAddressBookContacts) then
     FillContactlist;
end;

// Async creating list of contacts & showing contacts list.
procedure TContactsListFrame.FillContactlist;
begin
  ContactsList.Clear;
  ProgressBar.Max := FPreloadAddressBookContacts.Count;
  lblLoading.Text := sLoadingContacts;
  ProgressBar.Visible := True;
  TThread.CreateAnonymousThread(
    procedure
    begin
      (GetMainForm as IBackGroundTask).SetBackgroundTask(TThread.CurrentThread);
      FBackgroundTask := TThread.CurrentThread;
      ContactsList.BeginUpdate;
      try
        for var i: Integer := 0 to FPreloadAddressBookContacts.Count - 1 do
        begin
          TThread.Synchronize(
            TThread.CurrentThread,
            procedure
            begin
              if not Assigned(FPreloadAddressBookContacts) then
                Exit;
              var ContactItem: TContactListItemFrame := TContactListItemFrame.Create(FPreloadAddressBookContacts.Items[i]);
              var ListBoxItem := TListBoxItem.Create(nil);
              ListBoxItem.Size := ContactItem.Size;
              ListBoxItem.AddObject(ContactItem);
              ContactsList.AddObject(ListBoxItem);
              ProgressBar.Value := i;
            end
          );
        end;
        ContactsList.Visible := True;
        lblLoading.Visible := False;
      finally
        ContactsList.EndUpdate;
        if not Assigned(FPreloadAddressBookContacts) then
          GetMainForm.GetCurrentAtivity.ReCreate;
      end;
    end).Start;
end;
{$ENDIF}

initialization
  // Register frame
  RegisterClass(TContactsListFrame);
finalization
  // Unregister frame
  UnRegisterClass(TContactsListFrame);

end.
