//---------------------------------------------------------------------------

// This software is Copyright (c) 2021 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit View.Settings.Item;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, Data.Bind.Components,
  FMX.Layouts, FMX.Controls.Presentation, FMX.Objects, FMX.Edit, FMX.ListBox;

type
  // Class can be registered as Visual Component with bindable properties.
  // Atrributes makes published propeties visible by LiveBindings.
  [ObservableMember('EditValue')]
  [ObservableMember('SwitchValue')]
  [ObservableMember('ComboBoxValue')]
  TSettingsItemFrame = class(TFrame)
    SettingsItemPanel: TGridPanelLayout;
    SettingsItemLbl: TLabel;
    SeparatorLine: TLine;
    ValueComboBox: TComboBox;
    ValueEdit: TEdit;
    ValueSwitch: TSwitch;
    Layout: TLayout;
  private
    function GetEditText: string;
    procedure SetEditText(AText: string);
    procedure ObserverToggle(const AObserver: IObserver; const Value: Boolean);
    function GetComboBoxValue: string;
    function GetSwitchValue: Boolean;
    procedure SetComboBoxValue(const Value: string);
    procedure SetSwitchValue(const Value: Boolean);
  protected
    function CanObserve(const ID: Integer): Boolean; override;
    procedure ObserverAdded(const ID: Integer; const Observer: IObserver); override;
  published
    property EditValue: string read GetEditText write SetEditText;
    property SwitchValue: Boolean read GetSwitchValue write SetSwitchValue;
    property ComboBoxValue: string read GetComboBoxValue write SetComboBoxValue;
  end;

procedure Register;

implementation

{$R *.fmx}

{ TSettingsItemFrame }

// LiveBinding basic routines.
function TSettingsItemFrame.CanObserve(const ID: Integer): Boolean;
begin
  case ID of
    TObserverMapping.EditLinkID, { EditLinkID is the observer that is used for control-to-field links }
    TObserverMapping.ControlValueID:
      Result := True;
  else
    Result := False;
  end;
end;

// LiveBinding basic routines.
procedure TSettingsItemFrame.ObserverToggle(const AObserver: IObserver;
  const Value: Boolean);
var
  LEditLinkObserver: IEditLinkObserver;
begin
  if Value then
  begin
    if Supports(AObserver, IEditLinkObserver, LEditLinkObserver) then
      // Disable the trackbar if the associated field does not support editing.
      Enabled := not LEditLinkObserver.IsReadOnly;
  end
  else
    Enabled := True;
end;

// ComboBox getter.
function TSettingsItemFrame.GetComboBoxValue: string;
begin
  Result := ValueComboBox.Selected.Text;
end;

// EditText getter.
function TSettingsItemFrame.GetEditText: string;
begin
  Result := ValueEdit.Text;
end;

// SwitchValue getter.
function TSettingsItemFrame.GetSwitchValue: Boolean;
begin
  Result := ValueSwitch.IsChecked
end;

procedure TSettingsItemFrame.ObserverAdded(const ID: Integer;
  const Observer: IObserver);
begin
  if ID = TObserverMapping.EditLinkID then
    Observer.OnObserverToggle := ObserverToggle;
end;

// ComboBox setter.
procedure TSettingsItemFrame.SetComboBoxValue(const Value: string);
begin
  ValueComboBox.Selected.Text := Value
end;

// EditText setter.
procedure TSettingsItemFrame.SetEditText(AText: string);
begin
  ValueEdit.Text := AText;
end;

// SwitchValue setter.
procedure TSettingsItemFrame.SetSwitchValue(const Value: Boolean);
begin
  ValueSwitch.IsChecked := Value
end;

// Register component.
procedure Register;
begin
  RegisterComponents('LiveBindings AppWiz Sample', [TSettingsItemFrame]);
end;

initialization
  // Register LiveBindings properties.
  Data.Bind.Components.RegisterObservableMember(TArray<TClass>.Create(TSettingsItemFrame), 'EditText', 'DFM');
  Data.Bind.Components.RegisterObservableMember(TArray<TClass>.Create(TSettingsItemFrame), 'SwitchValue', 'DFM');
  Data.Bind.Components.RegisterObservableMember(TArray<TClass>.Create(TSettingsItemFrame), 'ComboBoxValue', 'DFM');

finalization
  // Unregister component in LiveBindings.
  Data.Bind.Components.UnregisterObservableMember(TArray<TClass>.Create(TSettingsItemFrame));

end.
