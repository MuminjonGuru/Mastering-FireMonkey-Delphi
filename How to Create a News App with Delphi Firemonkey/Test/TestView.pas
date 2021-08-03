//---------------------------------------------------------------------------

// This software is Copyright (c) 2021 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit TestView;

interface

uses
  DUnitX.TestFramework, View, FMX.AddressBook, Model.Types, Classes,
  FMX.Graphics, FMX.Types, FMX.StdCtrls, FMX.Forms, FMX.MultiView,
  FMX.MultiView.Types, View.Main;

type
  // Test methods for class TViewForm.
  [TestFixture]
  TestTViewForm = class(TObject)
  private
    // A view form.
    FViewForm: TViewForm;
    // A stub button for the master button.
    FMasterButton: TButton;
    // A stub frame for the activity frame.
    FActivityFrame: TFrame;
  public
    // Test initialization entry point. Here you can prepare your environment and
    // initialize necessary class instances for the test.
    [Setup]
    procedure Setup;
    // Here you can release your necessary class instances.
    [TearDown]
    procedure TearDown;
    // Simple single Test.
    [Test]
    procedure TestGetUserName;
    // Simple single Test.
    [Test]
    procedure TestSetUserName;
    // Simple single Test.
    [Test]
    procedure TestGetUserSettings;
    // Simple single Test.
    [Test]
    procedure TestGetBackgroundTask;
    // Simple single Test.
    [Test]
    procedure TestSetBackgroundTask;
    // Simple single Test.
    [Test]
    procedure TestDoOnPortraitOrientation;
    // Simple single Test.
    [Test]
    procedure TestDoOnLandscapeOrientation;
  end;

  // A stub thread for testing purposes.
  TStabThread = class(TThread)
  protected
    procedure Execute; override;
  public
    constructor Create;
  end;

implementation

{ TestTViewForm }

procedure TestTViewForm.Setup;
begin
  // Create an instance of the view form.
  FViewForm := TViewForm.Create(nil);

  // Create a stub button for the master button.
  FMasterButton := TButton.Create(FViewForm);
  FMasterButton.Parent := FViewForm;
  FViewForm.MultiView.MasterButton := FMasterButton;

  // Create a stub frame for the activity frame.
  FActivityFrame := TMainFrame.Create(FViewForm);
  FActivityFrame.Parent := FViewForm;
end;

procedure TestTViewForm.TearDown;
begin
  FViewForm.Free;
  FViewForm := nil;
end;

procedure TestTViewForm.TestDoOnPortraitOrientation;
var
  ExpectedLayout, ActualLayout: TAlignLayout;
  ExpectedMultiViewMode, ActualMultiViewMode: TMultiViewMode;
  ExpectedSlidingMode, ActualSlidingMode: TSlidingMode;
begin
  // Call the tested method.
  FViewForm.DoOnPortraitOrientation;

  // Validate method results.
  // Check the alignment of the activity frame within its parent.
  ExpectedLayout := TAlignLayout.Client;
  ActualLayout := FActivityFrame.Align;
  Assert.AreEqual(ExpectedLayout, ActualLayout, 'Failed to change the alignment of the activity frame within its parent.');

  // Check the multi view mode.
  ExpectedMultiViewMode := TMultiViewMode.Drawer;
  ActualMultiViewMode := FViewForm.MultiView.Mode;
  Assert.AreEqual(ExpectedMultiViewMode, ActualMultiViewMode, 'Failed to change the multi view mode.');

  // Check the sliding mode.
  ExpectedSlidingMode := TSlidingMode.OverlapDetailView;
  ActualSlidingMode := FViewForm.MultiView.DrawerOptions.Mode;
  Assert.AreEqual(ExpectedSlidingMode, ActualSlidingMode, 'Failed to change the sliding mode.');
end;

procedure TestTViewForm.TestDoOnLandscapeOrientation;
var
  ExpectedLayout, ActualLayout: TAlignLayout;
  ExpectedMultiViewMode, ActualMultiViewMode: TMultiViewMode;
  ExpectedSlidingMode, ActualSlidingMode: TSlidingMode;
begin
  // Call the tested method.
  FViewForm.DoOnLandscapeOrientation;

  // Validate method results.
  // Check the alignment of the activity frame within its parent.
  ExpectedLayout := TAlignLayout.Client;
  ActualLayout := FActivityFrame.Align;
  Assert.AreEqual(ExpectedLayout, ActualLayout, 'Failed to change the alignment of the activity frame within its parent.');

  // Check the multi view mode.
  ExpectedMultiViewMode := TMultiViewMode.Panel;
  ActualMultiViewMode := FViewForm.MultiView.Mode;
  Assert.AreEqual(ExpectedMultiViewMode, ActualMultiViewMode, 'Failed to change the multi view mode.');

  // Check the sliding mode.
  ExpectedSlidingMode := TSlidingMode.PushingDetailView;
  ActualSlidingMode := FViewForm.MultiView.DrawerOptions.Mode;
  Assert.AreEqual(ExpectedSlidingMode, ActualSlidingMode, 'Failed to change the sliding mode.');
end;

procedure TestTViewForm.TestGetUserName;
var
  ExceptedUserName, ActualUserName: string;
begin
  // Call the tested method.
  ActualUserName := FViewForm.GetUserName;

  // Validate method results.
  // Check the username.
  ExceptedUserName := '';
  Assert.AreEqual(ExceptedUserName, ActualUserName, 'The username must be blank.');
end;

procedure TestTViewForm.TestSetUserName;
var
  AValue: string;
  ExpectedUserName, ActualUserName: string;
const
  USER_NAME = 'UserName';
begin
  // Setup method call parameters.
  AValue := USER_NAME;

  // Call the tested method.
  FViewForm.SetUserName(AValue);

  // Validate method results.
  // Check the username.
  ExpectedUserName := AValue;
  ActualUserName := FViewForm.GetUserName;
  Assert.AreEqual(ExpectedUserName, ActualUserName, 'Failed to change the username.');
end;

procedure TestTViewForm.TestGetUserSettings;
var
  UserSettings: TUserSettings;
begin
  // Call the tested method.
  UserSettings := FViewForm.GetUserSettings;

  // Validate method results.
  // Check the user settings.
  Assert.IsNotNull(UserSettings, 'Failed to get the user settings.');
end;

procedure TestTViewForm.TestGetBackgroundTask;
var
  BackgroundTask: TThread;
begin
  // Call the tested method.
  BackgroundTask := FViewForm.GetBackgroundTask;

  // Validate method results.
  // Check the background task.
  Assert.IsNull(BackgroundTask, 'There should be no background task.');
end;

procedure TestTViewForm.TestSetBackgroundTask;
var
  ATask, ExpectedTask, ActualTask: TThread;
begin
  // Setup method call parameters.
  ATask := TStabThread.Create;
  try
    // Call the tested method.
    FViewForm.SetBackgroundTask(ATask);

    // Validate method results.
    // Check the background task.
    ExpectedTask := ATask;
    ActualTask := FViewForm.GetBackgroundTask;
    Assert.AreEqual(ExpectedTask, ActualTask, 'Failed to get the same background task.');
  finally
    ATask.Free;
  end;
end;

{ TStabThread }

constructor TStabThread.Create;
begin
  inherited Create(True);
end;

procedure TStabThread.Execute;
begin
  // Nothing happening here.
end;

initialization
  // Register any test cases with the test runner.
  TDUnitX.RegisterTestFixture(TestTViewForm);

end.
