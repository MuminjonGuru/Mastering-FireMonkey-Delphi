//---------------------------------------------------------------------------

// This software is Copyright (c) 2021 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit Model.Utils;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Edit, FMX.Controls, FMX.Forms,FMX.Dialogs,
  FMX.StdCtrls, FMX.Objects,System.Generics.Defaults,System.Generics.Collections,
  FMX.Controls.Presentation, FMX.ActnList, FMX.Canvas.GPU.Helpers,FMX.Utils,
  FMX.Layouts, Threading, System.IOUtils,System.UIConsts,FMX.MultiView,
  Model.Constants, Model.Types, Typinfo,RTTI,System.Permissions,
  FMX.MediaLibrary.Actions,FMX.MediaLibrary,FMX.DialogService, Character,
  System.Messaging, RegularExpressions,
{$IFDEF MSWINDOWS}
  Winapi.Windows,       // for the pre-defined registry key constants
  System.Win.Registry,  // for the registry read access
{$ENDIF}
  FMX.Graphics,
{$IFDEF ANDROID}
  FMX.Helpers.Android,
  Androidapi.JNI.App,
  Androidapi.Helpers,
  FMX.Platform.Android,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Net,
  Androidapi.JNI.Os,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNIBridge,
{$ENDIF}
  FMX.Platform;

type
  // Helper class for loading images from gallery.
  TAppWizardImageLoader = class
  private
    FTakePhotoFromLibraryAction: TTakePhotoFromLibraryAction;
    FBitmap: TBitmap;
    FOnDidFinishTaking: TOnDidFinishTaking;
  public
    constructor Create(AFinishTaking: TOnDidFinishTaking); overload;
    procedure Exec;
    property OnDidFinishTaking: TOnDidFinishTaking read FOnDidFinishTaking;
  end;

  TApplicationHelper = class helper for TApplication
    function IsUnitTestRunning: Boolean;
  end;

  // Helper class for TImage.
  TImageHelper = class helper for TImage
  private
    // Making all pixels of an image outside the mask transparent.
    procedure PremultiplyBitmapAlpha(ABmp: TBitmap);
    // Payload routine for Making Circle Mask.
    procedure MakeCircleMaskProc;
  public
    // Proxy routine for Making Circle Mask.
    procedure MakeCircleMask;
  end;

  // Helper class for MainForm. Implements basic routines to manage app screens.
  TFormHelper = class helper for TForm
    procedure DoOnControlClick(Sender: TObject);
    procedure ShowActivity(const ActivityName: string; const Clear: Boolean = False);
    procedure SetFocusOnFirtstControl(AClass: TClass);
    function GetCurrentAtivity: TFrame;
  end;

  //
  TLabelHelper = class helper for TLabel
    function GetStyledColor: TAlphaColor;
    procedure ResetStyleSettings;
    procedure EnableStyledColor;
    procedure EnableStyledTextSize;
  end;

  // Helper for TFrame.
  TFrameHelper = class helper for TFrame
    // Recreate itself.
    procedure ReCreate;
  end;

  // Implements external events for specified components.
  TProxyComponent = class(TComponent)
    class procedure EditCanFocus(Sender: TObject; var ACanFocus: Boolean);
  end;

  // Common routines.
  procedure SynchronizedRun(Proc: TProc);
  function GetControlsByClass(AOwner: TControl; AClass: TClass): TList<TControl>;
  procedure DoEditTyping(Sender: TObject);
  function GetMainForm: TForm;
  procedure DoShowPassImgClick(Sender: TObject);
  function ExtractActivityName(const ActivityName: string): string;
  function GetPlatformPath: string;
  function GetConfigPath: string;
  function ConfigExists: Boolean;
  function EnumRegisteredActivities(const IsRawActivity: Boolean = True): TStringList;
  function NameToUserFriendlyName(AName: string): string;
  function ClassNameToActivityName(AClassName: string): string;
  function IsClassPresent(AClassName: string): Boolean;
  function ValidateEmail(const emailAddress: string): Boolean; overload;
  function ValidateEmail(AEdit: TEdit): Boolean; overload;
{$IFDEF MSWINDOWS}
  function WindowsDarkModeIsEnabled: Boolean;
{$ENDIF}
{$IFDEF ANDROID}
  // Get neceesary permission from 'TJManifest_permission.JavaClass'.
  function EnumPermissions: JManifest_permissionClass;
  function GrantAndroidPermission(APermission: JString): Boolean;
  function IsPermissionGranted(APermission: JString): Boolean;
  function GetPermissionsSvc: TPermissionsService;
{$ENDIF}

implementation

uses View.Main;

function ValidateEmail(const emailAddress: string): Boolean;
var
  RegEx: TRegEx;
begin
  RegEx := TRegex.Create(sEmailValidationRegexExpr);
  Result := RegEx.Match(emailAddress).Success;
end;

class procedure TProxyComponent.EditCanFocus(Sender: TObject; var ACanFocus: Boolean);
begin
  var LLabel: TLAbel := TLabel(GetControlsByClass(TEdit(Sender), TLabel)[0]);
  LLabel.Text := sEmail;
  LLabel.EnableStyledColor;
  LLabel.EnableStyledTextSize;
end;

function ValidateEmail(AEdit: TEdit): Boolean;
begin
  var EmailEdit: TEdit := TEdit(AEdit);
  var EmailHiddenLbl: TLabel := TLabel(GetControlsByClass(TEdit(AEdit), TLabel)[0]);
  EmailEdit.OnCanFocus := TProxyComponent.EditCanFocus;
  Result := ValidateEmail(EmailEdit.Text);
  if not Result then
  begin
    EmailEdit.Text := EmptyStr;
    EmailHiddenLbl.ResetStyleSettings;
    EmailHiddenLbl.Text := sInvalidEmailFormat;
    EmailHiddenLbl.TextSettings.FontColor := TAlphaColorRec.Red;
    DoEditTyping(EmailEdit);
  end;
end;

{$IFDEF MSWINDOWS}
function WindowsDarkModeIsEnabled: boolean;
const
  TheKey   = 'Software\Microsoft\Windows\CurrentVersion\Themes\Personalize\';
  TheValue = 'AppsUseLightTheme';
var
  Reg: TRegistry;
begin
  Result := False;
// This relies on a registry setting only available on MS Windows
// If the developer has somehow managed to get to this point then tell
// them not to do this!
  Reg    := TRegistry.Create(KEY_READ);
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.KeyExists(TheKey) then
      if Reg.OpenKey(TheKey, False) then
      try
        if Reg.ValueExists(TheValue) then
          Result := Reg.ReadInteger(TheValue) = 0;
      finally
        Reg.CloseKey;
      end;
  finally
    Reg.Free;
  end;
end;
{$ENDIF}

function IsClassPresent(AClassName: string): Boolean;
begin
  Result := Assigned(GetClass(AClassName))
end;

// Convert string to user friendly string representation(for example: 'SinIn' -> 'Sign In').
function NameToUserFriendlyName(AName: string): string;
begin
  Result := AName;
  var Cnt: Byte := 0;
  var LName: string := AName;
  for var Idx: Integer := 1 to AName.Length do
    if AName[Idx].IsUpper and (Idx > 1) then
    begin
     Insert(sSpaceChar, LName, Idx + Cnt);
     Inc(Cnt);
    end; 
  Result := LName;   
end;

function ClassNameToActivityName(AClassName: string): string;
begin
  Result := AClassName.Replace(sActivityClassNameSuffix, '');
end;

function EnumRegisteredActivities(const IsRawActivity: Boolean = True): TStringList;

  function ActivityNameToFrameClassName(ActivityName: string): string;
  begin
    Result := Concat(sClassNamePrefix, ActivityName, sActivityClassNameSuffix)
  end;

begin
  Result := TStringList.Create;
  for var ActivityName: string in arrActivitiesList do
  begin
    if IsClassPresent(ActivityNameToFrameClassName(ActivityName)) then
    begin
      var ActivityClass: TPersistentClass := GetClass(ActivityNameToFrameClassName(ActivityName));
      if IsRawActivity then
        Result.Add(ActivityName)
      else
      if ActivityClass.Create.InheritsFrom(TMainFrame) then
        Result.Add(ActivityName);
    end;
  end  
end;

// Checking if config file exist. Config file contains serialized settings of all registerd user.
function ConfigExists: Boolean;
begin
  Result := FileExists(GetConfigPath)
end;

// Getting platform specific path.
function GetConfigPath: string;
begin
  Result := TPath.Combine(GetPlatformPath, CONFIG_FILENAME);
end;

function GetPlatformPath: string;
begin
  Result := TPath.GetDocumentsPath;
{$IFDEF MSWINDOWS}
  Result := TPath.GetLibraryPath;
{$ENDIF}
end;

// Getting current permission service.
// Routine to run simple async task.
procedure SynchronizedRun(Proc: TProc);
begin
  TTask.Run(
   procedure
   begin
     TThread.Synchronize(
     nil,
     procedure
     begin
       Proc
     end
     )
   end
 );
end;

{$IFDEF ANDROID}
// Getting list of all existing permissions.
function EnumPermissions: JManifest_permissionClass;
begin
  Result:=  TJManifest_permission.JavaClass
end;

function IsPermissionGranted(APermission: JString): Boolean;
begin
  Result := GetPermissionsSvc.IsPermissionGranted(JStringToString(APermission))
end;

// Granting specified permission.
function GrantAndroidPermission(APermission: JString): Boolean;
begin
  var LResult: Boolean;
  PermissionsService.RequestPermissions([JStringToString(APermission)],
    procedure(const APermissions: TArray<string>; const AGrantResults: TArray<TPermissionStatus>)
    begin
      if (Length(AGrantResults) = 1) and (AGrantResults[0] = TPermissionStatus.Granted) then
      begin
        LResult := True;
      end
      else
        LResult := False;
    end);
  Result := LResult;
end;

function GetPermissionsSvc: TPermissionsService;
begin
  Result := TPermissionsService.DefaultService;
end;
{$ENDIF}

// Return list of controls of specified type.
function GetControlsByClass(AOwner: TControl; AClass: TClass): TList<TControl>;
begin
  var AResult: TList<TControl> := TList<TControl>.Create;
  AOwner.EnumControls(
    function(Control: TControl): TEnumControlsResult
    begin
      if string(Control.Name).IsEmpty then
        Result := TEnumControlsResult.Discard
      else
      begin
        if Control.ClassType = AClass then
          AResult.Add(Control);
        Result := TEnumControlsResult.Continue;
      end;
    end);
  Result := AResult;
end;

// Helper routine to call specific Frame accordings the clicked control name.
function ExtractActivityName(const ActivityName: string): string;
begin
  Result := ActivityName.Split([ACTIVITY_NAME_DELIM])[1]
end;

// Hiding\showing shadow tips caption in Edits controls.
procedure DoEditTyping(Sender: TObject);
begin
  TLabel(GetControlsByClass(TEdit(Sender), TLabel)[0]).Visible := TEdit(Sender).Text.Length = 0;
end;

// Hiding\showing password in Edit controls
procedure DoShowPassImgClick(Sender: TObject);
begin
  var TmpEdit: TEdit := TEdit(TImage(Sender).ParentControl);
  TmpEdit.Password := not TmpEdit.Password;
  TImage(Sender).Opacity := ((Single(Integer(not TmpEdit.Password)) / SHOWPASS_IMG_OPACITY_DIV) * SHOWPASS_IMG_OPACITY_MUL) + SHOWPASS_IMG_OPACITY_VALUE;
end;

function GetMainForm: TForm;
begin
  Result := TForm(Screen.ActiveForm);
end;

{ TFormHelper }

procedure TFormHelper.DoOnControlClick(Sender: TObject);
begin
  ShowActivity(ExtractActivityName(TComponent(Sender).Name), True);
end;

// Main routine to showing specific Screen.
procedure TFormHelper.ShowActivity(const ActivityName: string; const Clear: Boolean = False);
begin
  SynchronizedRun(
    procedure
    begin
      var ComponentIdxStack := TStack<Cardinal>.Create;
      var PrevActivityName: string;
      if Clear then
        for var i:= 0 to ChildrenCount - 1 do
          if (Children[i] is TFrame) then
          begin
            ComponentIdxStack.Push(i);
            PrevActivityName := TFrame(Children[i]).Name;
          end;
        while ComponentIdxStack.Count > 0 do
          Children[ComponentIdxStack.Pop].DisposeOf;
      ComponentIdxStack.DisposeOf;
      if IsClassPresent(Concat(sClassNamePrefix, ActivityName, sActivityClassNameSuffix)) then
      begin
        var ActivityClass: TPersistent := GetClass(Concat(sClassNamePrefix,
          ActivityName, sActivityClassNameSuffix)).Create;
        var ActivityFrame: TFrame := TFrame(ActivityClass).Create(nil);
        AddObject(ActivityFrame);
        var ActivityInfo: IActivityInfo;
        if Supports(ActivityFrame, GetTypeData(TypeInfo(IActivityInfo))^.Guid, ActivityInfo) then
          if Assigned(ActivityInfo) then
          begin
            ActivityInfo.ActivityName := ActivityName;
            ActivityInfo.PrevActivityName := PrevActivityName;
            ActivityFrame.SendToBack;
          end;
      end;
    end);
end;


// Getting current active screen in case where we have many suspended Screens.
function TFormHelper.GetCurrentAtivity: TFrame;
begin
  Result := nil;
  var ComponentIdxStack := TStack<Cardinal>.Create;
    for var i:= 0 to ChildrenCount - 1 do
      if (Children[i] is TFrame)  then
        ComponentIdxStack.Push(i);
    while ComponentIdxStack.Count > 0 do
    begin
      var C: Cardinal := ComponentIdxStack.Pop;
      if Children[C] is TFrame then
        Result := TFrame(Children[C]);
    end;
end;

// Set focus to specified control(uses for debugging).
procedure TFormHelper.SetFocusOnFirtstControl(AClass: TClass);
begin
  if Assigned(GetCurrentAtivity()) then
  begin
    var C: TControl := GetControlsByClass(GetCurrentAtivity, AClass)[0];
    if Assigned(C) then
      C.SetFocus
  end;
end;

{ TAppWizardImageLoader }

// Constructor fo TAppWizardImageLoader.
constructor TAppWizardImageLoader.Create(AFinishTaking: TOnDidFinishTaking);
begin
  FBitmap := TBitmap.Create;
  FTakePhotoFromLibraryAction := TTakePhotoFromLibraryAction.Create(nil);
  FTakePhotoFromLibraryAction.OnDidFinishTaking := AFinishTaking;
  FTakePhotoFromLibraryAction.Editable := True;
end;

// Execition Action to take image from non-Windows gallery.
procedure TAppWizardImageLoader.Exec;
begin
  FTakePhotoFromLibraryAction.Execute;
end;

{ TBitMapHelper }

// Making all pixels of an image outside the mask transparent.
procedure TImageHelper.PremultiplyBitmapAlpha(ABmp: TBitmap);
begin
  var M: TBitMapData;
  if ABmp.Map(TMapAccess.ReadWrite, M) then
  try
    for var Y := 0 to ABmp.Height - 1 do
      for var X := 0 to ABmp.Width - 1 do
      begin
        var C: PAlphaColorRec:= @PAlphaColorArray(M.Data)[Y * (M.Pitch div 4) + X];
        C^.Color := PremultiplyAlpha(C^.Color);
      end;
  finally
    ABmp.Unmap(M);
  end;
end;

// Payload routine for Making Circle Mask.
procedure TImageHelper.MakeCircleMaskProc;
begin
  if Bitmap.Width <= 0 then
    Exit;
  var MaskedBmp: TBitmap := TBitmap.Create;
  var MaskCircle: TCircle := TCircle.Create(nil);
  MaskCircle.Stroke.Kind := TBrushKind.None;
  MaskCircle.Fill.Color := TAlphaColorRec.White;
  MaskCircle.Width := Bitmap.Width;
  MaskCircle.Height := Bitmap.Height;
  MaskedBmp.CreateFromBitmapAndMask(Bitmap, MaskCircle.MakeScreenshot);
  PremultiplyBitmapAlpha(MaskedBmp);
  Bitmap.Assign(MaskedBmp);
  MaskedBmp.Free;
  MaskCircle.Free;
end;

// Proxy routine for Making Circle Mask.
procedure TImageHelper.MakeCircleMask;
begin
{$REGION 'Need to sync with UI Thread to avoid bitmap glitches.'}

  //  var BgTask: IBackGroundTask := (GetMainForm as IBackGroundTask);
  //  if Assigned(BgTask) then
  //  begin
  //    TThread.Synchronize(
  //      BgTask.GetBackgroundTask,
  //      procedure
  //      begin
  //        MakeCircleMaskProc;
  //      end)
  //  end
  //  else
{$ENDREGION}
  MakeCircleMaskProc;
end;

{ TFrameHelper }

// ReCreate itself.
procedure TFrameHelper.Recreate;
begin
  var ActivityInfo: IActivityInfo;
  if Supports(Self, GetTypeData(TypeInfo(IActivityInfo))^.Guid, ActivityInfo) then
    if Assigned(ActivityInfo) then
      GetMainForm.ShowActivity(ActivityInfo.ActivityName, True);
end;

{ TLabelHelper }

procedure TLabelHelper.EnableStyledTextSize;
begin
  Self.StyledSettings := Self.StyledSettings + [TStyledSetting.Size];
end;

function TLabelHelper.GetStyledColor: TAlphaColor;
begin
  Result := Self.TextSettings.FontColor;
  var Text: TText := TText(TControl(Self).FindStyleResource(sLabelStyleTextResName));
  if Assigned(Text) then
    Result := Text.TextSettings.FontColor
end;

procedure TLabelHelper.EnableStyledColor;
begin
  Self.StyledSettings := Self.StyledSettings + [TStyledSetting.FontColor];
end;

procedure TLabelHelper.ResetStyleSettings;
begin
  Self.StyledSettings := [];
end;

{ TApplicationHelper }

function TApplicationHelper.IsUnitTestRunning: Boolean;
begin
  Result := Extractfilename(ParamStr(0)).Contains(sUnitTestAppPostfix);
end;

end.
