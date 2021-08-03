//---------------------------------------------------------------------------

// This software is Copyright (c) 2021 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit View.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Layouts, FMX.Controls.Presentation, FMX.MultiView, FMX.Ani,
  FMX.ScrollBox, FMX.Memo, System.ImageList, FMX.ImgList, Model.Constants,
  System.Generics.Collections, FMX.Effects, FMX.MultiView.Types,
  Model.Utils, Model.Types;

type
  // Parent frame for other content frames.
  TMainFrame = class(TFrame, IActivityInfo)
    MainLayout: TGridPanelLayout;
    HeaderRect: TRectangle;
    CaptionLbl: TLabel;
    HamburgerImg: TPath;
    HitBox: TLayout;
    MasterShadowEffect: TShadowEffect;
    procedure MultiViewOnHidden(Sender: TObject);
    procedure HitBoxClick(Sender: TObject);
    procedure CaptionLblApplyStyleLookup(Sender: TObject);
    procedure FloatAnimation1Process(Sender: TObject);
    procedure FrameResize(Sender: TObject);
  private
    FMultiView: TMultiView;
    FActivityName: string;
    FPrevActivityName: string;
  protected
    function GetMultiView: TMultiView;
    function GetActivityName: string;
    procedure SetActivityName(AActivityName: string);
    function GetPrevActivityName: string;
    procedure SetPrevActivityName(AActivityName: string);
    property ActivityName: string read GetActivityName write SetActivityName;
    property PrevActivityName: string read GetPrevActivityName write SetPrevActivityName;
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{$R *.fmx}

{ TMainFrame }

procedure TMainFrame.CaptionLblApplyStyleLookup(Sender: TObject);
begin
  HamburgerImg.Fill.Color := TLabel(Sender).GetStyledColor;
  HeaderRect.Stroke.Color := HamburgerImg.Fill.Color;
end;

constructor TMainFrame.Create(AOwner: TComponent);
begin
  inherited;
  if Assigned(GetMultiView()) then
  begin
    if (Name <> Concat(sActivityMenuName, sActivityClassNameSuffix)) and
      (Name = Concat(sActivityHomeName, sActivityClassNameSuffix)) then
    begin
      FMultiView.MasterButton := HitBox;
      FMultiView.OnHidden := MultiViewOnHidden;
    end
    else
    begin
      FMultiView.MasterButton := nil;
      FMultiView.OnHidden := nil;
    end;
  end;
  // Realign View after activity creation.
  var VI: IViewInfo := GetMainForm as IViewInfo;
  var VU: IViewUtils := GetMainForm as IViewUtils;
  if Assigned(VI) and Assigned(VU) then
    if (VI.GetScreenOrientation = TScreenOrientation.Landscape) and string(Name).Contains(sActivityHomeName)
      and (TOSVersion.Platform <> pfWindows) then
        SynchronizedRun(
          procedure
          begin
            while not Assigned(GetMainForm.GetCurrentAtivity()) do
              Sleep(dLoopWaitForRealign);
            VU.LandscapeRealign;
          end)
    else
      FMultiView.HideMaster;
end;

procedure TMainFrame.FloatAnimation1Process(Sender: TObject);
begin
  Application.ProcessMessages;
end;

procedure TMainFrame.FrameResize(Sender: TObject);
begin
  // Hide Hamburger. It means that Screen in Landscape mode.
  var VI: IViewInfo := GetMainForm as IViewInfo;
  if Assigned(VI) and (TOSVersion.Platform <> pfWindows) then
  begin
    case (GetMainForm as IViewInfo).GetScreenOrientation of
      TScreenOrientation.Portrait: MainLayout.RowCollection[0].Value := fPortraitModeToolBarPercetage;
      TScreenOrientation.Landscape: MainLayout.RowCollection[0].Value := fLandscapeModeToolBarPercetage;
    end;
    if not string(Name).Contains(sActivityMenuName) then
      HamburgerImg.Visible := VI.GetScreenOrientation = TScreenOrientation.Portrait;
  end;
end;

// Accsess to MultiView from specific Screen.
function TMainFrame.GetMultiView: TMultiView;
begin
  FMultiView := nil;
  Result := nil;
  if Screen.Forms[0].FindComponent('MultiView') is TMultiView  then
    FMultiView := TMultiView(Screen.Forms[0].FindComponent('MultiView'));
  if Assigned(FMultiView) then
    Result := FMultiView;
end;

function TMainFrame.GetPrevActivityName: string;
begin
  Result := FPrevActivityName;
end;

// 'Hamburger' click handler.
// Needed to use an additional 'hitbox' since the child control does not have enough area for clicking.
procedure TMainFrame.HitBoxClick(Sender: TObject);
begin
  if FMultiView.MasterButton <> HitBox then
  begin
    if (ActivityName.Equals(sActivityPrivacy) or ActivityName.Equals(sActivityTermsOfUse)) then
      GetMainForm.ShowActivity(ClassNameToActivityName(PrevActivityName), True)
    else
      GetMainForm.ShowActivity(sActivityHomeName, True);
  end;
end;

procedure TMainFrame.SetActivityName(AActivityName: string);
begin
  FActivityName := AActivityName;
end;

procedure TMainFrame.SetPrevActivityName(AActivityName: string);
begin
  FPrevActivityName := AActivityName;
end;

function TMainFrame.GetActivityName: string;
begin
  Result := FActivityName;
end;

procedure TMainFrame.MultiViewOnHidden(Sender: TObject);
begin
  BringToFront; // Prevent block MainForm controls after adding Activity.
end;

initialization
  // Register frame
  RegisterClass(TMainFrame);
finalization
  // Unregister frame
  UnRegisterClass(TMainFrame);

end.
