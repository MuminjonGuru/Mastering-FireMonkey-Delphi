unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, FMX.Edit, FMX.Objects,
  FMX.Effects, FMX.Filter.Effects, FMX.Ani, FMX.TabControl, FMX.Gestures,
  System.Actions, FMX.ActnList;

type
  TFrmMain = class(TForm)
    LblTitleLogin: TLabel;
    EdtUsername: TEdit;
    EdtPassword: TEdit;
    LblUsername: TLabel;
    LblPassword: TLabel;
    LytBody: TLayout;
    LytHeader: TLayout;
    BtnLogin: TButton;
    SpeedButton1: TSpeedButton;
    LytBottom: TLayout;
    RctMain: TRectangle;
    VSBLogin: TVertScrollBox;
    RctBackground: TRectangle;
    LytMain: TLayout;
    GaussianBlurEffect1: TGaussianBlurEffect;
    StyleBookCustom: TStyleBook;
    TxtRqrdUsername: TText;
    LytRqrdUsername: TLayout;
    RRRed: TRoundRect;
    FltAniRqrdUserShow: TFloatAnimation;
    RR4Text: TRoundRect;
    FltAniRqrdUserHide: TFloatAnimation;
    LytRqrdPassword: TLayout;
    RRRedPassword: TRoundRect;
    FltAniRqrdPassShow: TFloatAnimation;
    RR4Text2: TRoundRect;
    TxtRqrdPass: TText;
    FltAniRqrdPassHide: TFloatAnimation;
    FltAniLoginForm: TFloatAnimation;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    GestureManager1: TGestureManager;
    BtnContinue: TButton;
    Label2: TLabel;
    Label3: TLabel;
    ActionList1: TActionList;
    NextTabAction1: TNextTabAction;
    Rectangle1: TRectangle;
    Rectangle2: TRectangle;
    GaussianBlurEffect2: TGaussianBlurEffect;
    procedure GestureDone(Sender: TObject; const EventInfo: TGestureEventInfo; var Handled: Boolean);
    procedure BtnLoginClick(Sender: TObject);
    procedure EdtUsernameChangeTracking(Sender: TObject);
    procedure EdtPasswordChangeTracking(Sender: TObject);
    procedure BtnContinueClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmMain: TFrmMain;

implementation

{$R *.fmx}
{$R *.NmXhdpiPh.fmx ANDROID}

uses
  System.Threading;

procedure TFrmMain.BtnContinueClick(Sender: TObject);
begin
  ShowMessage('test');
end;

procedure TFrmMain.BtnLoginClick(Sender: TObject);
var
  Task: ITask;
begin
  // username field
  if EdtUsername.Text = '' then
    FltAniRqrdUserShow.Start;

  // pasword field
  if EdtPassword.Text = '' then
    FltAniRqrdPassShow.Start;

  Task := TTask.Create(procedure()
  begin
    // Do all logic in here
    Sleep(500);
    VSBLogin.Visible := False;
  end);

  // if fields filled
  if (EdtUsername.Text <> '') and (EdtPassword.Text <> '') then
  begin
    FltAniLoginForm.Start;
//    VSBLogin.Visible := False;
    Task.Start;
  end;
end;

procedure TFrmMain.EdtPasswordChangeTracking(Sender: TObject);
begin
  // password field
  if EdtPassword.Text <> '' then
    FltAniRqrdPassHide.Start;
end;

procedure TFrmMain.EdtUsernameChangeTracking(Sender: TObject);
begin
  // username field
  if EdtUsername.Text <> '' then
    FltAniRqrdUserHide.Start;
end;

procedure TFrmMain.FormCreate(Sender: TObject);
begin
  EdtUsername.SetFocus;
end;

procedure TFrmMain.GestureDone(Sender: TObject;
  const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
  case EventInfo.GestureID of
    sgiLeft:
      begin
        if TabControl1.ActiveTab <> TabControl1.Tabs[TabControl1.TabCount - 1] then
          TabControl1.ActiveTab := TabControl1.Tabs[TabControl1.TabIndex + 1];
        Handled := True;
      end;

    sgiRight:
      begin
        if TabControl1.ActiveTab <> TabControl1.Tabs[0] then
          TabControl1.ActiveTab := TabControl1.Tabs[TabControl1.TabIndex - 1];
        Handled := True;
      end;
  end;
end;

end.
