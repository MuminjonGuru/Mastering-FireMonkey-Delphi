unit uMain;

interface

uses
  System.SysUtils
, System.Types
, System.UITypes
, System.Classes
, System.Variants
, FMX.Types
, FMX.Controls
, FMX.Forms
, FMX.Graphics
, FMX.Dialogs
, FMX.StdCtrls
, FMX.Objects
, FMX.Layouts
, FMX.Controls.Presentation
, FMX.Ani
, FMX.TabControl
, System.Actions
, FMX.ActnList;

type
  TFrmMain = class(TForm)
    SBtnDownload: TSpeedButton;
    LytTop: TLayout;
    Rct1: TRectangle;
    Btn1: TButton;
    FloatAnimation1: TFloatAnimation;
    FloatAnimation2: TFloatAnimation;
    Layout2: TLayout;
    Layout3: TLayout;
    Layout4: TLayout;
    Rectangle1: TRectangle;
    Label1: TLabel;
    Label2: TLabel;
    Button1: TButton;
    StyleBook1: TStyleBook;
    Button2: TButton;
    Circle1: TCircle;
    Circle2: TCircle;
    Circle3: TCircle;
    FloatAnimation3: TFloatAnimation;
    FloatAnimation4: TFloatAnimation;
    FloatAnimation5: TFloatAnimation;
    Circle4: TCircle;
    FloatAnimation6: TFloatAnimation;
    Circle5: TCircle;
    FloatAnimation7: TFloatAnimation;
    Rectangle2: TRectangle;
    BtnSetStopValue: TButton;
    TabControl1: TTabControl;
    BtnNext: TButton;
    BtnPrevious: TButton;
    ActionList1: TActionList;
    NextTabAction1: TNextTabAction;
    PreviousTabAction1: TPreviousTabAction;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    TabItem3: TTabItem;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    TabItem4: TTabItem;
    RctToggleBtn: TRectangle;
    RctToggle: TRectangle;
    ColorAnimationGo: TColorAnimation;
    FloatAnimationGo: TFloatAnimation;
    FloatAnimationBack: TFloatAnimation;
    ColorAnimationBack: TColorAnimation;
    procedure Btn1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure SBtnDownloadClick(Sender: TObject);
    procedure BtnSetStopValueClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BtnNextClick(Sender: TObject);
    procedure BtnPreviousClick(Sender: TObject);
    procedure RctToggleClick(Sender: TObject);
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
{$R *.LgXhdpiPh.fmx ANDROID}
{$R *.Windows.fmx MSWINDOWS}
{$R *.SmXhdpiPh.fmx ANDROID}

uses
  System.Threading;

procedure TFrmMain.Btn1Click(Sender: TObject);
begin
  // Rct1 this is a Special Button.
  // We have to make it expand smoothly and then we can show the Details
  var Task: ITask := TTask.Create(
    procedure()
    begin
      Sleep(300);
      Layout2.Visible := True;
    end);

  if Rct1.Width = 120 then
  begin
    FloatAnimation1.Start;
    Task.Start;
  end
  else
  begin
    Layout2.Visible := False;
    FloatAnimation2.Start
  end;
end;

procedure TFrmMain.Button2Click(Sender: TObject);
begin
  ShowMessage('Test'); // just for a test
end;

procedure TFrmMain.BtnSetStopValueClick(Sender: TObject);
begin
  // Giving a stop value to the animations
  // we have to subtract the Circle Width from the application width
  // it helps to show up the full size of circles
  FloatAnimation3.StopValue := FrmMain.Width - 17;
  FloatAnimation4.StopValue := FrmMain.Width - 17;
  FloatAnimation5.StopValue := FrmMain.Width - 17;
  FloatAnimation6.StopValue := FrmMain.Width - 17;
  FloatAnimation7.StopValue := FrmMain.Width - 17;

  // start animation by clicking this button
  SBtnDownload.Enabled := True;
end;

procedure TFrmMain.BtnNextClick(Sender: TObject);
begin
  // Slide right
  ActionList1.Actions[0].Execute;
end;

procedure TFrmMain.BtnPreviousClick(Sender: TObject);
begin
  // Slide left
  ActionList1.Actions[1].Execute;
end;

procedure TFrmMain.FormCreate(Sender: TObject);
begin
  RctToggle.Position.X := 0;

  SBtnDownload.Enabled := False;
end;

procedure TFrmMain.RctToggleClick(Sender: TObject);
begin
  if RctToggle.Position.X = 0 then
  begin
    FloatAnimationGo.Start;
    ColorAnimationGo.Start;
  end
  else if RctToggle.Position.X = 50 then
  begin
    FloatAnimationBack.Start;
    ColorAnimationBack.Start
  end;
end;

procedure TFrmMain.SBtnDownloadClick(Sender: TObject);
begin
  // staring circles' animations
  FloatAnimation3.Start;
  FloatAnimation4.Start;
  FloatAnimation5.Start;
  FloatAnimation6.Start;
  FloatAnimation7.Start;

  // preventing circle animations glitches
  SBtnDownload.Enabled := False;
end;

end.
