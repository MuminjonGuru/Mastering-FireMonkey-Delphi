unit QuizAppTemplate.uMain;

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
, FMX.Objects
, FMX.Layouts
, FMX.Ani
, FMX.TabControl
, FMX.Controls.Presentation
, FMX.Edit
, System.Actions
, FMX.ActnList
, FMX.Media
, System.IOUtils
, System.Threading;

type
  TFrmMain = class(TForm)
    LytRight: TLayout;
    RctTrue: TRectangle;
    TxtTrueScore: TText;
    LytLeft: TLayout;
    RctFalse: TRectangle;
    TxtFalseScore: TText;
    LytTop: TLayout;
    RctQuiz: TRectangle;
    TxtChooseAns: TText;
    TxtQuestion: TText;
    LytAnswers: TLayout;
    RctOptionFirst: TRectangle;
    RctOptionThird: TRectangle;
    RctOptionSecond: TRectangle;
    TxtOptionFirst: TText;
    TxtOptionSecond: TText;
    TxtOptionThird: TText;
    FloatAnimation1: TFloatAnimation;
    FloatAnimation2: TFloatAnimation;
    FloatAnimation3: TFloatAnimation;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    Rectangle1: TRectangle;
    Text1: TText;
    Text2: TText;
    Layout1: TLayout;
    Rectangle2: TRectangle;
    Text3: TText;
    FloatAnimation4: TFloatAnimation;
    Edit1: TEdit;
    ActionList1: TActionList;
    NextTabAction1: TNextTabAction;
    procedure RctOptionFirstClick(Sender: TObject);
    procedure RctOptionSecondClick(Sender: TObject);
    procedure RctOptionThirdClick(Sender: TObject);
    procedure Rectangle2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    TrueScore: Byte;
    FalseScore: Byte;
  public
    { Public declarations }
  end;

var
  FrmMain: TFrmMain;
  Task: ITask;

implementation

uses
{$IFDEF ANDROID}
  Androidapi.Helpers,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Os,
{$ENDIF}
  FMX.DialogService;

{$R *.fmx}

procedure TFrmMain.FormCreate(Sender: TObject);
begin
  TrueScore := 0;
  FalseScore := 0;
end;

procedure TFrmMain.RctOptionFirstClick(Sender: TObject);
begin
  FloatAnimation1.Start;
  RctOptionFirst.Fill.Color := TAlphaColorRec.Red;

  Inc(FalseScore);
  TxtFalseScore.Text := IntToStr(FalseScore);

  ActionList1[0].Execute;
end;

procedure TFrmMain.RctOptionSecondClick(Sender: TObject);
begin
  FloatAnimation2.Start;
  RctOptionSecond.Fill.Color := TAlphaColorRec.Green;

  Task := TTask.Create(procedure()
  begin
    // Do all logic in here
    Sleep(600);
    Inc(TrueScore);
    TxtTrueScore.Text := IntToStr(TrueScore);

    ActionList1[0].Execute;
  end);

  Task.Start;
end;

procedure TFrmMain.RctOptionThirdClick(Sender: TObject);
begin
  FloatAnimation3.Start;
  RctOptionThird.Fill.Color := TAlphaColorRec.Red;

  Inc(FalseScore);
  TxtFalseScore.Text := IntToStr(FalseScore);

  ActionList1[0].Execute;
end;

procedure TFrmMain.Rectangle2Click(Sender: TObject);
begin
  if Edit1.Text.Equals('some') then
  begin
    Inc(TrueScore);
    TxtTrueScore.Text := IntToStr(TrueScore);
    ActionList1[0].Execute;
  end
  else
  begin
    Inc(FalseScore);
    TxtFalseScore.Text := IntToStr(FalseScore);
    ActionList1[0].Execute;
  end;
end;

end.
