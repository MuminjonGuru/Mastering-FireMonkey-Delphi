unit DelphiAnimation.uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Effects,
  FMX.Objects, FMX.Layouts, FMX.ScrollBox, FMX.Memo, FMX.Controls.Presentation,
  FMX.StdCtrls, FMX.Ani;

type
  TFormMain = class(TForm)
    LytMain: TLayout;
    RctAnimate: TRectangle;
    LytMainBottom: TLayout;
    BtnAction: TButton;
    StyleBook1: TStyleBook;
    Label1: TLabel;
    VertScrollBoxClient: TVertScrollBox;
    CheckBox1: TCheckBox;
    procedure BtnActionClick(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}
{$R *.Windows.fmx MSWINDOWS}

procedure TFormMain.BtnActionClick(Sender: TObject);
begin
  if RctAnimate.Height = 250 then
  begin
    TAnimator.AnimateFloat(RctAnimate, 'Height', 100, 0.8,
                          TAnimationType.&In, TInterpolationType.Linear);
    BtnAction.Text := 'Expand';
  end
  else
  begin
    TAnimator.AnimateFloat(RctAnimate, 'Height', 250, 0.8,
                          TAnimationType.&In, TInterpolationType.Linear);
    BtnAction.Text := 'Collapse';
  end;
end;

procedure TFormMain.CheckBox1Change(Sender: TObject);
begin
  if CheckBox1.IsChecked = True then
    VertScrollBoxClient.ShowScrollBars := CheckBox1.IsChecked
  else
    VertScrollBoxClient.ShowScrollBars := CheckBox1.IsChecked;
end;

end.
