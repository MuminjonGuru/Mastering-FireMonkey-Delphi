unit DelphiAnimation.uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Effects,
  FMX.Objects, FMX.Layouts, FMX.ScrollBox, FMX.Memo, FMX.Controls.Presentation,
  FMX.StdCtrls, FMX.Ani;

type
  TForm1 = class(TForm)
    LytMain: TLayout;
    RctAnimate: TRectangle;
    LytMainBottom: TLayout;
    BtnAction: TButton;
    StyleBook1: TStyleBook;
    Label1: TLabel;
    VertScrollBoxClient: TVertScrollBox;
    procedure BtnActionClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.BtnActionClick(Sender: TObject);
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

end.
