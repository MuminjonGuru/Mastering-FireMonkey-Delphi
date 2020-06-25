{**************************************************}
{   Muminjon.com - Delphi.uz                       }
{                    @MuminjonGuru                 }
{**************************************************}

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
, FMX.Objects
, FMX.Controls.Presentation
, FMX.StdCtrls
, FMX.Layouts;

type
  TFormMain = class(TForm)
    RctMain: TRectangle;
    RRTop: TRoundRect;
    Label1: TLabel;
    RoundRect1: TRoundRect;
    Label2: TLabel;
    RoundRect2: TRoundRect;
    Label3: TLabel;
    VSBMain: TVertScrollBox;
    RctClient1: TRectangle;
    RctLeftBar: TRectangle;
    LytClient: TLayout;
    LytClientBottom: TLayout;
    Label4: TLabel;
    Label5: TLabel;
    LytRightBar: TLayout;
    CircleEdit: TCircle;
    CircleDelete: TCircle;
    CircleCall: TCircle;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

end.
