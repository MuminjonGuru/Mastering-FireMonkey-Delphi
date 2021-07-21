unit View.Menu.Item;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Controls.Presentation, FMX.Layouts, Model.Constants, Model.Utils;

type
  TMenuItemFrame = class(TFrame)
    Layout: TLayout;
    imgArrrow: TPath;
    BgRect: TRectangle;
    lblCaption: TLabel;
    procedure lblCaptionApplyStyleLookup(Sender: TObject);
  private
    function GetBGColor: TAlphaColor;
    procedure SetBGColor(const Value: TAlphaColor);
    function GetCaption: string;
    procedure SetCaption(const Value: string);
    { Private declarations }
  public
    property BackgroundColor: TAlphaColor read GetBGColor write SetBGColor;
    property Caption: string read GetCaption write SetCaption;
    constructor Create(ACaption: string; ABackgroundColor: TAlphaColor); reintroduce;
  end;

implementation

{$R *.fmx}

{ TMenuItemFrame }

constructor TMenuItemFrame.Create(ACaption: string;
  ABackgroundColor: TAlphaColor);
begin
  inherited Create(nil);
  lblCaption.Text := ACaption;

  //BgRect.Fill.Color := ABackgroundColor
end;

function TMenuItemFrame.GetBGColor: TAlphaColor;
begin
  Result := BgRect.Fill.Color
end;

function TMenuItemFrame.GetCaption: string;
begin
  Result := lblCaption.Text
end;

procedure TMenuItemFrame.lblCaptionApplyStyleLookup(Sender: TObject);
begin
  imgArrrow.Fill.Color := TLabel(Sender).GetStyledColor;
end;

procedure TMenuItemFrame.SetBGColor(const Value: TAlphaColor);
begin
  BgRect.Fill.Color := Value
end;

procedure TMenuItemFrame.SetCaption(const Value: string);
begin
  lblCaption.Text := Value
end;

end.
