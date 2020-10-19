unit CarRacing.uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, System.Math,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.Controls.Presentation, FMX.StdCtrls;

type
  TFormMain = class(TForm)
    Rct1: TRectangle;
    Rct2: TRectangle;
    Rct3: TRectangle;
    Rct4: TRectangle;
    RctLeftSide: TRectangle;
    RctRightSide: TRectangle;
    Timer1: TTimer;
    Car: TRectangle;
    Enemy1: TRectangle;
    Enemy2: TRectangle;
    Label1: TLabel;
    Coin1: TRectangle;
    Coin3: TRectangle;
    Coin4: TRectangle;
    Coin2: TRectangle;
    LblScore: TLabel;
    procedure Timer1Timer(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
  private
    GameSpeed: Integer;
    CollectedCoins: Integer;
    procedure MoveLine(Speed: Integer);
    procedure Enemy(Speed: Integer);
    procedure GameOver;
    procedure MoveCoins(Speed: Integer);
    procedure CollectCoins;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

procedure TFormMain.CollectCoins;
begin
  var X: Integer := 0;

  if(Car.BoundsRect.IntersectsWith(Coin1.BoundsRect)) then
  begin
    Inc(CollectedCoins);
    LblScore.Text := CollectedCoins.ToString;
    X := RandomRange(50, 300);
    Coin1.Position.X := X;
    Coin1.Position.Y := 0;
  end;

  if(Car.BoundsRect.IntersectsWith(Coin2.BoundsRect)) then
  begin
    Inc(CollectedCoins);
    LblScore.Text := CollectedCoins.ToString;
    X := RandomRange(0, 200);
    Coin2.Position.X := X;
    Coin2.Position.Y := 0;
  end;

  if(Car.BoundsRect.IntersectsWith(Coin3.BoundsRect)) then
  begin
    Inc(CollectedCoins);
    LblScore.Text := CollectedCoins.ToString;
    X := RandomRange(0, 200);
    Coin3.Position.X := X;
    Coin3.Position.Y := 0;
  end;

  if(Car.BoundsRect.IntersectsWith(Coin4.BoundsRect)) then
  begin
    Inc(CollectedCoins);
    LblScore.Text := CollectedCoins.ToString;
    X := RandomRange(0, 200);
    Coin4.Position.X := X;
    Coin4.Position.Y := 0;
  end;
end;

procedure TFormMain.Enemy(Speed: Integer);
begin
  var X: Integer;

  if(Enemy1.Position.Y >= 500) then
  begin
    X := RandomRange(0, 200);
    Enemy1.Position.X := X;
    Enemy1.Position.Y := 0;
  end
  else
    Enemy1.Position.Y := Enemy1.Position.Y + Speed;

  if(Enemy2.Position.Y >= 500) then
  begin
    X := RandomRange(0, 400);
    Enemy2.Position.X := X;
    Enemy2.Position.Y := 0;
  end
  else
    Enemy2.Position.Y := Enemy2.Position.Y + Speed;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  GameSpeed := 0;
  CollectedCoins := 0;
  LblScore.Text := CollectedCoins.ToString;
end;

procedure TFormMain.FormKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  case Key of
    vkUp:
      begin
        if(GameSpeed < 20) then
          Inc(GameSpeed);
      end;
    vkDown:
      begin
         if(GameSpeed > 0) then
          Dec(GameSpeed);
      end;
    vkLeft:
      begin
        if(Car.Position.X > 0) then
          Car.Position.X := Car.Position.X - 10;
      end;
    vkRight:
      begin
        if(Car.Position.X < 350) then
          Car.Position.X := Car.Position.X + 10;
      end;
  end;
end;

procedure TFormMain.GameOver;
begin
  if (Car.BoundsRect.IntersectsWith(Enemy1.BoundsRect)) then
  begin
    Timer1.Enabled := False;
    Label1.Visible := True;
  end;

  if (Car.BoundsRect.IntersectsWith(Enemy2.BoundsRect)) then
  begin
    Timer1.Enabled := False;
    Label1.Visible := True;
  end;
end;

procedure TFormMain.MoveLine(Speed: Integer);
begin
  if (Rct1.Position.Y >= 500) then
    Rct1.Position.Y := 0
  else
    Rct1.Position.Y := Rct1.Position.Y + Speed;

  if (Rct2.Position.Y >= 500) then
    Rct2.Position.Y := 0
  else
    Rct2.Position.Y := Rct2.Position.Y + Speed;

  if (Rct3.Position.Y >= 500) then
    Rct3.Position.Y := 0
  else
    Rct3.Position.Y := Rct3.Position.Y + Speed;

  if (Rct4.Position.Y >= 500) then
    Rct4.Position.Y := 0
  else
    Rct4.Position.Y := Rct4.Position.Y + Speed;
end;

procedure TFormMain.MoveCoins(Speed: Integer);
begin
  var X: Integer;

  if(Coin1.Position.Y >= 500) then
  begin
    X := RandomRange(0, 200);
    Coin1.Position.X := X;
    Coin1.Position.Y := 0;
  end
  else
    Coin1.Position.Y := Coin1.Position.Y + Speed;

  if(Coin2.Position.Y >= 500) then
  begin
    X := RandomRange(0, 400);
    Coin2.Position.X := X;
    Coin2.Position.Y := 0;
  end
  else
    Coin2.Position.Y := Coin2.Position.Y + Speed;

  if(Coin3.Position.Y >= 500) then
  begin
    X := RandomRange(200, 355);
    Coin3.Position.X := X;
    Coin3.Position.Y := 0;
  end
  else
    Coin3.Position.Y := Coin3.Position.Y + Speed;

  if(Coin4.Position.Y >= 500) then
  begin
    X := RandomRange(200, 355);
    Coin4.Position.X := X;
    Coin4.Position.Y := 0;
  end
  else
    Coin4.Position.Y := Coin4.Position.Y + Speed;
end;

procedure TFormMain.Timer1Timer(Sender: TObject);
begin
  MoveLine(GameSpeed);
  Enemy(GameSpeed);
  MoveCoins(GameSpeed);
  CollectCoins;
  GameOver;
end;

initialization
  Randomize;

end.
