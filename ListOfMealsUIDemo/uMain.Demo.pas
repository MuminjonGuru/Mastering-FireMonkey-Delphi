unit uMain.Demo;

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
, System.ImageList
, FMX.ImgList
, FMX.StdCtrls
, FMX.Controls.Presentation
, FMX.Layouts
, FMX.ListBox
, FMX.Edit
, FMX.Effects;

type
  TFrmMain = class(TForm)
    RctTop: TRectangle;
    BtnMasterMenu: TButton;
    RctClubMember: TRectangle;
    BtnClubMember: TSpeedButton;
    ImageList1: TImageList;
    CustomStyles: TStyleBook;
    CBUsers: TComboBox;
    LayoutUserArea: TLayout;
    CircleAvatar: TCircle;
    LytMain: TLayout;
    RctLeftList: TRectangle;
    LytItems: TLayout;
    TxtTotalItems: TText;
    VertScrollBox1: TVertScrollBox;
    RctBottom: TRectangle;
    LytPrice: TLayout;
    LblDiscount: TLabel;
    LblTotal: TLabel;
    LytActions: TLayout;
    LytNewOrCancel: TLayout;
    RctCheckout: TRectangle;
    Text1: TText;
    RctNewOrder: TRectangle;
    Text2: TText;
    RctCancel: TRectangle;
    Text3: TText;
    RctItem: TRectangle;
    LytTop: TLayout;
    ImgItem: TImage;
    TxtItem: TText;
    LytBottom: TLayout;
    LytBottomLeft: TLayout;
    Text4: TText;
    LytBottomRight: TLayout;
    Text5: TText;
    Text6: TText;
    RctMain: TRectangle;
    Layout1: TLayout;
    LytItemsTop: TLayout;
    TxtSelectCategory: TText;
    TxtPopular: TText;
    RctSearchBar: TRectangle;
    Image1: TImage;
    Rectangle1: TRectangle;
    Image2: TImage;
    Edit1: TEdit;
    Image3: TImage;
    Rectangle2: TRectangle;
    Layout3: TLayout;
    Image4: TImage;
    Text7: TText;
    Layout4: TLayout;
    Layout5: TLayout;
    Text8: TText;
    Layout6: TLayout;
    Text9: TText;
    Text10: TText;
    Rectangle3: TRectangle;
    Layout7: TLayout;
    Image5: TImage;
    Text11: TText;
    Layout8: TLayout;
    Layout9: TLayout;
    Text12: TText;
    Layout10: TLayout;
    Text13: TText;
    Text14: TText;
    Rectangle4: TRectangle;
    Layout11: TLayout;
    Image6: TImage;
    Text15: TText;
    Layout12: TLayout;
    Layout14: TLayout;
    Text17: TText;
    Rectangle5: TRectangle;
    Layout15: TLayout;
    Image7: TImage;
    Text19: TText;
    Layout16: TLayout;
    Layout18: TLayout;
    Text21: TText;
    Text22: TText;
    Rectangle6: TRectangle;
    Layout19: TLayout;
    Image8: TImage;
    Text23: TText;
    Layout20: TLayout;
    Layout22: TLayout;
    Text25: TText;
    Rectangle7: TRectangle;
    Layout23: TLayout;
    Image9: TImage;
    Text27: TText;
    Layout24: TLayout;
    Layout26: TLayout;
    Text29: TText;
    ShadowEffect1: TShadowEffect;
    ShadowEffect2: TShadowEffect;
    ShadowEffect3: TShadowEffect;
    ShadowEffect4: TShadowEffect;
    ShadowEffect5: TShadowEffect;
    ShadowEffect6: TShadowEffect;
    ShadowEffect7: TShadowEffect;
    RctItems: TRectangle;
    FlowLayoutItems: TFlowLayout;
    RctDrinks: TRectangle;
    ImgDrinks: TImage;
    LytItemsDrinkDetails: TLayout;
    LblItemsDetails: TLabel;
    Image10: TImage;
    Rectangle8: TRectangle;
    Image11: TImage;
    Layout2: TLayout;
    Label1: TLabel;
    Image12: TImage;
    Rectangle9: TRectangle;
    Image13: TImage;
    Layout13: TLayout;
    Label2: TLabel;
    Image14: TImage;
    Rectangle10: TRectangle;
    Image15: TImage;
    Layout17: TLayout;
    Label3: TLabel;
    Image16: TImage;
    Rectangle11: TRectangle;
    Image17: TImage;
    Layout21: TLayout;
    Label4: TLabel;
    Image18: TImage;
    Rectangle12: TRectangle;
    Image19: TImage;
    Layout25: TLayout;
    Label5: TLabel;
    Image20: TImage;
    Rectangle13: TRectangle;
    Image21: TImage;
    Layout27: TLayout;
    Label6: TLabel;
    Image22: TImage;
    Rectangle14: TRectangle;
    Image23: TImage;
    Layout28: TLayout;
    Label7: TLabel;
    Image24: TImage;
    Rectangle16: TRectangle;
    Image27: TImage;
    Layout30: TLayout;
    Label9: TLabel;
    Image28: TImage;
    Rectangle17: TRectangle;
    Image29: TImage;
    Layout31: TLayout;
    Label10: TLabel;
    Image30: TImage;
    Rectangle18: TRectangle;
    Image31: TImage;
    Layout32: TLayout;
    Label11: TLabel;
    Image32: TImage;
    procedure RctCheckoutClick(Sender: TObject);
    procedure RctNewOrderClick(Sender: TObject);
    procedure RctCancelClick(Sender: TObject);
    procedure RctDrinksClick(Sender: TObject);
    procedure Image3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmMain: TFrmMain;

implementation

{$R *.fmx}

procedure TFrmMain.Image3Click(Sender: TObject);
begin
  ShowMessage('Show Popular Items');
end;

procedure TFrmMain.RctCancelClick(Sender: TObject);
begin
  ShowMessage('Cancel Items');
end;

procedure TFrmMain.RctCheckoutClick(Sender: TObject);
begin
  ShowMessage('Checkout');
end;

procedure TFrmMain.RctDrinksClick(Sender: TObject);
begin
  ShowMessage('More Details');
end;

procedure TFrmMain.RctNewOrderClick(Sender: TObject);
begin
  ShowMessage('New Order');
end;

end.
