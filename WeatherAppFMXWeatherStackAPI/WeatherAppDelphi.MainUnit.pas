unit WeatherAppDelphi.MainUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Edit, FMX.MultiView, FMX.Layouts,
  FMX.Effects, FMX.Filter.Effects, FMX.ListBox, REST.Types, REST.Client,
  Data.Bind.Components, Data.Bind.ObjectScope, System.JSON,
  System.Generics.Collections, System.Net.URLClient, System.Net.HttpClient,
  System.Net.HttpClientComponent;

type
  TFormMain = class(TForm)
    BtnReFresh: TButton;
    BtnSearch: TButton;
    MultiView1: TMultiView;
    EdtCityName: TEdit;
    BtnRequest: TButton;
    RctBackground: TRectangle;
    LytMain: TLayout;
    GaussianBlurEffect1: TGaussianBlurEffect;
    LytBackgroundPic: TLayout;
    RctData: TRectangle;
    Layout1: TLayout;
    ToolBar1: TToolBar;
    Layout2: TLayout;
    LblTemp: TLabel;
    Layout3: TLayout;
    LblCity: TLabel;
    RctDataLayer: TRectangle;
    ListBoxObservationList: TListBox;
    LstBoxItemHumidity: TListBoxItem;
    LstBoxItemFeelsLike: TListBoxItem;
    LstBoxItemCloudCover: TListBoxItem;
    LstBoxItemIsDay: TListBoxItem;
    GridPanelLayout1: TGridPanelLayout;
    LblType: TLabel;
    ImgWeatherIcon: TImage;
    RESTClient1: TRESTClient;
    RESTRequest1: TRESTRequest;
    RESTResponse1: TRESTResponse;
    StyleBook2: TStyleBook;
    procedure BtnRequestClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

procedure TFormMain.BtnRequestClick(Sender: TObject);
begin
  MultiView1.HideMaster;

  RESTClient1.ResetToDefaults;
  RESTClient1.Accept := 'application/json';
  RESTClient1.AcceptCharset := 'UTF-8, *;q=0.8';
  RESTClient1.BaseURL := 'http://api.weatherstack.com/current';

  // give city name in the URL query
  RESTRequest1.Resource :=
    Format('?access_key=1ec3dade5a2d89dc10a6aecd5b84d0b5&query=%s',
    [EdtCityName.Text]);

  RESTResponse1.ContentType := 'application/json';

  // request
  RESTRequest1.Execute;

  // parse json and open the current object
  var
  JSONObject := TJSONObject.ParseJSONValue(RESTResponse1.Content)
    as TJSONObject;

  var JSONValue := JSONObject.Get('current').JSONValue;
  try
    LblTemp.Text := JSONValue.GetValue<String>('temperature') + 'º';
    LblCity.Text := EdtCityName.Text;

    // get the first element from weather description array
    var
    JSONArray := JSONValue.GetValue<TJSONArray>('weather_descriptions');
    LblType.Text := JSONArray.Items[0].Value;

    // download weather icon and set to TImage component
    var MemoryStream := TMemoryStream.Create;
    var HttpClient := TNetHTTPClient.Create(nil);
    var HTTPReq := TNetHTTPRequest.Create(nil);
    HTTPReq.Client := HttpClient;
    try
      // get weather icon URL from weather_icons array
      JSONArray := JSONValue.GetValue<TJSONArray>('weather_icons');
      var ImgURL := JSONArray.Items[0];

      // download image
      HTTPReq.Get(ImgURL.Value, MemoryStream);
      MemoryStream.Seek(0, soFromBeginning);

      // load streamed data to TImage
      ImgWeatherIcon.Bitmap.LoadFromStream(MemoryStream);
    finally
      FreeAndNil(MemoryStream);
      FreeAndNil(HttpClient);
      FreeAndNil(HTTPReq);
    end;

    // little more details on the weather
    LstBoxItemHumidity.Text := 'Humidity:               ' +
      JSONValue.GetValue<String>('humidity');
    LstBoxItemFeelsLike.Text := 'Feels like:               ' +
      JSONValue.GetValue<String>('feelslike') + 'º';
    LstBoxItemCloudCover.Text := 'Cloud cover:           ' +
      JSONValue.GetValue<String>('cloudcover');
    LstBoxItemIsDay.Text := 'Is day:                    ' +
      JSONValue.GetValue<String>('is_day');
  finally
    JSONValue.Free;
  end;
end;

end.
