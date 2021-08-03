//---------------------------------------------------------------------------

// This software is Copyright (c) 2021 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit View.NewForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  View.Main, FMX.Effects, FMX.Layouts, FMX.Ani, FMX.Objects,
  FMX.Controls.Presentation, REST.Types, REST.Client, Data.Bind.Components,
  Data.Bind.ObjectScope, System.JSON, System.Generics.Collections,
  System.Net.URLClient, System.Net.HttpClient, System.Net.HttpClientComponent;

type
  TNewFormFrame = class(TMainFrame)
    vrtscrlbx1: TVertScrollBox;
    BtnRefresh: TButton;
    RESTClient1: TRESTClient;
    RESTRequest1: TRESTRequest;
    RESTResponse1: TRESTResponse;
    procedure BtnRefreshClick(Sender: TObject);
  private
    // runtime components for news card
    RctNewsCard: TRectangle;
    NewsImage: TImage;
    LabelTitle: TLabel;
    LabelDescription: TLabel;

    // save runtime components to the list
    RctList: TList<TRectangle>;
    ImgList: TList<TImage>;
    LblTitleList: TList<TLabel>;
    LblDescpList: TList<TLabel>;
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

procedure TNewFormFrame.BtnRefreshClick(Sender: TObject);
begin
  inherited;
  RESTRequest1.Execute;  // send request to endpoint

  var JSONValue: TJSONValue;
  var JSONArray: TJSONArray;
  var ArrayElement: TJSONValue;

  // after using object we just free them within the Lists
  RctList := TList<TRectangle>.Create;
  ImgList := TList<TImage>.Create;
  LblTitleList := TList<TLabel>.Create;
  LblDescpList := TList<TLabel>.Create;

  try
    JSONValue := TJSONObject.ParseJSONValue(RESTResponse1.Content);
    JSONArray := JSONValue.GetValue<TJSONArray>('data');  // articles are stored in the data array in the JSON response

    for ArrayElement in JSONArray do
    begin

      {$region 'Create news card' }
      RctNewsCard := TRectangle.Create(vrtscrlbx1);
      RctNewsCard.Parent := vrtscrlbx1;
      RctNewscard.HitTest := False;
      RctNewsCard.Fill.Color := TAlphaColorRec.Ghostwhite;
      RctNewsCard.Fill.Kind  := TBrushKind.Solid;
      RctNewsCard.Stroke.Thickness := 0;
      RctNewsCard.Align := TAlignLayout.Top;
      RctNewsCard.Height := 400;
      RctNewsCard.Width  := 389;
      RctNewsCard.XRadius := 15;
      RctNewsCard.YRadius := 15;
      RctNewsCard.Margins.Top := 5;
      RctNewsCard.Margins.Bottom := 5;
      RctNewsCard.Margins.Left := 5;
      RctNewsCard.Margins.Right := 5;
      RctList.Add(RctNewsCard);  // add to the TList instance
      {$endregion}

      {$region 'create image and load image from the url' }
      NewsImage := TImage.Create(RctNewsCard);
      NewsImage.Parent := RctNewsCard;
      NewsImage.HitTest := False;
      NewsImage.Align := TAlignLayout.Top;
      NewsImage.Height := 225;
      NewsImage.Width  := 389;
      NewsImage.Margins.Top := 5;
      NewsImage.Margins.Left := 15;
      NewsImage.Margins.Right := 15;
      NewsImage.Margins.Bottom := 5;
      NewsImage.HitTest := False;
      NewsImage.MarginWrapMode := TImageWrapMode.Stretch;
      NewsImage.WrapMode := TImageWrapMode.Fit;
      ImgList.Add(NewsImage);

      // load images to the newly created TImage component
      var MemoryStream := TMemoryStream.Create;
      var HTTPClient   := TNetHTTPClient.Create(nil);
      var HTTPRequest  := TNetHTTPRequest.Create(nil);
      HTTPRequest.Client := HTTPClient;
      try
        var ImageURL := ArrayElement.GetValue<String>('image');
        HTTPRequest.Get(ImageURL, MemoryStream);
        MemoryStream.Seek(0, soFromBeginning);
        NewsImage.Bitmap.LoadFromStream(MemoryStream);
      finally
        FreeAndNil(MemoryStream);
        FreeAndNil(HTTPClient);
        FreeAndNil(HTTPRequest);
      end;
      {$endregion}

      {$region 'create title and summary texts in the News Card' }
      LabelTitle := TLabel.Create(RctNewsCard);
      LabelTitle.Parent := RctNewsCard;
      LabelTitle.Align := TAlignLayout.Top;
      LabelTitle.Height := 27;
      LabelTitle.Width  := 359;
      LabelTitle.HitTest := False;
      LabelTitle.AutoSize := True;
      LabelTitle.Font.Size := 22;
      LabelTitle.Margins.Left := 15;
      LabelTitle.Margins.Right := 15;
      LabelTitle.Margins.Top := 5;
      LabelTitle.Margins.Bottom := 5;
      LabelTitle.Text := 'Title: ' + ArrayElement.GetValue<String>('title');
      LblTitleList.Add(LabelTitle);

      LabelDescription := TLabel.Create(RctNewsCard);
      LabelDescription.Parent := RctNewsCard;
      LabelDescription.Align := TAlignLayout.Client;
      LabelDescription.Height := 131;
      LabelDescription.Width  := 359;
      LabelDescription.HitTest := False;
      LabelDescription.AutoSize := True;
      LabelDescription.Font.Size := 15;
      LabelDescription.Margins.Left := 15;
      LabelDescription.Margins.Right := 15;
      LabelDescription.Margins.Top := 5;
      LabelDescription.Margins.Bottom := 5;
      LabelDescription.Text := 'Summary: ' + ArrayElement.GetValue<String>('description');
      LblDescpList.Add(LabelDescription);
      {$endregion}
    end;
  finally
    RctList.Free;
    ImgList.Free;
    LblTitleList.Free;
    LblDescpList.Free;
  end;
end;

initialization
  // Register frame
  RegisterClass(TNewFormFrame);
finalization
  // Unregister frame
  UnRegisterClass(TNewFormFrame);

end.
