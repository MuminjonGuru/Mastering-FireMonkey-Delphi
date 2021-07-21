// ---------------------------------------------------------------------------

// This software is Copyright (c) 2021 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

// ---------------------------------------------------------------------------

unit View.NewForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  View.Main, FMX.Effects, FMX.Layouts, FMX.Ani, FMX.Objects,
  FMX.Controls.Presentation, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMX.Edit,
  REST.Types, REST.Client, Data.Bind.Components, Data.Bind.ObjectScope,
  System.Rtti, System.Bindings.Outputs, FMX.Bind.Editors, Data.Bind.EngExt,
  FMX.Bind.DBEngExt, System.JSON;

type
  TNewFormFrame = class(TMainFrame)
    Layout1: TLayout;
    EditAccKey: TEdit;
    EditEmailID: TEdit;
    Memo1: TMemo;
    Panel1: TPanel;
    BtnSendRequest: TButton;
    BtnParseValues: TButton;
    RESTClient1: TRESTClient;
    RESTRequest1: TRESTRequest;
    RESTResponse1: TRESTResponse;
    BindingsList1: TBindingsList;
    LinkControlToField1: TLinkControlToField;
    procedure BtnSendRequestClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

procedure TNewFormFrame.BtnSendRequestClick(Sender: TObject);
begin
  inherited;
//  RESTRequest1.Execute;

  Memo1.Lines.Clear;

  RESTClient1.ResetToDefaults;
  RESTClient1.Accept := 'application/json';
  RESTClient1.AcceptCharset := 'UTF-8, *;q=0.8';
  RESTClient1.BaseURL := 'http://apilayer.net/api/check';

  // pass given parameters for the URL
  RESTRequest1.Resource := Format('?access_key=%s&email=%s&smtp=1&format=1',
    [EditAccKey.text, EditEmailID.text]);

  RESTResponse1.ContentType := 'application/json';

  RESTRequest1.Execute;  // send request to the endpoint

  // if parse btn clicked then parse values
  if Sender = BtnParseValues then
  begin
    var JSONValue := TJSONObject.ParseJSONValue(RESTResponse1.Content);
    try
      Memo1.Lines.Add('Parsed Values: ');

      if JSONValue is TJSONObject then
      begin
        Memo1.Lines.Add(JSONValue.GetValue<String>('user'));
        Memo1.Lines.Add(JSONValue.GetValue<String>('domain'));
        Memo1.Lines.Add(JSONValue.GetValue<String>('format_valid'));
        Memo1.Lines.Add(JSONValue.GetValue<String>('disposable'));
      end;
    finally
      JSONValue.Free;
    end;
  end;
end;

initialization

// Register frame
RegisterClass(TNewFormFrame);

finalization

// Unregister frame
UnRegisterClass(TNewFormFrame);

end.
