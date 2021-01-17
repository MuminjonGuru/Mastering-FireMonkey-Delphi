unit uReceiveSendSMSFMXAndroid;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Objects,
  FMX.Layouts,
  FMX.ListBox,
{$IFDEF ANDROID}
  Androidapi.Helpers,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.Os,
  Androidapi.JNIBridge,
  Androidapi.JNI.App,
  Androidapi.JNI.Embarcadero,
  Androidapi.JNI.Net,
  Androidapi.JNI.Util,
  Androidapi.JNI.Provider,
  Androidapi.JNI.Telephony,
{$ENDIF}
  SMSBroadcast,
  System.Permissions,
  FMX.DialogService,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Memo.Types,
  FMX.ScrollBox,
  FMX.Memo,
  FMX.Edit;

type
  TFormMain = class(TForm)
    RctClient: TRectangle;
    RctToolbar: TRoundRect;
    LBLogs: TListBox;
    BtnFetchSMS: TButton;
    GLBottom: TGridLayout;
    BtnSendSMS: TButton;
    EdtPhoneNumber: TEdit;
    MemoSMSBody: TMemo;
    StyleBook1: TStyleBook;
    Label1: TLabel;
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BtnSendSMSClick(Sender: TObject);
    procedure BtnFetchSMSClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
{$REGION 'Permissions'}
    FPermission_SEND, FPermission_READ, FPermission_RECEIVE: string;
    procedure RequestResult(Sender: TObject; const APermissions: TArray<string>;
      const AGrantResults: TArray<TPermissionStatus>);
    procedure DisplayRationale(Sender: TObject;
      const APermissions: TArray<string>; const APostRationaleProc: TProc);
    procedure RequestPermissions;
{$ENDREGION}
    procedure CreateBroadcastReceiver;
    procedure BroadcastReceiverOnReceive(Context: JContext; Intent: JIntent);
    procedure CheckIncomingSMS(Context: JContext; Intent: JIntent);

    // Used to calculate time when fetching the SMS
    function UnixToDateTime(USec: Longint): TDateTime;
    procedure AddToLog(Line: string);
{$REGION 'Fetch/Send SMS and Check Winner'}
    procedure FetchSMS;
    function SendSMS(SMSTo: String; SMSBody: String): Boolean;
{$ENDREGION}
  public
    BroadcastReceiver: TDelphiUzBroadcastReceiver;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

procedure TFormMain.RequestPermissions;
begin
{$IFDEF ANDROID}
  FPermission_SEND := JStringToString(TJManifest_permission.JavaClass.SEND_SMS);
  FPermission_READ := JStringToString(TJManifest_permission.JavaClass.READ_SMS);
  FPermission_RECEIVE := JStringToString(TJManifest_permission.JavaClass.RECEIVE_SMS);
  PermissionsService.RequestPermissions([FPermission_SEND, FPermission_READ,
    FPermission_RECEIVE], RequestResult, DisplayRationale)
{$ENDIF}
end;

procedure TFormMain.RequestResult(Sender: TObject;
  const APermissions: TArray<string>;
  const AGrantResults: TArray<TPermissionStatus>);
begin
{$IFDEF ANDROID}
  if (AGrantResults[0] = TPermissionStatus.Granted) and
    (AGrantResults[1] = TPermissionStatus.Granted) and
    (AGrantResults[2] = TPermissionStatus.Granted) then
  begin
  end
{$ENDIF}
end;

function TFormMain.SendSMS(SMSTo: String; SMSBody: String): Boolean;
begin
{$IFDEF ANDROID}
  AddToLog('Send SMS To ' + SMSTo);
  try
    var
    SmsManager := TJSmsManager.JavaClass.getDefault;
    SmsManager.sendTextMessage(StringToJstring(SMSTo), nil,
      StringToJstring(SMSBody), nil, nil);
    Result := True;
  except
    AddToLog('Sending Error');
    Result := False;
  end;
{$ENDIF}
end;

procedure TFormMain.DisplayRationale(Sender: TObject;
  const APermissions: TArray<string>; const APostRationaleProc: TProc);
var
  I: Integer;
  RationaleMsg: string;
begin
  for I := 0 to High(APermissions) do
  begin
    if APermissions[I] = FPermission_SEND then
      RationaleMsg := RationaleMsg + 'The app needs to SEND SMS' + SLineBreak +
        SLineBreak;
    if APermissions[I] = FPermission_READ then
      RationaleMsg := RationaleMsg + 'The app needs to READ SMS' + SLineBreak +
        SLineBreak;
    if APermissions[I] = FPermission_RECEIVE then
      RationaleMsg := RationaleMsg + 'The app needs to RECEIVE SMS' + SLineBreak
        + SLineBreak;
  end;

  TDialogService.ShowMessage(RationaleMsg,
    procedure(const AResult: TModalResult)
    begin
      APostRationaleProc;
    end)
end;

procedure TFormMain.AddToLog(Line: string);
begin
  LBLogs.Items.add(Line);
  LBLogs.ItemIndex := LBLogs.Items.Count - 1;
end;

procedure TFormMain.BroadcastReceiverOnReceive(Context: JContext;
Intent: JIntent);
begin
  CheckIncomingSMS(Context, Intent);
end;

procedure TFormMain.BtnFetchSMSClick(Sender: TObject);
begin
  FetchSMS;
end;

procedure TFormMain.BtnSendSMSClick(Sender: TObject);
begin
  if Length(EdtPhoneNumber.Text) < 1 then
  begin
    AddToLog('No phone number');
    Exit;
  end;

  SendSMS(EdtPhoneNumber.Text, MemoSMSBody.Text);
end;

procedure TFormMain.CheckIncomingSMS(Context: JContext; Intent: JIntent);
var
{$IFDEF ANDROID}
  aSmss: TJavaObjectArray<JSmsMessage>;
  aSms: JSmsMessage;
{$ENDIF}
  aFrom: string;
  aBody: string;
  I: Integer;
begin
  try
    AddToLog('Incoming SMS...');
    try
      if (Intent <> nil) and (Intent.getAction <> nil) and
        (Intent.getAction.compareToIgnoreCase
        (StringToJstring('android.provider.Telephony.SMS_RECEIVED')) = 0) then
      begin
        AddToLog('SMS Received');
        aSmss := TJavaObjectArray<JSmsMessage>.Create;
        aSmss := TJSms_Intents.JavaClass.getMessagesFromIntent(Intent);
        aFrom := JStringToString(aSmss[0].getDisplayOriginatingAddress);
        aBody := '';
        for I := 0 to aSmss.Length - 1 do
        begin
          aSms := aSmss[I];
          aBody := aBody + JStringToString(aSms.getDisplayMessageBody);
        end;
        AddToLog('SMS from: ' + aFrom);
        AddToLog('SMS body: ' + aBody);
      end;
    except
      on E: Exception do
        ShowMessage(E.Message + ' : ' + E.ClassName);
    end;
  finally
    aSmss.Free;
  end;
end;

procedure TFormMain.CreateBroadcastReceiver;
begin
  if not Assigned(BroadcastReceiver) then
  begin
    BroadcastReceiver := TDelphiUzBroadcastReceiver.Create(nil);
    BroadcastReceiver.OnReceive := BroadcastReceiverOnReceive;
    BroadcastReceiver.RegisterReceive;
    BroadcastReceiver.add('android.provider.Telephony.SMS_RECEIVED');
  end;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  LBLogs.Items.Clear;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  if Assigned(BroadcastReceiver) then
    BroadcastReceiver.Free;
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  RequestPermissions;
  CreateBroadcastReceiver;
end;

function TFormMain.UnixToDateTime(USec: Longint): TDateTime;
begin
  Result := (USec / 86400) + UnixDateDelta;
end;

procedure TFormMain.FetchSMS;
var
{$IFDEF ANDROID}
  Cursor: JCursor;
  uri: Jnet_Uri;
{$ENDIF}
  IdSmsId, IdSmsSender: Integer;
  IdSmsBody: Integer;
  IdSmsDate: Integer;
  SmsId: string;
  SMSSender: string;
  SMSBody: string;
  I: Integer;
  DateTime: TDateTime;
  msgunixtimestampms: Int64;
begin
{$IFDEF ANDROID}
  try
    uri := StrToJURI('content://sms/inbox');
    Cursor := TAndroidHelper.Activity.getContentResolver.query(uri, nil, nil,
      nil, nil);
    IdSmsId := Cursor.getColumnIndex(StringToJstring('_id'));
    IdSmsSender := Cursor.getColumnIndex(StringToJstring('address'));
    IdSmsBody := Cursor.getColumnIndex(StringToJstring('body'));
    IdSmsDate := Cursor.getColumnIndex(StringToJstring('date'));
    Cursor.moveToFirst;
    I := Cursor.getCount;
    if I > 15 then // get 15 smsS
      I := 15;
    AddToLog('Read SMS ' + I.ToString + '/' + Cursor.getCount.ToString);

    while I > 1 do
    begin
      SmsId := JStringToString(Cursor.getString(IdSmsId));
      SMSSender := JStringToString(Cursor.getString(IdSmsSender));
      SMSBody := JStringToString(Cursor.getString(IdSmsBody));
      msgunixtimestampms := Cursor.getLong(IdSmsDate);

      // calculate time
      DateTime := UnixToDateTime(msgunixtimestampms div 1000);
      AddToLog('SMS #' + SmsId + ' date:' + DateToStr(DateTime) + ' from:' +
        SMSSender);
      AddToLog(SMSBody);
      Dec(I);
      Cursor.moveToNext;
    end;
  except
    AddToLog('SMS Reading Is Not Allowed');
  end;
{$ENDIF}
end;

end.
