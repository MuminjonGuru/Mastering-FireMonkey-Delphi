unit SMSBroadcast;

interface

uses
{$IFDEF ANDROID}
  Androidapi.JNIBridge,
  Androidapi.JNI.Embarcadero,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.Helpers,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.App,
{$ENDIF}
  System.SysUtils,
  System.Classes;

type

{$IFNDEF ANDROID}
  JIntent = class
  end;

  JContext = class
  end;
{$ENDIF}

  TDelphiUzBroadcastReceiver = class;
  TOnReceive = procedure(csContext: JContext; csIntent: JIntent) of object;

{$IFDEF ANDROID}

  TDelphiUzListener = class(TJavaLocal, JFMXBroadcastReceiverListener)
  private
    FOwner: TDelphiUzBroadcastReceiver;
  public
    constructor Create(AOwner: TDelphiUzBroadcastReceiver);
    procedure OnReceive(csContext: JContext; csIntent: JIntent); cdecl;
  end;
{$ENDIF}

  TDelphiUzBroadcastReceiver = class(TComponent)
  private
{$IFDEF ANDROID}
    FReceiver: JBroadcastReceiver;
    FListener: TDelphiUzListener;
{$ENDIF}
    FOnReceive: TOnReceive;
    FItems: TStringList;
    function GetItem(const csIndex: Integer): String;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SendBroadcast(csValue: String);
    procedure Add(csValue: String);
    procedure Delete(csIndex: Integer);
    procedure Clear;
{$IFDEF ANDROID}
    procedure SetResultData(Data: JString);
{$ENDIF}
    function Remove(const csValue: String): Integer;
    function First: String;
    function Last: String;
    function HasPermission(const csPermission: string): Boolean;
    procedure RegisterReceive;
    property Item[const csIndex: Integer]: string read GetItem; default;
    property Items: TStringList read FItems write FItems;
  published
    property OnReceive: TOnReceive read FOnReceive write FOnReceive;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Delphi.Uz', [TDelphiUzBroadcastReceiver]);
end;

{ TCSBroadcastReceiver }

{$IFDEF ANDROID}

procedure TDelphiUzBroadcastReceiver.SetResultData(Data: JString);
begin
  FReceiver.SetResultData(Data);
end;
{$ENDIF}

procedure TDelphiUzBroadcastReceiver.Add(csValue: String);
{$IFDEF ANDROID}
var
  Filter: JIntentFilter;
{$ENDIF}
begin
{$IFDEF ANDROID}
  if (FListener = nil) or (FReceiver = nil) then
  begin
    Raise Exception.Create('Utilize RegisterReceive!');
    Exit;
  end;
{$ENDIF}
  if FItems <> nil then
    if FItems.IndexOf(csValue) = -1 then
    begin
{$IFDEF ANDROID}
      Filter := TJIntentFilter.Create;
      Filter.addAction(StringToJString(csValue));
      TAndroidHelper.Context.registerReceiver(FReceiver, Filter);
{$ENDIF}
      FItems.Add(csValue);
    end;
end;

procedure TDelphiUzBroadcastReceiver.Clear;
begin
  FItems.Clear;
end;

constructor TDelphiUzBroadcastReceiver.Create(AOwner: TComponent);
begin
  inherited;
  FItems := TStringList.Create;
end;

destructor TDelphiUzBroadcastReceiver.Destroy;
begin
  FItems.Free;
{$IFDEF ANDROID}
  if FReceiver <> nil then
    TAndroidHelper.Activity.UnregisterReceiver(FReceiver);
{$ENDIF}
  inherited;
end;

procedure TDelphiUzBroadcastReceiver.Delete(csIndex: Integer);
begin
  if FItems <> nil then
  begin
    FItems.Delete(csIndex);
{$IFDEF ANDROID}
    TAndroidHelper.Activity.UnregisterReceiver(FReceiver);
    RegisterReceive;
{$ENDIF}
  end;
end;

function TDelphiUzBroadcastReceiver.GetItem(const csIndex: Integer): String;
begin
  Result := FItems[csIndex];
end;

function TDelphiUzBroadcastReceiver.First: String;
begin
  Result := FItems[0];
end;

function TDelphiUzBroadcastReceiver.HasPermission(const csPermission
  : string): Boolean;
{$IFDEF ANDROID}
begin
  Result := TAndroidHelper.Activity.checkCallingOrSelfPermission
    (StringToJString(csPermission)) = TJPackageManager.JavaClass.
    PERMISSION_GRANTED;
{$ELSE}
begin
  Result := False;
{$ENDIF}
end;

function TDelphiUzBroadcastReceiver.Last: String;
begin
  Result := FItems[FItems.Count];
end;

procedure TDelphiUzBroadcastReceiver.RegisterReceive;
{$IFDEF ANDROID}
begin
  if FListener = nil then
    FListener := TDelphiUzListener.Create(Self);
  if FReceiver = nil then
    FReceiver := TJFMXBroadcastReceiver.JavaClass.init(FListener);
  if FItems <> nil then
    if FItems.Count > 0 then
      for var I: Integer := 0 to FItems.Count - 1 do
        Add(FItems[I]);
{$ELSE}
begin
{$ENDIF}
end;

procedure TDelphiUzBroadcastReceiver.SendBroadcast(csValue: String);
{$IFDEF ANDROID}
begin
  var Inx := TJIntent.Create;
  Inx.setAction(StringToJString(csValue));
  TAndroidHelper.Context.SendBroadcast(Inx);
{$ELSE}
begin
{$ENDIF}
end;

function TDelphiUzBroadcastReceiver.Remove(const csValue: String): Integer;
begin
  Result := FItems.IndexOf(csValue);
  if Result > -1 then
    FItems.Delete(Result);
end;

{$IFDEF ANDROID}

procedure TDelphiUzListener.OnReceive(csContext: JContext; csIntent: JIntent);
begin
  if Assigned(FOwner.OnReceive) then
    FOwner.OnReceive(csContext, csIntent);
end;

constructor TDelphiUzListener.Create(AOwner: TDelphiUzBroadcastReceiver);
begin
  inherited Create;
  FOwner := AOwner;
end;
{$ENDIF}

end.
