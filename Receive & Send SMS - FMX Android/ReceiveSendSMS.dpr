program ReceiveSendSMS;

uses
  System.StartUpCopy,
  FMX.Forms,
  uReceiveSendSMSFMXAndroid in 'uReceiveSendSMSFMXAndroid.pas' {FormMain},
  SMSBroadcast in 'SMSBroadcast.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
