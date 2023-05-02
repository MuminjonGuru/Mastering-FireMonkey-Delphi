procedure TFormRegistration.CopyBtn(Sender: TObject);
var
  MyClipboard: IFMXClipboardService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService) then
  begin
    MyClipboard := IFMXClipboardService(TPlatformServices.Current.GetPlatformService(IFMXClipboardService));
    MyClipboard.SetClipboard(MyLabelTop.Text);
  end;
end;
