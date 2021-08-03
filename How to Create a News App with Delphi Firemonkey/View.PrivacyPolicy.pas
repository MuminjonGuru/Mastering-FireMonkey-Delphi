//---------------------------------------------------------------------------

// This software is Copyright (c) 2021 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit View.PrivacyPolicy;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  View.Placeholder, FMX.Objects, FMX.Layouts, FMX.Ani, FMX.Controls.Presentation,
  FMX.Effects;

type
  TPrivacyPolicyFrame = class(TPlaceholderFrame)
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

initialization
  // Register frame
  RegisterClass(TPrivacyPolicyFrame);
finalization
  // Unregister frame
  UnRegisterClass(TPrivacyPolicyFrame);

end.
