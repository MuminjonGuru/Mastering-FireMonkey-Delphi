//---------------------------------------------------------------------------

// This software is Copyright (c) 2021 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit View.Home.DataRect;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Layouts, FMX.Objects, Model.Utils;

type
  TDataRectFrame = class(TFrame)
    BgRect: TRectangle;
    ContactsGridPanel: TGridPanelLayout;
    DataCaptionLbl: TLabel;
    DataValueLbl: TLabel;
    procedure DataCaptionLblApplyStyleLookup(Sender: TObject);
  private
    function GetDataCaption: string;
    function GetDataValue: string;
    procedure SetDataCaption(const ACaption: string);
    procedure SetDataValue(const AValue: string);
  public
    property DataCaption: string read GetDataCaption write SetDataCaption;
    property DataValue: string read GetDataValue write SetDataValue;
    procedure DisableEvents;
  end;

implementation

{$R *.fmx}

{ TDataRectFrame }

procedure TDataRectFrame.DataCaptionLblApplyStyleLookup(Sender: TObject);
var
  LLabel: TLabel absolute Sender;
begin
  BgRect.Stroke.Color := LLabel.GetStyledColor;
end;

procedure TDataRectFrame.DisableEvents;
begin
  DataCaptionLbl.OnClick := nil;
  DataValueLbl.OnClick := nil;
end;

function TDataRectFrame.GetDataCaption: string;
begin
  Result := DataCaptionLbl.Text
end;

function TDataRectFrame.GetDataValue: string;
begin
  Result := DataValueLbl.Text
end;

procedure TDataRectFrame.SetDataCaption(const ACaption: string);
begin
  DataCaptionLbl.Text := ACaption
end;

procedure TDataRectFrame.SetDataValue(const AValue: string);
begin
  DataValueLbl.Text := AValue;
end;

end.
