//---------------------------------------------------------------------------

// This software is Copyright (c) 2021 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit ViewModel;

interface

uses Model, Model.Constants, Data.DB, Variants, Model.Utils;

type
  // Hi-level representation of DB data acceess
  TViewModel = class
  private
    FModel: TModel;
  public
    function GetTotalSumSales(ACustNo: Integer): Integer;
    function GetCustNoByUserName(AUsername: string): Integer;
    function GetAddressLine1(AUsername: string): string;
    destructor Destroy; override;
    constructor Create;
  end;

implementation

{ TViewModel }

constructor TViewModel.Create;
begin
  FModel := TModel.Create;
end;

destructor TViewModel.Destroy;
begin
  FModel.DisposeOf;
  inherited;
end;

function TViewModel.GetAddressLine1(AUsername: string): string;
begin
  var V: Variant := FModel.ExecSQLCommand(SQLCMD_GetAddressLine1, SQLCMD_GetAddressLine1_FIELDNAME, USER_NAME_PARAM, AUsername);
  if VarIsNull(V) then
    Result := sNotDefined
  else
    Result := V;
end;

function TViewModel.GetCustNoByUserName(AUsername: string): Integer;
begin
  var V: Variant := FModel.ExecSQLCommand(SQLCMD_GetCustNoByUserName, SQLCMD_GetCustNoByUserName_FIELDNAME, USER_NAME_PARAM, AUsername);
  if VarIsNull(V) then
    Result := -1
  else
    Result := V;
end;

function TViewModel.GetTotalSumSales(ACustNo: Integer): Integer;
begin
  var V: Variant := FModel.ExecSQLCommand(SQLCMD_GetTotalSumSales, SQLCMD_GetTotalSumSales_FIELDNAME, CUST_NO_PARAM, ACustNo);
  if VarIsNull(V) then
    Result := -1
  else
    Result := V;
end;

end.
