//---------------------------------------------------------------------------

// This software is Copyright (c) 2021 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit Model;

interface

uses
  System.Classes, System.Variants, FMX.Forms,
  Data.DB, FireDAC.Comp.Client, FireDAC.Stan.Param;

type
  // Implements basic routines to DB data access
  TModel = class(TObject)
  private
    FModelData: TDataModule;
  public
    constructor Create;
    destructor Destroy; override;
    function ExecSQLCommand(ASQLCommand, AFieldName, AParamName: string; AValue: Variant): Variant;
  end;

var
  ModelData: TDataModule;

implementation

uses
  Model.Constants, Model.Types, Model.Utils;

{ TModel }

constructor TModel.Create;
begin
  // Create Model.Data instance
  if IsClassPresent(sModelDataClassName) then
  begin
    var ModelDataClass: TPersistent := GetClass(sModelDataClassName).Create;
    FModelData := TDataModule(ModelDataClass).Create(Application);
    ModelData := FModelData;
  end;
end;

// Get data from DB.
destructor TModel.Destroy;
begin
  ModelData := nil;
  FModelData.Destroy;
  inherited;
end;

function TModel.ExecSQLCommand(ASQLCommand, AFieldName, AParamName: string;
  AValue: Variant): Variant;
begin
  Result := Null;
  if not Assigned(FModelData) then
    Exit;
  var Qry: TFDQuery := TFDQuery.Create(nil);
  try
    Qry.Connection := (FModelData as IModelData).GetFDConnection;
    Qry.Connection.Connected := True;
    Qry.SQL.Text := ASQLCommand;
    Qry.ParamByName(AParamName).AsString := AValue;
    Qry.Open;
    Result := Qry.FieldByName(AFieldName).AsVariant;
  finally
    Qry.Close;
    Qry.DisposeOf;
  end;
end;


end.
