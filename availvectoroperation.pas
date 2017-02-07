unit availVectorOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, vectorOperation;

type

   { TAvailableVectorOperations }

   TAvailableVectorOperations = class(TObject)
   private
       availOperations : array[0..1] of IVectorOperation;
       function getAvailableOperations(const indx:integer): IVectorOperation;
   public
       constructor Create();
       destructor Destroy(); override;
       property availableOperations[indx : integer]:IVectorOperation read getAvailableOperations; default;
   end;

implementation
uses vectorStdOperation, vectorSSEOperation;

{ TAvailableVectorOperations }

function TAvailableVectorOperations.getAvailableOperations(const indx: integer): IVectorOperation;
begin
  result := availOperations[indx];
end;

constructor TAvailableVectorOperations.Create;
begin
  availOperations[0] := TStdVectorOperation.Create();
  availOperations[1] := TSSEVectorOperation.Create();
end;

destructor TAvailableVectorOperations.Destroy;
begin
  availOperations[0] := nil;
  availOperations[1] := nil;
  inherited Destroy;
end;

end.

