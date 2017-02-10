{-----------------------------------
 Vector operation helper
-------------------------------------
(c) 2017 Zamrony P. Juhara <zamronypj@yahoo.com>
http://github.com/zamronypj/simd
-------------------------------------
Tested on Ubuntu 14.04 64 bit
Intel core i7
-------------------------------------}
unit availVectorOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, vectorOperation;

type

   { TAvailableVectorOperations }

   TAvailableVectorOperations = class(TObject)
   private
       availOperations : array[0..3] of IVectorOperation;
       availOperationNames : TStringList;
       function getAvailableOperations(const indx:integer): IVectorOperation;
       function getOperationNames(const indx:integer) : string;
       function getTotalOperation() : integer;
   public
       constructor Create();
       destructor Destroy(); override;
       property availableOperations[indx : integer]:IVectorOperation read getAvailableOperations; default;
       property operationNames[indx : integer] : string read getOperationNames;
       property count : integer read getTotalOperation;
   end;

implementation
uses vectorStdOperation, vectorSSEOperation, vectorSSE3Operation, vectorSSE41Operation;

{ TAvailableVectorOperations }

function TAvailableVectorOperations.getAvailableOperations(const indx: integer): IVectorOperation;
begin
  result := availOperations[indx];
end;

function TAvailableVectorOperations.getOperationNames(const indx: integer): string;
begin
  result := availOperationNames[indx];
end;

function TAvailableVectorOperations.getTotalOperation() : integer;
begin
  result:= availOperationNames.Count;
end;

constructor TAvailableVectorOperations.Create();
begin
  availOperations[0] := TStdVectorOperation.Create();
  availOperations[1] := TSSEVectorOperation.Create();
  availOperations[2] := TSSE3VectorOperation.Create();
  availOperations[3] := TSSE41VectorOperation.Create();
  availOperationNames := TStringList.Create();
  availOperationNames.Add('Default');
  availOperationNames.Add('SSE');
  availOperationNames.Add('SSE 3');
  availOperationNames.Add('SSE 4.1');
end;

destructor TAvailableVectorOperations.Destroy();
begin
  availOperations[0] := nil;
  availOperations[1] := nil;
  availOperations[2] := nil;
  availOperations[3] := nil;
  availOperationNames.free();
  inherited Destroy;
end;

end.

