unit vectorstdoperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, vectorType, vectorBaseOperation;

type

   { TStdVectorOperation }

   TStdVectorOperation = class(TBaseVectorOperation)
   public
     function add(const vect1:TVector; const vect2:TVector) : TVector; override;
     function mulScalar(const vect1:TVector; const scalar:single) : TVector; override;
   end;

implementation

{ TStdVectorOperation }

function TStdVectorOperation.add(const vect1: TVector; const vect2: TVector
  ): TVector;
begin
  result.x := vect1.x + vect2.x;
  result.y := vect1.y + vect2.y;
  result.z := vect1.z + vect2.z;
  result.w := vect1.w + vect2.w;
end;

function TStdVectorOperation.mulScalar(const vect1: TVector;
  const scalar: single): TVector;
begin
  result.x := vect1.x + scalar;
  result.y := vect1.y + scalar;
  result.z := vect1.z + scalar;
  result.w := vect1.w + scalar;
end;

end.

