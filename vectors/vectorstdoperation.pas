{-----------------------------------
 Vector operation with basic
 Pascal operator implementation
-------------------------------------
(c) 2017 Zamrony P. Juhara <zamronypj@yahoo.com>
http://github.com/zamronypj/simd
-------------------------------------
Tested on Ubuntu 14.04 64 bit
Intel core i7
-------------------------------------}
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
     function sub(const vect1:TVector; const vect2:TVector) : TVector; override;
     function mulScalar(const vect1:TVector; const scalar:single) : TVector; override;
     function dot(const vect1:TVector; const vect2:TVector) : single; override;
   end;

implementation

{ TStdVectorOperation }

{-------------------------------------
 Add two vectors
--------------------------------------}
function TStdVectorOperation.add(const vect1: TVector; const vect2: TVector): TVector;
begin
  result.x := vect1.x + vect2.x;
  result.y := vect1.y + vect2.y;
  result.z := vect1.z + vect2.z;
  result.w := vect1.w + vect2.w;
end;

{-------------------------------------
 Subtract two vectors
--------------------------------------}
function TStdVectorOperation.sub(const vect1: TVector; const vect2: TVector): TVector;
begin
  result.x := vect1.x - vect2.x;
  result.y := vect1.y - vect2.y;
  result.z := vect1.z - vect2.z;
  result.w := vect1.w - vect2.w;
end;

{-------------------------------------
 Multiply a vectors with scalar
--------------------------------------}
function TStdVectorOperation.mulScalar(const vect1: TVector; const scalar: single): TVector;
begin
  result.x := vect1.x * scalar;
  result.y := vect1.y * scalar;
  result.z := vect1.z * scalar;
  result.w := vect1.w * scalar;
end;

{-------------------------------------
 Dot product of two vectors
--------------------------------------}
function TStdVectorOperation.dot(const vect1: TVector; const vect2: TVector): single;
begin
  result := vect1.x * vect2.x +
            vect1.y * vect2.y +
            vect1.z * vect2.z +
            vect1.w * vect2.w;
end;

end.

