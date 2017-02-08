unit vectorOperation;

{$mode objfpc}{$H+}

interface

uses
   vectorType;

type
  IVectorOperation = interface
     {-------------------------------------
      Add two vectors
      -------------------------------------
      result.x = vect1.x + vect2.x
      result.y = vect1.y + vect2.y
      result.z = vect1.z + vect2.z
      result.w = vect1.w + vect2.w
     --------------------------------------}
     function add(const vect1:TVector; const vect2:TVector) : TVector;

     {-------------------------------------
      Subtract two vectors
      -------------------------------------
      result.x = vect1.x - vect2.x
      result.y = vect1.y - vect2.y
      result.z = vect1.z - vect2.z
      result.w = vect1.w - vect2.w
     --------------------------------------}
     function sub(const vect1:TVector; const vect2:TVector) : TVector;

     {-------------------------------------
      Multiply a vector with a scalar
      -------------------------------------
      result.x = vect1.x + scalar
      result.y = vect1.y + scalar
      result.z = vect1.z + scalar
      result.w = vect1.w + scalar
     --------------------------------------}
     function mulScalar(const vect1:TVector; const scalar:single) : TVector;

     function dot(const vect1:TVector; const vect2:TVector) : single;
  end;

implementation

end.

