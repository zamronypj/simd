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

     {-------------------------------------
      Dot product of two vectors
      -------------------------------------
      result = vect1.x * vect2.x +
               vect1.y * vect2.y +
               vect1.z * vect2.z +
               vect1.w * vect2.w
     --------------------------------------}
     function dot(const vect1:TVector; const vect2:TVector) : single;

     {-------------------------------------
      Cross product of two vectors
      -------------------------------------
      result.x = vect1.y * vect2.z - vect1.z * vect2.y
      result.y = vect1.z * vect2.x - vect1.x * vect2.z
      result.z = vect1.x * vect2.y - vect1.y * vect2.x
      result.w = 0
     --------------------------------------}
     function cross(const vect1:TVector; const vect2:TVector) : TVector;

     {-------------------------------------
      Length of a vector
      -------------------------------------
      result = sqrt(vect1.x * vect1.x +
                    vect1.y * vect1.y +
                    vect1.z * vect1.z +
                    vect1.w * vect1.w)
     --------------------------------------}
     function length(const vect1:TVector) : single;

     {-------------------------------------
      Normalize vector to unit vector
      -------------------------------------
      result.x = vect1.x / len
      result.y = vect1.y / len
      result.z = vect1.z / len
      result.w = vect1.w / len
      where len = length of vector vect1
     --------------------------------------}
     function normalize(const vect1:TVector) : TVector;

     {-------------------------------------
      Distance between two vectors
      -------------------------------------
      dist.x = vect1.x - vect2.x
      dist.y = vect1.y - vect2.y
      dist.z = vect1.z - vect2.z
      dist.w = vect1.w - vect2.w
      result = length(dist)
     --------------------------------------}
     function distance(const vect1:TVector; const vect2:TVector) : single;
  end;

implementation

end.

