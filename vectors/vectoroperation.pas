unit vectorOperation;

{$mode objfpc}{$H+}

interface

uses
   vectorType;

type
  IVectorOperation = interface
     function add(const vect1:TVector; const vect2:TVector) : TVector;
     function sub(const vect1:TVector; const vect2:TVector) : TVector;
     function mulScalar(const vect1:TVector; const scalar:single) : TVector;
  end;

implementation

end.

