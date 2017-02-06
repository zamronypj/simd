unit vectorbaseoperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, vectorType, vectorOperation;

type
   TBaseVectorOperation = class(TInterfacedObject, IVectorOperation)
   public
     function add(const vect1:TVector; const vect2:TVector) : TVector; virtual; abstract;
     function mulScalar(const vect1:TVector; const scalar:single) : TVector; virtual; abstract;
   end;

implementation

end.

