{-----------------------------------
 Vector operation base class
-------------------------------------
(c) 2017 Zamrony P. Juhara <zamronypj@yahoo.com>
http://github.com/zamronypj/simd
-------------------------------------
Tested on Ubuntu 14.04 64 bit
Intel core i7
-------------------------------------}
unit vectorbaseoperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, vectorType, vectorOperation;

type
   TBaseVectorOperation = class(TInterfacedObject, IVectorOperation)
   public
     function add(const vect1:TVector; const vect2:TVector) : TVector; virtual; abstract;
     function sub(const vect1:TVector; const vect2:TVector) : TVector; virtual; abstract;
     function mulScalar(const vect1:TVector; const scalar:single) : TVector; virtual; abstract;
     function dot(const vect1:TVector; const vect2:TVector) : single; virtual; abstract;
   end;

implementation

end.

