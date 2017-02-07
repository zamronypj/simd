unit vectorsseoperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, vectorType, vectorBaseOperation;

type

  { TSSEVectorOperation }

  TSSEVectorOperation = class(TBaseVectorOperation)
  public
    function add(const vect1:TVector; const vect2:TVector) : TVector; override;
    function mulScalar(const vect1:TVector; const scalar:single) : TVector; override;
  end;

implementation

{$asmmode intel}

{ TSSEVectorOperation }

function TSSEVectorOperation.add(const vect1: TVector; const vect2: TVector): TVector; assembler;
asm
    addps xmm1, xmm2
    movaps xmm0, xmm1
end;

function TSSEVectorOperation.mulScalar(const vect1: TVector;
  const scalar: single): TVector; assembler;
asm

end;

end.

