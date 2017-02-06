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

function TSSEVectorOperation.add(const vect1: TVector; const vect2: TVector): TVector;
begin
  asm
    movaps xmm0, vect1
    movaps xmm1, vect2
    addps xmm0, xmm1
    movaps [eax], xmm0
  end;
end;

function TSSEVectorOperation.mulScalar(const vect1: TVector;
  const scalar: single): TVector; assembler;
asm

end;

end.

