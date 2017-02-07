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

{-------------------------------------
 Add two vector using SSE instruction
--------------------------------------
 input:
 For x86-64 architecture
 vect1 and vect2 value will be passed
 in xmm0, xmm1, xmm2, xmm3 in following order
 xmm0 = { vect1.x, vec1.y, [not used], [not used]}
 xmm1 = { vect1.z, vec1.w, [not used], [not used]}
 xmm2 = { vect2.x, vec2.y, [not used], [not used]}
 xmm3 = { vect2.z, vec2.w, [not used], [not used]}
--------------------------------------
 output:
 result will be stored in xmm0 and xmm1
 register with following order
 xmm0 = { result.x, result.y, [not used], [not used]}
 xmm1 = { result.z, result.w, [not used], [not used]}
--------------------------------------}
function TSSEVectorOperation.add(const vect1: TVector; const vect2: TVector): TVector; assembler;
asm
    //shuffle xmm0 and xmm1 so that
    //xmm0 = {vect1.x, vect1.y, vect1.z, vect1.w}
    shufps xmm0, xmm1, 01000100b

    //shuffle xmm2 and xmm3 so that
    //xmm3 = {vect2.x, vect2.y, vect2.z, vect2.w}
    shufps xmm2, xmm3, 01000100b

    //add xmm0 and xmm2
    //xmm0 = {vect1.x + vect2.x,
    //        vect1.y + vect2.y,
    //        vect1.z + vect2.z,
    //        vect1.w + vect2.w}
    addps xmm0, xmm2

    //copy xmm0 to xmm1
    //xmm1 = {result.x, result.y, result.z, result.w}
    movaps xmm1, xmm0

    //shuffle xmm1 and xmm0 so that
    //xmm1 = {result.z, result.w, [not used], [not used]}
    shufps xmm1, xmm0, 00001110b
end;

function TSSEVectorOperation.mulScalar(const vect1: TVector;
  const scalar: single): TVector; assembler;
asm

end;

end.

