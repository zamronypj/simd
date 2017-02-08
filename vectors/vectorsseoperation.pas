{-----------------------------------
 Vector operation with Intel SSE
 instruction implementation
-------------------------------------
(c) 2017 Zamrony P. Juhara <zamronypj@yahoo.com>
http://github.com/zamronypj/simd
-------------------------------------
Tested on Ubuntu 14.04 64 bit
Intel core i7
-------------------------------------
TODO: Need to be tested on other
platform
-------------------------------------}
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
    function sub(const vect1:TVector; const vect2:TVector) : TVector; override;
    function mulScalar(const vect1:TVector; const scalar:single) : TVector; override;
    function dot(const vect1:TVector; const vect2:TVector) : single; override;
  end;

implementation

{$asmmode intel}

{ TSSEVectorOperation }

{-------------------------------------
 Add two vectors using SSE instruction
 -------------------------------------
 result.x = vect1.x + vect2.x
 result.y = vect1.y + vect2.y
 result.z = vect1.z + vect2.z
 result.w = vect1.w + vect2.w
 -------------------------------------
 input:
 For x86-64 architecture
 vect1 and vect2 value will be passed
 in xmm0, xmm1, xmm2, xmm3 in following order
 xmm0 = [ vect1.x, vec1.y, (not used), (not used)]
 xmm1 = [ vect1.z, vec1.w, (not used), (not used)]
 xmm2 = [ vect2.x, vec2.y, (not used), (not used)]
 xmm3 = [ vect2.z, vec2.w, (not used), [not used]]
--------------------------------------
 output:
 result will be stored in xmm0 and xmm1
 register with following order
 xmm0 = [ result.x, result.y, [not used], [not used]]
 xmm1 = [ result.z, result.w, [not used], [not used]]
--------------------------------------}
function TSSEVectorOperation.add(const vect1: TVector; const vect2: TVector): TVector; assembler;
asm
    //copy low quadword of xmm1 to high quadword of xmm0
    //xmm0 = {vect1.x, vect1.y, vect1.z, vect1.w}
    movlhps xmm0, xmm1

    //copy low quadword of xmm3 to high quadword of xmm2
    //xmm2 = {vect2.x, vect2.y, vect2.z, vect2.w}
    movlhps xmm2, xmm3

    //add xmm0 and xmm2
    //xmm0 = {vect1.x + vect2.x,
    //        vect1.y + vect2.y,
    //        vect1.z + vect2.z,
    //        vect1.w + vect2.w}
    addps xmm0, xmm2

    //copy high quadword of xmm0 to low quadword of xmm1
    //xmm1 = {result.z, result.w, [not used], [not used]}
    movhlps xmm1, xmm0
end;


{-------------------------------------
 Substract two vector using SSE instruction
 -------------------------------------
 result.x = vect1.x - vect2.x
 result.y = vect1.y - vect2.y
 result.z = vect1.z - vect2.z
 result.w = vect1.w - vect2.w
 -------------------------------------
 input:
 For x86-64 architecture
 vect1 and vect2 value will be passed
 in xmm0, xmm1, xmm2, xmm3 in following order
 xmm0 = [ vect1.x, vec1.y, (not used), (not used)]
 xmm1 = [ vect1.z, vec1.w, (not used), (not used)]
 xmm2 = [ vect2.x, vec2.y, (not used), (not used)]
 xmm3 = [ vect2.z, vec2.w, (not used), [not used]]
--------------------------------------
 output:
 result will be stored in xmm0 and xmm1
 register with following order
 xmm0 = [ result.x, result.y, [not used], [not used]]
 xmm1 = [ result.z, result.w, [not used], [not used]]
--------------------------------------}
function TSSEVectorOperation.sub(const vect1: TVector; const vect2: TVector): TVector; assembler;
asm
    //copy low quadword of xmm1 to high quadword of xmm0
    //xmm0 = {vect1.x, vect1.y, vect1.z, vect1.w}
    movlhps xmm0, xmm1

    //copy low quadword of xmm3 to high quadword of xmm2
    //xmm2 = {vect2.x, vect2.y, vect2.z, vect2.w}
    movlhps xmm2, xmm3

    //add xmm0 and xmm2
    //xmm0 = {vect1.x - vect2.x,
    //        vect1.y - vect2.y,
    //        vect1.z - vect2.z,
    //        vect1.w - vect2.w}
    subps xmm0, xmm2

    //copy high quadword of xmm0 to low quadword of xmm1
    //xmm1 = {result.z, result.w, [not used], [not used]}
    movhlps xmm1, xmm0
end;


{-------------------------------------
 multiply a vector with a scalar using
 SSE instruction
 --------------------------------------
 result.x = vect1.x * scalar
 result.y = vect1.y * scalar
 result.z = vect1.z * scalar
 result.w = vect1.w * scalar
 -------------------------------------
 input:
 For x86-64 architecture
 vect1 and scalar value will be passed
 in xmm0, xmm1, xmm2 in following order
 xmm0 = [ vect1.x, vec1.y, (not used), (not used)]
 xmm1 = [ vect1.z, vec1.w, (not used), (not used)]
 xmm2 = [ scalar, (not used), (not used), (not used)]
 --------------------------------------
 output:
 result will be stored in xmm0 and xmm1
 register with following order
 xmm0 = [ result.x, result.y, [not used], [not used]]
 xmm1 = [ result.z, result.w, [not used], [not used]]
--------------------------------------}
function TSSEVectorOperation.mulScalar(const vect1: TVector; const scalar: single): TVector; assembler;
asm
    //copy low quadword of xmm1 to high quadword of xmm0
    //xmm0 = {vect1.x, vect1.y, vect1.z, vect1.w}
    movlhps xmm0, xmm1

    //shuffle xmm2 so that
    //xmm2 = {scalar, scalar, scalar, scalar}
    shufps xmm2, xmm2, 00000000b

    //multiply xmm0 and xmm2
    //xmm0 = {vect1.x * scalar,
    //        vect1.y * scalar,
    //        vect1.z * scalar,
    //        vect1.w * scalar}
    mulps xmm0, xmm2

    //copy high quadword of xmm0 to low quadword of xmm1
    //xmm1 = {result.z, result.w, [not used], [not used]}
    movhlps xmm1, xmm0
end;


{-------------------------------------
 Dot product of two vectors using SSE instruction
 -------------------------------------
 dotProd = vect1.x * vect2.x +
           vect1.y * vect2.y +
           vect1.z * vect2.z +
           vect1.w * vect2.w
 -------------------------------------
 input:
 For x86-64 architecture
 vect1 and vect2 value will be passed
 in xmm0, xmm1, xmm2, xmm3 in following order
 xmm0 = [ vect1.x, vec1.y, (not used), (not used)]
 xmm1 = [ vect1.z, vec1.w, (not used), (not used)]
 xmm2 = [ vect2.x, vec2.y, (not used), (not used)]
 xmm3 = [ vect2.z, vec2.w, (not used), (not used)]
--------------------------------------
 output:
 result will be stored in xmm0 register with following order
 xmm0 = [ dotProd, (not used), (not used), (not used)]
--------------------------------------}
function TSSEVectorOperation.dot(const vect1: TVector; const vect2: TVector ): single; assembler;
asm
    //copy low quadword of xmm1 to high quadword of xmm0
    //xmm0 = {vect1.x, vect1.y, vect1.z, vect1.w}
    movlhps xmm0, xmm1

    //copy low quadword of xmm3 to high quadword of xmm2
    //xmm2 = {vect2.x, vect2.y, vect2.z, vect2.w}
    movlhps xmm2, xmm3

    //multiply xmm0 and xmm2
    //xmm0 = {vect1.x * vect2.x,
    //        vect1.y * vect2.y,
    //        vect1.z * vect2.z,
    //        vect1.w * vect2.w}
    mulps xmm0, xmm2

    //copy high quadword of xmm0 to low quadword of xmm1
    //xmm1 = {result.z, result.w, [not used], [not used]}
    movhlps xmm1, xmm0

    //add horizontal fields so that
    //xmm0 = {result.x + result.z, result.y + result.w, [not used], [not used]}
    addps xmm0, xmm1

    //copy xmm0 to xmm1
    //xmm1 = {result.x + result.z, result.y + result.w, [not used], [not used]}
    movaps xmm1, xmm0

    //shuffle so that
    //xmm1 = {result.y + result.w, [not used], [not used], [not used]}
    shufps xmm1, xmm0, 0000001b

    //add so that
    //xmm0 = {result.x + result.z + result.y + result.w, [not used], [not used]}
    addps xmm0, xmm1
end;

end.

