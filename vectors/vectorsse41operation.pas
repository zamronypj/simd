{-----------------------------------
 Vector operation with Intel SSE 4.1
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
unit vectorsse41operation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, vectorType, vectorsseoperation;

type

   { TSSE41VectorOperation }

   TSSE41VectorOperation = class(TSSEVectorOperation)
   public
     function dot(const vect1:TVector; const vect2:TVector) : single; override;
     function length(const vect1:TVector) : single; override;
     function normalize(const vect1:TVector) : TVector; override;
   end;

implementation

{$asmmode intel}

{ TSSE41VectorOperation }

function TSSE41VectorOperation.dot(const vect1: TVector; const vect2: TVector): single; assembler;
asm
    //copy low quadword of xmm1 to high quadword of xmm0
    //xmm0 = [vect1.x, vect1.y, vect1.z, vect1.w]
    movlhps xmm0, xmm1

    //copy low quadword of xmm3 to high quadword of xmm2
    //xmm2 = {vect2.x, vect2.y, vect2.z, vect2.w}
    movlhps xmm2, xmm3

    //dot product
    //xmm0 = {vect1.x * vect2.x + vect1.y * vect2.y + vect1.z * vect2.z, 0, 0, 0}
    dpps xmm0, xmm2, 01110001b
end;


function TSSE41VectorOperation.length(const vect1: TVector): single; assembler;
asm
    //copy low quadword of xmm1 to high quadword of xmm0
    //xmm0 = {vect1.x, vect1.y, vect1.z, vect1.w}
    movlhps xmm0, xmm1

    //dot product
    //xmm0 = {vect1.x * vect1.x + vect1.y * vect1.y + vect1.z * vect1.z, 0, 0, 0}
    dpps xmm0, xmm0, 01110001b

    //square root
    //xmm0 = {sqrt(vect1.x * vect1.x + vect1.y * vect1.y + vect1.z * vect1.z), 0, 0, 0}
    sqrtss xmm0, xmm0
end;

function TSSE41VectorOperation.normalize(const vect1: TVector): TVector; assembler;
asm
    //copy low quadword of xmm1 to high quadword of xmm0
    //xmm0 = {vect1.x, vect1.y, vect1.z, vect1.w}
    movlhps xmm0, xmm1

    //xmm1 = {vect1.x, vect1.y, vect1.z, vect1.w}
    movaps xmm1, xmm0

    //dot product
    //xmm1 = {a, a, a, 0}
    //where a = vect1.x * vect1.x + vect1.y * vect1.y + vect1.z * vect1.z
    dpps xmm1, xmm1, 01110111b

    //reciprocal square root
    //xmm1 = {1/b, 1/b, 1/b, 0}
    //where b = sqrt(vect1.x * vect1.x + vect1.y * vect1.y + vect1.z * vect1.z)
    rsqrtps xmm1, xmm1

    //xmm0 = {vect1.x/b, vect1.y/b, vect1.z/b, 0}
    mulps xmm0, xmm1

    //xmm1 = {vect1.z/b, 0, not used, not used}
    movhlps xmm1, xmm0
end;

end.

