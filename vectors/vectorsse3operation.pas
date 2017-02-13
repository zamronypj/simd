{-----------------------------------
 Vector operation with Intel SSE 3
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
unit vectorsse3operation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, vectorType, vectorSSEOperation;

type

     { TSSE3VectorOperation }

     TSSE3VectorOperation = class(TSSEVectorOperation)
     public
       function dot(const vect1:TVector; const vect2:TVector) : single; override;
       function length(const vect1:TVector) : single; override;
       function normalize(const vect1:TVector) : TVector; override;
     end;

implementation

{$asmmode intel}

{ TSSE3VectorOperation }

function TSSE3VectorOperation.dot(const vect1: TVector; const vect2: TVector): single; assembler;
asm
    //before shuffle
    //xmm1 = {vect1.z, vect1.w, 0, 0}
    //after shuffle
    //xmm1 = {vect1.z, 0, 0, 0}
    shufps xmm1, xmm1, 11101000b

    //before shuffle
    //xmm3 = {vect2.z, vect2.w, 0, 0}
    //after shuffle
    //xmm3 = {vect2.z, 0, 0, 0}
    shufps xmm3, xmm3, 11101000b

    //copy low quadword of xmm1 to high quadword of xmm0
    //xmm0 = {vect1.x, vect1.y, vect1.z, 0}
    movlhps xmm0, xmm1

    //copy low quadword of xmm3 to high quadword of xmm2
    //xmm2 = {vect2.x, vect2.y, vect2.z, 0}
    movlhps xmm2, xmm3

    //multiply xmm0 and xmm2
    //xmm0 = {vect1.x * vect2.x,
    //        vect1.y * vect2.y,
    //        vect1.z * vect2.z,
    //        0}
    mulps xmm0, xmm2

    //horizontal add
    //xmm0 = [a, b, a, b]
    //where a = vect1.x * vect2.x + vect1.y * vect2.y
    //      b = vect1.z * vect2.z + 0
    haddps xmm0, xmm0

    //horizontal add
    //xmm0 = [c, c, c, c]
    //where c = a + b
    //        = vect1.x * vect2.x + vect1.y * vect2.y + vect1.z * vect2.z
    haddps xmm0, xmm0
end;

{-------------------------------------
 Length of a vector
 -------------------------------------
 input:
  xmm0 = [vect1.x, vect1.y, 0, 0]
  xmm1 = [vect1.z, vect1.w, 0, 0]
 -------------------------------------
 output:
  result := sqrt(vect1.x * vect1.x +
                 vect1.y * vect1.y +
                 vect1.z * vect1.z);
--------------------------------------}
function TSSE3VectorOperation.length(const vect1: TVector): single; assembler;
asm
    //---------this can be removed if vect1.w = 0 --------------
    //before shuffle
    //xmm1 = {vect1.z, vect1.w, 0, 0}
    //after shuffle
    //xmm1 = {vect1.z, 0, 0, 0}
    shufps xmm1, xmm1, 11101000b
    //---------this can be removed if vect1.w = 0 --------------

    //copy low quadword of xmm1 to high quadword of xmm0
    //xmm0 = {vect1.x, vect1.y, vect1.z, 0}
    movlhps xmm0, xmm1

    //multiply
    //xmm0 = [ vect1.x * vect1.x, vect1.y * vect1.y, vect1.z * vect1.z, 0 ]
    mulps xmm0, xmm0

    //horizontal add
    //xmm0 = [ a, b, a, b ]
    //where a = (vect1.x * vect1.x) + (vect1.y * vect1.y),
    //      b = vect1.z * vect1.z + 0
    haddps xmm0, xmm0

    //horizontal add
    //xmm0 = [ c, c, c, c ]
    //where a = (vect1.x * vect1.x) + (vect1.y * vect1.y),
    //      b = vect1.z * vect1.z
    //      c = a + b
    haddps xmm0, xmm0

    //square root
    //xmm0 = [ sqrt(c), not used, not used, not used]
    sqrtss xmm0, xmm0
end;

function TSSE3VectorOperation.normalize(const vect1: TVector): TVector; assembler;
asm
    //---------this can be removed if vect1.w = 0 --------------
    //before shuffle
    //xmm1 = {vect1.z, vect1.w, 0, 0}
    //after shuffle
    //xmm1 = {vect1.z, 0, 0, 0}
    shufps xmm1, xmm1, 11101000b
    //---------this can be removed if vect1.w = 0 --------------

    //copy low quadword of xmm1 to high quadword of xmm0
    //xmm0 = {vect1.x, vect1.y, vect1.z, 0}
    //xmm1 = {vect1.x, vect1.y, vect1.z, 0}
    movlhps xmm0, xmm1
    movaps xmm1, xmm0

    //multiply
    //xmm1 = [ vect1.x * vect1.x, vect1.y * vect1.y, vect1.z * vect1.z, 0 ]
    mulps xmm1, xmm1

    //horizontal add
    //xmm1 = [ a, b, a, b ]
    //where a = (vect1.x * vect1.x) + (vect1.y * vect1.y),
    //      b = vect1.z * vect1.z + 0
    haddps xmm1, xmm1

    //horizontal add
    //xmm1 = [ c, c, c, c ]
    //where a = (vect1.x * vect1.x) + (vect1.y * vect1.y),
    //      b = vect1.z * vect1.z
    //      c = a + b
    haddps xmm1, xmm1

    //square root
    //xmm1 = [ 1/sqrt(c), 1/sqrt(c), 1/sqrt(c), 1/sqrt(c)]
    rsqrtps xmm1, xmm1

    mulps xmm0, xmm1
end;

end.

