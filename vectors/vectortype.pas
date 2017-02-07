{-----------------------------------
 Vector type declaration
-------------------------------------
(c) 2017 Zamrony P. Juhara <zamronypj@yahoo.com>
http://github.com/zamronypj/simd
-------------------------------------
Tested on Ubuntu 14.04 64 bit
Intel core i7
-------------------------------------
Use data alignment 16 bytes boundary
-------------------------------------}
unit vectorType;

{$mode objfpc}{$H+}

{$ALIGN 16}

interface

uses
  Classes, SysUtils;

type
   TVector = record
      x,y,z,w : single;
   end;

implementation

end.

