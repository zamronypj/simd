{-----------------------------------
 Intel SSE/SSE2/SSE3/SSE4.1/SSE4.2
 feature detection helper implementation
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
unit ssedetection;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

   { TSSEFeatureDetection }

   TSSEFeatureDetection = class(TObject)
   private
      cpuidECXResult : dword;
      cpuidEDXResult : dword;
      function getSSESupportedStatus() : boolean;
      function getSSE2SupportedStatus() : boolean;
      function getSSE3SupportedStatus() : boolean;
      function getSSSE3SupportedStatus() : boolean;
      function getSSE41SupportedStatus() : boolean;
      function getSSE42SupportedStatus() : boolean;
      procedure runFeatureDetection();
   public
      constructor Create();
      property SSESupported : boolean read getSSESupportedStatus;
      property SSE2Supported : boolean read getSSE2SupportedStatus;
      property SSE3Supported : boolean read getSSE3SupportedStatus;
      property SSSE3Supported : boolean read getSSSE3SupportedStatus;
      property SSE41Supported : boolean read getSSE41SupportedStatus;
      property SSE42Supported : boolean read getSSE42SupportedStatus;
   end;

implementation

{$asmmode intel}

const
    SSE_SUPPORTED_FLAG    = $02000000;
    SSE2_SUPPORTED_FLAG   = $04000000;
    SSE3_SUPPORTED_FLAG   = $00000001;
    SSSE3_SUPPORTED_FLAG  = $00000100;
    SSE4_1_SUPPORTED_FLAG = $00080000;
    SSE4_2_SUPPORTED_FLAG = $00100000;

{ TSSEFeatureDetection }

function TSSEFeatureDetection.getSSESupportedStatus() : boolean;
begin
  result:= (cpuidEDXResult and SSE_SUPPORTED_FLAG) = SSE_SUPPORTED_FLAG;
end;

function TSSEFeatureDetection.getSSE2SupportedStatus() : boolean;
begin
  result:= (cpuidEDXResult and SSE2_SUPPORTED_FLAG) = SSE2_SUPPORTED_FLAG;
end;

function TSSEFeatureDetection.getSSE3SupportedStatus(): boolean;
begin
  result:= (cpuidECXResult and SSE3_SUPPORTED_FLAG) = SSE3_SUPPORTED_FLAG;
end;

function TSSEFeatureDetection.getSSSE3SupportedStatus() : boolean;
begin
  result:= (cpuidECXResult and SSSE3_SUPPORTED_FLAG) = SSSE3_SUPPORTED_FLAG;
end;

function TSSEFeatureDetection.getSSE41SupportedStatus() : boolean;
begin
  result:= (cpuidECXResult and SSE4_1_SUPPORTED_FLAG) = SSE4_1_SUPPORTED_FLAG;
end;

function TSSEFeatureDetection.getSSE42SupportedStatus() : boolean;
begin
  result:= (cpuidECXResult and SSE4_2_SUPPORTED_FLAG) = SSE4_2_SUPPORTED_FLAG;
end;

procedure TSSEFeatureDetection.runFeatureDetection();
var regEcx, regEdx : DWord;
begin
  asm
     mov eax, 01h
     cpuid
     mov regEcx, ecx
     mov regEdx, edx
  end;
  cpuidECXResult := regEcx;
  cpuidEDXResult := regEdx;
end;

constructor TSSEFeatureDetection.Create();
begin
  inherited Create();
  cpuidECXResult := 0;
  cpuidEDXResult := 0;
  runFeatureDetection();
end;

end.

