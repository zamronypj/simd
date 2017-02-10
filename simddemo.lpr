program simddemo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, ufrmMain, vectorOperation, vectorType, 
vectorstdoperation, vectorbaseoperation, vectorsseoperation, 
availVectorOperation, vectorsse41operation, vectorsse3operation
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfrmAddVector, frmAddVector);
  Application.Run;
end.

