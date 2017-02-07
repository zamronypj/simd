unit ufrmMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  vectorType, vectorOperation;

type

  { TfrmAddVector }

  TfrmAddVector = class(TForm)
    btnAdd: TButton;
    btnMultiply: TButton;
    edVec1X: TEdit;
    edResultY: TEdit;
    edResultZ: TEdit;
    edResultW: TEdit;
    edVec1Y: TEdit;
    edVec1Z: TEdit;
    edVec1W: TEdit;
    edVec2X: TEdit;
    edVec2Y: TEdit;
    edVec2Z: TEdit;
    edVec2W: TEdit;
    edResultX: TEdit;
    procedure btnAddClick(Sender: TObject);
    procedure btnMultiplyClick(Sender: TObject);
  private
    { private declarations }
    vectOperation : IVectorOperation;
    function getInputVector(edx : TEdit; edy : TEdit; edz : TEdit; edw : TEdit) : TVector;
    procedure displayOutputVector(output:TVector; edx : TEdit; edy : TEdit; edz : TEdit; edw : TEdit);
  public
    { public declarations }
    constructor Create(AOwner : TComponent); override;
  end;

var
  frmAddVector: TfrmAddVector;

implementation

uses vectorStdOperation, vectorSSEOperation;

{$R *.lfm}

{ TfrmAddVector }

procedure TfrmAddVector.btnAddClick(Sender: TObject);
var input1: TVector;
    input2: TVector;
    output: TVector;
begin
  input1 := getInputVector(edVec1X, edVec1Y, edVec1Z, edVec1W);
  input2 := getInputVector(edVec2X, edVec2Y, edVec2Z, edVec2W);
  output := vectOperation.add(input1, input2);
  displayOutputVector(output, edResultX, edResultY, edResultZ, edResultW);
end;

procedure TfrmAddVector.btnMultiplyClick(Sender: TObject);
var input1: TVector;
    scalar: single;
    output: TVector;
begin
  input1 := getInputVector(edVec1X, edVec1Y, edVec1Z, edVec1W);
  scalar := strToFloat(edVec2X.text);
  output := vectOperation.mulScalar(input1, scalar);
  displayOutputVector(output, edResultX, edResultY, edResultZ, edResultW);
end;

function TfrmAddVector.getInputVector(edx: TEdit; edy: TEdit; edz: TEdit;
  edw: TEdit): TVector;
begin
  result.x := strToFloat(edx.Text);
  result.y := strToFloat(edy.Text);
  result.z := strToFloat(edz.Text);
  result.w := strToFloat(edw.Text);
end;

procedure TfrmAddVector.displayOutputVector(output: TVector; edx: TEdit;
  edy: TEdit; edz: TEdit; edw: TEdit);
begin
  edx.text := floatToStr(output.x);
  edy.text := floatToStr(output.y);
  edz.text := floatToStr(output.z);
  edw.text := floatToStr(output.w);
end;

constructor TfrmAddVector.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //vectOperation := TStdVectorOperation.Create();
  vectOperation := TSSEVectorOperation.Create();
end;

end.

