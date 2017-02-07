unit ufrmMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, vectorType, vectorOperation, availVectorOperation;

type

  { TfrmAddVector }

  TfrmAddVector = class(TForm)
    btnAdd: TButton;
    btnMultiply: TButton;
    btnBenchmark: TButton;
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
    lblBenchmarkResult: TLabel;
    rdgrpInstruction: TRadioGroup;
    procedure btnAddClick(Sender: TObject);
    procedure btnBenchmarkClick(Sender: TObject);
    procedure btnMultiplyClick(Sender: TObject);
    procedure rdgrpInstructionSelectionChanged(Sender: TObject);
  private
    { private declarations }
    vectOperation : IVectorOperation;
    availableVectorOperation : TAvailableVectorOperations;
    function getInputVector(edx : TEdit; edy : TEdit; edz : TEdit; edw : TEdit) : TVector;
    procedure displayOutputVector(output:TVector; edx : TEdit; edy : TEdit; edz : TEdit; edw : TEdit);
  public
    { public declarations }
    constructor Create(AOwner : TComponent); override;
    destructor Destroy(); override;
  end;

var
  frmAddVector: TfrmAddVector;

implementation

{$R *.lfm}

{ TfrmAddVector }

constructor TfrmAddVector.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  availableVectorOperation := TAvailableVectorOperations.Create();
  vectOperation := availableVectorOperation[rdgrpInstruction.itemIndex];
end;

destructor TfrmAddVector.Destroy;
begin
  availableVectorOperation.free();
  inherited Destroy;
end;

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

procedure TfrmAddVector.btnBenchmarkClick(Sender: TObject);
var input1: TVector;
    scalar: single;
    output: TVector;
    i : integer;
    tick :QWord;
begin
  input1 := getInputVector(edVec1X, edVec1Y, edVec1Z, edVec1W);
  scalar := strToFloat(edVec2X.text);

  tick := getTickCount64();
  for i:=0 to 100000000 do
  begin
    output := vectOperation.mulScalar(input1, scalar);
  end;
  tick := getTickCount64() - tick;

  displayOutputVector(output, edResultX, edResultY, edResultZ, edResultW);
  lblBenchmarkResult.Caption := rdgrpInstruction.items[rdgrpInstruction.itemIndex] + ' multiply vector scalar (10,000,000 iteration): ' + inttostr(tick) + ' ms';
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

procedure TfrmAddVector.rdgrpInstructionSelectionChanged(Sender: TObject);
begin
  //assume intemIndex value is 0 or 1
  vectOperation := availableVectorOperation[rdgrpInstruction.itemIndex];
end;

function TfrmAddVector.getInputVector(edx: TEdit; edy: TEdit; edz: TEdit;
  edw: TEdit): TVector;
begin
  //no validation for simplicity
  result.x := strToFloat(edx.Text);
  result.y := strToFloat(edy.Text);
  result.z := strToFloat(edz.Text);
  result.w := strToFloat(edw.Text);
end;

procedure TfrmAddVector.displayOutputVector(output: TVector; edx: TEdit;
  edy: TEdit; edz: TEdit; edw: TEdit);
begin
  //no validation for simplicity
  edx.text := floatToStr(output.x);
  edy.text := floatToStr(output.y);
  edz.text := floatToStr(output.z);
  edw.text := floatToStr(output.w);
end;

end.

