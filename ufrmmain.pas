unit ufrmMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, vectorType, vectorOperation, availVectorOperation, SSEDetection;

type

  { TfrmAddVector }

  TfrmAddVector = class(TForm)
    btnAdd: TButton;
    btnMultiplyScalar: TButton;
    btnBenchmark: TButton;
    btnSubtract: TButton;
    btnDot: TButton;
    btnLength: TButton;
    chkbxSSSE3Supported: TCheckBox;
    chkbxSSE42Supported: TCheckBox;
    chkbxSSESupported: TCheckBox;
    chkbxSSE2Supported: TCheckBox;
    chkbxSSE41Supported: TCheckBox;
    chkbxSSE3Supported: TCheckBox;
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
    procedure btnDotClick(Sender: TObject);
    procedure btnLengthClick(Sender: TObject);
    procedure btnMultiplyScalarClick(Sender: TObject);
    procedure btnSubtractClick(Sender: TObject);
    procedure rdgrpInstructionSelectionChanged(Sender: TObject);
  private
    { private declarations }
    vectOperation : IVectorOperation;
    availableVectorOperation : TAvailableVectorOperations;
    SSEFeatureDetector : TSSEFeatureDetection;
    function getInputVector(edx : TEdit; edy : TEdit; edz : TEdit; edw : TEdit) : TVector;
    procedure displayOutputVector(output:TVector; edx : TEdit; edy : TEdit; edz : TEdit; edw : TEdit);
    procedure displayBenchmarkResult(const tick : QWord);
    procedure buildAvailableOperationUI();
  public
    { public declarations }
    constructor Create(AOwner : TComponent); override;
    destructor Destroy(); override;
  end;

var
  frmAddVector: TfrmAddVector;

implementation

{$R *.lfm}

const MAX_ITERATION = 10000000;

{ TfrmAddVector }

constructor TfrmAddVector.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  availableVectorOperation := TAvailableVectorOperations.Create();
  buildAvailableOperationUI();
  rdgrpInstruction.itemIndex := 0;
  vectOperation := availableVectorOperation[rdgrpInstruction.itemIndex];

  SSEFeatureDetector := TSSEFeatureDetection.Create();
  chkbxSSESupported.checked := SSEFeatureDetector.SSESupported;
  chkbxSSE2Supported.checked := SSEFeatureDetector.SSE2Supported;
  chkbxSSE3Supported.checked := SSEFeatureDetector.SSE3Supported;
  chkbxSSSE3Supported.checked := SSEFeatureDetector.SSSE3Supported;
  chkbxSSE41Supported.checked := SSEFeatureDetector.SSE41Supported;
  chkbxSSE42Supported.checked := SSEFeatureDetector.SSE42Supported;
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
  for i:=0 to MAX_ITERATION do
  begin
    output := vectOperation.mulScalar(input1, scalar);
  end;
  tick := getTickCount64() - tick;

  displayOutputVector(output, edResultX, edResultY, edResultZ, edResultW);
  displayBenchmarkResult(tick);
end;

procedure TfrmAddVector.btnDotClick(Sender: TObject);
var input1: TVector;
    input2: TVector;
    output : TVector;
begin
  input1 := getInputVector(edVec1X, edVec1Y, edVec1Z, edVec1W);
  input2 := getInputVector(edVec2X, edVec2Y, edVec2Z, edVec2W);
  output.x := vectOperation.dot(input1, input2);
  output.y := 0;
  output.z := 0;
  output.w := 0;
  displayOutputVector(output, edResultX, edResultY, edResultZ, edResultW);
end;

procedure TfrmAddVector.btnLengthClick(Sender: TObject);
var input1: TVector;
    output: TVector;
begin
  input1 := getInputVector(edVec1X, edVec1Y, edVec1Z, edVec1W);
  output.x := vectOperation.length(input1);
  output.y := 0;
  output.z := 0;
  output.w := 0;
  displayOutputVector(output, edResultX, edResultY, edResultZ, edResultW);
end;

procedure TfrmAddVector.displayBenchmarkResult(const tick: QWord);
begin
  lblBenchmarkResult.Caption := rdgrpInstruction.items[rdgrpInstruction.itemIndex] +
                             ' multiply vector scalar ('+
                             inttostr(MAX_ITERATION) +' iteration): ' +
                             inttostr(tick) + ' ms';
end;

procedure TfrmAddVector.btnMultiplyScalarClick(Sender: TObject);
var input1: TVector;
    scalar: single;
    output: TVector;
begin
  input1 := getInputVector(edVec1X, edVec1Y, edVec1Z, edVec1W);
  scalar := strToFloat(edVec2X.text);
  output := vectOperation.mulScalar(input1, scalar);
  displayOutputVector(output, edResultX, edResultY, edResultZ, edResultW);
end;

procedure TfrmAddVector.btnSubtractClick(Sender: TObject);
var input1: TVector;
    input2: TVector;
    output: TVector;
begin
  input1 := getInputVector(edVec1X, edVec1Y, edVec1Z, edVec1W);
  input2 := getInputVector(edVec2X, edVec2Y, edVec2Z, edVec2W);
  output := vectOperation.sub(input1, input2);
  displayOutputVector(output, edResultX, edResultY, edResultZ, edResultW);
end;

procedure TfrmAddVector.rdgrpInstructionSelectionChanged(Sender: TObject);
begin
  vectOperation := availableVectorOperation[rdgrpInstruction.itemIndex];
  caption := 'Vector Operation ' + rdgrpInstruction.items[rdgrpInstruction.itemIndex];
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


procedure TfrmAddVector.buildAvailableOperationUI();
var i:integer;
begin
  rdgrpInstruction.Items.clear();
  for i := 0 to availableVectorOperation.count - 1 do
  begin
    rdgrpInstruction.Items.add(availableVectorOperation.operationNames[i]);
  end;
end;

end.

