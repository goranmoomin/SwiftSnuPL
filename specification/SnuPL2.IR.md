# SnuPL/2 Intermediate Representation

The SnuPL/2 IR is implemented in `ir.cpp/h` and largely follows the textbook. The class hierarchy is illustrated below:

![](specification/resources/ir.class.diagram.png)

`CTacAddr` and subclasses represent symbols, temporaries, and constant values. `CTacAddr` and its subclasses form the operands of `CTacInstr` instructions.

Operations are implemented using `CTacInstr`. `CTacLabel` is a special instruction that simply serves as a label and does not actually execute any code. `CTacLabel` can be used as an operand for branching operations (goto, if relop goto..., see below). Different operations require different operands, both in type and number; refer to the table below.

The `CCodeBlock` class manages the list of instructions and is also responsible to generate (unique) temporary values and labels. The relevant methods are:
```
  CTacTemp* CCodeBlock::CreateTemp(const CType *type);
  CTacLabel* CCodeBlock::CreateLabel(const char *hint=NULL);
  CTacInstr* CCodeBlock::AddInstr(CTacInstr *instr);
```

`CScope` and its subclasses, finally, represent the module and procedures/functions of the program.

## SnuPL/2 IR

| Opcode        | Dst      | Src1     | Src2     | Description                         |
|:--------------|:---------|:---------|:---------|:------------------------------------|
| opAdd         | result   | operand1 | operand2 | result := operand1 + operand2       |
| opSub         | result   | operand1 | operand2 | result := operand1 - operand2       |
| opMul         | result   | operand1 | operand2 | result := operand1 * operand2       |
| opDiv         | result   | operand1 | operand2 | result := operand1 / operand2       |
| opAnd         | result   | operand1 | operand2 | result := operand1 && operand2      |
| opOr          | result   | operand1 | operand2 | result := operand1 || operand2      |
|               |          |          |          |                                     |
| opNeg         | result   | operand  |          | result := -operand                  |
| opPos         | result   | operand  |          | result := +operand                  |
| opNot         | result   | operand  |          | result := ~operand                  |
|               |          |          |          |                                     |
| opNeg         | result   | operand  |          | result := -operand                  |
| opEqual       | target   | operand1 | operand2 | if operand1 = operand2 goto target  |
| opNotEqual    | target   | operand1 | operand2 | if operand1 # operand2 goto target  |
| opLessThan    | target   | operand1 | operand2 | if operand1 < operand2 goto target  |
| opLessEqual   | target   | operand1 | operand2 | if operand1 <= operand2 goto target |
| opBiggerThan  | target   | operand1 | operand2 | if operand1 > operand2 goto target  |
| opBiggerEqual | target   | operand1 | operand2 | if operand1 >= operand2 goto target |
|               |          |          |          |                                     |
| opAssign      | LHS      | RHS      |          | LHS := RHS                          |
|               |          |          |          |                                     |
| opAddress     | result   | operand  |          | result := &operand                  |
| opCast        | result   | operand  |          | result := (type)operand             |
| opDeref       | result   | operand  |          | result := *operand                  |
| opWiden       | result   | operand  |          | result := (type)operand             |
| opNarrow      | result   | operand  |          | result := (type)operand             |
|               |          |          |          |                                     |
| opGoto        | target   |          |          | goto target                         |
|               |          |          |          |                                     |
| opCall        | result   | target   |          | result := call target               |
| opReturn      |          | operand  |          | return operand                      |
| opParam       | index    | operand  |          | index-th parameter := operand       |
|               |          |          |          |                                     |
| opLabel       |          |          |          | jump target                         |
| opNop         |          |          |          | no operation                        |
