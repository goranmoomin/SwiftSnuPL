
import Foundation

enum TacBinaryOp {
    case add
    case sub
    case mul
    case div
    case and
    case or
}

enum TacUnaryOp {
    case neg
    case pos
    case not

    case assign

    case address
    case deref
    case cast
    case widen
    case narrow

    case call
    case param
}

enum TacBranchOp {
    case equal
    case notEqual
    case lessThan
    case lessEqual
    case biggerThan
    case biggerEqual
}

enum TacAddress {
    case const(value: Int64)
    case temp(name: String, type: Parser.`Type`)
    case reference(symbol: Resolver.Symbol)
}

enum TacInstruction {
    case unary(op: TacUnaryOp, dst: TacAddress, src: TacAddress)
    case binary(op: TacBinaryOp, dst: TacAddress, src1: TacAddress, src2: TacAddress)
    case goto(dst: String)
    case branch(op: TacBranchOp, dst: String, src1: TacAddress, src2: TacAddress)
    case label(name: String)
}

class TacGenerator {
    var labelId = 0
    var tempId = 0
    let module: Parser.Module
    let resolvedSymbols: [Token: Resolver.Symbol]

    init(module: Parser.Module, resolvedSymbols: [Token: Resolver.Symbol]) {
        self.module = module
        self.resolvedSymbols = resolvedSymbols
    }

    func createTac() -> [TacInstruction] { return createTac(module: module) }

    func createLabel() -> String {
        let labelName = "lbl\(labelId)"
        labelId += 1
        return labelName
    }

    func createTemp(type: Parser.`Type`) -> TacAddress {
        let tempName = "_tmp\(tempId)"
        tempId += 1
        return .temp(name: tempName, type: type)
    }

    func createTac(module: Parser.Module) -> [TacInstruction] {
        Array(module.block.body.map(createTac(statement:)).joined())
    }

    func createTac(statement: Parser.Statement) -> [TacInstruction] {
        switch statement {
        case .assignment(let target, let value):
            let (insts, resultAddr) = createTac(expression: value)
            guard case .variable(name: let name) = target else { fatalError() }
            let loadInst: TacInstruction = .unary(
                op: .assign, dst: .reference(symbol: resolvedSymbols[name]!), src: resultAddr)
            return insts + [loadInst]
            // [ createTac(expression: value, to: <_tmp0>)
            // , .assign(<_tmp0>, to: target) ] <- target?
        case .call: fatalError()
            // procedure, argument
            // [ createTac(expression: argument[*], to: x*)
            // , .call() ]
        case .if: fatalError()
            // condition, thenBody, elseBody
            // [ createTac(expression: condition, to: <_tmp0>)
            // , .branch(op: .cbz, <_tmp0>, <_lbl0>) (compare and branch if zero)
            // , createTac(thenBody)
            // , .jump(<_lbl1>)
            // , <_lbl0>:
            // , createTac(elseBody)
            // , <_lbl1>: ]
        case .while: fatalError()
            // condition, body
            // [ <_lbl0>:
            // , createTac(expression: condition, to: <_tmp0>)
            // , .branch(op: .cbz, <_tmp0>, <_lbl1>) (compare and branch if zero)
            // , createTac(body)
            // , .jump(<_lbl0>)
            // , <_lbl1>: ]
        case .return: fatalError()
            // createTac(expression: value, to: x0)
        }
    }

    func createTac(expression: Parser.Expression) -> ([TacInstruction], TacAddress) {
        // might need a return value argument? e.g. write 'to' this address
        switch expression {
        case .integer(let integer):
            return ([], .const(value: Int64(integer)))
            // [ .assign(const, to: result) ]
        case .variable(let name):
            return ([], .reference(symbol: resolvedSymbols[name]!))
            // [ .assign(addr, to: result) ]
        case .binary(operator: let `operator`, let left, let right):
            let (leftInsts, leftAddr) = createTac(expression: left)
            let (rightInsts, rightAddr) = createTac(expression: right)
            let resultAddr = createTemp(type: .integer)
            let resultInst: TacInstruction = .binary(op: `operator`.string == "+" ? .add : .mul, dst: resultAddr, src1: leftAddr, src2: rightAddr)
            return (leftInsts + rightInsts + [resultInst], resultAddr)
            // [ .createTac(left, to: <_tmp0>)
            // , .createTac(right, to: <_tmp1>)
            // , .binary(op, left, right, to: result) ]
        case .unary: fatalError() // -> create temp, .assign(from: addr, to: resultAddr)
            // [ .createTac(value, to: <_tmp0>)
            // , .unary(<_tmp0>, to: result) ]
//        case .binary: fatalError()
        case .subscript: fatalError()
            // temp: pointerAddr, resultAddr
            // .binary(.mul, from: indexAddr, .const(size(of: resolvedType(of: array).base)), to: pointerAddr)
            // .dereference(from: pointerAddr, size: size(of: resolvedType(of: array).base) to: resultAddr)
            //
            // mov <_tmp0>, <_arr>
            // mov <_tmp1>, <_idx>
            // ldr <_val>, [<_tmp0>, <_tmp1>, lsl #3] (if longint or pointer)
            // ldrsw ... #2 (if integer)
            // ldrb (if unsigned char)
            // ldrb; and (if boolean)
        case .call: fatalError()
            // function, argument
            // .param(argument[*], *) // mv instruction
            // .call(functionAddr, to: resultAddr) // jump to function
            //
            // mov x0, <_arg0> <- we need a temporary
            // mov x1, <_arg1>
            // mov x2, <_arg2>
            // bl <_func> <- we need a label of the calling function
            // mov <_ret>, x0
//        case .variable: fatalError()
//        case .integer: fatalError()
        case .longint: fatalError()
        case .boolean: fatalError()
        case .char: fatalError()
        case .string: fatalError()
        }
    }
}
