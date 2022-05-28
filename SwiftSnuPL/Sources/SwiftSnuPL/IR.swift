import Foundation

class Generator {
    let module: Parser.Module
    let resolvedSymbols: [Token: Resolver.Symbol]
    let resolvedTypes: [Parser.Expression: Resolver.`Type`]

    init(
        module: Parser.Module, resolvedSymbols: [Token: Resolver.Symbol],
        resolvedTypes: [Parser.Expression: Resolver.`Type`]
    ) {
        self.module = module
        self.resolvedSymbols = resolvedSymbols
        self.resolvedTypes = resolvedTypes
    }

    // MARK: - Types

    enum UnaryOp {
        case neg
        case pos
        case not
    }

    enum BinaryOp {
        case add
        case sub
        case mul
        case div
        case and
        case or

        case equal
        case notEqual
        case lessThan
        case lessEqual
        case biggerThan
        case biggerEqual
    }

    enum Operand {
        case constant(Int64)
        case temporary(name: String)
        case string(name: String)
        case symbol(Resolver.Symbol)
    }

    enum Instruction {
        case move(dst: Operand, src: Operand)
        case unary(op: UnaryOp, dst: Operand, src: Operand)
        case binary(op: BinaryOp, dst: Operand, src1: Operand, src2: Operand)
        case parameter(dst: Operand, index: Int)  // always 64-bit

        case jump(dst: String)
        case branch(dst: String, src1: Operand, src2: Operand)  // branch if equal
        case call(dst: Operand?, symbol: Resolver.Symbol, arguments: [Operand])
        case `return`(value: Operand?)

        case load(dst: Operand, symbol: Resolver.Symbol, indices: [Operand])
        case store(symbol: Resolver.Symbol, indices: [Operand], src: Operand)

        case label(name: String)
    }

    // MARK: - Generating Instructions

    func generate() -> [Instruction] { return makeInstructions(module: module) }

    var labelID = 0
    var operandID = 0
    var stringID = 0

    var stringLiterals: [String: [UInt8]] = [:]

    func makeLabel() -> String {
        let labelName = "lbl\(labelID)"
        labelID += 1
        return labelName
    }

    func makeOperand() -> Operand {
        let name = "tmp\(operandID)"
        operandID += 1
        return .temporary(name: name)
    }

    func makeLiteral(string: [UInt8]) -> Operand {
        let name = "string\(stringID)"
        stringLiterals[name] = string
        stringID += 1
        return .string(name: name)
    }

    func makeInstructions(module: Parser.Module) -> [Instruction] {
        return makeInstructions(block: module.block)
    }

    func makeInstructions(parameters: [Parser.Parameter], block: Parser.Block) -> [Instruction] {
        var instructions: [Instruction] = []
        for (index, parameter) in parameters.enumerated() {
            let symbol = resolvedSymbol(of: parameter.name)
            instructions.append(.parameter(dst: .symbol(symbol), index: index))
        }
        instructions.append(contentsOf: makeInstructions(block: block))
        return instructions
    }

    func makeInstructions(block: Parser.Block) -> [Instruction] {
        var instructions: [Instruction] = []
        for declaration in block.declarations {
            switch declaration {
            case .const(let name, _, _):
                let symbol = resolvedSymbol(of: name)
                guard case .const(_, _, let initializer) = symbol else { fatalError() }
                let initializerOperand: Operand
                switch initializer {
                case let initializer as Int64: initializerOperand = .constant(initializer)
                case let initializer as Int32: initializerOperand = .constant(Int64(initializer))
                case let initializer as UInt8: initializerOperand = .constant(Int64(initializer))
                case let initializer as Bool: initializerOperand = .constant(initializer ? 1 : 0)
                case let initializer as [UInt8]:
                    initializerOperand = makeLiteral(string: initializer)
                default: fatalError()
                }
                instructions.append(
                    .move(dst: .symbol(resolvedSymbol(of: name)), src: initializerOperand))
            default: continue
            }
        }
        instructions.append(contentsOf: makeInstructions(statements: block.body))
        return instructions
    }

    func makeInstructions(statements: [Parser.Statement]) -> [Instruction] {
        return Array(statements.map(makeInstructions(statement:)).joined())
    }

    func makeInstructions(statement: Parser.Statement) -> [Instruction] {
        switch statement {
        case .assignment(let target, let value):
            let valueOperand = makeOperand()
            var instructions = makeInstructions(expression: value, to: valueOperand)
            if case .variable(name: let name) = target {
                let targetOperand: Operand = .symbol(resolvedSymbol(of: name))
                instructions.append(.move(dst: targetOperand, src: valueOperand))
            } else if case .subscript = target {
                var variable = target
                var indexOperands: [Operand] = []
                while case .subscript(array: let array, index: let index) = variable {
                    variable = array
                    let indexOperand = makeOperand()
                    instructions.append(
                        contentsOf: makeInstructions(expression: index, to: indexOperand))
                    indexOperands.append(indexOperand)
                }
                guard case .variable(name: let name) = variable else { fatalError() }
                let symbol = resolvedSymbol(of: name)
                instructions.append(
                    .store(symbol: symbol, indices: indexOperands, src: valueOperand))
            }
            return instructions

        case .call(let procedure, let arguments):
            var instructions: [Instruction] = []
            var argumentOperands: [Operand] = []
            for argument in arguments {
                let operand = makeOperand()
                instructions.append(contentsOf: makeInstructions(expression: argument, to: operand))
                argumentOperands.append(operand)
            }
            guard case .variable(name: let name) = procedure else { fatalError() }
            instructions.append(
                .call(dst: nil, symbol: resolvedSymbol(of: name), arguments: argumentOperands))
            return instructions

        case .if(let condition, let thenBody, let elseBody):
            // condition, thenBody, elseBody
            // [ makeInstructions(expression: condition, to: <_tmp0>)
            // , .branch(op: .cbz, <_tmp0>, <_lbl0>) (compare and branch if zero)
            // , makeInstructions(thenBody)
            // , .jump(<_lbl1>)
            // , <_lbl0>:
            // , makeInstructions(elseBody)
            // , <_lbl1>: ]
            var instructions: [Instruction] = []
            let operand = makeOperand()
            let elseLabel = makeLabel()
            let endLabel = makeLabel()
            instructions.append(contentsOf: makeInstructions(expression: condition, to: operand))
            instructions.append(.branch(dst: elseLabel, src1: operand, src2: .constant(0)))
            instructions.append(contentsOf: makeInstructions(statements: thenBody))
            instructions.append(.jump(dst: endLabel))
            instructions.append(.label(name: elseLabel))
            instructions.append(contentsOf: makeInstructions(statements: elseBody))
            instructions.append(.label(name: endLabel))
            return instructions

        case .while(let condition, let body):
            // [ <_lbl0>:
            // , makeInstructions(expression: condition, to: <_tmp0>)
            // , .branch(op: .cbz, <_tmp0>, <_lbl1>) (compare and branch if zero)
            // , makeInstructions(body)
            // , .jump(<_lbl0>)
            // , <_lbl1>: ]
            var instructions: [Instruction] = []
            let operand = makeOperand()
            let conditionLabel = makeLabel()
            let endLabel = makeLabel()
            instructions.append(.label(name: conditionLabel))
            instructions.append(contentsOf: makeInstructions(expression: condition, to: operand))
            instructions.append(.branch(dst: endLabel, src1: operand, src2: .constant(0)))
            instructions.append(contentsOf: makeInstructions(statements: body))
            instructions.append(.jump(dst: conditionLabel))
            instructions.append(.label(name: endLabel))
            return instructions

        case .return(let value):
            // makeInstructions(expression: value, to: x0)
            if let value = value {
                let operand = makeOperand()
                return makeInstructions(expression: value, to: operand) + [.return(value: operand)]
            } else {
                return [.return(value: nil)]
            }
        }
    }

    func makeInstructions(expression: Parser.Expression, to operand: Operand) -> [Instruction] {
        switch expression {
        case .integer(let integer): return [.move(dst: operand, src: .constant(Int64(integer)))]
        case .longint(let longint): return [.move(dst: operand, src: .constant(longint))]
        case .boolean(let boolean): return [.move(dst: operand, src: .constant(boolean ? 1 : 0))]
        case .char(let char): return [.move(dst: operand, src: .constant(Int64(char)))]
        case .string(let string): return [.move(dst: operand, src: makeLiteral(string: string))]
        case .variable(let name):
            return [.move(dst: operand, src: .symbol(resolvedSymbol(of: name)))]
        case .binary(operator: let `operator`, let left, let right):
            // [ .makeInstructions(left, to: <_tmp0>)
            // , .makeInstructions(right, to: <_tmp1>)
            // , .binary(op, left, right, to: result) ]
            let leftOperand = makeOperand()
            let rightOperand = makeOperand()
            var instructions: [Instruction] = []
            if `operator`.string == "&&" || `operator`.string == "||" {
                // [ .makeInstructions(left, to: result)
                // , .branch(op: .cbz, result, <_lbl0>)
                // , .makeInstructions(right, to: <_tmp0>)
                // , .binary(.and, result, <_tmp0>, to: result)
                // , <_lbl0>: ]

                // [ .makeInstructions(left, to: result)
                // , .branch(op: .cbnz, result, <_lbl0>)
                // , .makeInstructions(right, to: <_tmp0>)
                // , .binary(.or, result, <_tmp0>, to: result)
                // , <_lbl0>: ]

                let branchLabel = makeLabel()
                let branchOperand: Operand = .constant(`operator`.string == "&&" ? 0 : 1)
                let op: BinaryOp = `operator`.string == "&&" ? .and : .or
                instructions.append(contentsOf: makeInstructions(expression: left, to: operand))
                instructions.append(.branch(dst: branchLabel, src1: operand, src2: branchOperand))
                instructions.append(
                    contentsOf: makeInstructions(expression: right, to: rightOperand))
                instructions.append(
                    .binary(op: op, dst: operand, src1: rightOperand, src2: operand))
                instructions.append(.label(name: branchLabel))
            } else {
                instructions.append(contentsOf: makeInstructions(expression: left, to: leftOperand))
                instructions.append(
                    contentsOf: makeInstructions(expression: right, to: rightOperand))
                let op: BinaryOp
                switch `operator`.string {
                case "+": op = .add
                case "-": op = .sub
                case "*": op = .mul
                case "/": op = .div
                case "=": op = .equal
                case "#": op = .notEqual
                case "<": op = .biggerThan
                case "<=": op = .biggerEqual
                case ">": op = .lessThan
                case ">=": op = .lessEqual
                default: fatalError()
                }
                instructions.append(
                    .binary(op: op, dst: operand, src1: leftOperand, src2: rightOperand))
            }
            return instructions

        case .unary(operator: let `operator`, let value):
            // -> create temp, .assign(from: addr, to: resultAddr)
            // [ .makeInstructions(value, to: <_tmp0>)
            // , .unary(<_tmp0>, to: result) ]
            var instructions: [Instruction] = []
            let valueOperand = makeOperand()
            instructions.append(contentsOf: makeInstructions(expression: value, to: valueOperand))
            let op: UnaryOp
            switch `operator`.string {
            case "+": op = .pos
            case "-": op = .neg
            case "!": op = .not
            default: fatalError()
            }
            instructions.append(.unary(op: op, dst: operand, src: valueOperand))
            return instructions

        case .subscript:
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
            var instructions: [Instruction] = []
            var indexOperands: [Operand] = []
            var variable = expression
            while case .subscript(array: let array, index: let index) = variable {
                variable = array
                let indexOperand = makeOperand()
                instructions.append(
                    contentsOf: makeInstructions(expression: index, to: indexOperand))
                indexOperands.append(indexOperand)
            }
            guard case .variable(name: let name) = variable else { fatalError() }
            let symbol = resolvedSymbol(of: name)
            instructions.append(.load(dst: operand, symbol: symbol, indices: indexOperands))
            if resolvedType(of: expression) == .boolean {
                instructions.append(
                    .binary(op: .and, dst: operand, src1: operand, src2: .constant(1)))
            }
            return instructions

        case .call(let function, let arguments):
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
            var instructions: [Instruction] = []
            var argumentOperands: [Operand] = []
            for argument in arguments {
                let operand = makeOperand()
                instructions.append(contentsOf: makeInstructions(expression: argument, to: operand))
                argumentOperands.append(operand)
            }
            guard case .variable(name: let name) = function else { fatalError() }
            instructions.append(
                .call(dst: operand, symbol: resolvedSymbol(of: name), arguments: argumentOperands))
            return instructions
        }
    }

    func resolvedSymbol(of token: Token) -> Resolver.Symbol {
        guard let symbol = resolvedSymbols[token] else { fatalError() }
        return symbol
    }

    func resolvedType(of expression: Parser.Expression) -> Resolver.`Type` {
        guard let type = resolvedTypes[expression] else { fatalError() }
        return type
    }
}
