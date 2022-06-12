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

        case eq
        case neq
        case lt
        case leq
        case gt
        case geq
    }

    enum Operand {
        case constant(Int64)
        case temporary(name: String)
        case string(name: String)
        case symbol(Resolver.Symbol)
    }

    enum Instruction {
        case move(destination: Operand, source: Operand)
        case unary(op: UnaryOp, destination: Operand, source: Operand)
        case binary(op: BinaryOp, destination: Operand, source1: Operand, source2: Operand)
        case parameter(destination: Operand, index: Int)  // always 64-bit

        case jump(destination: String)
        case branch(destination: String, source1: Operand, source2: Operand)  // branch if equal
        case call(destination: Operand?, symbol: Resolver.Symbol, arguments: [Operand])
        case `return`(value: Operand?)

        case load(destination: Operand, symbol: Resolver.Symbol, indices: [Operand])
        case store(symbol: Resolver.Symbol, indices: [Operand], source: Operand)

        case label(name: String)
    }

    // MARK: - Generating Instructions

    func generate() -> [Resolver.Symbol: [Instruction]] {
        var instructions: [Resolver.Symbol: [Instruction]] = [:]
        for declaration in module.block.declarations {
            switch declaration {
            case .procedure(let name, let parameters, let block):
                guard let block = block else { continue }
                let symbol = resolvedSymbol(of: name)
                instructions[symbol] = makeInstructions(parameters: parameters, block: block)
            case .function(let name, let parameters, _, let block):
                guard let block = block else { continue }
                let symbol = resolvedSymbol(of: name)
                instructions[symbol] = makeInstructions(parameters: parameters, block: block)
            default: continue
            }
        }
        let symbol = resolvedSymbol(of: module.name)
        instructions[symbol] = makeInstructions(module: module)
        return instructions
    }

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
            instructions.append(.parameter(destination: .symbol(symbol), index: index))
        }
        instructions.append(contentsOf: makeInstructions(block: block))
        return instructions
    }

    func makeInstructions(block: Parser.Block) -> [Instruction] {
        var instructions: [Instruction] = []
        for declaration in block.declarations {
            // TODO: Add array initialization
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
                instructions.append(.move(destination: .symbol(symbol), source: initializerOperand))
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
            var instructions: [Instruction] = []
            if case .variable(name: let name) = target {
                let symbol = resolvedSymbol(of: name)
                instructions.append(
                    contentsOf: makeInstructions(expression: value, to: .symbol(symbol)))
            } else if case .subscript = target {
                let valueOperand = makeOperand()
                instructions.append(
                    contentsOf: makeInstructions(expression: value, to: valueOperand))

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
                    .store(symbol: symbol, indices: indexOperands, source: valueOperand))
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
            let symbol = resolvedSymbol(of: name)
            instructions.append(
                .call(destination: nil, symbol: symbol, arguments: argumentOperands))
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
            instructions.append(
                .branch(destination: elseLabel, source1: operand, source2: .constant(0)))
            instructions.append(contentsOf: makeInstructions(statements: thenBody))
            instructions.append(.jump(destination: endLabel))
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
            instructions.append(
                .branch(destination: endLabel, source1: operand, source2: .constant(0)))
            instructions.append(contentsOf: makeInstructions(statements: body))
            instructions.append(.jump(destination: conditionLabel))
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
        case .integer(let integer):
            return [.move(destination: operand, source: .constant(Int64(integer)))]
        case .longint(let longint): return [.move(destination: operand, source: .constant(longint))]
        case .boolean(let boolean):
            return [.move(destination: operand, source: .constant(boolean ? 1 : 0))]
        case .char(let char): return [.move(destination: operand, source: .constant(Int64(char)))]
        case .string(let string):
            return [.move(destination: operand, source: makeLiteral(string: string))]
        case .variable(let name):
            let symbol = resolvedSymbol(of: name)
            return [.move(destination: operand, source: .symbol(symbol))]
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
                instructions.append(
                    .branch(destination: branchLabel, source1: operand, source2: branchOperand))
                instructions.append(
                    contentsOf: makeInstructions(expression: right, to: rightOperand))
                instructions.append(
                    .binary(op: op, destination: operand, source1: rightOperand, source2: operand))
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
                case "=": op = .eq
                case "#": op = .neq
                case "<": op = .gt
                case "<=": op = .geq
                case ">": op = .lt
                case ">=": op = .leq
                default: fatalError()
                }
                instructions.append(
                    .binary(
                        op: op, destination: operand, source1: leftOperand, source2: rightOperand))
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
            instructions.append(.unary(op: op, destination: operand, source: valueOperand))
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
            instructions.append(.load(destination: operand, symbol: symbol, indices: indexOperands))
            if resolvedType(of: expression) == .boolean {
                instructions.append(
                    .binary(op: .and, destination: operand, source1: operand, source2: .constant(1))
                )
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
            let symbol = resolvedSymbol(of: name)
            instructions.append(
                .call(destination: operand, symbol: symbol, arguments: argumentOperands))
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

// MARK: - Pretty Printer

extension String {
    fileprivate func indented(with prefix: String = "  ") -> String {
        self.split(separator: "\n").map({ prefix + $0 }).joined(separator: "\n")
    }
}

func format(symbol: Resolver.Symbol, instructions: [Generator.Instruction]) -> String {
    """
    <\(symbol.token)>:
    \(instructions.map(String.init(describing:)).joined(separator: "\n"))
    """
}

extension Generator.Instruction: CustomStringConvertible {
    var description: String {
        switch self {
        case .move(let destination, let source): return "\tmov \(destination) \(source)"
        case .unary(let op, let destination, let source): return "\t\(op) \(destination) \(source)"
        case .binary(let op, let destination, let source1, let source2):
            return "\t\(op) \(destination) \(source1) \(source2)"
        case .parameter(let destination, let index): return "\tparam #\(index) \(destination)"
        case .jump(let destination): return "\tjmp .\(destination)"
        case .branch(let destination, let source1, let source2):
            return "\tbeq \(destination) \(source1) \(source2)"
        case .call(let destination, let symbol, let arguments):
            let argumentsDescription = arguments.map(String.init(describing:))
                .joined(separator: ",")
            if let destination = destination {
                return "\tcall \(destination) \(symbol.token)(\(argumentsDescription))"
            } else {
                return "\tcall \(symbol.token)(\(argumentsDescription))"
            }
        case .return(let value):
            if let value = value { return "\tret \(value)" } else { return "\tret" }
        case .load(let destination, let symbol, let indices):
            return
                "\tld \(destination) \(symbol.token)[\(indices.map(String.init(describing:)).joined(separator: ","))]"
        case .store(let symbol, let indices, let source):
            return
                "\tst \(symbol.token)[\(indices.map(String.init(describing:)).joined(separator: ","))] \(source)"
        case .label(let name): return "\(name):"
        }
    }
}

extension Generator.Operand: CustomStringConvertible {
    var description: String {
        switch self {
        case .constant(let value): return "#\(value)"
        case .temporary(let name): return "\(name)"
        case .string(let name): return "=\(name)"
        case .symbol(let symbol): return "\(symbol.token)"
        }
    }
}

extension Generator.UnaryOp: CustomStringConvertible {
    var description: String {
        switch self {
        case .neg: return "neg"
        case .pos: return "pos"
        case .not: return "not"
        }
    }
}

extension Generator.BinaryOp: CustomStringConvertible {
    var description: String {
        switch self {
        case .add: return "add"
        case .sub: return "sub"
        case .mul: return "mul"
        case .div: return "div"
        case .and: return "and"
        case .or: return "or"
        case .eq: return "eq"
        case .neq: return "neq"
        case .lt: return "lt"
        case .leq: return "leq"
        case .gt: return "gt"
        case .geq: return "geq"
        }
    }
}
