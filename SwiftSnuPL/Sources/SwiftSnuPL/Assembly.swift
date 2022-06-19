import Foundation

class AssemblyGenerator {
    var instructions: [Resolver.Symbol: [IRGenerator.Instruction]]
    var allocations: [String: Int64]
    var stringLiterals: [String: [UInt8]]
    var globalVariables: Set<String>

    init(
        instructions: [Resolver.Symbol: [IRGenerator.Instruction]], allocations: [String: Int64],
        stringLiterals: [String: [UInt8]], globalVariables: Set<String>
    ) {
        self.instructions = instructions
        self.allocations = allocations
        self.stringLiterals = stringLiterals
        self.globalVariables = globalVariables
    }

    func generate() -> String {
        var retasm = """
            \t.text
            \(instructions.map(generate(symbol:instructions:)).joined(separator: "\n"))
            \t.macro embed_string, str
            \t.word 2f - 1f
            \t.word 1
            1:
            \t.string "\\str"
            2:
            \t.endm

            \t.data\n
            """
        for glbVar in globalVariables {
            retasm += "\t.comm \(glbVar),8\n"
        }
        func literalize(_ string: String) -> String {
            return string.replacingOccurrences(of: "\0", with: "\\0")
                .replacingOccurrences(of: "\\", with: "\\\\")
                .replacingOccurrences(of: "\t", with: "\\t")
                .replacingOccurrences(of: "\n", with: "\\n")
                .replacingOccurrences(of: "\r", with: "\\r")
                .replacingOccurrences(of: "\"", with: "\\\"")
                .replacingOccurrences(of: "\'", with: "\\'")
        }
        for (key, value) in stringLiterals {
            retasm += """
                \(key):
                \tembed_string "\(literalize(String(Array(value.map{ Character(UnicodeScalar($0)) }))))"\n
                """
        }
        return retasm
    }

    func generate(symbol: Resolver.Symbol, instructions: [IRGenerator.Instruction]) -> String {
        let operands = operands(in: instructions)
        var stackMapping: [IRGenerator.Operand: Int64] = [:]
        var stackSize: Int64 = 0
        for operand in operands {
            let size = size(of: operand)
            stackMapping[operand] = stackSize
            stackSize += size
        }
        if stackSize % 16 != 0 { stackSize += 16 - (stackSize % 16) }
        func withOperands(
            load sourceOperands: [IRGenerator.Operand] = [], to sourceRegisters: [String] = [],
            store destinationOperands: [IRGenerator.Operand] = [],
            from destinationRegisters: [String] = [], scratch scratchRegister: String,
            offset stackExpanded: Int64 = 0, body: (() -> String)? = nil
        ) -> String {
            guard
                !sourceRegisters.contains(scratchRegister)
                    && !destinationRegisters.contains(scratchRegister)
            else { fatalError() }
            guard sourceOperands.count == sourceRegisters.count else { fatalError() }
            guard destinationOperands.count == destinationRegisters.count else { fatalError() }
            var assembly = ""
            for (operand, register) in zip(sourceOperands, sourceRegisters) {
                switch operand {
                case .constant(let value): assembly += "\tmov \(register), #\(value)\n"
                case .temporary:
                    let stackOffset = stackMapping[operand]! + stackExpanded
                    if stackOffset < 256 {
                        assembly += "\tldr \(register), [sp, #\(stackOffset)] // \(operand)\n"
                    } else {
                        assembly += """
                            \tadd \(register), sp, #\(stackOffset)
                            \tldr \(register), [\(register)] // \(operand)\n
                            """
                    }
                case .symbol(let symbol):
                    let stackOffset = stackMapping[operand]! + stackExpanded
                    if symbol.isGlobal {
                        assembly += """
                            \tadrp \(register), \(symbol.token.string)
                            \tadd \(register), \(register), :lo12:\(symbol.token.string)
                            \tldr \(register), [\(register)] // \(symbol.token.string)\n
                            """
                    } else {
                        if stackOffset < 256 {
                            assembly += "\tldr \(register), [sp, #\(stackOffset)] // \(operand)\n"
                        } else {
                            assembly += """
                                \tadd \(register), sp, #\(stackOffset)
                                \tldr \(register), [\(register)] // \(operand)\n
                                """
                        }
                    }
                case .string(let name):
                    assembly += """
                        \tadrp \(register), \(name)
                        \tadd \(register), \(register), :lo12:\(name)\n
                        """
                case .allocation:
                    assembly += "\tadd \(register), sp, #\(stackMapping[operand]!) // \(operand)\n"
                }
            }
            if let body = body { assembly += body() }
            for (operand, register) in zip(destinationOperands, destinationRegisters) {
                switch operand {
                case .temporary:
                    let stackOffset = stackMapping[operand]! + stackExpanded
                    if stackOffset < 256 {
                        assembly += "\tstr \(register), [sp, #\(stackOffset)] // \(operand)\n"
                    } else {
                        assembly += """
                            \tadd \(scratchRegister), sp, #\(stackOffset)
                            \tstr \(register), [\(scratchRegister)] // \(operand)\n
                            """
                    }
                case .symbol(let symbol):
                    let stackOffset = stackMapping[operand]! + stackExpanded
                    if symbol.isGlobal {
                        assembly += """
                            \tadrp \(scratchRegister), \(symbol.token.string)
                            \tadd \(scratchRegister), \(scratchRegister), :lo12:\(symbol.token.string)
                            \tstr \(register), [\(scratchRegister)]\n
                            """
                    } else {
                        if stackOffset < 256 {
                            assembly += "\tstr \(register), [sp, #\(stackOffset)] // \(operand)\n"
                        } else {
                            assembly += """
                                \tadd \(scratchRegister), sp, #\(stackOffset)
                                \tstr \(register), [\(scratchRegister)] // \(operand)\n
                                """
                        }
                    }
                default: break
                }
            }
            return assembly
        }
        var assembly = """
            \t.globl \(symbol.token.string)
            \(symbol.token.string):
            \tsub sp, sp, #\(stackSize)\n
            """
        for instruction in instructions {
            switch instruction {
            case .move(let destination, let source):
                assembly += withOperands(
                    load: [source], to: ["x8"], store: [destination], from: ["x8"], scratch: "x10")
            case .unary(let op, let destination, let source):
                guard op == .neg else { fatalError() }
                assembly += withOperands(
                    load: [source], to: ["x8"], store: [destination], from: ["x8"], scratch: "x10"
                ) {
                    """
                    \tmov x9, xzr
                    \tsubs x8, x8, x9\n
                    """
                }
            case .binary(let op, let destination, let source1, let source2):
                guard op == .add || op == .mul || op == .eq || op == .gt else { fatalError() }
                assembly += withOperands(
                    load: [source1, source2], to: ["x8", "x9"], store: [destination], from: ["x8"],
                    scratch: "x10"
                ) {
                    switch op {
                    case .add: return "\tadd x8, x8, x9\n"
                    case .sub: fatalError()
                    case .mul: return "\tmul x8, x8, x9\n"
                    case .div: fatalError()
                    case .and: fatalError()
                    case .or: fatalError()

                    case .eq:
                        return """
                            \tcmp x9, x8
                            \tcset x8, eq\n
                            """
                    case .neq:
                        return """
                            \tcmp x9, x8
                            \tcset x8, ne\n
                            """
                    case .lt:
                        return """
                            \tcmp x9, x8
                            \tcset x8, lt\n
                            """
                    case .leq:
                        return """
                            \tcmp x9, x8
                            \tcset x8, le\n
                            """
                    case .gt:
                        return """
                            \tcmp x9, x8
                            \tcset x8, gt\n
                            """
                    case .geq:
                        return """
                            \tcmp x9, x8
                            \tcset x8, ge\n
                            """
                    }
                }
            case .parameter(let destination, let index):
                // guard index < 8 else { fatalError() }
                if index < 8 {
                    assembly += withOperands(
                        store: [destination], from: ["x\(index)"], scratch: "x8")
                } else {
                    assembly += """
                        \tadd x8, sp, #\(stackSize + Int64(8 * (index - 8)))
                        \tldr x8, [x8]\n
                        """
                    assembly += withOperands(store: [destination], from: ["x8"], scratch: "x9")
                }
            case .jump(let destination): assembly += "\tb .\(destination)\n"
            case .branch(let destination, let source1, let source2):
                assembly += withOperands(
                    load: [source1, source2], to: ["x8", "x9"], scratch: "x10"
                ) {
                    """
                    \tcmp x8, x9
                    \tb.eq .\(destination)\n
                    """
                }
            case .call(let destination, let symbol, let arguments):
                // guard arguments.count < 8 else { fatalError() }
                let registers = arguments.prefix(8).enumerated().map { (index, _) in "x\(index)" }
                assembly += withOperands(
                    load: Array(arguments.prefix(8)), to: registers, scratch: "x8"
                ) {
                    """
                    \tstp x29, x30, [sp, #-16]!
                    \tmov x29, sp\n
                    """
                }
                if arguments.count > 8 {
                    let extraArgumentsSize =
                        8 * ((arguments.count % 2 == 1 ? arguments.count + 1 : arguments.count) - 8)
                    assembly += "\tadd sp, sp, #-\(extraArgumentsSize)\n"
                    for i in 8..<arguments.count {
                        assembly += withOperands(
                            load: [arguments[i]], to: ["x8"], scratch: "x9",
                            offset: Int64(16 + extraArgumentsSize)
                        ) {
                            if 8 * (i - 8) < 256 {
                                return "\tstr x8, [sp, #\(8 * (i-8))]\n"
                            } else {
                                return """
                                    \tadd x9, sp, #\(8 * (i-8))
                                    \tstr x8, x9\n
                                    """
                            }
                        }
                    }
                    assembly += """
                        \tbl \(symbol.token.string)
                        \tadd sp, sp, #\(extraArgumentsSize)
                        \tldp x29, x30, [sp], #16\n
                        """
                } else {
                    assembly += """
                        \tbl \(symbol.token.string)
                        \tldp x29, x30, [sp], #16\n
                        """
                }

                assembly += withOperands(
                    store: destination != nil ? [destination!] : [],
                    from: destination != nil ? ["x0"] : [], scratch: "x8")
            case .return(let value):
                assembly += withOperands(
                    load: value != nil ? [value!] : [], to: value != nil ? ["x0"] : [],
                    scratch: "x10"
                ) {
                    """
                    \tadd sp, sp, #\(stackSize)
                    \tret\n
                    """
                }
            case .load(let destination, let source, let size):
                assembly += withOperands(
                    load: [source], to: ["x8"], store: [destination], from: ["x8"], scratch: "x9"
                ) {
                    switch size {
                    case .byte: return "\tldrb w8, [x8]\n"
                    case .word: return "\tldrsw x8, [x8]\n"
                    case .doubleWord: return "\tldr x8, [x8]\n"
                    }
                }
            case .store(let source, let destination, let size):
                assembly += withOperands(
                    load: [source, destination], to: ["x8", "x9"], scratch: "x10"
                ) {
                    switch size {
                    case .byte: return "\tstrb w8, [x9]\n"
                    case .word: return "\tstr w8, [x9]\n"
                    case .doubleWord: return "\tstr x8, [x9]\n"
                    }
                }
            case .label(let name): assembly += ".\(name):\n"
            }
        }
        assembly += """
            \tadd sp, sp, #\(stackSize)
            \tret\n
            """
        return assembly
    }

    func size(of operand: IRGenerator.Operand) -> Int64 {
        switch operand {
        case .constant, .string: return 0
        case .temporary, .symbol: return 8
        case .allocation(let name): return allocations[name]!
        }
    }

    func operands(in instruction: IRGenerator.Instruction) -> Set<IRGenerator.Operand> {
        switch instruction {
        case .move(let destination, let source), .unary(_, let destination, let source):
            return [source, destination]
        case .binary(_, let destination, let source1, let source2):
            return [source1, source2, destination]
        case .parameter(let destination, _): return [destination]
        case .jump: return []
        case .branch(_, let source1, let source2): return [source1, source2]
        case .call(let destination, _, let arguments):
            var operands: Set<IRGenerator.Operand> = []
            for argument in arguments { operands.insert(argument) }
            if let destination = destination { operands.insert(destination) }
            return operands
        case .return(let value): if let value = value { return [value] } else { return [] }
        case .load(let destination, let source, _), .store(let source, let destination, _):
            return [source, destination]
        case .label: return []
        }
    }

    func operands(in instructions: [IRGenerator.Instruction]) -> Set<IRGenerator.Operand> {
        var operands: Set<IRGenerator.Operand> = []
        for instruction in instructions { operands.formUnion(self.operands(in: instruction)) }
        return operands
    }
}
