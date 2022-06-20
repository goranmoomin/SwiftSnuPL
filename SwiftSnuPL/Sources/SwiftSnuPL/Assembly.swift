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
        var assembly = """
            \t.text
            \(instructions.map(generate(symbol:instructions:)).joined(separator: "\n"))
            \t.data\n
            """
        for globalVariable in globalVariables { assembly += "\t.comm \(globalVariable),8\n" }
        for (key, value) in stringLiterals {
            assembly += """
                \(key):
                \t.word \(value.count + 1)
                \t.word 1\n
                """
            for char in value { assembly += "\t.byte \(char)\n" }
            assembly += "\t.byte 0\n"
        }
        return assembly
    }
    func embedImm(imm: Int64, scratch scratchRegister: String) -> String {
        if imm < 0 {
            return """
                \tmov \(scratchRegister), #\(imm % (1 << 16))
                \tmovk \(scratchRegister), #\((imm >> 16) % (1 << 16)), LSL 16
                \tmovk \(scratchRegister), #\((imm >> 32) % (1 << 16)), LSL 32
                \tmovk \(scratchRegister), #\((imm >> 48) % (1 << 16)), LSL 48
                """
        } else {
            var retasm = "\tmov \(scratchRegister), #\(imm % (1 << 16))\n"
            if imm >= (1 << 16) { retasm += "\tmovk \(scratchRegister), #\((imm >> 16) % (1 << 16)), LSL 16\n" }
            if imm >= (1 << 32) { retasm += "\tmovk \(scratchRegister), #\((imm >> 32) % (1 << 16)), LSL 32\n" }
            if imm >= (1 << 48) { retasm += "\tmovk \(scratchRegister), #\((imm >> 48) % (1 << 16)), LSL 48\n" }
            return retasm
        }
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
            store destinationOperands: [IRGenerator.Operand] = [], from destinationRegisters: [String] = [],
            scratch scratchRegister: String, offset stackExpanded: Int64 = 0, body: (() -> String)? = nil
        ) -> String {
            guard !sourceRegisters.contains(scratchRegister) && !destinationRegisters.contains(scratchRegister) else {
                fatalError()
            }
            guard sourceOperands.count == sourceRegisters.count else { fatalError() }
            guard destinationOperands.count == destinationRegisters.count else { fatalError() }
            var assembly = ""
            for (operand, register) in zip(sourceOperands, sourceRegisters) {
                switch operand {
                case .constant(let value):
                    if 0 <= value && value < 4096 {
                        assembly += "\tmov \(register), #\(value)\n"
                    } else {
                        assembly += embedImm(imm: value, scratch: "x15") + "\tmov \(register), x15\n"
                    }
                case .temporary:
                    let stackOffset = stackMapping[operand]! + stackExpanded
                    if stackOffset < 256 {
                        assembly += "\tldr \(register), [sp, #\(stackOffset)] // \(operand)\n"
                    } else if stackOffset < 4096 {
                        assembly += """
                            \tadd \(register), sp, #\(stackOffset)
                            \tldr \(register), [\(register)] // \(operand)\n
                            """
                    } else {
                        assembly += embedImm(imm: stackOffset, scratch: "x15")
                        assembly += """
                            \tadd \(register), sp, x15
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
                        } else if stackOffset < 4096 {
                            assembly += """
                                \tadd \(register), sp, #\(stackOffset)
                                \tldr \(register), [\(register)] // \(operand)\n
                                """
                        } else {
                            assembly += embedImm(imm: stackOffset, scratch: "x15")
                            assembly += """
                                \tadd \(register), sp, x15
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
                    let value = stackMapping[operand]!
                    if 0 <= value && value < 4096 {
                        assembly += "\tadd \(register), sp, #\(value) // \(operand)\n"
                    } else {
                        assembly += embedImm(imm: value, scratch: "x15") + "\tadd \(register), sp, x15 // \(operand)\n"
                    }
                }
            }
            if let body = body { assembly += body() }
            for (operand, register) in zip(destinationOperands, destinationRegisters) {
                switch operand {
                case .temporary:
                    let stackOffset = stackMapping[operand]! + stackExpanded
                    if stackOffset < 256 {
                        assembly += "\tstr \(register), [sp, #\(stackOffset)] // \(operand)\n"
                    } else if stackOffset < 4096 {
                        assembly += """
                            \tadd \(scratchRegister), sp, #\(stackOffset)
                            \tstr \(register), [\(scratchRegister)] // \(operand)\n
                            """
                    } else {
                        assembly += embedImm(imm: stackOffset, scratch: "x15")
                        assembly += """
                            \tadd \(scratchRegister), sp, x15
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
                        } else if stackOffset < 4096 {
                            assembly += """
                                \tadd \(scratchRegister), sp, #\(stackOffset)
                                \tstr \(register), [\(scratchRegister)] // \(operand)\n
                                """
                        } else {
                            assembly += embedImm(imm: stackOffset, scratch: "x15")
                            assembly += """
                                \tadd \(scratchRegister), sp, x15
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
            """
        if stackSize < 4096 {
            assembly += "\tsub sp, sp, #\(stackSize)\n"
        } else {
            assembly += embedImm(imm: stackSize, scratch: "x15") + "\tsub sp, sp, x15\n"
        }
        for instruction in instructions {
            switch instruction {
            case .move(let destination, let source):
                assembly += withOperands(load: [source], to: ["x8"], store: [destination], from: ["x8"], scratch: "x10")
            case .unary(let op, let destination, let source):
                assembly += withOperands(load: [source], to: ["x8"], store: [destination], from: ["x8"], scratch: "x10")
                {
                    switch op {
                    case .neg:
                        return """
                            \tmov x9, xzr
                            \tsubs x8, x8, x9\n
                            """
                    case .not:
                        return """
                            \tcmp x8, xzr
                            \tcset x8, eq\n
                            """
                    default: fatalError()
                    }
                }
            case .binary(let op, let destination, let source1, let source2):
                assembly += withOperands(
                    load: [source1, source2], to: ["x8", "x9"], store: [destination], from: ["x8"], scratch: "x10"
                ) {
                    switch op {
                    case .add: return "\tadd x8, x8, x9\n"
                    case .sub:
                        return """
                            \tmov x10, xzr
                            \tsubs x9, x10, x9
                            \tadd x8, x8, x9\n
                            """
                    case .mul: return "\tmul x8, x8, x9\n"
                    case .div: return "\tsdiv x8, x8, x9\n"
                    case .and: return "\tand x8, x8, x9\n"
                    case .or: return "\torr x8, x8, x9\n"
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
                    assembly += withOperands(store: [destination], from: ["x\(index)"], scratch: "x8")
                } else {
                    let offset = stackSize + Int64(8 * (index - 8))
                    if offset < 4096 {
                        assembly += """
                            \tadd x8, sp, #\(offset)
                            \tldr x8, [x8]\n
                            """
                    } else {
                        assembly += embedImm(imm: offset, scratch: "x15")
                        assembly += """
                            \tadd x8, sp, x15
                            \tldr x8, [x8]\n
                            """
                    }
                    assembly += withOperands(store: [destination], from: ["x8"], scratch: "x9")
                }
            case .jump(let destination): assembly += "\tb .\(destination)\n"
            case .branch(let destination, let source1, let source2):
                assembly += withOperands(load: [source1, source2], to: ["x8", "x9"], scratch: "x10") {
                    """
                    \tcmp x8, x9
                    \tb.eq .\(destination)\n
                    """
                }
            case .call(let destination, let symbol, let arguments):
                // guard arguments.count < 8 else { fatalError() }
                let registers = arguments.prefix(8).enumerated().map { (index, _) in "x\(index)" }
                assembly += withOperands(load: Array(arguments.prefix(8)), to: registers, scratch: "x8") {
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
                        let offsetLoadAsm = embedImm(imm: Int64(8 * (i - 8)), scratch: "x15")
                        assembly += withOperands(
                            load: [arguments[i]], to: ["x8"], scratch: "x9", offset: Int64(16 + extraArgumentsSize)
                        ) {
                            if 8 * (i - 8) < 256 {
                                return "\tstr x8, [sp, #\(8 * (i-8))]\n"
                            } else if (8 * (i - 8)) < 4096 {
                                return """
                                    \tadd x9, sp, #\(8 * (i-8))
                                    \tstr x8, x9\n
                                    """
                            } else {
                                return offsetLoadAsm + """
                                    \tadd x9, sp, x15
                                    \tstr x8, x9\n
                                    """
                            }
                        }
                    }
                    if extraArgumentsSize < 4096 {
                        assembly += """
                            \tbl \(symbol.token.string)
                            \tadd sp, sp, #\(extraArgumentsSize)
                            \tldp x29, x30, [sp], #16\n
                            """
                    } else {
                        assembly += """
                            \tbl \(symbol.token.string)
                            \tmov x15, xzr
                            \tmovk x15, #\((extraArgumentsSize >> 16) % (1 << 16)), LSL 16
                            \tmovk x15, #\(extraArgumentsSize % (1 << 16))
                            \tadd sp, sp, x15
                            \tldp x29, x30, [sp], #16\n
                            """
                    }
                } else {
                    assembly += """
                        \tbl \(symbol.token.string)
                        \tldp x29, x30, [sp], #16\n
                        """
                }

                assembly += withOperands(
                    store: destination != nil ? [destination!] : [], from: destination != nil ? ["x0"] : [],
                    scratch: "x8")
            case .return(let value):
                let offsetLoadAsm = embedImm(imm: stackSize, scratch: "x15")
                assembly += withOperands(
                    load: value != nil ? [value!] : [], to: value != nil ? ["x0"] : [], scratch: "x10"
                ) {
                    if stackSize < 4096 {
                        return """
                            \tadd sp, sp, #\(stackSize)
                            \tret\n
                            """
                    } else {
                        return offsetLoadAsm + """
                            \tadd sp, sp, x15
                            \tret\n
                            """
                    }
                }
            case .load(let destination, let source, let size):
                assembly += withOperands(load: [source], to: ["x8"], store: [destination], from: ["x8"], scratch: "x9")
                {
                    switch size {
                    case .byte: return "\tldrb w8, [x8]\n"
                    case .word: return "\tldrsw x8, [x8]\n"
                    case .doubleWord: return "\tldr x8, [x8]\n"
                    }
                }
            case .store(let source, let destination, let size):
                assembly += withOperands(load: [source, destination], to: ["x8", "x9"], scratch: "x10") {
                    switch size {
                    case .byte: return "\tstrb w8, [x9]\n"
                    case .word: return "\tstr w8, [x9]\n"
                    case .doubleWord: return "\tstr x8, [x9]\n"
                    }
                }
            case .label(let name): assembly += ".\(name):\n"
            }
        }
        if stackSize < 4096 {
            assembly += """
                \tadd sp, sp, #\(stackSize)
                \tret\n
                """
        } else {
            assembly +=
                embedImm(imm: stackSize, scratch: "x15") + """
                \tadd sp, sp, x15
                \tret\n
                """
        }
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
        case .move(let destination, let source), .unary(_, let destination, let source): return [source, destination]
        case .binary(_, let destination, let source1, let source2): return [source1, source2, destination]
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
