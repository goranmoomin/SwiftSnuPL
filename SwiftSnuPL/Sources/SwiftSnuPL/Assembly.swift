import Foundation

class AssemblyGenerator {
    var instructions: [Resolver.Symbol: [IRGenerator.Instruction]]
    var allocations: [String: Int64]
    var stringLiterals: [String: [UInt8]]

    init(
        instructions: [Resolver.Symbol: [IRGenerator.Instruction]], allocations: [String: Int64],
        stringLiterals: [String: [UInt8]]
    ) {
        self.instructions = instructions
        self.allocations = allocations
        self.stringLiterals = stringLiterals
    }

    func generate() -> String {
        """
        \t.text
        \(instructions.map(generate(symbol:instructions:)).joined(separator: "\n"))
        """
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
            from destinationRegisters: [String] = [], body: (() -> String)? = nil
        ) -> String {
            guard sourceOperands.count == sourceRegisters.count else { fatalError() }
            guard destinationOperands.count == destinationRegisters.count else { fatalError() }
            var assembly = ""
            for (operand, register) in zip(sourceOperands, sourceRegisters) {
                switch operand {
                case .constant(let value): assembly += "\tmov \(register), #\(value)\n"
                case .temporary, .symbol:
                    assembly += "\tldr \(register), [sp, #\(stackMapping[operand]!)] // \(operand)\n"
                case .string(let name):
                    assembly += """
                        \tadrp \(register), \(name)
                        \tadd \(register), \(register), :lo12:\(name)\n
                        """
                case .allocation: assembly += "\tadd \(register), sp, #\(stackMapping[operand]!) // \(operand)\n"
                }
            }
            if let body = body { assembly += body() }
            for (operand, register) in zip(destinationOperands, destinationRegisters) {
                switch operand {
                case .temporary, .symbol:
                    assembly += "\tstr \(register), [sp, #\(stackMapping[operand]!)] // \(operand)\n"
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
                    load: [source], to: ["x8"], store: [destination], from: ["x8"])
            case .unary(let op, let destination, let source):
                guard op == .neg else { fatalError() }
                assembly += withOperands(
                    load: [source], to: ["x8"], store: [destination], from: ["x8"]
                ) {
                    """
                    \tmov x9, xzr
                    \tsubs x8, x8, x9\n
                    """
                }
            case .binary(let op, let destination, let source1, let source2):
                guard op == .add || op == .mul else { fatalError() }
                assembly += withOperands(
                    load: [source1, source2], to: ["x8", "x9"], store: [destination], from: ["x8"]
                ) {
                    switch op {
                    case .add: return "\tadd x8, x8, x9\n"
                    case .mul: return "\tmul x8, x8, x9\n"
                    default: fatalError()
                    }
                }
            case .parameter(let destination, let index):
                guard index < 8 else { fatalError() }
                assembly += withOperands(store: [destination], from: ["x\(index)"])
            case .jump(let destination): assembly += "\tb .\(destination)\n"
            case .branch(let destination, let source1, let source2):
                assembly += withOperands(load: [source1, source2], to: ["x8", "x9"]) {
                    """
                    \tcmp x8, x9
                    \tb.eq .\(destination)\n
                    """
                }
            case .call(let destination, let symbol, let arguments):
                guard arguments.count < 8 else { fatalError() }
                let registers = arguments.enumerated().map { (index, _) in "x\(index)" }
                assembly += withOperands(
                    load: arguments, to: registers, store: destination != nil ? [destination!] : [],
                    from: destination != nil ? ["x0"] : []
                ) {
                    """
                    \tstp x29, x30, [sp, #-16]!
                    \tmov x29, sp
                    \tbl \(symbol.token.string)
                    \tldp x29, x30, [sp], #16\n
                    """
                }
            case .return(let value):
                assembly += withOperands(
                    load: value != nil ? [value!] : [], to: value != nil ? ["x0"] : []
                ) {
                    """
                    \tadd sp, sp, #\(stackSize)
                    \tret\n
                    """
                }
            case .load(let destination, let source, let size):
                assembly += withOperands(
                    load: [source], to: ["x8"], store: [destination], from: ["x8"]
                ) {
                    switch size {
                    case .byte: fatalError()
                    case .word:
                        return "\tldrsw x8, [x8]\n"
                    case .doubleWord:
                        return "\tldr x8, [x8]\n"
                    }
                }
            case .store(let source, let destination, let size):
                assembly += withOperands(load: [source, destination], to: ["x8", "x9"]) {
                    switch size {
                    case .byte: fatalError()
                    case .word:
                        return "\tstr w8, [x9]\n"
                    case .doubleWord:
                        return "\tstr x8, [x9]\n"
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
