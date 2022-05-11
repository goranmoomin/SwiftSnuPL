import Foundation

class Resolver {
    var scopes: [Scope] = []
    var resolvedTypes: [Parser.Expression: `Type`] = [:]
    var resolvedSymbols: [Token: Symbol] = [:]

    var hasError: Bool { errors.count > 0 }
    var errors: [ResolveError] = []

    var module: Parser.Module

    init(module: Parser.Module) { self.module = module }

    // MARK: - Types

    indirect enum `Type`: Equatable, Hashable {
        case boolean
        case char
        case integer
        case longint
        case array(base: `Type`?, size: Int32?)  // base is only nil if array is untyped
        case procedure(parameters: [`Type`])
        case function(parameters: [`Type`], `return`: `Type`)

        var isScalar: Bool {
            switch self {
            case .boolean, .char, .integer, .longint: return true
            default: return false
            }
        }

        func isAssignable(to target: Self) -> Bool { target.isScalar && isConvertible(to: target) }

        func isConvertible(to target: Self) -> Bool {
            switch self {
            case .boolean, .char, .integer, .longint: return self == target
            // disable integer widening for now
            // case .integer: return target == .integer || target == .longint
            case .procedure, .function: return false
            case .array(let base, let size):
                guard case .array(let targetBase, let targetSize) = target else { return false }
                // allow passing any array to an untyped array (if size allows)
                // TODO: Add size check
                guard let targetBase = targetBase else { return true }
                // can't pass an untyped array to a typed array
                guard let base = base else { return false }
                // can pass an array to an open array, or a smaller array of an exact type
                // to allow converting between arrays, indexing must check and use the right instruction to load the right amout of data from the pointer
                guard base.isConvertible(to: targetBase) else { return false }
                // base is convertible to target base
                guard let targetSize = targetSize else { return true }
                // TODO: Check if passing to a smaller array is possible
                return size == targetSize
            }
        }
    }

    enum Symbol: Equatable, Hashable {
        case `var`(token: Token, type: `Type`)
        case const(token: Token, type: `Type`, initializer: AnyHashable)

        var token: Token {
            switch self {
            case .`var`(let token, _): return token
            case .const(let token, _, _): return token
            }
        }

        var type: `Type` {
            switch self {
            case .`var`(_, let type): return type
            case .const(_, let type, _): return type
            }
        }
    }

    struct Scope {
        var symbols: Set<Symbol>
        let `return`: `Type`?

        mutating func addVar(token: Token, type: `Type`) throws {
            guard !symbols.contains(where: { $0.token.string != token.string }) else {
                throw ResolveError(
                    kind: .symbolError, message: "cannot redefine \(token.string) in current scope."
                )
            }
            symbols.insert(.`var`(token: token, type: type))
        }

        mutating func addConst(token: Token, type: `Type`, initializer: AnyHashable) throws {
            guard !symbols.contains(where: { $0.token.string != token.string }) else {
                throw ResolveError(
                    kind: .symbolError, message: "cannot redefine \(token.string) in current scope."
                )
            }
            symbols.insert(.const(token: token, type: type, initializer: initializer))
        }

        func findSymbol(named name: String) -> Symbol? {
            symbols.first(where: { $0.token.string == name })
        }
    }

    // MARK: - Error handling

    struct ResolveError: Error {
        enum Kind {
            case typeError
            case symbolError
            case evaluationError
        }
        let kind: Kind
        let token: Token
        let message: String
    }

    // MARK: - Comptime evaluation

    func evaluate<T: Hashable>(expression: Parser.Expression, as type: T.Type) throws -> T {
        try evaluate(expression: expression) as! T
    }

    func evaluate(expression: Parser.Expression) throws -> AnyHashable {
        // TODO: Implement everything
        switch expression {
        case .unary(let `operator`, let value):
            let type = resolvedType(of: value)
            if type == .integer {
                switch `operator`.string {
                case "+": return try evaluate(expression: value, as: Int32.self)
                case "-": return try -evaluate(expression: value, as: Int32.self)
                default: fatalError()
                }
            } else if type == .longint {
                switch `operator`.string {
                case "+": return try evaluate(expression: value, as: Int64.self)
                case "-": return try -evaluate(expression: value, as: Int64.self)
                default: fatalError()
                }
            } else if type == .boolean {
                guard `operator`.string == "!" else { fatalError() }
                return try !evaluate(expression: value, as: Bool.self)
            } else {
                fatalError()
            }
        case .binary(let `operator`, let left, let right):
            let leftType = resolvedType(of: left)
            let rightType = resolvedType(of: right)

            // implement short-circuiting, though compile time evaluation won't cause any side effects
            if `operator`.string == "&&" {
                guard leftType == .boolean && rightType == .boolean else { fatalError() }
                if try evaluate(expression: left, as: Bool.self) {
                    return try evaluate(expression: right, as: Bool.self)
                } else {
                    return false
                }
            } else if `operator`.string == "||" {
                guard leftType == .boolean && rightType == .boolean else { fatalError() }
                if try evaluate(expression: left, as: Bool.self) {
                    return true
                } else {
                    return try evaluate(expression: right, as: Bool.self)
                }
            }

            func arithmetic<T: BinaryInteger>(as type: T.Type) throws -> T {
                let leftValue = try evaluate(expression: left, as: T.self)
                let rightValue = try evaluate(expression: right, as: T.self)
                // TODO: handle overflows that happen in compile time
                switch `operator`.string {
                case "+": return leftValue + rightValue
                case "-": return leftValue - rightValue
                case "*": return leftValue * rightValue
                case "/":
                    if rightValue == 0 {
                        throw ResolveError(
                            kind: .evaluationError, token: right.token, message: "division by zero is not allowed.")
                    }
                    return leftValue / rightValue
                default: fatalError()
                }
            }

            func equality<T: Equatable & Hashable>(as type: T.Type) throws -> Bool {
                let leftValue = try evaluate(expression: left, as: T.self)
                let rightValue = try evaluate(expression: right, as: T.self)
                switch `operator`.string {
                case "=": return leftValue == rightValue
                case "#": return leftValue != rightValue
                default: fatalError()
                }
            }

            func relational<T: Comparable & Hashable>(as type: T.Type) throws -> Bool {
                let leftValue = try evaluate(expression: left, as: T.self)
                let rightValue = try evaluate(expression: right, as: T.self)
                switch `operator`.string {
                case "<": return leftValue < rightValue
                case "<=": return leftValue <= rightValue
                case ">": return leftValue > rightValue
                case ">=": return leftValue >= rightValue
                default: fatalError()
                }
            }

            if leftType == .integer && rightType == .integer {
                switch `operator`.string {
                case "+", "-", "*", "/": return try arithmetic(as: Int32.self)
                case "=", "#": return try equality(as: Int32.self)
                case "<", "<=", ">=", ">": return try relational(as: Int32.self)
                default: fatalError()
                }
            } else if leftType == .longint && rightType == .longint {
                switch `operator`.string {
                case "+", "-", "*", "/": return try arithmetic(as: Int64.self)
                case "=", "#": return try equality(as: Int64.self)
                case "<", "<=", ">=", ">": return try relational(as: Int64.self)
                default: fatalError()
                }
            } else if leftType == .char && rightType == .char {
                switch `operator`.string {
                case "=", "#": return try equality(as: UInt8.self)
                case "<", "<=", ">=", ">": return try relational(as: UInt8.self)
                default: fatalError()
                }
            } else if leftType == .boolean && rightType == .boolean {
                switch `operator`.string {
                case "=", "#": return try equality(as: Bool.self)
                default: fatalError()
                }
            } else {
                fatalError()
            }
        case .subscript(let array, let index):
            let arrayValue = try evaluate(expression: array, as: [AnyHashable].self)
            let indexValue = try evaluate(expression: index, as: Int32.self)
            if arrayValue.count <= indexValue {
                throw ResolveError(
                    kind: .evaluationError, token: array.token, message: "size of array is shorter than index.")
            }
            return arrayValue[Int(indexValue)]
        case .call(let function, _):
            throw ResolveError(
                kind: .evaluationError, token: function.token, message: "cannot evaluate function call at compile time.")
        case .variable(let name):
            guard case let .const(_, _, initializer) = resolvedSymbol(of: name) else {
                throw ResolveError(
                    kind: .evaluationError,
                    token: name.token,
                    message: "cannot evaluate non-const \(name) at compile time.")
            }
            return initializer
        case .integer(let integer, _): return integer
        case .longint(let longint, _): return longint
        case .boolean(let boolean, _): return boolean
        case .char(let char, _): return char
        case .string(let string, _): return string
        }
    }

    func evaluate(type: Parser.`Type`) throws -> `Type` {
        switch type {
        case .boolean: return .boolean
        case .char: return .char
        case .integer: return .integer
        case .longint: return .longint
        case .array(let base, let size):
            let sizeValue: Int32?
            if let size = size {
                try resolve(expression: size)
                guard resolvedType(of: size) == .integer else {
                    throw ResolveError(
                        kind: .typeError, token: size.token, message: "the size of an array must be an integer.")
                }
                sizeValue = try evaluate(expression: size, as: Int32.self)
            } else {
                sizeValue = nil
            }
            return .array(base: try evaluate(type: base), size: sizeValue)
        }
    }

    // MARK: - Resolving symbols

    func withScope<T>(_ scope: Scope, _ body: () throws -> T) rethrows -> T {
        scopes.append(scope)
        defer { scopes.removeLast() }
        return try body()
    }

    func resolve() throws {
        func builtin(withSignature signature: StaticString) -> Symbol {
            let signature = "\(signature)"
            let leftIndex = signature.firstIndex(of: "(")!
            let rightIndex = signature.lastIndex(of: ")")!
            let name = signature[signature.startIndex..<leftIndex]
            let token = Token(
                id: "builtin:\(name)", kind: .ident, string: String(name), lineNumber: 0,
                charPosition: 0)
            let parameters = signature[signature.index(after: leftIndex)..<rightIndex]
                .split(separator: " ")
            let `return` = signature[signature.index(after: rightIndex)..<signature.endIndex]
            var parameterTypes: [`Type`] = []
            for parameter in parameters {
                var type: `Type`?
                var index = parameter.startIndex
                while index != parameter.endIndex {
                    let character = parameter[index]
                    switch character {
                    case "_": type = nil
                    case "I": type = .integer
                    case "L": type = .longint
                    case "C": type = .char
                    case "B": type = .boolean
                    case "[":
                        index = parameter.index(after: index)
                        let sizeIndex = index
                        while parameter[index] != "]" { index = parameter.index(after: index) }
                        let size: Int32? =
                            sizeIndex < index ? Int32(parameter[sizeIndex..<index])! : nil
                        type = .array(base: type, size: size)
                    default: fatalError()
                    }
                    index = parameter.index(after: index)
                }
                parameterTypes.append(type!)
            }
            let type: `Type`
            switch `return` {
            case "": type = .procedure(parameters: parameterTypes)
            case "I": type = .function(parameters: parameterTypes, return: .integer)
            case "L": type = .function(parameters: parameterTypes, return: .longint)
            case "C": type = .function(parameters: parameterTypes, return: .char)
            case "B": type = .function(parameters: parameterTypes, return: .boolean)
            default: fatalError()
            }
            return .`var`(token: token, type: type)
        }

        let globalSymbols: Set<Symbol> = [
            builtin(withSignature: "DIM(_[] I)I"), builtin(withSignature: "DOFS(_[])I"),
            builtin(withSignature: "ReadInt()I"), builtin(withSignature: "ReadLong()L"),
            builtin(withSignature: "WriteInt(I)"), builtin(withSignature: "WriteLong(L)"),
            builtin(withSignature: "WriteChar(C)"), builtin(withSignature: "WriteStr(C[])"),
            builtin(withSignature: "WriteLn()"),
        ]

        resolve(block: module.block, symbols: globalSymbols, return: nil)
    }

    func resolve(block: Parser.Block, symbols: Set<Symbol> = [], return: `Type`?) {
        var scope = Scope(symbols: symbols, return: `return`)
        // TODO: Check if declarations do not clash
        for declaration in block.declarations {
            do {
                switch declaration {
                case .`var`(let name, let type):
                    try scope.addVar(
                        token: name, type: try withScope(scope) { try evaluate(type: type) })
                case .const(let name, let type, let initializer):
                    // TODO: Check initializer type
                    let initializerValue: AnyHashable = try withScope(scope) {
                        try resolve(expression: initializer)
                        return try evaluate(expression: initializer)
                    }
                    try scope.addConst(
                        token: name, type: try withScope(scope) { try evaluate(type: type) },
                        initializer: initializerValue)
                case .procedure(let name, let parameters, let block):
                    // TODO: Check if type evaluation requires scope
                    let parameterTypes = try withScope(scope) {
                        try parameters.map(\.type).map(evaluate(type:))
                    }
                    try scope.addVar(token: name, type: .procedure(parameters: parameterTypes))
                    if let block = block {
                        var symbols: Set<Symbol> = []
                        // TODO: Add itself to symbols
                        // TODO: Check if parameters do not clash
                        for (parameter, type) in zip(parameters, parameterTypes) {
                            symbols.insert(.`var`(token: parameter.name, type: type))
                        }
                        withScope(scope) { resolve(block: block, symbols: symbols, return: nil) }
                    }
                case .function(let name, let parameters, let `return`, let block):
                    // TODO: Check if type evaluation requires scope
                    let parameterTypes = try withScope(scope) {
                        try parameters.map(\.type).map(evaluate(type:))
                    }
                    let returnType = try withScope(scope) { try evaluate(type: `return`) }
                    try scope.addVar(
                        token: name, type: .function(parameters: parameterTypes, return: returnType)
                    )
                    if let block = block {
                        var symbols: Set<Symbol> = []
                        // TODO: Add itself to symbols
                        // TODO: Check if parameters do not clash
                        for (parameter, type) in zip(parameters, parameterTypes) {
                            symbols.insert(.`var`(token: parameter.name, type: type))
                        }
                        withScope(scope) {
                            resolve(block: block, symbols: symbols, return: returnType)
                        }
                    }
                }
            } catch let error as ResolveError { errors.append(error) } catch { fatalError() }
        }
        // TODO: Check if return statement exists
        withScope(scope) { resolve(statements: block.body) }
    }

    func resolve(statements: [Parser.Statement]) {
        for statement in statements {
            do { try resolve(statement: statement) } catch let error as ResolveError {
                errors.append(error)
            } catch { fatalError() }

        }
    }

    func resolve(statement: Parser.Statement) throws {
        switch statement {
        case .assignment(let target, let value):
            try resolve(expression: value)
            try resolve(expression: target)

            // the only assignable target is a subscript or a bare variable
            var variable = target
            while case .subscript(let array, index: _) = variable { variable = array }
            guard case .variable(let name) = variable else { fatalError() }

            let symbol = resolvedSymbol(of: name)
            guard case .var = symbol else {
                throw ResolveError(kind: .typeError, token: target.token, message: "constant \(name) is not assignable.")
            }

            let valueType = resolvedType(of: value)
            let targetType = resolvedType(of: target)
            guard valueType.isAssignable(to: targetType) else {
                throw ResolveError(
                    kind: .typeError,
                    token: value.token,
                    message:
                        "\(format(type: valueType)) is not assignable to \(format(type: targetType))."
                )
            }
        case .call(let procedure, let arguments):
            try resolve(expression: procedure)
            for argument in arguments { try resolve(expression: argument) }
            let procedureType = resolvedType(of: procedure)
            guard case .procedure(let parameterTypes) = procedureType else {
                throw ResolveError(
                    kind: .typeError,
                    token: procedure.token,
                    message: "cannot call \(format(type: procedureType)) as a procedure.")
            }

            guard parameterTypes.count == arguments.count else {
                throw ResolveError(
                    kind: .typeError,
                    token: procedure.token,
                    message:
                        "procedure requires exactly \(parameterTypes.count) arguments, but found \(arguments.count)."
                )
            }
            for (parameterType, argument) in zip(parameterTypes, arguments) {
                let argumentType = resolvedType(of: argument)
                guard argumentType.isConvertible(to: parameterType) else {
                    throw ResolveError(
                        kind: .typeError,
                        token: argument.token,
                        message:
                            "argument \(format(type: argumentType)) is not convertible to parameter \(format(type: parameterType))."
                    )
                }
            }
        case .if(let condition, let thenBody, let elseBody):
            try resolve(expression: condition)
            resolve(statements: thenBody)
            resolve(statements: elseBody)
            let conditionType = resolvedType(of: condition)
            guard conditionType == .boolean else {
                throw ResolveError(
                    kind: .typeError,
                    token: condition.token,
                    message:
                        "expected boolean for if condition, but found \(format(type: conditionType))."
                )
            }
        case .while(let condition, let body):
            try resolve(expression: condition)
            resolve(statements: body)
            let conditionType = resolvedType(of: condition)
            guard conditionType == .boolean else {
                throw ResolveError(
                    kind: .typeError,
                    token: condition.token,
                    message:
                        "expected boolean for while condition, but found \(format(type: conditionType))."
                )
            }
        case .return(let value):
            if let value = value {
                try resolve(expression: value)
                let type = resolvedType(of: value)
                guard type == scopes.last!.return else {
                    if let returnType = scopes.last!.return {
                        throw ResolveError(
                            kind: .typeError,
                            token: value.token,
                            message:
                                "expected \(format(type: returnType)) return value, but found \(format(type: type)))."
                        )
                    } else {
                        throw ResolveError(
                            kind: .typeError,
                            token: value.token,
                            message: "expected no return value, but found \(format(type: type))).")
                    }
                }
            } else {
                guard scopes.last!.return == nil else {
                    throw ResolveError(
                        kind: .typeError,
                        message:
                            "expected \(format(type: scopes.last!.return!)) return value, but found no value."
                    )
                }
            }
        }
    }

    func resolve(expression: Parser.Expression) throws {
        switch expression {
        case .unary(let `operator`, let value):
            try resolve(expression: value)

            // "+" | "-" <integer> -> <integer>
            // "+" | "-" <longint> -> <longint>
            // ! <boolean> -> <boolean>

            let type = resolvedType(of: value)

            // TODO: Move type calculations outside of the resolver
            switch (type, `operator`.kind) {
            case (.integer, .plusMinus): resolveType(of: expression, as: .integer)
            case (.longint, .plusMinus): resolveType(of: expression, as: .longint)
            case (.boolean, .not): resolveType(of: expression, as: .boolean)
            default:
                throw ResolveError(
                    kind: .typeError,
                    message: "cannot use unary \(`operator`.string) on \(format(type: type)).")
            }
        case .binary(let `operator`, let left, let right):
            try resolve(expression: left)
            try resolve(expression: right)

            // <integer> "+" | "-" | "*" | "/" <integer> -> <integer>
            // <longint> "+" | "-" | "*" | "/" <longint> -> <longint>
            // <boolean> "&&" | "||" <boolean> -> <boolean>
            // <integer> "=" | "#" | "<" | "<=" | ">" | ">=" <integer> -> <boolean>
            // <longint> "=" | "#" | "<" | "<=" | ">" | ">=" <longint> -> <boolean>
            // <char>    "=" | "#" | "<" | "<=" | ">" | ">=" <char>    -> <boolean>
            // <boolean> "=" | "#" <boolean> -> <boolean>

            let leftType = resolvedType(of: left)
            let rightType = resolvedType(of: right)

            switch (leftType, rightType, `operator`.kind) {
            case (.integer, .integer, .plusMinus), (.integer, .integer, .mulDiv):
                resolveType(of: expression, as: .integer)
            case (.longint, .longint, .plusMinus), (.longint, .longint, .mulDiv):
                resolveType(of: expression, as: .longint)
            case (.boolean, .boolean, .and), (.boolean, .boolean, .or),
                (.integer, .integer, .relOp), (.longint, .longint, .relOp), (.char, .char, .relOp):
                resolveType(of: expression, as: .boolean)
            case (.boolean, .boolean, .relOp):
                if `operator`.string == "=" || `operator`.string == "#" {
                    resolveType(of: expression, as: .boolean)
                } else {
                    fallthrough
                }
            default:
                throw ResolveError(
                    kind: .typeError,
                    message:
                        "cannot use binary \(`operator`.string) between \(format(type: leftType)) and \(format(type: rightType))."
                )
            }
        case .subscript(let array, let index):
            try resolve(expression: array)
            try resolve(expression: index)

            let arrayType = resolvedType(of: array)
            let indexType = resolvedType(of: index)

            // We probably want to prevent subscripting an untyped array
            guard indexType == .integer, case .array(let base, size: _) = arrayType, let base = base
            else {
                throw ResolveError(
                    kind: .typeError,
                    message:
                        "cannot subcript \(format(type: arrayType)) with \(format(type: indexType))."
                )
            }
            resolveType(of: expression, as: base)
        case .call(let function, let arguments):
            try resolve(expression: function)
            for argument in arguments { try resolve(expression: argument) }
            let functionType = resolvedType(of: function)
            guard case .function(let parameterTypes, let returnType) = functionType else {
                throw ResolveError(
                    kind: .typeError,
                    message: "cannot call \(format(type: functionType)) as a function.")
            }
            guard parameterTypes.count == arguments.count else {
                throw ResolveError(
                    kind: .typeError,
                    message:
                        "function requires exactly \(parameterTypes.count) arguments, but found \(arguments.count)."
                )
            }
            for (parameterType, argument) in zip(parameterTypes, arguments) {
                let argumentType = resolvedType(of: argument)
                guard argumentType.isConvertible(to: parameterType) else {
                    throw ResolveError(
                        kind: .typeError,
                        message:
                            "argument \(format(type: argumentType)) is not convertible to parameter \(format(type: parameterType))."
                    )
                }
            }
            resolveType(of: expression, as: returnType)
        case .variable(let name):
            try resolveSymbol(of: name)
            let symbol = resolvedSymbol(of: name)
            resolveType(of: expression, as: symbol.type)
        case .integer: resolveType(of: expression, as: .integer)
        case .longint: resolveType(of: expression, as: .longint)
        case .boolean: resolveType(of: expression, as: .boolean)
        case .char: resolveType(of: expression, as: .char)
        case .string(let string, _):
            resolveType(of: expression, as: .array(base: .char, size: Int32(string.count) + 1))
        }
    }

    func resolveSymbol(of token: Token) throws {
        for scope in scopes.reversed() {
            if let symbol = scope.findSymbol(named: token.string) {
                resolvedSymbols[token] = symbol
                return
            }
        }
        throw ResolveError(kind: .symbolError, message: "cannot find \(token.string) in scope.")
    }

    func resolveType(of expression: Parser.Expression, as type: `Type`) {
        resolvedTypes[expression] = type
    }

    func resolvedSymbol(of token: Token) -> Symbol {
        guard let symbol = resolvedSymbols[token] else { fatalError() }
        return symbol
    }
    func resolvedType(of expression: Parser.Expression) -> `Type` {
        guard let type = resolvedTypes[expression] else { fatalError() }
        return type
    }
}

func format(type: Resolver.`Type`) -> String {
    switch type {
    case .boolean: return "boolean"
    case .char: return "char"
    case .integer: return "integer"
    case .longint: return "longint"
    case .array:
        var type: Resolver.`Type`? = type
        var sizes: [Int32?] = []
        while case .array(let base, let size) = type {
            sizes.append(size)
            type = base
        }
        var string: String
        if let type = type { string = format(type: type) } else { string = "ANY" }
        for size in sizes { if let size = size { string += "[\(size)]" } else { string += "[]" } }
        return string
    case .procedure(let parameters):
        return "procedure(\(parameters.map(format(type:)).joined(separator: "; ")))"
    case .function(let parameters, let `return`):
        return
            "function(\(parameters.map(format(type:)).joined(separator: "; "))) : \(format(type: `return`))"
    }
}
