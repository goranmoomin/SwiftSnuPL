import Foundation

class Resolver {
    var scopes: [Scope] = []
    var resolvedTypes: [Parser.Expression: `Type`] = [:]
    var resolvedSymbols: [Token: Symbol] = [:]
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
                guard case .array(base: let targetBase, size: let targetSize) = target else {
                    return false
                }
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

        mutating func addVar(token: Token, type: `Type`) {
            symbols.insert(.`var`(token: token, type: type))
        }

        mutating func addConst(token: Token, type: `Type`, initializer: AnyHashable) {
            symbols.insert(.const(token: token, type: type, initializer: initializer))
        }

        func findSymbol(named name: String) -> Symbol? {
            symbols.first(where: { $0.token.string == name })
        }
    }

    // MARK: - Comptime evaluation

    func evaluate<T: Hashable>(expression: Parser.Expression, as type: T.Type) throws -> T {
        try evaluate(expression: expression) as! T
    }

    func evaluate(expression: Parser.Expression) throws -> AnyHashable {
        // TODO: Implement everything
        switch expression {
        case .unary(operator: let `operator`, let value):
            let type = try resolvedType(of: value)
            if type == .integer {
                guard `operator`.string == "-" else { fatalError() }
                return try -evaluate(expression: value, as: Int32.self)
            } else if type == .longint {
                guard `operator`.string == "-" else { fatalError() }
                return try -evaluate(expression: value, as: Int64.self)
            } else if type == .boolean {
                guard `operator`.string == "!" else { fatalError() }
                return try !evaluate(expression: value, as: Bool.self)
            } else {
                fatalError()
            }
        case .binary(operator: let `operator`, let left, let right):
            let leftType = try resolvedType(of: left)
            let rightType = try resolvedType(of: right)

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
                case "/": return leftValue / rightValue  // TODO: handle division-by-zero and rounding
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
                case "=": return leftValue == rightValue
                case "#": return leftValue != rightValue
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
        case .subscript: fatalError()
        case .call: fatalError()
        case .variable(let name):
            guard case let .const(_, _, initializer) = try resolvedSymbol(of: name) else {
                fatalError()
            }
            return initializer
        case .integer(let integer): return integer
        case .longint(let longint): return longint
        case .boolean(let boolean): return boolean
        case .char(let char): return char
        case .string(let string): return string
        }
    }

    func evaluate(type: Parser.`Type`) throws -> `Type` {
        switch type {
        case .boolean: return .boolean
        case .char: return .char
        case .integer: return .integer
        case .longint: return .longint
        case .array(let base, size: let sizeExpression):
            let size: Int32?
            if let sizeExpression = sizeExpression {
                try resolve(expression: sizeExpression)
                size = try evaluate(expression: sizeExpression) as? Int32
                guard size != nil else { fatalError() }
            } else {
                size = nil
            }
            return .array(base: try evaluate(type: base), size: size)
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

        // TODO: Check the implications of this
        resolveSymbol(of: module.name, as: builtin(withSignature: "main()"))

        // TODO: Check if main should be returnable
        try resolve(block: module.block, symbols: globalSymbols, return: nil)
    }

    func resolve(block: Parser.Block, symbols: Set<Symbol> = [], return: `Type`?) throws {
        var scope = Scope(symbols: symbols, return: `return`)
        // TODO: Check if declarations do not clash
        for declaration in block.declarations {
            switch declaration {
            case .`var`(let name, let type):
                scope.addVar(token: name, type: try withScope(scope) { try evaluate(type: type) })
                try withScope(scope) { try resolveSymbol(of: name) }
            case .const(let name, let type, let initializer):
                let initializerValue: AnyHashable = try withScope(scope) {
                    try resolve(expression: initializer)
                    return try evaluate(expression: initializer)
                }
                scope.addConst(
                    token: name, type: try withScope(scope) { try evaluate(type: type) },
                    initializer: initializerValue)
                try withScope(scope) { try resolveSymbol(of: name) }
            case .procedure(let name, let parameters, let block):
                // TODO: Check if type evaluation requires scope
                let parameterTypes = try withScope(scope) {
                    try parameters.map(\.type).map(evaluate(type:))
                }
                scope.addVar(token: name, type: .procedure(parameters: parameterTypes))
                if let block = block {
                    var symbols: Set<Symbol> = []
                    // TODO: Add itself to symbols
                    // TODO: Check if parameters do not clash
                    for (parameter, type) in zip(parameters, parameterTypes) {
                        symbols.insert(.`var`(token: parameter.name, type: type))
                    }
                    try withScope(scope) {
                        try resolve(block: block, symbols: symbols, return: nil)
                    }
                }
                try withScope(scope) { try resolveSymbol(of: name) }
            case .function(let name, let parameters, return: let `return`, let block):
                // TODO: Check if type evaluation requires scope
                let parameterTypes = try withScope(scope) {
                    try parameters.map(\.type).map(evaluate(type:))
                }
                let returnType = try withScope(scope) { try evaluate(type: `return`) }
                scope.addVar(
                    token: name, type: .function(parameters: parameterTypes, return: returnType))
                if let block = block {
                    var symbols: Set<Symbol> = []
                    // TODO: Add itself to symbols
                    // TODO: Check if parameters do not clash
                    for (parameter, type) in zip(parameters, parameterTypes) {
                        symbols.insert(.`var`(token: parameter.name, type: type))
                    }
                    try withScope(scope) {
                        try resolve(block: block, symbols: symbols, return: returnType)
                    }
                }
                try withScope(scope) { try resolveSymbol(of: name) }
            }
        }
        // TODO: Check if return statement exits
        try withScope(scope) { for statement in block.body { try resolve(statement: statement) } }
    }

    func resolve(statements: [Parser.Statement]) throws {
        for statement in statements { try resolve(statement: statement) }
    }

    func resolve(statement: Parser.Statement) throws {
        switch statement {
        case .assignment(let target, let value):
            try resolve(expression: value)
            try resolve(expression: target)

            // the only assignable target is a subscript (including a bare variable)
            // only checking if target is assignable, type checking doesn't happen here
            var variable = target
            while case .subscript(array: let array, index: _) = variable { variable = array }
            guard case .variable(name: let name) = variable else { fatalError() }

            // type checking if assignable
            let symbol = try resolvedSymbol(of: name)
            // TODO: Check if parameters are assignable as well
            if case .const = symbol { fatalError() }
            // TODO: Move type calculations outside of the resolver
            // TODO: Check if assignable
            // valueType.isConvertible(to: targetType)
            guard try resolvedType(of: target) == resolvedType(of: value) else { fatalError() }
        case .call(let procedure, let arguments):
            try resolve(expression: procedure)
            for argument in arguments { try resolve(expression: argument) }
            // TODO: Check if arguments are implicitly convertible to parameters
            // e.g. array -> compatible pointer, integer -> longint
            guard case .procedure(parameters: let parameters) = try resolvedType(of: procedure)
            else { fatalError() }

            // Are there differences between assignment and parameter passing?
            // You can't assign an array to another array
            // An array can't be convertible to itself, but it can be convertible to a pointer to itself
            // If we ever had a pointer of array of integer pointers, we can't pass in an array of integers
            // isPointable && isAssignable are different operations
            // isPassable: can you smuggle in a pointer to the parameter if self isn't a scalar?
            // isPointable: can you make a pointer to a target type point self?
            // isn't an array implicitly a pointer?
            // .array(base: integer, size: 5) should be passable to .pointer(base: .array(base: integer, size: nil))
            // isAssignable: can you assign self to a target type? -> you can't, unless if it's a scalar type

            // is array accessing and pointer accessing different?
            // arr[i] is compiled to *(arr + sizeof(arr[0]) * i) anyway
            // the only difference is declaration?
            // and you can't declare a pointer anyway
            // arr becomes a stack pointer if you do an array

            // if we don't have pointers,
            // the only thing to consider is if you can pass something or if you can assign something
            // you can't assign to anything complex
            // you can pass a pointer to anything complex if it is compatible

            // zip(parameters, arguments).all { (parameter, argument) in
            // resolvedType(of: argument).isConvertible(to: resolvedType(parameter)) }
            // TODO: Move type calculations outside of the resolver
            for (parameter, argument) in zip(parameters, arguments) {
                guard try resolvedType(of: argument).isConvertible(to: parameter) else {
                    fatalError()
                }
            }
        case .if(let condition, let thenBody, let elseBody):
            try resolve(expression: condition)
            try resolve(statements: thenBody)
            try resolve(statements: elseBody)
            guard try resolvedType(of: condition) == .boolean else { fatalError() }
        case .while(let condition, let body):
            try resolve(expression: condition)
            try resolve(statements: body)
            guard try resolvedType(of: condition) == .boolean else { fatalError() }
        case .return(let value):
            // TODO: Check if type of value is returnable in current context
            if let value = value {
                try resolve(expression: value)
                guard try resolvedType(of: value) == scopes.last!.return else { fatalError() }
            } else {
                guard scopes.last!.return == nil else { fatalError() }
            }
        }
    }

    func resolve(expression: Parser.Expression) throws {
        switch expression {
        case .unary(operator: let `operator`, let value):
            try resolve(expression: value)

            // "+" | "-" <integer> -> <integer>
            // "+" | "-" <longint> -> <longint>
            // ! <boolean> -> <boolean>

            let type = try resolvedType(of: value)

            // TODO: Move type calculations outside of the resolver
            switch (type, `operator`.kind) {
            case (.integer, .plusMinus): resolveType(of: expression, as: .integer)
            case (.longint, .plusMinus): resolveType(of: expression, as: .longint)
            case (.boolean, .not): resolveType(of: expression, as: .boolean)
            default: fatalError()
            }
        case .binary(operator: let `operator`, let left, let right):
            try resolve(expression: left)
            try resolve(expression: right)

            // <integer> "+" | "-" | "*" | "/" <integer> -> <integer>
            // <longint> "+" | "-" | "*" | "/" <longint> -> <longint>
            // <boolean> "&&" | "||" <boolean> -> <boolean>
            // <integer> "=" | "#" | "<" | "<=" | ">" | ">=" <integer> -> <boolean>
            // <longint> "=" | "#" | "<" | "<=" | ">" | ">=" <longint> -> <boolean>
            // <char> "=" | "#" | "<" | "<=" | ">" | ">=" <char> -> <boolean>
            // <boolean> "=" | "#" <boolean> -> <boolean>

            let leftType = try resolvedType(of: left)
            let rightType = try resolvedType(of: right)

            // TODO: Move type calculations outside of the resolver
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
                    fatalError()
                }
            default: fatalError()
            }
        case .subscript(let array, let index):
            try resolve(expression: array)
            try resolve(expression: index)

            let arrayType = try resolvedType(of: array)
            let indexType = try resolvedType(of: index)

            // TODO: Move type calculations outside of the resolver
            // pointers from subroutine parameters are implicitly casted to arrays
            // if case .pointer(base: let base) = arrayType, case .array = base { arrayType = base }
            // We probably want to prevent subscripting an untyped array
            guard indexType == .integer, case .array(base: let base, size: _) = arrayType,
                let base = base
            else { fatalError() }
            resolveType(of: expression, as: base)
        case .call(let function, let arguments):
            try resolve(expression: function)
            for argument in arguments { try resolve(expression: argument) }
            guard
                case .function(parameters: let parameters, return: let `return`) = try resolvedType(
                    of: function)
            else { fatalError() }
            for (parameter, argument) in zip(parameters, arguments) {
                guard try resolvedType(of: argument).isConvertible(to: parameter) else {
                    fatalError()
                }
            }
            resolveType(of: expression, as: `return`)
        case .variable(let name):
            try resolveSymbol(of: name)
            let symbol = try resolvedSymbol(of: name)
            resolveType(of: expression, as: symbol.type)
        case .integer: resolveType(of: expression, as: .integer)
        case .longint: resolveType(of: expression, as: .longint)
        case .boolean: resolveType(of: expression, as: .boolean)
        case .char: resolveType(of: expression, as: .char)
        case .string(let string):
            // TODO: Check if this is correct
            resolveType(of: expression, as: .array(base: .char, size: Int32(string.count) + 1))
        }
    }

    func resolveSymbol(of token: Token) throws {
        for scope in scopes.reversed() {
            if let symbol = scope.findSymbol(named: token.string) {
                resolveSymbol(of: token, as: symbol)
                return
            }
        }
        fatalError()
    }

    func resolveSymbol(of token: Token, as symbol: Symbol) { resolvedSymbols[token] = symbol }

    func resolveType(of expression: Parser.Expression, as type: `Type`) {
        resolvedTypes[expression] = type
    }

    func resolvedSymbol(of token: Token) throws -> Symbol {
        guard let symbol = resolvedSymbols[token] else { fatalError() }
        return symbol
    }
    func resolvedType(of expression: Parser.Expression) throws -> `Type` {
        guard let type = resolvedTypes[expression] else { fatalError() }
        return type
    }
}
