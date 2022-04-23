import Foundation

class Parser {
    var token: Token? = nil
    var scanner: AnyIterator<Token>

    var hasError = false
    var errors: [ParseError] = []

    convenience init(tokens: [Token]) { self.init(scanner: tokens.makeIterator()) }

    init<T: IteratorProtocol>(scanner: T) where T.Element == Token {
        self.scanner = AnyIterator(scanner)
        token = self.scanner.next()
    }

    // MARK: - Types

    indirect enum Expression: Equatable, Hashable {
        case unary(`operator`: Token, value: Expression)
        case binary(`operator`: Token, `left`: Expression, `right`: Expression)
        case `subscript`(array: Expression, index: Expression)
        case call(function: Expression, arguments: [Expression])
        case variable(name: Token)
        case integer(Int32)
        case longint(Int64)
        case boolean(Bool)
        case char(UInt8)
        case string([UInt8])
    }

    indirect enum Statement: Equatable, Hashable {
        case assignment(target: Expression, value: Expression)
        case call(procedure: Expression, arguments: [Expression])
        case `if`(condition: Expression, thenBody: [Statement], elseBody: [Statement])
        case `while`(condition: Expression, body: [Statement])
        case `return`(value: Expression?)
    }

    indirect enum `Type`: Equatable, Hashable {
        case boolean
        case char
        case integer
        case longint
        case array(base: `Type`, size: Expression?)
    }

    struct Parameter: Equatable, Hashable {
        let name: Token
        let type: `Type`
    }

    indirect enum Declaration: Equatable, Hashable {
        case `var`(name: Token, type: `Type`)
        case `const`(name: Token, type: `Type`, initializer: Expression)
        case procedure(name: Token, parameters: [Parameter], block: Block?)
        case function(name: Token, parameters: [Parameter], `return`: `Type`, block: Block?)

        static func procedure(
            name: Token, parameters: [Parameter], declarations: [Declaration], body: [Statement]
        ) -> Self {
            return .procedure(
                name: name, parameters: parameters,
                block: Block(declarations: declarations, body: body))
        }

        static func externProcedure(name: Token, parameters: [Parameter]) -> Self {
            return .procedure(name: name, parameters: parameters, block: nil)
        }

        static func function(
            name: Token, parameters: [Parameter], return: `Type`, declarations: [Declaration],
            body: [Statement]
        ) -> Self {
            return .function(
                name: name, parameters: parameters, return: `return`,
                block: Block(declarations: declarations, body: body))
        }

        static func externFunction(name: Token, parameters: [Parameter], return: `Type`) -> Self {
            return .function(name: name, parameters: parameters, return: `return`, block: nil)
        }
    }

    struct Module: Equatable, Hashable {
        let name: Token
        let block: Block

        init(name: Token, declarations: [Declaration], body: [Statement]) {
            self.name = name
            self.block = Block(declarations: declarations, body: body)
        }
    }

    struct Block: Equatable, Hashable {
        let declarations: [Declaration]
        let body: [Statement]
    }

    // MARK: - Error handling

    struct ParseError: Error {
        let token: Token?
        let message: String?
    }

    func expected(_ kinds: Token.Kind..., at place: String? = nil) -> Recover {
        var message = "unexpected "
        if let token = token { message += "\(token.kind)" } else { message += "end of file" }
        message += ": expected \(kinds.map { "\($0)" }.joined())"
        if let place = place { message += " at \(place)." } else { message += "." }
        errors.append(ParseError(token: token, message: message))
        return Recover.tryRecover
    }

    func expected(_ string: String, at place: String? = nil) -> Recover {
        let message: String
        if let place = place {
            message =
                "unexpected \(token?.string ?? "end of file"): expected \(string) at \(place)."
        } else {
            message = "unexpected \(token?.string ?? "end of file"): expected \(string)."
        }
        errors.append(ParseError(token: token, message: message))
        return Recover.tryRecover
    }

    enum Recover: Error { case tryRecover }

    func recover(on starts: Token.Kind..., after ends: Token.Kind..., body: () throws -> Void)
        rethrows
    {
        do { try body() } catch Recover.tryRecover {
            hasError = true  // just to be sure
            while !check(starts + ends) { advance() }
            _ = match(ends)
        }
    }

    // MARK: - Consuming tokens

    @discardableResult func advance() -> Token? {
        guard let token = token else { return nil }
        self.token = scanner.next()
        return token
    }

    func check(_ kinds: Token.Kind...) -> Bool { check(kinds) }
    func check(_ kinds: [Token.Kind]) -> Bool {
        guard let token = token else { return false }
        return kinds.contains(token.kind)
    }

    func match(_ kinds: Token.Kind...) -> Token? { match(kinds) }
    func match(_ kinds: [Token.Kind]) -> Token? {
        guard check(kinds) else { return nil }
        return advance()!
    }

    @discardableResult func consume(_ kind: Token.Kind) throws -> Token {
        guard check(kind) else { throw expected(kind) }
        return advance()!
    }

    @discardableResult func consume(_ kind: Token.Kind, string: String) throws -> Token {
        guard check(kind) && token!.string == string else { throw expected(string) }
        return advance()!
    }

    // MARK: - Recursive descent parsing

    func parse() throws -> Module { return try parseModule() }

    func parseModule() throws -> Module {
        // module = "module" ident ";"
        //          { constDeclarations | varDeclarations | functionDeclaration | procedureDeclaration }
        //          [ "begin" statements ] "end" ident "."
        try consume(.module)
        let name = try consume(.ident)
        try consume(.semicolon)
        var declarations: [Declaration] = []
        while check(.var, .const, .function, .procedure) {
            // TODO: What guarantees are needed for recovery to work?
            // Intuition suggests that all instances of the following recover tokens must be recovered recursively.
            // We must guarantee that each declaration parse function will eat all of the scoped declarations and blocks.
            try recover(on: .var, .const, .function, .procedure, .begin, .end, after: .semicolon) {
                if check(.var) {
                    declarations.append(contentsOf: try parseVarDeclarations())
                } else if check(.const) {
                    declarations.append(contentsOf: try parseConstDeclarations())
                } else if check(.function) {
                    declarations.append(try parseFunctionDeclaration())
                } else if check(.procedure) {
                    declarations.append(try parseProcedureDeclaration())
                } else {
                    throw expected(.var, .const, .function, .procedure, at: "start of declaration")
                }
            }
        }
        let body: [Statement]
        if match(.begin) != nil { body = try parseStatements() } else { body = [] }
        try consume(.end)
        try consume(.ident, string: name.string)
        try consume(.dot)
        return Module(name: name, declarations: declarations, body: body)
    }

    // MARK: - Parsing declarations

    // varDeclarations = "var" varDeclaration ";" { varDeclaration ";" }
    // varDeclaration = ident { "," ident } ":" type
    // constDeclarations = "const" constDeclaration ";" { constDeclaration ";" }
    // constDeclaration = ident { "," ident } ":" type "=" expression
    // functionDeclaration = "function" ident [ parameters ] ":" type ";"
    //                       ( "extern" | functionBody ident ) ";"
    // functionBody = [ constDeclarations ] [ varDeclarations ] "begin" statements "end"
    // procedureDeclaration = "procedure" ident [ parameters ] ";"
    //                        ( "extern" | procedureBody ident ) ";"
    // procedureBody = [ constDeclarations ] [ varDeclarations ] "begin" statements "end"
    // parameters = "(" parameterDeclarations ")"
    // parameterDeclarations = [ parameterDeclaration { ";" parameterDeclaration } ]
    // parameterDeclaration = ident { "," ident } ":" type

    func parseVarDeclarations() throws -> [Declaration] {
        // varDeclarations = "var" varDeclaration ";" { varDeclaration ";" }
        // varDeclaration = ident { "," ident } ":" type
        try consume(.var)
        var declarations: [Declaration] = []
        repeat {
            try recover(after: .semicolon) {
                var names: [Token] = []
                repeat { names.append(try consume(.ident)) } while match(.comma) != nil
                try consume(.colon)
                let type = try parseType()
                for name in names { declarations.append(.`var`(name: name, type: type)) }
                try consume(.semicolon)
            }
        } while check(.ident)
        return declarations
    }

    func parseConstDeclarations() throws -> [Declaration] {
        // constDeclarations = "const" constDeclaration ";" { constDeclaration ";" }
        // constDeclaration = ident { "," ident } ":" type "=" expression
        try consume(.const)
        var declarations: [Declaration] = []
        repeat {
            try recover(after: .semicolon) {
                var names: [Token] = []
                repeat { names.append(try consume(.ident)) } while match(.comma) != nil
                try consume(.colon)
                let type = try parseType()
                try consume(.relOp, string: "=")
                let initializer = try parseExpression()
                for name in names {
                    declarations.append(.const(name: name, type: type, initializer: initializer))
                }
                try consume(.semicolon)
            }
        } while check(.ident)
        return declarations
    }

    func parseFunctionDeclaration() throws -> Declaration {
        // functionDeclaration = "function" ident [ parameters ] ":" type ";"
        //                       ( "extern" | functionBody ident ) ";"
        // functionBody = [ constDeclarations ] [ varDeclarations ] "begin" statements "end"
        // TODO: Find a way to consume the entire block even on error
        try consume(.function)
        let name = try consume(.ident)
        let parameters: [Parameter]
        if check(.lParen) { parameters = try parseParameters() } else { parameters = [] }
        try consume(.colon)
        let `return` = try parseType()
        try consume(.semicolon)
        if check(.extern) {
            try consume(.semicolon)
            return .externFunction(name: name, parameters: parameters, return: `return`)
        } else {
            // TODO: Possible recovery point
            var declarations: [Declaration] = []
            if check(.const) { declarations.append(contentsOf: try parseConstDeclarations()) }
            if check(.var) { declarations.append(contentsOf: try parseVarDeclarations()) }
            try consume(.begin)
            let body = try parseStatements()
            try consume(.end)
            try consume(.ident, string: name.string)
            try consume(.semicolon)
            return .function(
                name: name, parameters: parameters, return: `return`, declarations: declarations,
                body: body)
        }
    }

    func parseProcedureDeclaration() throws -> Declaration {
        // procedureDeclaration = "procedure" ident [ parameters ] ";"
        //                        ( "extern" | procedureBody ident ) ";"
        // procedureBody = [ constDeclarations ] [ varDeclarations ] "begin" statements "end"
        try consume(.procedure)
        let name = try consume(.ident)
        let parameters: [Parameter]
        if check(.lParen) { parameters = try parseParameters() } else { parameters = [] }
        try consume(.semicolon)
        if check(.extern) {
            try consume(.semicolon)
            return .externProcedure(name: name, parameters: parameters)
        } else {
            // TODO: Possible recovery point
            var declarations: [Declaration] = []
            if check(.const) { declarations.append(contentsOf: try parseConstDeclarations()) }
            if check(.var) { declarations.append(contentsOf: try parseVarDeclarations()) }
            try consume(.begin)
            let body = try parseStatements()
            try consume(.end)
            try consume(.ident, string: name.string)
            try consume(.semicolon)
            return .procedure(
                name: name, parameters: parameters, declarations: declarations, body: body)
        }
    }

    func parseParameters() throws -> [Parameter] {
        // parameters = "(" parameterDeclarations ")"
        // parameterDeclarations = [ parameterDeclaration { ";" parameterDeclaration } ]
        // parameterDeclaration = ident { "," ident } ":" type
        try consume(.lParen)
        var parameters: [Parameter] = []
        // FIRST(parameterDeclaration) = { .ident }
        if check(.ident) {
            repeat {
                var names: [Token] = []
                repeat { names.append(try consume(.ident)) } while match(.comma) != nil
                try consume(.colon)
                let type = try parseType()
                for name in names { parameters.append(Parameter(name: name, type: type)) }
            } while match(.semicolon) != nil
        }
        try consume(.rParen)
        return parameters
    }

    // MARK: - Parsing statements

    // statements = [ statement { ";" statement } ]
    // statement = assignment | procedureCall | if | while | return
    // assignment = subscript ":=" expression
    // procedureCall = ident "(" [ expression { "," expression } ] ")"
    // if = "if" "(" expression ")" "then" statements [ "else" statements ] "end"
    // while = "while" "(" expression ")" "do" statements "end"
    // return = "return" [ expression ]

    func parseStatements() throws -> [Statement] {
        // statements = [ statement { ";" statement } ]
        // TODO: Possible recovery point
        var statements: [Statement] = []
        // FIRST(statement) = { .ident, .if, .while, .return }
        if check(.ident, .if, .while, .return) {
            repeat {
                try recover(on: .if, .while, .return, after: .semicolon) {
                    statements.append(try parseStatement())
                }
            } while match(.semicolon) != nil
        }
        return statements
    }

    func parseStatement() throws -> Statement {
        // statement = assignment | procedureCall | if | while | return
        if check(.ident) {
            let value = try parseVariable()
            if check(.lParen) {
                return try parseProcedureCall(procedure: value)
            } else {
                let `subscript` = try parseSubscript(array: value)
                return try parseAssignment(target: `subscript`)
            }
        } else if check(.if) {
            return try parseIf()
        } else if check(.while) {
            return try parseWhile()
        } else if check(.return) {
            return try parseReturn()
        }
        throw expected(.ident, .if, .while, .return, at: "start of statement")
    }

    func parseAssignment(target: Expression) throws -> Statement {
        // assignment = subscript ":=" expression
        // Note: The assignment target is already consumed and passed in as the parameter target.
        try consume(.assign)
        let value = try parseExpression()
        return .assignment(target: target, value: value)
    }

    func parseProcedureCall(procedure: Expression) throws -> Statement {
        // procedureCall = ident "(" [ expression { "," expression } ] ")"
        // Note: The procedure is already consumed and passed in as the parameter procedure.
        try consume(.lParen)
        var arguments: [Expression] = []
        // FIRST(expression) = { .plusMinus, .ident, .number, .boolConst, .charConst, .stringConst, .lParen, .not }
        // TODO: Handle .charConstInv and .stringConstInv as well
        if check(.plusMinus, .ident, .number, .boolConst, .charConst, .stringConst, .lParen, .not) {
            repeat { arguments.append(try parseExpression()) } while match(.comma) != nil
        }
        try consume(.rParen)
        return .call(procedure: procedure, arguments: arguments)
    }

    func parseIf() throws -> Statement {
        // if = "if" "(" expression ")" "then" statements [ "else" statements ] "end"
        try consume(.if)
        try consume(.lParen)
        let condition = try parseExpression()
        try consume(.rParen)
        try consume(.then)
        let thenBody = try parseStatements()
        let elseBody: [Statement]
        if match(.else) != nil { elseBody = try parseStatements() } else { elseBody = [] }
        try consume(.end)
        return .`if`(condition: condition, thenBody: thenBody, elseBody: elseBody)
    }

    func parseWhile() throws -> Statement {
        // while = "while" "(" expression ")" "do" statements "end"
        try consume(.while)
        try consume(.lParen)
        let condition = try parseExpression()
        try consume(.rParen)
        try consume(.do)
        let body = try parseStatements()
        try consume(.end)
        return .`while`(condition: condition, body: body)
    }

    func parseReturn() throws -> Statement {
        // return = "return" [ expression ]
        try consume(.return)
        let value: Expression?
        // FIRST(expression) = { .plusMinus, .ident, .number, .boolConst, .charConst, .stringConst, .lParen, .not }
        // TODO: Handle .charConstInv and .stringConstInv as well
        if check(.plusMinus, .ident, .number, .boolConst, .charConst, .stringConst, .lParen, .not) {
            value = try parseExpression()
        } else {
            value = nil
        }
        return .return(value: value)
    }

    // MARK: - Parsing types

    // type = baseType { "[" [ plusMinus ] "]" }
    // baseType = "boolean" | "char" | "integer" | "longint"

    func parseType() throws -> `Type` {
        // type = baseType { "[" [ plusMinus ] "]" }
        var type = try parseBaseType()
        var sizes: [Expression?] = []
        while match(.lBrack) != nil {
            // FIRST(plusMinus) = { .plusMinus, .ident, .number, .boolConst, .charConst, .stringConst, .lParen, .not }
            if check(
                .plusMinus, .ident, .number, .boolConst, .charConst, .stringConst, .lParen, .not)
            {
                sizes.append(try parsePlusMinus())
            } else {
                sizes.append(nil)
            }
            try consume(.rBrack)
        }
        for size in sizes.reversed() { type = .array(base: type, size: size) }
        return type
    }

    func parseBaseType() throws -> `Type` {
        // baseType = "boolean" | "char" | "integer" | "longint"
        if match(.boolean) != nil {
            return .boolean
        } else if match(.char) != nil {
            return .char
        } else if match(.integer) != nil {
            return .integer
        } else if match(.longint) != nil {
            return .longint
        }
        throw expected(.boolean, .char, .integer, .longint, at: "start of type")
    }

    // MARK: - Parsing expressions

    // expression = relation
    // relation = plusMinus [ ( "=" | "#" | "<" | "<=" | ">" | ">=" ) plusMinus ]
    // plusMinus = [ "+" | "-" ] mulDiv { ( "+" | "-" | "||" ) mulDiv }
    // mulDiv = primary { ( "*" | "/" | "&&" ) primary }
    // primary = subscript | literal | "(" expression ")" | functionCall | "!" primary
    // literal = number | boolean | char | string
    // subscript = variable [ "[" plusMinus "]" ]
    // functionCall = variable "(" [ expression { "," expression } ] ")"
    // variable = ident

    func parseExpression() throws -> Expression {
        // expression = relation
        return try parseRelation()
    }

    func parseRelation() throws -> Expression {
        // relation = plusMinus [ ( "=" | "#" | "<" | "<=" | ">" | ">=" ) plusMinus ]
        let plusMinus = try parsePlusMinus()
        if let `operator` = match(.relOp) {
            return .binary(operator: `operator`, left: plusMinus, right: try parsePlusMinus())
        } else {
            return plusMinus
        }
    }

    func parsePlusMinus() throws -> Expression {
        // plusMinus = [ "+" | "-" ] mulDiv { ( "+" | "-" | "||" ) mulDiv }
        var plusMinus: Expression
        if let `operator` = match(.plusMinus) {
            plusMinus = .unary(operator: `operator`, value: try parseMulDiv())
        } else {
            plusMinus = try parseMulDiv()
        }
        while let `operator` = match(.plusMinus, .or) {
            let mulDiv = try parseMulDiv()
            plusMinus = .binary(operator: `operator`, left: plusMinus, right: mulDiv)
        }
        return plusMinus
    }

    func parseMulDiv() throws -> Expression {
        // mulDiv = primary { ( "*" | "/" | "&&" ) primary }
        var mulDiv = try parsePrimary()
        while let `operator` = match(.mulDiv, .and) {
            let primary = try parsePrimary()
            mulDiv = .binary(operator: `operator`, left: mulDiv, right: primary)
        }
        return mulDiv
    }

    func parsePrimary() throws -> Expression {
        // primary = subscript | literal | "(" expression ")" | functionCall | "!" primary
        if check(.ident) {
            let value = try parseVariable()
            if check(.lParen) {
                return try parseFunctionCall(function: value)
            } else {
                return try parseSubscript(array: value)
            }
        } else if check(.number, .boolConst, .charConst, .stringConst) {
            return try parseLiteral()
        } else if match(.lParen) != nil {
            let expression = try parseExpression()
            try consume(.rParen)
            return expression
        } else if let `operator` = match(.not) {
            return .unary(operator: `operator`, value: try parsePrimary())
        }
        throw expected(
            .ident, .number, .boolConst, .charConst, .stringConst, .lParen,
            at: "start of primary expression")
    }

    func parseSubscript(array: Expression) throws -> Expression {
        // subscript = variable [ "[" plusMinus "]" ]
        // Note: The array is already consumed and passed in as the parameter array.
        var `subscript`: Expression = array
        while match(.lBrack) != nil {
            `subscript` = .subscript(array: `subscript`, index: try parsePlusMinus())
            try consume(.rBrack)
        }
        return `subscript`
    }

    func parseFunctionCall(function: Expression) throws -> Expression {
        // functionCall = variable "(" [ expression { "," expression } ] ")"
        // Note: The function is already consumed and passed in as the parameter function.
        try consume(.lParen)
        var arguments: [Expression] = []
        // FIRST(expression) = { .plusMinus, .ident, .number, .boolConst, .charConst, .stringConst, .lParen, .not }
        // TODO: Handle .charConstInv and .stringConstInv as well
        if check(.plusMinus, .ident, .number, .boolConst, .charConst, .stringConst, .lParen, .not) {
            repeat { arguments.append(try parseExpression()) } while match(.comma) != nil
        }
        try consume(.rParen)
        return .call(function: function, arguments: arguments)
    }

    func parseLiteral() throws -> Expression {
        // literal = number | boolean | char | string
        // TODO: Handle .charConstInv and .stringConstInv as well
        if let token = match(.number) {
            if token.string.last == "L" {
                return .longint(Int64(token.string.trimmingCharacters(in: ["L"]))!)
            } else {
                // FIXME: Handle overflow
                return .integer(Int32(token.string)!)
            }
        } else if let token = match(.boolConst) {
            return .boolean(token.string == "true")
        } else if let token = match(.charConst) {
            return .char(Token.unescape(token.string)[0])
        } else if let token = match(.stringConst) {
            return .string(Token.unescape(token.string))
        }
        throw expected(.number, .boolConst, .charConst, .stringConst, at: "start of literal")
    }

    func parseVariable() throws -> Expression {
        // variable = ident
        return .variable(name: try consume(.ident))
    }
}

// MARK: - Pretty Printer

extension String {
    fileprivate func indented(with prefix: String = "  ") -> String {
        self.split(separator: "\n").map({ prefix + $0 }).joined(separator: "\n")
    }
}

func format(module: Parser.Module) -> String {
    """
    module \(module.name.string);
    \(format(declarations: module.block.declarations).indented())
    begin
    \(module.block.body.map(format(statement:)).joined(separator: ";\n").indented())
    end \(module.name.string).
    """
}

func format(declarations: [Parser.Declaration]) -> String {
    // TODO: Simplify declaration combining
    guard declarations.count > 0 else { return "" }
    guard declarations.count > 1 else { return format(declaration: declarations[0]) }
    // Successive var, const declarations can be combined if the type and initializer match.
    var string = ""
    let firstDeclaration = declarations[0]
    if case .`var`(name: let name, type: _) = firstDeclaration {
        string += "var \(name.string)"
    } else if case .const(name: let name, type: _, initializer: _) = firstDeclaration {
        string += "const \(name.string)"
    } else {
        string += format(declaration: declarations[0])
    }
    for (previousDeclaration, declaration) in zip(declarations, declarations[1...]) {
        // var+var, const+const -> extend current or end previous
        // var+*, const+* -> end previous
        // *+var, *+const -> start new
        // var+const, const+var -> end previous and start new
        switch (previousDeclaration, declaration) {
        case (.`var`(name: _, type: let previousType), .`var`(name: let name, type: let type)):
            if previousType == type {
                string += ", \(name.string)"
            } else {
                string += ": \(format(type: previousType)); \(name.string)"
            }
        case (
            .const(name: _, type: let previousType, initializer: let previousInitializer),
            .const(name: let name, type: let type, let initializer)
        ):
            if previousType == type && previousInitializer == initializer {
                string += ", \(name.string)"
            } else {
                string +=
                    ": \(format(type: previousType)) = \(format(expression: previousInitializer)); \(name.string)"
            }
        case (
            .`var`(name: _, type: let previousType), .const(name: let name, type: _, initializer: _)
        ): string += ": \(format(type: previousType));\nconst \(name.string)"
        case (
            .const(name: _, type: let previousType, initializer: let previousInitializer),
            .`var`(name: let name, type: _)
        ):
            string +=
                ": \(format(type: previousType)) = \(format(expression: previousInitializer));\nvar \(name.string)"
        case (_, .`var`(name: let name, type: _)): string += "\nvar \(name.string)"
        case (_, .const(name: let name, type: _, initializer: _)):
            string += "\nconst \(name.string)"
        case (.`var`(name: _, type: let previousType), let declaration):
            string += ": \(format(type: previousType));\n\(format(declaration: declaration))"
        case (
            .const(name: _, type: let previousType, initializer: let previousInitializer),
            let declaration
        ):
            string +=
                ": \(format(type: previousType)) = \(format(expression: previousInitializer));\n\(format(declaration: declaration))"
        case (_, let declaration): string += "\n\(format(declaration: declaration))"
        }
    }
    // End the last declaration if it is one of var or const.
    let lastDeclaration = declarations[declarations.count - 1]
    if case .`var`(name: _, type: let type) = lastDeclaration {
        string += ": \(format(type: type));"
    } else if case .const(name: _, type: let type, initializer: let initializer) = lastDeclaration {
        string += ": \(format(type: type)) = \(format(expression: initializer));"
    }
    return string
}

func format(declaration: Parser.Declaration) -> String {
    switch declaration {
    case .`var`(let name, let type): return "var \(name.string): \(format(type: type));"
    case .const(let name, let type, let initializer):
        return "const \(name.string): \(format(type: type)) = \(format(expression: initializer));"
    case .function(let name, let parameters, return: let `return`, let block):
        guard let block = block else {
            return """
                function \(name.string)(\(parameters.map(format(parameter:)).joined(separator: "; "))): \(format(type: `return`)); extern;
                """
        }

        return """
            function \(name.string)(\(parameters.map(format(parameter:)).joined(separator: "; "))): \(format(type: `return`));
            \(format(declarations: block.declarations).indented())
            begin
            \(block.body.map(format(statement:)).joined(separator: ";\n").indented())
            end \(name.string);
            """
    case .procedure(let name, let parameters, let block):
        guard let block = block else {
            return """
                procedure \(name.string)(\(parameters.map(format(parameter:)).joined(separator: "; "))); extern;
                """
        }

        return """
            procedure \(name.string)(\(parameters.map(format(parameter:)).joined(separator: "; ")));
            \(format(declarations: block.declarations).indented())
            begin
            \(block.body.map(format(statement:)).joined(separator: ";\n").indented())
            end \(name.string);
            """
    }
}

func format(parameter: Parser.Parameter) -> String {
    "\(parameter.name.string): \(format(type: parameter.type))"
}

func format(type: Parser.`Type`) -> String {
    switch type {
    case .boolean: return "boolean"
    case .char: return "char"
    case .integer: return "integer"
    case .longint: return "longint"
    case .array:
        var type = type
        var sizes: [Parser.Expression?] = []
        while case .array(base: let base, size: let size) = type {
            sizes.append(size)
            type = base
        }
        var string = format(type: type)
        for size in sizes {
            if let size = size { string += "[\(format(expression: size))]" } else { string += "[]" }
        }
        return string
    }
}

func format(expression: Parser.Expression) -> String {
    switch expression {
    case .unary(operator: let `operator`, let value):
        return "(\(`operator`.string)\(format(expression: value)))"
    case .binary(operator: let `operator`, let left, let right):
        return "(\(format(expression: left)) \(`operator`.string) \(format(expression: right)))"
    case .subscript(let array, let index):
        return "\(format(expression: array))[\(format(expression: index))]"
    case .call(let function, let arguments):
        return
            "\(format(expression: function))(\(arguments.map(format(expression:)).joined(separator: ", ")))"
    case .variable(let name): return "\(name.string)"
    case .integer(let integer): return "\(integer)"
    case .longint(let longint): return "\(longint)L"
    case .boolean(let boolean): return "\(boolean ? "true" : "false")"
    case .char(let char): return "'\(Token.escape(char))'"
    case .string(let string):
        return """
            "\(Token.escape(string))"
            """
    }
}

func format(statement: Parser.Statement) -> String {
    switch statement {
    case .assignment(let target, let value):
        return "\(format(expression: target)) := \(format(expression: value))"
    case .call(let procedure, let arguments):
        return
            "\(format(expression: procedure))(\(arguments.map(format(expression:)).joined(separator: ", ")))"
    case .if(let condition, let thenBody, let elseBody):
        return """
            if (\(format(expression: condition))) then
            \(thenBody.map(format(statement:)).joined(separator: ";\n").indented())
            else
            \(elseBody.map(format(statement:)).joined(separator: ";\n").indented())
            end
            """
    case .while(let condition, let body):
        return """
            while (\(format(expression: condition))) do
            \(body.map(format(statement:)).joined(separator: ";\n").indented())
            end
            """
    case .return(let value):
        guard let value = value else { return "return" }
        return "return \(format(expression: value))"
    }
}
