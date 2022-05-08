import CppSnuPL
import Foundation

struct Token: Identifiable, Equatable, Hashable {
    enum Kind: Int {
        case number
        case ident
        case plusMinus
        case mulDiv
        case and
        case or
        case not
        case relOp
        case assign
        case colon
        case semicolon
        case dot
        case comma
        case lParen
        case rParen
        case lBrack
        case rBrack

        case boolean
        case char
        case integer
        case longint

        case const
        case `var`
        case extern
        case procedure
        case function
        case module
        case begin
        case end

        case `if`
        case then
        case `else`
        case `while`
        case `do`
        case `return`

        case boolConst
        case charConst
        case stringConst

        case eof
        case ioError
        case invCharConst
        case invStringConst
        case undefined
    }

    let id: String
    let kind: Kind
    let string: String

    let lineNumber: Int
    let charPosition: Int

    init(id: String, kind: Kind, string: String, lineNumber: Int, charPosition: Int) {
        self.id = id
        self.kind = kind
        self.string = string
        self.lineNumber = lineNumber
        self.charPosition = charPosition
    }

    static func escape(_ value: UInt8) -> String {
        switch value {
        case 0x0a:  // NL
            return "\\n"
        case 0x09:  // TAB
            return "\\t"
        case 0x00:  // NUL
            return "\\0"
        case 0x27:  // '
            return "\\'"
        case 0x5c:  // \
            return "\\\\"
        case 0x20..<0x7f: return String(UnicodeScalar(value))
        default: return "\\x\(String(value, radix: 16))"
        }
    }

    static func escape(_ value: [UInt8]) -> String {
        value.map {
            switch $0 {
            case 0x0a:  // NL
                return "\\n"
            case 0x09:  // TAB
                return "\\t"
            case 0x00:  // NUL
                return "\\0"
            case 0x22:  // "
                return "\\\""
            case 0x5c:  // \
                return "\\\\"
            case 0x20..<0x7f: return String(UnicodeScalar($0))
            default: return "\\x\(String($0, radix: 16))"
            }
        }
        .joined(separator: "")
    }

    static func unescape(_ string: String) -> [UInt8] {
        var unescaped_value: UnsafeMutablePointer<UInt8>!
        UnescapedSnuPLTokenValue(string, &unescaped_value)
        var value: [UInt8] = []  // NUL terminated
        var index = 0
        repeat {
            value.append(unescaped_value[index])
            index += 1
        } while value.last != .zero
        free(unescaped_value)
        return value
    }
}

class Scanner: IteratorProtocol {
    let buffer: UnsafeBufferPointer<UInt8>
    let scanner: UnsafeRawPointer

    convenience init(source: String) { self.init(data: source.data(using: .utf8)!) }

    init(data: Data) {
        let mutableBuffer: UnsafeMutableBufferPointer<UInt8> = .allocate(capacity: data.count + 1)
        let (_, index) = data.withUnsafeBytes { mutableBuffer.initialize(from: $0) }
        mutableBuffer[index] = .zero
        buffer = UnsafeBufferPointer(mutableBuffer)
        scanner = CreateSnuPLScanner(buffer.baseAddress)
    }

    deinit {
        DestroySnuPLScanner(scanner)
        buffer.deallocate()
    }

    func next() -> Token? {
        var value: UnsafeMutablePointer<CChar>!
        var type: Int32 = 0
        var line_num: Int32 = 0
        var char_pos: Int32 = 0
        GetSnuPLToken(scanner, &value, &type, &line_num, &char_pos)
        defer { free(value) }
        if type == Token.Kind.eof.rawValue || type == Token.Kind.ioError.rawValue { return nil }
        // Note: The scanner escapes non-ascii values for valid tokens.
        return Token(
            id: "tok:\(line_num):\(char_pos)", kind: Token.Kind(rawValue: Int(type))!,
            string: String(cString: value), lineNumber: Int(line_num), charPosition: Int(char_pos))
    }
}
