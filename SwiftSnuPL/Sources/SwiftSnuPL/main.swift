import Foundation

let source = #"""
    //
    // test2
    //
    // IR generation
    //

    module test2;

    var i: integer;

    procedure foo(v: integer);
    var i: integer;
    begin
      WriteInt(v)
    end foo;

    function bar(p1, p2, p3, p4: integer): integer;
    begin
      return p1 + p2 + p3 + p4
    end bar;

    begin
      foo(bar(1, 2, 3, 4))
    end test2.
    """#


let sourceData = source.data(using: .utf8)!
let scanner = Scanner(data: sourceData)
let parser = Parser(scanner: scanner)
guard let module = try? parser.parse() else {
    for parseError in parser.errors {
        if let token = parseError.token, let message = parseError.message {
            print("error: \(token.lineNumber):\(token.charPosition) \(message)")
        }
    }
    print("error: parsing failed.")
    exit(-1)
}
let resolver = Resolver(module: module)
guard (try? resolver.resolve()) != nil else {
    print("error: resolving failed.")
    exit(-1)
}

let irGenerator = IRGenerator(
    module: module, resolvedSymbols: resolver.resolvedSymbols, resolvedTypes: resolver.resolvedTypes
)
let tac = irGenerator.generate()
let asmGenerator = AssemblyGenerator(
    instructions: tac, allocations: irGenerator.allocations,
    stringLiterals: irGenerator.stringLiterals)
print(asmGenerator.generate())
