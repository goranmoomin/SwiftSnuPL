import Foundation

let source = #"""
    module big_sort;

    const N: integer = 100000;

    procedure insertion_sort(a: integer[]);
    var i, j, t: integer;
    begin
        i := 0;
        while (i < DIM(a, 1)) do
            t := a[i];
            j := i;
            while ((j > 0) && (a[j - 1] > t)) do
                a[j] := a[j - 1];
                j := j - 1
            end;
            a[j] := t;
            i := i + 1
        end
    end insertion_sort;

    var arr: integer[N];
    var i: integer;
        correct: integer;
    begin
        i := 0;
        while (i < N / 2) do
            arr[i] := i * 2;
            i := i + 1
        end;
        while (i < N) do
            arr[i] := ((i - N / 2) * 2) + 1;
            i := i + 1
        end;

        insertion_sort(arr);
        i := 0;
        correct := 0;
        while (i < N) do
            if (arr[i] = i) then
                correct := correct + 1
            end;
            i := i + 1
        end;

        WriteInt(correct); WriteStr(" out of "); WriteInt(N); WriteStr(" correct.\n")
    end big_sort.

    """#

let sourceData = source.data(using: .utf8)!
let scanner = Scanner(data: sourceData)
let parser = Parser(scanner: scanner)
guard let module = try? parser.parse(), !parser.hasError else {
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
    module: module, resolvedSymbols: resolver.resolvedSymbols, resolvedTypes: resolver.resolvedTypes)
let tac = irGenerator.generate()
let asmGenerator = AssemblyGenerator(
    instructions: tac, allocations: irGenerator.allocations, stringLiterals: irGenerator.stringLiterals,
    globalVariables: resolver.globalVariables)
print(asmGenerator.generate())
