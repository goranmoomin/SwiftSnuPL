import Foundation

let source = #"""
    //
    // Matrix Add
    //
    // A valid SnuPL/2 test program
    //

    module MatrixAdd;

    const
      N : integer = 2*(2+2);
      ProgramName : char[] = "Matrix\t Adder\n\n";

    procedure minit(m: integer[][]);
    var x,y,v: integer;
    begin
      y := 0;
      v := 1;
      while (y < N) do
        x := 0;
        while (x < N) do
          m[y][x] := v;
          v := v+1;
          if (v = 10) then v := 0 end;
          x := x+1
        end;
        y := y+1
      end
    end minit;

    procedure madd(sum: integer[N][N]; a,b: integer[N][N]);
    var x,y: integer;
    begin
      y := 0;
      while (y < N) do
        x := 0;
        while (x < N) do
          sum[y][x] := a[y][x] + b[y][x];
          x := x+1
        end;
        y := y+1
      end
    end madd;

    procedure mprint(m: integer[][]; title: char[]);
    const MStr : char[] = ". Matrix ";
    var N,M,x,y: integer;
    begin
      M := DIM(m, 1);
      N := DIM(m, 2);

      WriteStr(title); WriteStr(MStr); WriteInt(M); WriteChar('x'); WriteInt(N);
      WriteLn();
      WriteStr("[\n");

      y := 0;
      while (y < M) do
        WriteStr("  "); WriteInt(y); WriteStr(":  [   ");

        x := 0;
        while (x < N) do
          WriteInt(m[y][x]); WriteStr("   ");
          x := x+1
        end;

        WriteStr("]\n");
        y := y+1
      end;

      WriteStr("]\n\n")
    end mprint;

    var
      A, B, C : integer[N][N];
    begin
      WriteStr(ProgramName); WriteLn();

      minit(A);
      minit(B);
      minit(C);

      mprint(A, "A");
      mprint(B, "B");

      madd(C, A, B);

      mprint(C, "C = A+B")
    end MatrixAdd.
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
    module: module, resolvedSymbols: resolver.resolvedSymbols, resolvedTypes: resolver.resolvedTypes
)
let tac = irGenerator.generate()
let asmGenerator = AssemblyGenerator(
    instructions: tac, allocations: irGenerator.allocations,
    stringLiterals: irGenerator.stringLiterals, globalVariables: resolver.globalVariables)
print(asmGenerator.generate())
