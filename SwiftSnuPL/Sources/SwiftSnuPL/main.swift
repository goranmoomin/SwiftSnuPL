import Foundation

let testsDirectoryURL = URL(fileURLWithPath: #file).deletingLastPathComponent()
    .appendingPathComponent("../../../test/parser/").standardized

for testSourceURL in try FileManager.default
    .contentsOfDirectory(at: testsDirectoryURL, includingPropertiesForKeys: nil)
    .filter({ $0.pathExtension == "mod" })
{
    print("parsing and resolving \(testSourceURL.lastPathComponent)...")
    let sourceData = try Data(contentsOf: testSourceURL)
    let scanner = Scanner(data: sourceData)
    let parser = Parser(scanner: scanner)
    guard let module = try? parser.parse() else {
        for parseError in parser.errors {
            if let token = parseError.token, let message = parseError.message {
                print("error: \(token.lineNumber):\(token.charPosition) \(message)")
            }
        }
        print("error: parsing failed.")
        continue
    }
    let resolver = Resolver(module: module)
    guard (try? resolver.resolve()) != nil else {
        print("error: resolving failed.")
        continue
    }

    let generator = Generator(
        module: module, resolvedSymbols: resolver.resolvedSymbols,
        resolvedTypes: resolver.resolvedTypes)
    let tac = generator.generate()
    for (symbol, instructions) in tac { print(format(symbol: symbol, instructions: instructions)) }
    print("succesfully resolved \(testSourceURL.lastPathComponent).")
}
