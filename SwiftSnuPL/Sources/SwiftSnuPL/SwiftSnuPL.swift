import ArgumentParser
import Foundation

@main struct SwiftSnuPL: ParsableCommand {
    static var configuration = CommandConfiguration(
        abstract: "The SwiftSnuPL compiler.", subcommands: [Compile.self, Stdlib.self], defaultSubcommand: Compile.self)
}

struct Compile: ParsableCommand {
    @Argument var sourceFilePath: String?
    @Option(name: [.short, .customLong("output")]) var outputFilePath: String?
    @Flag(name: [.customShort("S"), .customLong("assembly")]) var generateAssembly = false

    mutating func run() throws {
        if sourceFilePath == "-" { sourceFilePath = nil }

        if sourceFilePath == nil && outputFilePath == nil { generateAssembly = true }

        let sourceData: Data
        if let sourceFilePath = sourceFilePath {
            let sourceURL = URL(fileURLWithPath: sourceFilePath)
            sourceData = try Data(contentsOf: sourceURL)
        } else {
            sourceData = try FileHandle.standardInput.readToEnd() ?? Data()
        }
        let scanner = Scanner(data: sourceData)
        let parser = Parser(scanner: scanner)
        guard let module = try? parser.parse(), !parser.hasError else {
            for parseError in parser.errors {
                if let token = parseError.token, let message = parseError.message {
                    print("Error: \(sourceFilePath ?? "stdin"):\(token.lineNumber):\(token.charPosition) \(message)")
                }
            }
            print("Error: Failed to parse source.")
            throw ExitCode.failure
        }
        let resolver = Resolver(module: module)
        guard (try? resolver.resolve()) != nil else {
            print("Error: Failed to resolve source.")
            throw ExitCode.failure
        }

        let irGenerator = IRGenerator(
            module: module, resolvedSymbols: resolver.resolvedSymbols, resolvedTypes: resolver.resolvedTypes)
        let tac = irGenerator.generate()
        let asmGenerator = AssemblyGenerator(
            instructions: tac, allocations: irGenerator.allocations, stringLiterals: irGenerator.stringLiterals,
            globalVariables: resolver.globalVariables)
        let assembly = asmGenerator.generate()
        if generateAssembly {
            if let outputFilePath = outputFilePath {
                let outputURL = URL(fileURLWithPath: outputFilePath)
                try assembly.data(using: .ascii)!.write(to: outputURL, options: .atomic)
            } else {
                print(assembly)
            }
        } else {
            print("Error: automatic compilation not yet supported. Try again with option '-S'.")
            throw ExitCode.failure// let sourceURL = URL(fileURLWithPath: sourceFilePath ?? "stdin.mod")
            // let sourceName = sourceURL.deletingPathExtension().lastPathComponent
            // let temporaryDirectoryURL = try FileManager.default.url(
            //     for: .itemReplacementDirectory, in: .userDomainMask, appropriateFor: sourceURL, create: true)
            // let assemblySourceURL = temporaryDirectoryURL.appendingPathComponent("\(sourceName).s")
            // let objectFileURL = temporaryDirectoryURL.appendingPathComponent("\(sourceName).o")
            // try assembly.data(using: .ascii)!.write(to: assemblySourceURL, options: .atomic)
            // let mainAssemblerProcess = Process()
            // mainAssemblerProcess.launchPath = "/usr/bin/aarch64-linux-gnu-as"
            // mainAssemblerProcess.arguments = ["-o", objectFileURL.path, assemblySourceURL.path]
            // try mainAssemblerProcess.run()
            // mainAssemblerProcess.waitUntilExit()
            // guard mainAssemblerProcess.terminationStatus == 0 else {
            //     print("Failed assembling object file.")
            //     throw ExitCode.failure
            // }
            // try FileManager.default.moveItem(at: objectFileURL, to: outputFileURL)
        }
    }
}

struct Stdlib: ParsableCommand {
    @Option(name: [.short, .customLong("output")]) var outputFilePath: String?
    @Flag(name: [.customShort("S"), .customLong("assembly")]) var generateAssembly = false

    mutating func run() throws {
        let assembly = """
                .data
            ReadInt.str:
                .asciz  "%d"
            ReadLong.str:
                .asciz  "%lld"
            WriteLong.str:
                .asciz  "%lld"
            WriteStr.str:
                .asciz  "%s"

                .text
                .globl ReadInt
            ReadInt:
                sub sp, sp, #16
                adrp    x0, ReadLong.str
                add x0, x0, :lo12:ReadLong.str
                add x1, sp, #4
                stp x29, x30, [sp, #-16]!
                mov x29, sp
                bl  scanf
                ldp x29, x30, [sp], #16
                ldrsw   x0, [sp, #4]
                add sp, sp, #16
                ret

                .globl ReadLong
            ReadLong:
                sub sp, sp, #16
                adrp    x0, ReadLong.str
                add x0, x0, :lo12:ReadLong.str
                add x1, sp, #8
                stp x29, x30, [sp, #-16]!
                mov x29, sp
                bl  scanf
                ldp x29, x30, [sp], #16
                ldr x0, [sp, #8]
                add sp, sp, #16
                ret

                .globl WriteLong
            WriteLong:
                mov x1, x0
                adrp    x0, WriteLong.str
                add x0, x0, :lo12:WriteLong.str
                b   printf

                .globl WriteInt
            WriteInt:
                b   WriteLong

                .globl WriteChar
            WriteChar:
                b   putchar

                .globl WriteStr
            WriteStr:
                add x1, x0, #8
                adrp    x0, WriteStr.str
                add x0, x0, :lo12:WriteStr.str
                b   printf

                .globl  WriteLn
            WriteLn:
                mov x0, #10
                b   putchar

                .globl DIM
            DIM:
                add x1, x1, #-1
                lsl x1, x1, #3
                add x9, x0, x1
                add x8, x9, #4
                ldrsw   x8, [x8]
                ldrsw   x9, [x9]
                sdiv    x0, x9, x8
                ret

            """
        if generateAssembly {
            if let outputFilePath = outputFilePath {
                let outputURL = URL(fileURLWithPath: outputFilePath)
                try assembly.data(using: .ascii)!.write(to: outputURL, options: .atomic)
            } else {
                print(assembly)
            }
        } else {
            print("Error: automatic compilation not yet supported. Try again with option '-S'.")
            throw ExitCode.failure// let sourceURL = URL(fileURLWithPath: sourceFilePath ?? "libSnuPL.s")
            // let sourceName = sourceURL.deletingPathExtension().lastPathComponent
            // let temporaryDirectoryURL = try FileManager.default.url(
            //     for: .itemReplacementDirectory, in: .userDomainMask, appropriateFor: sourceURL, create: true)
            // let assemblySourceURL = temporaryDirectoryURL.appendingPathComponent("libSnuPL.s")
            // let objectFileURL = temporaryDirectoryURL.appendingPathComponent("libSnuPL.o")
            // try assembly.data(using: .ascii)!.write(to: assemblySourceURL, options: .atomic)
            // let mainAssemblerProcess = Process()
            // mainAssemblerProcess.launchPath = "/usr/bin/aarch64-linux-gnu-as"
            // mainAssemblerProcess.arguments = ["-o", objectFileURL.path, assemblySourceURL.path]
            // try mainAssemblerProcess.run()
            // mainAssemblerProcess.waitUntilExit()
            // guard mainAssemblerProcess.terminationStatus == 0 else {
            //     print("Failed assembling object file.")
            //     throw ExitCode.failure
            // }
            // try FileManager.default.moveItem(at: objectFileURL, to: outputFileURL)
        }
    }
}

// let binaryURL = temporaryDirectoryURL.appendingPathComponent("main")
// let linkerProcess = Process()
// linkerProcess.launchPath = "/usr/bin/aarch64-linux-gnu-ld"
// linkerProcess.arguments = [
//     "-o", binaryURL.path, "-dynamic-linker", "/lib/ld-linux-aarch64.so.1",
//     "/usr/lib/aarch64-linux-gnu/crt1.o",
//     "/usr/lib/aarch64-linux-gnu/crti.o",
//     // Cross compiling:
//     // "/usr/aarch64-linux-gnu/lib/crt1.o",
//     // "/usr/aarch64-linux-gnu/lib/crti.o",
//     "-lc", libSnuPLObjectFileURL.path, mainObjectFileURL.path,
//     "/usr/lib/aarch64-linux-gnu/crtn.o",
//     // Cross compiling:
//     // "/usr/aarch64-linux-gnu/lib/crtn.o"
// ]
// linkerProcess.launch()
// linkerProcess.waitUntilExit()
// guard linkerProcess.terminationStatus == 0 else {
//     print("Failed linking main")
//     exit(-1)
// }
//
// try FileManager.default.moveItem(at: binaryURL, to: URL(fileURLWithPath: outputFileURL))
