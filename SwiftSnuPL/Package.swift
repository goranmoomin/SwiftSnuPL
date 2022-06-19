// swift-tools-version: 5.5

import PackageDescription

let package = Package(
    name: "SwiftSnuPL", products: [.executable(name: "SwiftSnuPL", targets: ["SwiftSnuPL"])],
    targets: [
        .target(name: "CppSnuPL", dependencies: []), .executableTarget(name: "SwiftSnuPL", dependencies: ["CppSnuPL"]),
    ], cxxLanguageStandard: .cxx11)
