// swift-tools-version: 5.5

import PackageDescription

let package = Package(
    name: "SwiftSnuPL", platforms: [.macOS(.v11)], products: [.executable(name: "SwiftSnuPL", targets: ["SwiftSnuPL"])],
    dependencies: [.package(url: "https://github.com/apple/swift-argument-parser", from: "1.0.0")],
    targets: [
        .target(name: "CppSnuPL", dependencies: []),
        .executableTarget(
            name: "SwiftSnuPL",
            dependencies: ["CppSnuPL", .product(name: "ArgumentParser", package: "swift-argument-parser")]),
    ], cxxLanguageStandard: .cxx11)
