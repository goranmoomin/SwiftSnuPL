FROM swift:5.6-focal AS build-stage
WORKDIR /root
COPY . .
RUN swift build --configuration release --static-swift-stdlib

FROM scratch AS export-stage
COPY --from=build-stage /root/.build/release/SwiftSnuPL /
