
# 0.5.1
* Support newer versions of proto3-wire, bytestring, and turtle
* Increase minimum version of base for canonicalize-proto-file from 4.8 to 4.11
* Support numeric enumerator codes in JSONPB,
  as required by the protobuf standard.
* Handle unrecognized enumerator codes within packed repeated
  enumeration fields in the same way as in other enumeration fields.
* Avoid compilation errors in code generated for messages having BytesValue
  fields.  The errors would trigger only when the "swagger-wrapper-format"
  Cabal flag of this package was False.
* Code generated from a .proto file that imports the google.protobuf package
  no longer depends upon any Haskell module generated from "wrappers.proto".
  Instead the proto3-suite library provides the necessary functionality.

# 0.5.0
* [BREAKING CHANGE: Proto3 standard compatibility improvements](https://github.com/awakesecurity/proto3-suite/pull/143)
  * This is a breaking change due to adding a new `DotProtoMessageOption`
    constructor to `DotProtoMessagePart`, which requires updating any
    exhaustive pattern matches
* [BREAKING CHANGE: Don't capitalize first character of prefixed service method names](https://github.com/awakesecurity/proto3-suite/pull/171)
  * This is a breaking change because the generated Haskell code will have
    slightly different field names for service methods
* [BREAKING CHANGE: Remove `Optional` from Protobuf AST](https://github.com/awakesecurity/proto3-suite/pull/165)
  * This is a technically breaking change because we no longer support the
    `optional` keyword, but this wasn't supported by proto3 anyway
* [Support GHC 9.0.2](https://github.com/awakesecurity/proto3-suite/pull/176)
* [Fix dashes in Haskell module names](https://github.com/awakesecurity/proto3-suite/pull/173)
  * This prevents the code generator from generating invalid Haskell module
    names with dashes in them
* [Add `Message` instance for wrapped types](https://github.com/awakesecurity/proto3-suite/pull/162)
  * This adds `Message` instances for several scalar Haskell types that
    correspond to the standard `*Wrapper` protobuf types
* [Use Swagger schema `format` to distinguish wrapper types from primitives](https://github.com/awakesecurity/proto3-suite/pull/167)
* [Update codegen to add the `serverMaxMetadataSize` field to generated `ServiceOptions`](https://github.com/awakesecurity/proto3-suite/pull/181)
* [Fix module renaming in `compile-proto-file`](https://github.com/awakesecurity/proto3-suite/pull/183)
