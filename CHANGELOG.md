# 0.9.0
* [#272](https://github.com/awakesecurity/proto3-suite/pull/272) removed support for `large-records` and `large-generics`.
* Support aeson-2.2.

# 0.8.3
* Fix overlong encoding of packed "sint32" fields containing elements in
  [-0x80000000, -0x40000001] or [0x40000000, 0x7FFFFFFF], which increased
  message size and hindered forward compatibility of "sint32" with "sint64".
* Add "--stringType" as a preferred spelling of "--string-type"
  because its style matches that of other options.
* Add compiler error "RedefinedFields".
* Export codeFromEnumerated and codeToEnumerated.
* Add an experimental feature to encode without using
  intermediate data structures; see Proto3.Suite.Form and
  the new compile-proto-file option --typeLevelFormat

# 0.8.2
* Support GHC 9.10.
* Test with nixpkgs-24.11.

# 0.8.1
* Fix support for dhall-1.42.
* Support dhall on GHC 9.8.
* Fix aeson upper bound in library target (was correct in test target).
* Fix default compiler version in shell.nix.
* Test with GHC 9.8.2 instead of GHC 9.8.1 and GHC 9.6.5 instead of GHC 9.6.2.
* Test with nixpkgs-24.05 but always using aeson-2.1.2.1.

# 0.8.0
* [BREAKING CHANGE: Use "ghc" library in place of "haskell-src".]
  The "ghc" library is now used to parse and print Haskell source code.
  Switching to "ghc" adds support for language features beyond Haskell 98
  and should improve diagnostic messages for sources specified with
  "--extraInstanceFile".  Breakage should be limited to users of:
  * Proto3.Suite.DotProto.Generate
  * Proto3.Suite.DotProto.Generate.LargeRecord
  * Proto3.Suite.DotProto.Generate.Syntax
* Drop support for GHC 8.10.
* On Darwin, drop support for GHC 9.0.
* Add support for GHC 9.6 (without large-records).
* Add support for GHC 9.8 (without large-records, dhall).

# 0.7.0
* Support GHC 9.2, 9.4.
* Support proto files without a package declaration.
* Modify "Eq (Enumerated a)" to identify "Enumerated (Right e)"
  with "Enumerated (Left (fromProtoEnum e))" because those two
  values encode to the same octet sequence.  They are already
  equivalent as arguments to "isDefault @(Enumerated e)".
* Derive Data and Generic instances for Protobuf AST types.

# 0.6.0
* Support use of ShortText as the Haskell type of a protobuf string.
* Replace OverrideToSchema with String and Bytes in order to clarify which
  instances of various type classes are selected, expecially ToSchema.
* To disambiguate "String" in generated code, rename
  the qualified import of AST-related identifiers.
* Improve test coverage.

# 0.5.2
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

# 0.5.1
* Support newer versions of proto3-wire, bytestring, and turtle
* Increase minimum version of base for canonicalize-proto-file from 4.8 to 4.11

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
