# `proto3-suite`

[![Hackage](https://img.shields.io/hackage/v/proto3-suite.svg?logo=haskell&label=proto3-suite)](https://hackage.haskell.org/package/proto3-suite)
[![Build Status](https://github.com/awakesecurity/proto3-suite/actions/workflows/ci.yml/badge.svg)](https://github.com/awakesecurity/proto3-suite/actions/workflows/ci.yml)

This package defines tools for working with protocol buffers version 3 in
Haskell.

This library provides a higher-level API to [the `proto3-wire` library](https://github.com/awakesecurity/proto3-wire)
that supports:

- Type classes for encoding and decoding messages, and instances for all wire
  formats identified in the specification
- A higher-level approach to encoding and decoding, based on `GHC.Generics`
- A way of creating `.proto` files from Haskell types

See [the `Proto3.Suite.Tutorial` module](https://hackage.haskell.org/package/proto3-suite/docs/Proto3-Suite-Tutorial.html)
for more details.

## Building

### GHC Versions

#### GHC 9.8

We do not support features making use of the "dhall" or "large-records"
libraries because neither library currently builds on GHC 9.8, and
therefore we cannot test our support.

Otherwise we support GHC 9.8 on Linux and Darwin.

#### GHC 9.6

We do not support features making use of the "large-records"
library because it does not currently build on GHC 9.6, and
therefore we cannot test our support.

Otherwise we support GHC 9.6 on Linux and Darwin.

#### GHC 9.4

Supported on Linux and Darwin.

#### GHC 9.2

Supported on Linux and Darwin.

#### GHC 9.0

Supported only on Linux because "crypton" fails a test on Darwin,
probably due to [this issue](https://github.com/kazu-yamamoto/crypton/issues/35).

### Nix shell + Cabal (recommended)

The Nix shell provides an incremental build environment (but see below for
testing). From the root of this repository, run:

```bash
$ nix-shell
[nix-shell]$ cabal build
```

Once your source code compiles and you want to test, run this instead:

```bash
$ nix-shell
[nix-shell]$ cabal configure --enable-tests
[nix-shell]$ cabal build
[nix-shell]$ cabal test
```

### Nix

Building with Nix is simple, but not incremental. From the root of this
repository, run:

```bash
$ nix-build --attr proto3-suite
```

The build products will be available via the `./result` symlink.

### Stack

We use Nix and Cabal at Awake Security, so those are the most exercised paths.
Stack support is provided on a best effort basis.

Building the library and executable components is straightforward. From the root
of this repository, run:

```bash
$ stack build
```

Building and running tests is more complicated when using Stack. You'll need to
use the `compile-proto-file` executable you just compiled to convert
`test_*.proto` files to Haskell modules, by running the following from the root
of this repository:

```bash
$ mkdir gen
$ for proto in $(find test-files -name 'test_*.proto'); do
    stack run compile-proto-file -- --out gen --includeDir test-files --proto "${proto#test-files/}"
  done
$ stack test
```

## Running the language interop tests

We test inter-language interop using `protoc`'s built-in Python code generation.
In order to successfully run these tests, you'll need to install the Google
`protobuf` Python library. It's best to create a `virtualenv` and then use `pip`
to install the right version (`virtualenv` is a Python utility which can be
installed with `pip`).

```bash
$ virtualenv pyenv
$ source pyenv/bin/activate
$ pip install protobuf==3.0.0b3  # Need the latest version for the newest protoc
```

`brew install python` may also work.

## Installing `compile-proto-file` and `canonicalize-proto-file`

To install the `compile-proto-file` and `canonicalize-proto-file` executables,
run the following commmand from the root of this repository:

```bash
$ nix-env --file default.nix --install --attr proto3-suite
```

To uninstall, removing the executables from your Nix user profile `PATH`, run:

```bash
$ nix-env --uninstall proto3-suite
```

## Using `compile-proto-file`

```bash
$ compile-proto-file --help
Usage: compile-proto-file [--includeDir DIR] [--extraInstanceFile FILE]
                          --proto FILE --out DIR
  Compiles a .proto file to a Haskell module

Available options:
  -h,--help                Show this help text
  --includeDir DIR         Path to search for included .proto files (can be
                           repeated, and paths will be searched in order; the
                           current directory is used if this option is not
                           provided)
  --extraInstanceFile FILE Additional file to provide instances that would
                           otherwise be generated. Can be used multiple times.
                           Types for which instance overrides are given must be
                           fully qualified.
  --proto FILE             Path to input .proto file
  --out DIR                Output directory path where generated Haskell modules
                           will be written (directory is created if it does not
                           exist; note that files in the output directory may be
                           overwritten!)
```

`compile-proto-file` bases the name (and hence, path) of the generated Haskell
module on the filename of the input `.proto` file, _relative_ to the include
path where it was found, converting snake case to camel case as needed.

As an example, let's assume this is our current directory structure before
performing any code generation:

```
.
├── my_protos
│   └── my_package.proto
└── other_protos
    └── google
        └── protobuf
            ├── duration.proto
            └── timestamp.proto
```

...where `my_package.proto` is:

```
syntax = "proto3";
package some_package_name;
import "google/protobuf/timestamp.proto";
import "google/protobuf/duration.proto";
message MyMessage {
  Timestamp timestamp = 1;
  Duration  duration  = 2;
}
```

Then, after running the following commands:

```bash
$ compile-proto-file --out gen --includeDir my_protos --includeDir other_protos --proto google/protobuf/duration.proto
$ compile-proto-file --out gen --includeDir my_protos --includeDir other_protos --proto google/protobuf/timestamp.proto
$ compile-proto-file --out gen --includeDir my_protos --includeDir other_protos --proto my_package.proto
```

...the directory tree will look like this:

```
.
├── gen
│   ├── Google
│   │   └── Protobuf
│   │       ├── Duration.hs
│   │       └── Timestamp.hs
│   └── MyPackage.hs
├── my_protos
│   └── my_package.proto
└── other_protos
    └── google
        └── protobuf
            ├── duration.proto
            └── timestamp.proto
```

Note that delimiting `.` characters in the input `.proto` basename are treated
as `/` characters, so the input filenames `google.protobuf.timestamp.proto` and
`google/protobuf/timestamp.proto` would produce the same generated Haskell
module name and path.

This is essentially the same module naming scheme as the `protoc` Python plugin
uses when compiling `.proto` files.

## Docker

For those unable to run Nix locally, a Dockerfile is provided:
```
docker build -t compile-proto-file .
docker run --rm -v $PWD:/opt compile-proto-file --proto proto/test.proto --out src/gen
```
