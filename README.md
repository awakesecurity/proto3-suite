# `proto3-suite`

This package defines tools for working with protocol buffers version 3 in Haskell.

This library provides a higher-level API to
[the `proto3-wire` library](https://github.com/awakenetworks/proto3-wire) that supports:

- Type classes for encoding and decoding messages, and instances for all
  wire formats identified in the specification
- A higher-level approach to encoding and decoding, based on `GHC.Generics`
- A way of creating `.proto` files from Haskell types.

See the `Proto3.Suite.Tutorial` module for more details.

# Running the language interop tests

We test inter-language interop using protoc's built-in Python code generation. In
order to successfully run these tests, you'll need to install the google protobuf
Python library. It's best to create a virtualenv and then use pip to install the
right version (virtualenv is a python utility which can be installed with pip).

```bash
$ virtualenv pyenv
$ source pyenv/bin/activate
$ pip install protobuf==3.0.0b3  # Need the latest version for the newest protoc
```

`brew install python` may also work.

Alternate, the `nix-shell` environment provides a complete incremental build
and test environment. From the root fo this repository:

```bash
$ nix-shell release.nix -A proto3-suite.env
[nix-shell]$ cabal configure --with-gcc=clang --enable-tests
[nix-shell]$ cabal build
[nix-shell]$ cabal test
```

# `compile-proto-file`

Run the following commmand from the root of this repository to install the
`compile-proto-file` executable:

```bash
$ nix-env -iA proto3-suite -f release.nix
```

To remove it from your nix user profile path, use:

```bash
$ nix-env -e proto3-suite
```
