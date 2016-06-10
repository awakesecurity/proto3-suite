# protobuf-wire

This package defines tools for working with protocol buffers version 3 in Haskell.

Specifically, it provides:

- Low-level functions for encoding and decoding messages
- Type classes for encoding and decoding messages, and instances for all
  wire formats identified in the specification
- A higher-level approach to encoding and decoding, based on `GHC.Generics`
- A way of creating `.proto` files from Haskell types.

See the `Data.Protobuf.Wire.Tutorial` module for more details.
