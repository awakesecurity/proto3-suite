-- |
-- = Protocol Buffers v3 for Haskell
--
-- This package defines tools for working with protocol buffers version 3 in
-- Haskell.
--
-- Specifically, it provides:
--
-- * Low-level functions for encoding and decoding messages
-- * Type classes for encoding and decoding messages, and instances for all
--   wire formats identified in the specification
-- * A higher-level approach to encoding and decoding, based on "GHC.Generics"
-- * A way of creating .proto files from Haskell types.
--
-- See the "Data.Protobuf.Wire.Tutorial" module for more details.

module Data.Protobuf.Wire
  (
  -- * Message Encoding/Decoding
    toLazyByteString
  , fromByteString
  , Message(..)
  , MessageField(..)
  , Primitive(..)
  , HasDefault(..)
  , FieldNumber(..)
  , fieldNumber

  -- * Documentation
  , message
  , enum
  , RenderingOptions(..)
  , Named(..)
  , Finite(..)

  -- * Wire Formats
  , Fixed(..)
  , Signed(..)
  , Enumerated(..)
  , Packed(..)
  , Nested(..)
  , UnpackedVec(..)
  , PackedVec(..)
  , NestedVec(..)

  -- * AST
  , module DotProto
  ) where

import Data.Protobuf.Wire.Class
import Data.Protobuf.Wire.Types
import Data.Protobuf.Wire.DotProto as DotProto
