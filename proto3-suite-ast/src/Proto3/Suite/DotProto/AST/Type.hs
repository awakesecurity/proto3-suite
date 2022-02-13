-- |
--
-- @since 1.0.0
module Proto3.Suite.DotProto.AST.Type
  ( -- * Types
    DotProtoType (Prim, Repeated, NestedRepeated, Map),

    -- * Primitive Types
    DotProtoPrimType
      ( Int32,
        Int64,
        SInt32,
        SInt64,
        UInt32,
        UInt64,
        Fixed32,
        Fixed64,
        SFixed32,
        SFixed64,
        String,
        Bytes,
        Bool,
        Float,
        Double,
        Named
      ),
  )
where

import Text.PrettyPrint.HughesPJClass  (Pretty, pPrint)
import Text.PrettyPrint ((<+>))
import Text.PrettyPrint qualified as PP

import Proto3.Suite.DotProto.AST.Core (DotProtoIdentifier)

--------------------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data DotProtoType
  = Prim DotProtoPrimType
  | Repeated DotProtoPrimType
  | NestedRepeated DotProtoPrimType
  | Map DotProtoPrimType DotProtoPrimType
  deriving stock (Eq, Show)

-- | @since 1.0.0
instance Pretty DotProtoType where
  pPrint (Prim           ty) = pPrint ty
  pPrint (Repeated       ty) = PP.text "repeated" <+> pPrint ty
  pPrint (NestedRepeated ty) = PP.text "repeated" <+> pPrint ty
  pPrint (Map keyty valuety) = PP.text "map<" <> pPrint keyty <> PP.text ", " <> pPrint valuety <> PP.text ">"

--------------------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data DotProtoPrimType
  = Int32
  | Int64
  | SInt32
  | SInt64
  | UInt32
  | UInt64
  | Fixed32
  | Fixed64
  | SFixed32
  | SFixed64
  | String
  | Bytes
  | Bool
  | Float
  | Double
  | Named DotProtoIdentifier
  deriving stock (Eq, Ord, Show)

-- | @since 1.0.0
instance Pretty DotProtoPrimType where
  pPrint (Named i)  = pPrint i
  pPrint Int32      = PP.text "int32"
  pPrint Int64      = PP.text "int64"
  pPrint SInt32     = PP.text "sint32"
  pPrint SInt64     = PP.text "sint64"
  pPrint UInt32     = PP.text "uint32"
  pPrint UInt64     = PP.text "uint64"
  pPrint Fixed32    = PP.text "fixed32"
  pPrint Fixed64    = PP.text "fixed64"
  pPrint SFixed32   = PP.text "sfixed32"
  pPrint SFixed64   = PP.text "sfixed64"
  pPrint String     = PP.text "string"
  pPrint Bytes      = PP.text "bytes"
  pPrint Bool       = PP.text "bool"
  pPrint Float      = PP.text "float"
  pPrint Double     = PP.text "double"
