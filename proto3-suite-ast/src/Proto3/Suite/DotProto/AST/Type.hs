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
