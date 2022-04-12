{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE DerivingStrategies #-}

-- |
--
-- @since 1.0.0
module Proto3.Suite.DotProto.AST.Enumerate
  ( -- * Proto Enumerations
    DotProtoEnumPart (DotProtoEnumField, DotProtoEnumOption, DotProtoEnumEmpty),
    type DotProtoEnumValue,
  )
where

import Data.Int (Int32)

import Proto3.Suite.DotProto.AST.Identifier (DotProtoIdentifier)
import Proto3.Suite.DotProto.AST.Option (DotProtoOption)

--------------------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data DotProtoEnumPart
  = DotProtoEnumField DotProtoIdentifier DotProtoEnumValue [DotProtoOption]
  | DotProtoEnumOption DotProtoOption
  | DotProtoEnumEmpty
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------

-- TODO: remove
type DotProtoEnumValue = Int32
