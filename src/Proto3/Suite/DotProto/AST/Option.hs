{-# LANGUAGE DerivingStrategies #-}

-- |
--
-- @since 1.0.0
module Proto3.Suite.DotProto.AST.Option
  ( -- * Options
    DotProtoOption (DotProtoOption),
    dotProtoOptionIdentifier,
    dotProtoOptionValue,
  )
where

import Proto3.Suite.DotProto.AST.Identifier (DotProtoIdentifier (Single))
import Proto3.Suite.DotProto.AST.Value (DotProtoValue)

--------------------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data DotProtoOption = DotProtoOption
  { dotProtoOptionIdentifier :: DotProtoIdentifier
  , dotProtoOptionValue :: DotProtoValue
  }
  deriving stock (Eq, Ord, Show)
