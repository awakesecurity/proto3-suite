-- |
--
-- @since 1.0.0
module Proto3.Suite.DotProto.AST.Option
  ( -- * Options
    DotProtoOption (DotProtoOption),
    dotProtoOptionIdentifier,
    dotProtoOptionValue,

    -- * Identifiers
    DotProtoIdentifier (Single, Dots, Qualified, Anonymous),

    -- * Identifiers
    DotProtoValue (Identifier, StringLit, IntLit, FloatLit, BoolLit),

    -- * Path
    Path (Path),
    components,
  )
where

import Data.List.NonEmpty (NonEmpty)

--------------------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data DotProtoOption = DotProtoOption
  { dotProtoOptionIdentifier :: DotProtoIdentifier
  , dotProtoOptionValue :: DotProtoValue
  }
  deriving stock (Eq, Ord, Show)

--------------------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data DotProtoIdentifier
  = Single String
  | Dots Path
  | Qualified DotProtoIdentifier DotProtoIdentifier
  | Anonymous -- TODO: is there a better way to represent unnamed things?
  deriving stock (Eq, Ord, Show)

--------------------------------------------------------------------------------

-- | TODO: docs
--
-- Matches the definition of `constant` in the proto3 language spec
-- These are only used as rvalues
--
-- @since 1.0.0
data DotProtoValue
  = Identifier DotProtoIdentifier
  | StringLit String
  | IntLit Int
  | FloatLit Double
  | BoolLit Bool
  deriving stock (Eq, Ord, Show)

--------------------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
newtype Path = Path
  {components :: NonEmpty String}
  deriving stock (Eq, Ord, Show)
