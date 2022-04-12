{-# LANGUAGE DerivingStrategies #-}

-- |
--
-- @since 1.0.0
module Proto3.Suite.DotProto.AST.Identifier
  ( -- * Identifiers
    DotProtoIdentifier (Single, Dots, Qualified, Anonymous),

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
data DotProtoIdentifier
  = Single String
  | Dots Path
  | Qualified DotProtoIdentifier DotProtoIdentifier
  | Anonymous -- TODO: is there a better way to represent unnamed things?
  deriving stock (Eq, Ord, Show)

--------------------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
newtype Path = Path
  {components :: NonEmpty String}
  deriving stock (Eq, Ord, Show)
