{-# LANGUAGE DerivingStrategies #-}

-- |
--
-- @since 1.0.0
module Proto3.Suite.DotProto.AST.Value
  ( -- * Values
    DotProtoValue (Identifier, StringLit, IntLit, FloatLit, BoolLit),
  )
where

import Proto3.Suite.DotProto.AST.Identifier (DotProtoIdentifier)

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
