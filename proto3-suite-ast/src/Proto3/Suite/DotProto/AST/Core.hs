-- |
--
-- @since 1.0.0
module Proto3.Suite.DotProto.AST.Core
  ( -- * Identifiers
    DotProtoIdentifier (Single, Dots, Qualified, Anonymous),

    -- * Identifiers
    DotProtoValue (Identifier, StringLit, IntLit, FloatLit, BoolLit),

    -- * Path
    Path (Path),
    components,
  )
where

import Data.Char (toLower)
import Text.PrettyPrint.HughesPJClass  (Pretty, pPrint)
import Text.PrettyPrint ((<+>))
import Text.PrettyPrint qualified as PP

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty

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

-- | @since 1.0.0
instance Pretty DotProtoIdentifier where
  pPrint (Single name)                    = PP.text name
  pPrint (Dots (Path names))              = PP.hcat . PP.punctuate (PP.text ".") $ PP.text <$> NonEmpty.toList names
  pPrint (Qualified qualifier identifier) = PP.parens (pPrint qualifier) <> PP.text "." <> pPrint identifier
  pPrint Anonymous                        = PP.empty

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

-- | @since 1.0.0
instance Pretty DotProtoValue where
  pPrint (Identifier value) = pPrint value
  pPrint (StringLit  value) = PP.text $ show value
  pPrint (IntLit     value) = PP.text $ show value
  pPrint (FloatLit   value) = PP.text $ show value
  pPrint (BoolLit    value) = PP.text $ toLower <$> show value

--------------------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
newtype Path = Path
  {components :: NonEmpty String}
  deriving stock (Eq, Ord, Show)
