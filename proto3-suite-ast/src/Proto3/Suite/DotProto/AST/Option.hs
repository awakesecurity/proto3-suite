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

import Proto3.Suite.DotProto.AST.Core
  ( DotProtoIdentifier (Single),
    DotProtoValue,
  )

import Text.PrettyPrint.HughesPJClass (Pretty, pPrint)
import Text.PrettyPrint ((<+>))
import Text.PrettyPrint qualified as PP

--------------------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data DotProtoOption = DotProtoOption
  { dotProtoOptionIdentifier :: DotProtoIdentifier
  , dotProtoOptionValue :: DotProtoValue
  }
  deriving stock (Eq, Ord, Show)

-- | @since 1.0.0
instance Pretty DotProtoOption where
  pPrint (DotProtoOption key value) = pPrint key <+> PP.text "=" <+> pPrint value
