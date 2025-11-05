
module Proto3.Suite.DotProto.AST.Identifier
  ( -- * DotProtoIdentifier
    DotProtoIdentifier (..),
    fakePath,
  )
where

import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty

import GHC.Generics (Generic)

import Text.PrettyPrint qualified as PP
import Text.PrettyPrint.HughesPJClass (Pretty (..))

--- DotProtoIdentifier ---------------------------------------------------------

data DotProtoIdentifier
  = Single String
  | Dots (NonEmpty String)
  | Qualified DotProtoIdentifier DotProtoIdentifier
  | Anonymous -- [recheck] is there a better way to represent unnamed things
  deriving (Data, Eq, Generic, Ord, Show)

instance Pretty DotProtoIdentifier where
  pPrint (Single name)                    = PP.text name
  pPrint (Dots names)                     = PP.hcat . PP.punctuate (PP.text ".") $ PP.text <$> NonEmpty.toList names
  pPrint (Qualified qualifier identifier) = PP.parens (pPrint qualifier) <> PP.text "." <> pPrint identifier
  pPrint Anonymous                        = PP.empty

-- Used for testing
fakePath :: NonEmpty String
fakePath = "fakePath" :| []