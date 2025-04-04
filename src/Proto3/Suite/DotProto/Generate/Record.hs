-- | This module provides functions to generate Haskell records
module Proto3.Suite.DotProto.Generate.Record where

import GHC.Types.Name.Occurrence (tcName)
import Proto3.Suite.DotProto.Generate.Syntax

-- | Generate `Control.DeepSeq.NFData` instance for a type using GHC generics
nfDataInstD :: HsDecl -> String -> HsDecl
nfDataInstD _ typeName =
  instDecl_ (haskellName tcName "NFData")
            [ type_ typeName ]
            []
