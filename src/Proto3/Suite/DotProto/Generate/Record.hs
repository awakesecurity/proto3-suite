{- | This module provides functions to generate regular Haskell records
   without using the large-records library.
-}
module Proto3.Suite.DotProto.Generate.Record where

import Language.Haskell.Syntax
import Proto3.Suite.DotProto.Generate.Syntax

-- | Generate 'NFData' instance for a type using GHC generics
nfDataInstD :: String -> HsDecl
nfDataInstD typeName =
  instDecl_ (haskellName "NFData")
      [ type_ typeName ]
      []
