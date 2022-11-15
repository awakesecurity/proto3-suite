{- | This module provides functions to generate Haskell records using
   the large-records library.
-}
module Proto3.Suite.DotProto.Generate.LargeRecord where

import Language.Haskell.Syntax
import Proto3.Suite.DotProto.Generate.Syntax

-- | Generate 'NFData' instance for a type using large-generics
nfDataInstD :: String -> HsDecl
nfDataInstD typeName =
  instDecl_ (haskellName "NFData")
      [ type_ typeName ]
      [ HsFunBind [rnfDecl] ]
  where
    rnfDecl = match_ (HsIdent "rnf") []
                     (HsUnGuardedRhs (HsVar (lrName "grnf")))
                     []
