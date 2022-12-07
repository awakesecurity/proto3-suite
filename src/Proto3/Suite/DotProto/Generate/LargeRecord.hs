{- | This module provides functions to generate Haskell records using
   the large-records library.
-}
module Proto3.Suite.DotProto.Generate.LargeRecord where

import Language.Haskell.Syntax
import Proto3.Suite.DotProto.Generate.Syntax

isLargeRecord :: HsDecl -> Bool
isLargeRecord (HsDataDecl _ _ _ _ [HsRecDecl _ _ (_fld1:_fld2:_)] _) = True
isLargeRecord _ = False

-- | Generate 'NFData' instance for a type using large-generics
nfDataInstD :: HsDecl -> String -> HsDecl
nfDataInstD typeDecl typeName =
  instDecl_ (haskellName "NFData")
      [ type_ typeName ]
      [ HsFunBind [rnfDecl] | isLargeRecord typeDecl ]
  where
    rnfDecl = match_ (HsIdent "rnf") []
                     (HsUnGuardedRhs (HsVar (lrName "grnf")))
                     []
