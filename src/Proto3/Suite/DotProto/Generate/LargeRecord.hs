{- | This module provides functions to generate Haskell records using
   the large-records library.
-}

{-# LANGUAGE ViewPatterns #-}

module Proto3.Suite.DotProto.Generate.LargeRecord where

import Data.Maybe (isJust)
import GHC.Hs hiding (HsDecl)
import GHC.Types.Name.Occurrence (tcName, varName)
import GHC.Types.SrcLoc (GenLocated(..))
import Proto3.Suite.DotProto.Generate.Syntax

typeNameIfLargeRecord :: HsDecl -> Maybe HsName
typeNameIfLargeRecord ( L _ ( TyClD _ DataDecl{ tcdLName = n
                                              , tcdDataDefn = HsDataDefn {dd_cons = [c]}
                                              } ) )
    | largeRec c = Just n
    | otherwise = Nothing
  where
    largeRec :: HsConDecl -> Bool
    largeRec (L _ (ConDeclH98{con_args = RecCon (L _ fields)})) = twoOrMore fields
    largeRec _ = False
      -- TO DO: Support GADT syntax if we ever generate such data type definitions.

    twoOrMore :: [LConDeclField GhcPs] -> Bool
    twoOrMore (_ : _ : _) = True                       -- two or more field declarations
    twoOrMore [L _ (cd_fld_names -> _ : _ : _)] = True -- single declaration of two or more fields
    twoOrMore _ = False

typeNameIfLargeRecord _ = Nothing

isLargeRecord :: HsDecl -> Bool
isLargeRecord = isJust . typeNameIfLargeRecord

-- | Generate 'NFData' instance for a type using large-generics
nfDataInstD :: HsDecl -> String -> HsDecl
nfDataInstD typeDecl typeName =
    instDecl_ (haskellName tcName "NFData")
              [ type_ typeName ]
              [ rnfDecl | isLargeRecord typeDecl ]
  where
    rnfDecl = function_ (unqual_ varName "rnf")
                        [ ([], grhss_ [ unguardedRhs_ (var_ (lrName varName "grnf")) ] [] []) ]
