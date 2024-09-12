{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

{-| Utilities to manipulate Haskell AST -}
module Proto3.Suite.DotProto.Generate.Syntax where

import Data.Functor ((<&>))
import Data.Maybe (maybeToList)
import GHC.Data.Bag (listToBag)
import GHC.Data.FastString (mkFastString)
import qualified GHC.Hs as GHC
         (HsDecl(..), HsDerivingClause(..), HsModule(..), HsTyVarBndr(..), HsType(..))
import GHC.Types.Basic (Origin(..))
import GHC.Types.Name.Occurrence (NameSpace, dataName, mkOccName, tcName, tvName, varName)
import GHC.Types.Name.Reader (mkRdrQual, mkRdrUnqual, rdrNameSpace)
import GHC.Types.SrcLoc (GenLocated(..), SrcSpan, generatedSrcSpan)

#if MIN_VERSION_ghc(9,8,0)
import Control.Arrow ((***))
import Data.Bool (bool)
import Data.Ratio ((%))
import Data.Void (Void)
import GHC.Hs hiding (HsBind, HsDecl, HsDerivingClause, HsOuterFamEqnTyVarBndrs,
                      HsOuterSigTyVarBndrs, HsTyVarBndr, HsType)
import qualified GHC.Hs as GHC (HsOuterFamEqnTyVarBndrs, HsOuterSigTyVarBndrs)
import GHC.Types.Basic (DoPmc(..), TopLevelFlag(..))
import GHC.Types.Fixity (LexicalFixity(..))
import GHC.Types.PkgQual (RawPkgQual(..))
import GHC.Types.SourceText
         (IntegralLit(..), FractionalExponentBase(..), FractionalLit(..), SourceText(..))
#elif MIN_VERSION_ghc(9,6,0)
import Control.Arrow ((***))
import Data.Bool (bool)
import Data.Ratio ((%))
import Data.Void (Void)
import GHC.Hs hiding (HsBind, HsDecl, HsDerivingClause, HsOuterFamEqnTyVarBndrs,
                      HsOuterSigTyVarBndrs, HsTyVarBndr, HsType)
import qualified GHC.Hs as GHC (HsOuterFamEqnTyVarBndrs, HsOuterSigTyVarBndrs)
import GHC.Types.Basic (TopLevelFlag(..))
import GHC.Types.Fixity (LexicalFixity(..))
import GHC.Types.PkgQual (RawPkgQual(..))
import GHC.Types.SourceText
         (IntegralLit(..), FractionalExponentBase(..), FractionalLit(..), SourceText(..))
#elif MIN_VERSION_ghc(9,4,0)
import Data.Ratio ((%))
import Data.Void (Void)
import GHC.Hs hiding (HsBind, HsDecl, HsDerivingClause, HsOuterFamEqnTyVarBndrs,
                      HsOuterSigTyVarBndrs, HsTyVarBndr, HsType)
import qualified GHC.Hs as GHC (HsOuterFamEqnTyVarBndrs, HsOuterSigTyVarBndrs)
import GHC.Types.Basic (PromotionFlag(..), TopLevelFlag(..))
import GHC.Types.Fixity (LexicalFixity(..))
import GHC.Types.PkgQual (RawPkgQual(..))
import GHC.Types.SourceText
         (IntegralLit(..), FractionalExponentBase(..), FractionalLit(..), SourceText(..))
import GHC.Types.SrcLoc (LayoutInfo(..))
import GHC.Unit (IsBootInterface(..))
import GHC.Unit.Module (ModuleName, mkModuleName)
#elif MIN_VERSION_ghc(9,2,0)
import Data.Ratio ((%))
import Data.Void (Void)
import GHC.Hs hiding (HsBind, HsDecl, HsDerivingClause, HsOuterFamEqnTyVarBndrs,
                      HsOuterSigTyVarBndrs, HsTyVarBndr, HsType)
import qualified GHC.Hs as GHC (HsOuterFamEqnTyVarBndrs, HsOuterSigTyVarBndrs)
import GHC.Types.Basic (PromotionFlag(..), TopLevelFlag(..))
import GHC.Types.Fixity (LexicalFixity(..))
import GHC.Types.SourceText
         (IntegralLit(..), FractionalExponentBase(..), FractionalLit(..), SourceText(..))
import GHC.Types.SrcLoc (LayoutInfo(..), noLoc)
import GHC.Unit (IsBootInterface(..))
import GHC.Unit.Module (ModuleName, mkModuleName)
#else
import GHC.Hs hiding (HsBind, HsDecl, HsDerivingClause, HsTyVarBndr, HsType)
import GHC.Parser.Annotation (IsUnicodeSyntax(..))
import GHC.Types.Basic
         (IntegralLit(..), FractionalLit(..), LexicalFixity(..),
          PromotionFlag(..), SourceText(..), TopLevelFlag(..))
import GHC.Types.SrcLoc (LayoutInfo(..), Located, noLoc)
import GHC.Unit (IsBootInterface(..))
import GHC.Unit.Module (ModuleName, mkModuleName)
#endif

#if MIN_VERSION_base(4,16,0)
import GHC.Exts (considerAccessible)
#endif

type HsAlt = LMatch GhcPs HsExp
type HsBangType = LBangType GhcPs
type HsBind = LHsBind GhcPs
type HsConDecl = LConDecl GhcPs
type HsDecl = LHsDecl GhcPs
type HsDerivStrategy = LDerivStrategy GhcPs
type HsDerivingClause = LHsDerivingClause GhcPs
type HsExp = LHsExpr GhcPs
type HsExportSpec = LIE GhcPs
type HsGrhs = LGRHS GhcPs HsExp
type HsGuardedAlts = GRHSs GhcPs HsExp
type HsImportDecl = LImportDecl GhcPs
type HsImportSpec = LIE GhcPs
type HsMatch = LMatch GhcPs HsExp
type HsName = LIdP GhcPs
type HsOuterFamEqnTyVarBndrs =
#if MIN_VERSION_ghc(9,2,0)
  GHC.HsOuterFamEqnTyVarBndrs GhcPs
#else
  ()
#endif
type HsOuterSigTyVarBndrs =
#if MIN_VERSION_ghc(9,2,0)
  GHC.HsOuterSigTyVarBndrs GhcPs
#else
  ()
#endif
type HsPat = LPat GhcPs
type HsQName = LIdP GhcPs
type HsQOp = LHsExpr GhcPs
type HsSig = LSig GhcPs
type HsTyVarBndr = LHsTyVarBndr
#if MIN_VERSION_ghc(9,8,0)
                                (HsBndrVis GhcPs)
#else
                                ()
#endif
                                   GhcPs
type HsType = LHsType GhcPs
type Module = ModuleName

class SyntaxDefault a
  where
    synDef :: a

instance SyntaxDefault ()
  where
    synDef = ()

instance SyntaxDefault (Maybe a)
  where
    synDef = Nothing

instance SyntaxDefault [a]
  where
    synDef = []

instance (SyntaxDefault a, SyntaxDefault b) => SyntaxDefault (a, b)
  where
    synDef = (synDef, synDef)

instance SyntaxDefault NoExtField
  where
    synDef = NoExtField

instance SyntaxDefault SourceText
  where
    synDef = NoSourceText

instance SyntaxDefault SrcSpan
  where
    synDef = generatedSrcSpan

#if MIN_VERSION_ghc(9,8,0)

instance SyntaxDefault (HsBndrVis GhcPs)
  where
    synDef = HsBndrRequired

#endif

#if MIN_VERSION_ghc(9,4,0)

instance SyntaxDefault (SrcAnn a)
  where
    synDef = noSrcSpanA

instance SyntaxDefault a => SyntaxDefault (GenLocated TokenLocation a)
  where
    synDef = L NoTokenLoc synDef

instance SyntaxDefault (HsToken tok)
  where
    synDef = HsTok

instance SyntaxDefault (HsUniToken tok utok)
  where
    synDef = HsNormalTok

#else

instance SyntaxDefault IsUnicodeSyntax
  where
    synDef = NormalSyntax

#endif

#if MIN_VERSION_ghc(9,2,0)

instance SyntaxDefault e => SyntaxDefault (GenLocated (SrcAnn a) e)
  where
    synDef = noLocA synDef

instance SyntaxDefault (EpAnn a)
  where
    synDef = EpAnnNotUsed

instance SyntaxDefault EpAnnComments
  where
    synDef = emptyComments

instance SyntaxDefault AnnSortKey
  where
    synDef = NoAnnSortKey

pattern PfxCon :: [arg] -> HsConDetails Void arg r
pattern PfxCon args = PrefixCon [] args

#else

instance SyntaxDefault e => SyntaxDefault (GenLocated SrcSpan e)
  where
    synDef = noLocA synDef

pattern PfxCon :: [arg] -> HsConDetails arg r
pattern PfxCon args = PrefixCon args

pattern HsSig :: NoExtField -> () -> HsType -> HsImplicitBndrs GhcPs HsType
pattern HsSig ext bndrs ty <- (((), ) -> (bndrs, HsIB ext ty))
  where
    HsSig ext () ty = HsIB ext ty

pattern XHsSigType :: NoExtCon -> HsImplicitBndrs GhcPs HsType
pattern XHsSigType impossible = XHsImplicitBndrs impossible

{-# COMPLETE HsSig, XHsSigType #-}

pattern DctSingle ::
  NoExtField ->
  GenLocated SrcSpan (HsImplicitBndrs GhcPs HsType) ->
  [HsImplicitBndrs GhcPs HsType]
pattern DctSingle ext sig <- ((NoExtField, ) -> (ext, [noLoc -> sig]))
  where
    DctSingle NoExtField (L _ sig) = [sig]

pattern DctMulti ::
  NoExtField ->
  [GenLocated SrcSpan (HsImplicitBndrs GhcPs HsType)] ->
  [HsImplicitBndrs GhcPs HsType]
pattern DctMulti ext sigs <- ((NoExtField, ) -> (ext, map noLoc -> sigs))
  where
    DctMulti NoExtField sigs = sigs <&> (\(L _ sig) -> sig)

{-# COMPLETE DctSingle, DctMulti #-}

noLocA :: a -> Located a
noLocA = noLoc

#endif

#if !MIN_VERSION_ghc(9,4,0)

dataConCantHappen :: NoExtCon -> a
dataConCantHappen = noExtCon

#endif

haskellName, jsonpbName, grpcName, lrName, protobufName,
  protobufASTName, protobufFormName, proxyName ::
  NameSpace -> String -> HsQName
haskellName      = qual_ haskellNS
jsonpbName       = qual_ (mkModuleName "HsJSONPB")
grpcName         = qual_ (mkModuleName "HsGRPC")
lrName           = qual_ (mkModuleName "LR")
protobufName     = qual_ (mkModuleName "HsProtobuf")
protobufASTName  = qual_ (mkModuleName "HsProtobufAST")
protobufFormName = qual_ protobufFormNS
proxyName        = qual_ (mkModuleName "Proxy")

haskellNS :: ModuleName
haskellNS = mkModuleName "Hs"

protobufFormNS :: ModuleName
protobufFormNS = mkModuleName "HsProtobufForm"

protobufFormType :: NameSpace -> String -> HsType
protobufFormType ns = typeNamed_ . protobufFormName ns

formProtoTypeT :: HsType
formProtoTypeT = protobufFormType tcName "ProtoType"

formNamesOf, formNumberOf, formOneOfOf, formRepetitionOf, formProtoTypeOf :: HsQName
formNamesOf = protobufFormName tcName "NamesOf"
formNumberOf = protobufFormName tcName "NumberOf"
formOneOfOf = protobufFormName tcName "OneOfOf"
formRepetitionOf = protobufFormName tcName "RepetitionOf"
formProtoTypeOf = protobufFormName tcName "ProtoTypeOf"

formFieldNotFound, formFieldOrOneOfNotFound :: HsType
formFieldNotFound = protobufFormType tcName "FieldNotFound"
formFieldOrOneOfNotFound = protobufFormType tcName "FieldOrOneOfNotFound"

formImplicitT, formAlternativeT :: HsType
formImplicitT = protobufFormType dataName "Implicit"
formAlternativeT = protobufFormType dataName "Alternative"

formUnpackedT, formPackedT :: HsType
formUnpackedT = protobufFormType dataName "Unpacked"
formPackedT = protobufFormType dataName "Packed"

formRepetitionT, formSingularT, formOptionalT, formRepeatedT :: HsType
formRepetitionT = protobufFormType tcName "Repetition"
formSingularT = protobufFormType dataName "Singular"
formOptionalT = protobufFormType dataName "Optional"
formRepeatedT = protobufFormType dataName "Repeated"

formInt32T, formInt64T, formSInt32T, formSInt64T, formUInt32T, formUInt64T,
  formFixed32T, formFixed64T, formSFixed32T, formSFixed64T,
  formStringT, formBytesT, formBoolT, formFloatT, formDoubleT,
  formEnumerationT, formMessageT, formMapT :: HsType
formInt32T = protobufFormType dataName "Int32"
formInt64T = protobufFormType dataName "Int64"
formSInt32T = protobufFormType dataName "SInt32"
formSInt64T = protobufFormType dataName "SInt64"
formUInt32T = protobufFormType dataName "UInt32"
formUInt64T = protobufFormType dataName "UInt64"
formFixed32T = protobufFormType dataName "Fixed32"
formFixed64T = protobufFormType dataName "Fixed64"
formSFixed32T = protobufFormType dataName "SFixed32"
formSFixed64T = protobufFormType dataName "SFixed64"
formStringT = protobufFormType dataName "String"
formBytesT = protobufFormType dataName "Bytes"
formBoolT = protobufFormType dataName "Bool"
formFloatT = protobufFormType dataName "Float"
formDoubleT = protobufFormType dataName "Double"
formEnumerationT = protobufFormType dataName "Enumeration"
formMessageT = protobufFormType dataName "Message"
formMapT = protobufFormType dataName "Map"

formWrapperT :: HsType
formWrapperT = protobufFormType tcName "Wrapper"

--------------------------------------------------------------------------------
--
-- * Wrappers around ghc constructors
--

app :: HsExp -> HsExp -> HsExp
app f x = mkHsApp f (paren x)

apply :: HsExp -> [HsExp] -> HsExp
apply f xs = mkHsApps f (map paren xs)

appAt :: HsExp -> HsType -> HsExp
appAt f t = noLocA (HsAppType synDef f
#if MIN_VERSION_ghc(9,6,0)
                                       synDef
#endif
                                              (HsWC NoExtField (parenTy t)))

applyAt :: HsExp -> [HsType] -> HsExp
applyAt f = paren . foldl appAt f

opApp :: HsExp -> HsQOp -> HsExp -> HsExp
opApp x op y = noLocA $ OpApp synDef x op y

maybeModify :: HsExp -> Maybe HsExp -> HsExp
maybeModify x Nothing = x
maybeModify x (Just f) = paren (app f x)

paren :: HsExp -> HsExp
paren = mkLHsPar

parenPat :: HsPat -> HsPat
parenPat = mkParPat

parenTy :: HsType -> HsType
parenTy t@(L _ (GHC.HsParTy {})) = t
parenTy t@(L _ (GHC.HsTyVar {})) = t
parenTy t@(L _ (GHC.HsTupleTy {})) = t
parenTy t = nlHsParTy t

applicativeApply :: HsExp -> [HsExp] -> HsExp
applicativeApply f = foldl snoc nil
  where
    nil = apply pureE [f]
    snoc g x = noLocA (OpApp synDef g apOp x)

tyApp :: HsType -> HsType -> HsType
tyApp = mkHsAppTy

tyApply :: HsType -> [HsType] -> HsType
tyApply f = parenTy . foldl tyApp f

-- | Whenever @f@ is not itself a type application,
-- @'splitTyApp' ('tyApply' f as) = (f, as)@.
splitTyApp :: HsType -> (HsType, [HsType])
splitTyApp (L _ (GHC.HsAppTy NoExtField x y)) = (++ [y]) <$> splitTyApp x
splitTyApp x = (x, [])

tyConApp :: HsName -> HsType -> HsType
tyConApp = tyApp . typeNamed_

tyConApply :: HsName -> [HsType] -> HsType
tyConApply = tyApply . typeNamed_

-- | @'splitTyConApp' ('tyApply' (L _ (GHC.HsTyVar _ NotPromoted tc)) as) = Just (tc, as)@.
splitTyConApp :: HsType -> Maybe (HsName, [HsType])
splitTyConApp x = case splitTyApp x of
  (L _ (GHC.HsTyVar _ NotPromoted tc), as) -> Just (tc, as)
  _ -> Nothing

funTy :: HsType -> HsType -> HsType
funTy a b = noLocA (GHC.HsFunTy synDef unrestrictedArrow_ a b)

unrestrictedArrow_ :: HsArrow GhcPs
unrestrictedArrow_ = HsUnrestrictedArrow synDef

unbangedTy_ :: HsType -> HsBangType
unbangedTy_ = noLocA . GHC.HsBangTy synDef (HsSrcBang synDef NoSrcUnpack NoSrcStrict) . parenTy

module_ :: ModuleName -> Maybe [HsExportSpec] -> [HsImportDecl] -> [HsDecl] -> GHC.HsModule
#if MIN_VERSION_ghc(9,6,0)
                                                                                            GhcPs
#endif
module_ moduleName maybeExports imports decls = GHC.HsModule
  {
#if MIN_VERSION_ghc(9,6,0)
    hsmodExt = XModulePs
      { hsmodAnn = synDef
      , hsmodLayout = VirtualBraces 2
      , hsmodDeprecMessage = Nothing
      , hsmodHaddockModHeader = Nothing
      }
#else
#if MIN_VERSION_ghc(9,2,0)
    hsmodAnn = synDef,
#endif
    hsmodLayout = VirtualBraces 2
#endif
  , hsmodName = Just $ noLocA moduleName
  , hsmodExports = noLocA <$> maybeExports
  , hsmodImports = imports
  , hsmodDecls = decls
#if !MIN_VERSION_ghc(9,6,0)
  , hsmodDeprecMessage = Nothing
  , hsmodHaddockModHeader = Nothing
#endif
  }

importDecl_ ::
  ModuleName ->
  Bool ->
  Maybe ModuleName ->
  Maybe (Bool, [HsImportSpec]) ->
  HsImportDecl
importDecl_ moduleName qualified maybeAs details = noLocA ImportDecl
  {
#if MIN_VERSION_ghc(9,6,0)
    ideclExt = XImportDeclPass
      { ideclAnn = synDef
      , ideclSourceText = synDef
      , ideclImplicit = False
      }
#else
    ideclExt = synDef
  , ideclSourceSrc = NoSourceText
#endif
  , ideclName = noLocA moduleName
  , ideclPkgQual =
#if MIN_VERSION_ghc(9,4,0)
      NoRawPkgQual
#else
      Nothing
#endif
  , ideclSource = NotBoot
  , ideclSafe = False
  , ideclQualified = if qualified then QualifiedPre else NotQualified
#if !MIN_VERSION_ghc(9,6,0)
  , ideclImplicit = False
#endif
  , ideclAs = noLocA <$> maybeAs
#if MIN_VERSION_ghc(9,6,0)
  , ideclImportList = (bool Exactly EverythingBut *** noLocA) <$> details
#else
  , ideclHiding = fmap noLocA <$> details
#endif
  }

ieName_ :: HsName -> HsImportSpec
ieName_ = noLocA . IEVar synDef . noLocA . IEName
#if MIN_VERSION_ghc(9,6,0)
                                                      synDef
#endif

ieNameAll_ :: HsName -> HsImportSpec
ieNameAll_ = noLocA . IEThingAll synDef . noLocA . IEName
#if MIN_VERSION_ghc(9,6,0)
                                                          synDef
#endif

dataDecl_ :: String -> [HsTyVarBndr] -> [HsConDecl] -> [HsQName] -> HsDecl
dataDecl_ messageName bndrs constructors derivedInstances = noLocA $ GHC.TyClD NoExtField DataDecl
    { tcdDExt = synDef
    , tcdLName = unqual_ tcName messageName
    , tcdTyVars = HsQTvs NoExtField bndrs
    , tcdFixity = Prefix
    , tcdDataDefn = HsDataDefn
        { dd_ext = NoExtField
#if !MIN_VERSION_ghc(9,6,0)
        , dd_ND = maybe DataType (const NewType) newtypeCtor
#endif
        , dd_ctxt = synDef
        , dd_cType = Nothing
        , dd_kindSig = Nothing
        , dd_cons =
#if MIN_VERSION_ghc(9,6,0)
            maybe (DataTypeCons False constructors) NewTypeCon newtypeCtor
#else
            constructors
#endif

        , dd_derivs =
#if !MIN_VERSION_ghc(9,2,0)
            noLoc $
#endif
              maybeToList $ derivingClause_ Nothing $ derivedInstances <&> \className ->
                (implicitOuterSigTyVarBinders_, typeNamed_ className)
        }
    }
  where
    -- TO DO: Support GADT syntax, assuming we ever start to use it in generated code.
    newtypeCtor = case constructors of
      [ con@( L _ ( ConDeclH98 { con_forall =
#if !MIN_VERSION_ghc(9,2,0)
                                              L _
#endif
                                                  False
                               , con_ex_tvs = []
                               , con_mb_cxt = Nothing
                               , con_args = args
                               } ) ) ] -> case args of
        PfxCon [_] -> Just con
        RecCon (L _ [L _ ConDeclField { cd_fld_names = [_] }]) -> Just con
        _ -> Nothing
      _ -> Nothing

recDecl_ :: HsName -> [([HsName], HsBangType)] -> HsConDecl
recDecl_ name fields = noLocA ConDeclH98
  { con_ext = synDef
  , con_name = name
  , con_forall =
#if !MIN_VERSION_ghc(9,2,0)
                 noLoc
#endif
                       False
  , con_ex_tvs = []
  , con_mb_cxt = Nothing
  , con_args = RecCon $ noLocA $ fields <&> \(names, bangTy) -> noLocA ConDeclField
      { cd_fld_ext = synDef
      , cd_fld_names =
#if MIN_VERSION_ghc(9,4,0)
          noLocA
#elif MIN_VERSION_ghc(9,2,0)
          noLoc
#else
          noLocA
#endif
          . FieldOcc NoExtField <$> names
      , cd_fld_type = bangTy
      , cd_fld_doc = Nothing
      }
  , con_doc = Nothing
  }

conDecl_ :: HsName -> [HsBangType] -> HsConDecl
conDecl_ name fields = noLocA ConDeclH98
  { con_ext = synDef
  , con_name = name
  , con_forall =
#if !MIN_VERSION_ghc(9,2,0)
                 noLoc
#endif
                       False
  , con_ex_tvs = []
  , con_mb_cxt = Nothing
  , con_args = PfxCon (HsScaled unrestrictedArrow_ <$> fields)
  , con_doc = Nothing
  }

derivingClause_ ::
  Maybe HsDerivStrategy ->
  [(HsOuterSigTyVarBndrs, HsType)] ->
  Maybe HsDerivingClause
derivingClause_ _ [] = Nothing
derivingClause_ strategy classTypes = Just $ L synDef $
  GHC.HsDerivingClause
    { GHC.deriv_clause_ext = synDef
    , GHC.deriv_clause_strategy = strategy
    , GHC.deriv_clause_tys = noLocA $ DctMulti NoExtField $
        noLocA . uncurry (HsSig NoExtField) <$> classTypes
    }

splitDerivingClause ::
  HsDerivingClause ->
  (Maybe HsDerivStrategy, [(HsOuterSigTyVarBndrs, HsType)])
splitDerivingClause (L _ GHC.HsDerivingClause
                           { GHC.deriv_clause_strategy = strategy
                           , GHC.deriv_clause_tys = L _ clauseTypes
                           }) =
    case clauseTypes of
      DctSingle _ sig -> (strategy, [splitSig sig])
      DctMulti _ sigs -> (strategy, map splitSig sigs)
  where
    splitSig (L _ sig) = case sig of
      HsSig _ binders classType -> (binders, classType)
      XHsSigType impossible
#if MIN_VERSION_base(4,16,0)
        | considerAccessible
            -- We use 'considerAccessible' because GHC 9.4.6 will issue the warning
            -- "Pattern match is redundant" (-Woverlapping-patterns) if we provide
            -- this match *and* use its strict field 'impossible', and yet
            -- if we omit this match then GHC 9.4.6 will issue the warning
            -- "Pattern match(es) are non-exhaustive" (-Wincomplete-patterns).
            -- We cannot avoid the warning without either 'considerAccessible'
            -- or avoiding any use of 'impossible', which would require 'error'
            -- or similar to handle this impossible case match.
#endif
          -> dataConCantHappen impossible

instDecl_ :: HsQName -> [HsType] -> [HsBind] -> HsDecl
instDecl_ className classArgs binds = noLocA $ GHC.InstD NoExtField ClsInstD
  { cid_d_ext = NoExtField
  , cid_inst = ClsInstDecl
      { cid_ext = synDef
      , cid_poly_ty =
#if MIN_VERSION_ghc(9,2,0)
          noLocA $
#endif
            HsSig NoExtField implicitOuterSigTyVarBinders_ (tyConApply className classArgs)
      , cid_binds = listToBag binds
      , cid_sigs = []
      , cid_tyfam_insts = []
      , cid_datafam_insts = []
      , cid_overlap_mode = Nothing
      }
  }

typeOfInstDecl :: HsDecl -> Maybe (HsOuterSigTyVarBndrs, HsType)
typeOfInstDecl ( L _ ( GHC.InstD _ ClsInstD
                       { cid_inst = ClsInstDecl
                         { cid_poly_ty =
#if MIN_VERSION_ghc(9,2,0)
                             L _
#endif
                                 (HsSig _ binders classType)
                         } } ) ) =
  Just (binders, classType)
typeOfInstDecl _ =
  Nothing

closedTyFamDecl_ ::
  HsQName ->
  [HsTyVarBndr] ->
  HsType ->
  [(Maybe [HsTyVarBndr], [HsType], HsType)] ->
  HsDecl
closedTyFamDecl_ tyFamName famBndrs resultKind eqns =
  noLocA $ GHC.TyClD NoExtField $ FamDecl synDef $ FamilyDecl
    { fdExt = synDef
    , fdInfo = ClosedTypeFamily (Just (map onEqn eqns))
#if MIN_VERSION_ghc(9,2,0)
    , fdTopLevel = TopLevel
#endif
    , fdLName = tyFamName
    , fdTyVars = HsQTvs synDef famBndrs
    , fdFixity = Prefix
    , fdResultSig = noLocA $ KindSig synDef resultKind
    , fdInjectivityAnn = Nothing
    }
  where
    onEqn (eqnBndrs, pats, rhs) = noLocA $
#if !MIN_VERSION_ghc(9,2,0)
      HsIB synDef
#endif
        FamEqn
          { feqn_ext = synDef
          , feqn_tycon = tyFamName
          , feqn_bndrs =
#if MIN_VERSION_ghc(9,2,0)
              maybe (HsOuterImplicit NoExtField) (HsOuterExplicit synDef) eqnBndrs
#else
              bndrs
#endif
          , feqn_pats = map HsValArg pats
          , feqn_fixity = Prefix
          , feqn_rhs = rhs
          }

tyFamInstDecl_ :: HsQName -> Maybe [HsTyVarBndr] -> [HsType] -> HsType -> HsDecl
tyFamInstDecl_ tyFamName bndrs pats rhs = noLocA $ GHC.InstD NoExtField TyFamInstD
  { tfid_ext = NoExtField
  , tfid_inst = TyFamInstDecl
      {
#if MIN_VERSION_ghc(9,2,0)
        tfid_xtn = synDef,
#endif
        tfid_eqn =
#if !MIN_VERSION_ghc(9,2,0)
          HsIB synDef
#endif
                      FamEqn
            { feqn_ext = synDef
            , feqn_tycon = tyFamName
            , feqn_bndrs =
#if MIN_VERSION_ghc(9,2,0)
                maybe (HsOuterImplicit NoExtField) (HsOuterExplicit synDef) bndrs
#else
                bndrs
#endif
            , feqn_pats = map HsValArg pats
            , feqn_fixity = Prefix
            , feqn_rhs = rhs
            }
      }
  }

-- | 'HsBind' includes a location, and this is one of the few places
-- where we do not need a location.  Rather than distinguishing in
-- the type between bindings that have a location and those that
-- do not, we simply ignore any binding location given here.
valDecl_ :: HsBind -> HsDecl
valDecl_ (L _ b) = noLocA (GHC.ValD NoExtField b)

patBind_ :: HsPat -> HsExp -> HsBind
patBind_ (L _ (VarPat _ nm)) rhs =
  function_ nm [([], rhs)]  -- The comments at 'HsBindLR' say to use 'FunBind'.
patBind_ (L _ (BangPat _ (L _ (VarPat _ nm)))) rhs =
  functionLike_ SrcStrict nm [([], rhs)]  -- The comments at 'HsBindLR' say to use 'FunBind'.
patBind_ (L _ (LazyPat _ (L _ (VarPat _ nm)))) rhs =
  functionLike_ SrcLazy nm [([], rhs)]  -- The comments at 'HsBindLR' say to use 'FunBind'.
patBind_ pat rhs = noLocA PatBind
  { pat_ext = synDef
  , pat_lhs = pat
  , pat_rhs =
#if MIN_VERSION_ghc(9,2,0)
      unguardedGRHSs synDef rhs synDef
#else
      unguardedGRHSs rhs
#endif
#if !MIN_VERSION_ghc(9,6,0)
  , pat_ticks = synDef
#endif
  }

-- | @'functionS_' = 'function_' . 'unqual_' 'varName'@
functionS_ :: String -> [([HsPat], HsExp)] -> HsBind
functionS_ = function_ . unqual_ varName

-- | A function with prefix syntax (as opposed to infix).
function_ :: HsName -> [([HsPat], HsExp)] -> HsBind
function_ = functionLike_ NoSrcStrict

functionLike_ :: SrcStrictness -> HsName -> [([HsPat], HsExp)] -> HsBind
functionLike_ strictness name alts = noLocA $ mkFunBind generated name (map match alts)
  where
    generated :: Origin
    generated = Generated
#if MIN_VERSION_ghc(9,8,0)
                          DoPmc
#endif

    match :: ([HsPat], HsExp) -> HsMatch
    match (pats, rhs) = mkSimpleMatch ctxt pats rhs

    ctxt :: HsMatchContext GhcPs
    ctxt = FunRhs
      { mc_fun = name
      , mc_fixity = Prefix
      , mc_strictness = strictness
      }

typeSig_ :: [HsName] -> HsOuterSigTyVarBndrs -> HsType -> HsDecl
typeSig_ nms bndrs ty = noLocA $ GHC.SigD NoExtField $ TypeSig synDef nms $
  HsWC NoExtField $
#if MIN_VERSION_ghc(9,2,0)
    noLocA $
#endif
      HsSig NoExtField bndrs ty

implicitOuterFamEqnTyVarBinders_ :: HsOuterFamEqnTyVarBndrs
implicitOuterFamEqnTyVarBinders_ =
#if MIN_VERSION_ghc(9,2,0)
  HsOuterImplicit NoExtField
#else
  ()
#endif

implicitOuterSigTyVarBinders_ :: HsOuterSigTyVarBndrs
implicitOuterSigTyVarBinders_ =
#if MIN_VERSION_ghc(9,2,0)
  HsOuterImplicit NoExtField
#else
  ()
#endif

userTyVar_ :: flag -> HsName -> LHsTyVarBndr flag GhcPs
userTyVar_ flag nm = noLocA $ GHC.UserTyVar synDef flag nm

kindedTyVar_ :: flag -> HsName -> HsType -> LHsTyVarBndr flag GhcPs
kindedTyVar_ flag nm ty = noLocA $ GHC.KindedTyVar synDef flag nm ty

wild_ :: HsPat
wild_ = noLocA $ WildPat NoExtField

qual_ :: ModuleName -> NameSpace -> String -> HsQName
qual_ m ns name = noLocA $ mkRdrQual m (mkOccName ns name)

unqual_ :: NameSpace -> String -> HsName
unqual_ ns name = noLocA $ mkRdrUnqual (mkOccName ns name)

uvar_ :: String -> HsExp
uvar_ = var_ . unqual_ varName

var_ :: HsQName -> HsExp
var_ = noLocA . HsVar NoExtField

fieldBind_ :: HsName -> HsExp -> LHsRecField GhcPs HsExp
fieldBind_ nm val = noLocA
#if MIN_VERSION_ghc(9,4,0)
  HsFieldBind
    { hfbAnn = synDef
    , hfbLHS = noLocA $ FieldOcc NoExtField nm
    , hfbRHS = val
    , hfbPun = False
    }
#elif MIN_VERSION_ghc(9,2,0)
  HsRecField
    { hsRecFieldAnn = synDef
    , hsRecFieldLbl = noLoc $ FieldOcc NoExtField nm
    , hsRecFieldArg = val
    , hsRecPun = False
    }
#else
  HsRecField
    { hsRecFieldLbl = noLocA $ FieldOcc NoExtField nm
    , hsRecFieldArg = val
    , hsRecPun = False
    }
#endif

recordCtor_ :: HsName -> [LHsRecField GhcPs HsExp] -> HsExp
recordCtor_ nm fields = noLocA RecordCon
  { rcon_ext = synDef
#if MIN_VERSION_ghc(9,2,0)
  , rcon_con = nm
#else
  , rcon_con_name = nm
#endif
  , rcon_flds = HsRecFields
      { rec_flds = fields
      , rec_dotdot = Nothing
      }
  }

fieldUpd_ :: HsName -> HsExp -> LHsRecUpdField GhcPs
#if MIN_VERSION_ghc(9,8,0)
                                                     GhcPs
#endif
fieldUpd_ nm val = noLocA
#if MIN_VERSION_ghc(9,4,0)
  HsFieldBind
    { hfbAnn = synDef
    , hfbLHS = noLocA $ Ambiguous NoExtField nm
    , hfbRHS = val
    , hfbPun = False
    }
#elif MIN_VERSION_ghc(9,2,0)
  HsRecField
    { hsRecFieldAnn = synDef
    , hsRecFieldLbl = noLoc $ Ambiguous NoExtField nm
    , hsRecFieldArg = val
    , hsRecPun = False
    }
#else
  HsRecField
    { hsRecFieldLbl = noLoc $ Ambiguous NoExtField nm
    , hsRecFieldArg = val
    , hsRecPun = False
    }
#endif

recordUpd_ :: HsExp -> [ LHsRecUpdField GhcPs
#if MIN_VERSION_ghc(9,8,0)
                                              GhcPs
#endif
                       ] -> HsExp
recordUpd_ r fields = noLocA RecordUpd
  { rupd_ext = synDef
  , rupd_expr = r
  , rupd_flds =
#if MIN_VERSION_ghc(9,8,0)
      RegularRecUpdFields synDef
#elif MIN_VERSION_ghc(9,2,0)
      Left
#endif
        fields
  }

protobufType_, primType_, protobufStringType_, protobufBytesType_ :: String -> HsType
protobufType_ = typeNamed_ . protobufName tcName
primType_ = typeNamed_ . haskellName tcName
protobufStringType_ = tyApp (protobufType_ "String") . typeNamed_ . haskellName tcName
protobufBytesType_ = tyApp (protobufType_ "Bytes") . typeNamed_ . haskellName tcName

protobufFixedType_, protobufSignedType_, protobufWrappedType_ :: HsType -> HsType
protobufFixedType_ = tyApp (protobufType_ "Fixed")
protobufSignedType_ = tyApp (protobufType_ "Signed")
protobufWrappedType_ = tyApp (protobufType_ "Wrapped")

typeNamed_ :: HsName -> HsType
typeNamed_ nm@(L _ r) = noLocA $ GHC.HsTyVar synDef promotion nm
  where
    promotion
      | rdrNameSpace r == dataName = IsPromoted
      | otherwise = NotPromoted

type_ :: String -> HsType
type_ = typeNamed_ . unqual_ tcName

tvarn_ :: String -> HsName
tvarn_ = unqual_ tvName

tvar_ :: String -> HsType
tvar_ = typeNamed_ . tvarn_

kindSig_ :: HsType -> HsType -> HsType
kindSig_ ty = noLocA . GHC.HsKindSig synDef ty

tupleType_ :: [HsType] -> HsType
tupleType_ = noLocA . GHC.HsTupleTy synDef HsBoxedOrConstraintTuple

patVar :: String -> HsPat
patVar = noLocA . VarPat NoExtField . unqual_ varName

conPat :: HsQName -> [HsPat] -> HsPat
conPat (L _ ctor) = nlConPat ctor

recPat :: HsQName -> [LHsRecField GhcPs HsPat] -> HsPat
recPat ctor fields = noLocA $ ConPat synDef ctor $ RecCon $ HsRecFields
  { rec_flds = fields
  , rec_dotdot = Nothing
  }

fieldPunPat :: HsName -> LHsRecField GhcPs HsPat
fieldPunPat nm = noLocA
#if MIN_VERSION_ghc(9,4,0)
  HsFieldBind
    { hfbAnn = synDef
    , hfbLHS = noLocA $ FieldOcc NoExtField nm
    , hfbRHS = noLocA $ VarPat NoExtField nm
    , hfbPun = True
    }
#elif MIN_VERSION_ghc(9,2,0)
  HsRecField
    { hsRecFieldAnn = synDef
    , hsRecFieldLbl = noLoc $ FieldOcc NoExtField nm
    , hsRecFieldArg = noLocA $ VarPat NoExtField nm
    , hsRecPun = True
    }
#else
  HsRecField
    { hsRecFieldLbl = noLoc $ FieldOcc NoExtField nm
    , hsRecFieldArg = noLocA $ VarPat NoExtField nm
    , hsRecPun = True
    }
#endif

alt_ :: HsPat -> HsExp -> HsAlt
alt_ = mkHsCaseAlt

case_ :: HsExp -> [HsAlt] -> HsExp
case_ e = noLocA . HsCase synDef e . mkMatchGroup generated
#if MIN_VERSION_ghc(9,2,0)
                                                            . noLocA
#endif
  where
    generated :: Origin
    generated = Generated
#if MIN_VERSION_ghc(9,8,0)
                          DoPmc
#endif

-- | Simple let expression for ordinary bindings.
let_ :: [HsBind] -> HsExp -> HsExp
let_ locals e =
#if MIN_VERSION_ghc(9,4,0)
    noLocA $ HsLet synDef synDef binds synDef e
#elif MIN_VERSION_ghc(9,2,0)
    noLocA $ HsLet synDef binds e
#else
    noLocA $ HsLet synDef (noLocA binds) e
#endif
  where
    binds = HsValBinds synDef (ValBinds synDef (listToBag locals) [])

-- | Lambda abstraction.
lambda_ :: [HsPat] -> HsExp -> HsExp
lambda_ = mkHsLam

if_ :: HsExp -> HsExp -> HsExp -> HsExp
if_ c t f = noLocA $ mkHsIf c t f
#if MIN_VERSION_ghc(9,2,0)
                                  synDef
#endif

-- | A boxed tuple with all components present.
tuple_ :: [HsExp] -> HsExp
tuple_ xs = mkLHsTupleExpr xs
#if MIN_VERSION_ghc(9,2,0)
                              synDef
#endif

-- | A promoted boxed tuple value with all components present.
tupleT_ :: [HsType] -> HsType
tupleT_ = noLocA . HsExplicitTupleTy synDef

list_ :: [HsExp] -> HsExp
list_ = nlList

listT_ :: [HsType] -> HsType
listT_ = noLocA . HsExplicitListTy synDef IsPromoted

str_ :: String -> HsExp
str_ = noLocA . HsLit synDef . mkHsString

strPat :: String -> HsPat
strPat = noLocA . LitPat NoExtField . HsString NoSourceText . mkFastString

symT :: String -> HsType
symT = noLocA . HsTyLit synDef . HsStrTy NoSourceText . mkFastString

--------------------------------------------------------------------------------
--
-- * Common Haskell expressions, constructors, and operators
--

nothingN, justN :: HsQName

dotProtoFieldC, primC, repeatedC, nestedRepeatedC, namedC, mapC,
  fieldNumberC, singleC, dotsC, pathC, qualifiedC, anonymousC, dotProtoOptionC,
  identifierC, stringLitC, intLitC, floatLitC, boolLitC, trueC, falseC, nothingC,
  justC, forceEmitC, encodeMessageFieldE, fromStringE, decodeMessageFieldE,
  pureE, returnE, mappendE, memptyE, msumE, atE, oneofE, fmapE :: HsExp

justT, nothingT, natT, symbolT, typeErrorT :: HsType

dotProtoFieldC       = var_ (protobufASTName tcName "DotProtoField")
primC                = var_ (protobufASTName dataName "Prim")
repeatedC            = var_ (protobufASTName dataName "Repeated")
nestedRepeatedC      = var_ (protobufASTName dataName "NestedRepeated")
namedC               = var_ (protobufASTName tcName "Named")
mapC                 = var_ (protobufASTName dataName  "Map")
fieldNumberC         = var_ (protobufName dataName "FieldNumber")
singleC              = var_ (protobufASTName dataName "Single")
pathC                = var_ (protobufASTName dataName "Path")
dotsC                = var_ (protobufASTName dataName "Dots")
qualifiedC           = var_ (protobufASTName dataName "Qualified")
anonymousC           = var_ (protobufASTName dataName "Anonymous")
dotProtoOptionC      = var_ (protobufASTName tcName "DotProtoOption")
identifierC          = var_ (protobufASTName dataName "Identifier")
stringLitC           = var_ (protobufASTName dataName "StringLit")
intLitC              = var_ (protobufASTName dataName "IntLit")
floatLitC            = var_ (protobufASTName dataName "FloatLit")
boolLitC             = var_ (protobufASTName dataName "BoolLit")
forceEmitC           = var_ (protobufName dataName "ForceEmit")
encodeMessageFieldE  = var_ (protobufName varName "encodeMessageField")
decodeMessageFieldE  = var_ (protobufName varName "decodeMessageField")
atE                  = var_ (protobufName varName "at")
oneofE               = var_ (protobufName varName "oneof")

trueC                = var_ (haskellName dataName "True")
falseC               = var_ (haskellName dataName "False")
nothingC             = var_ nothingN
nothingN             = haskellName dataName "Nothing"
nothingT             = typeNamed_ (haskellName tcName "Nothing")
justC                = var_ justN
justN                =       haskellName dataName "Just"
justT                = typeNamed_ (haskellName tcName "Just")
fromStringE          = var_ (haskellName varName "fromString")
pureE                = var_ (haskellName varName "pure")
returnE              = var_ (haskellName varName "return")
mappendE             = var_ (haskellName varName "mappend")
memptyE              = var_ (haskellName varName "mempty")
msumE                = var_ (haskellName varName "msum")
fmapE                = var_ (haskellName varName "fmap")
natT                 = typeNamed_ (haskellName tcName "Nat")
symbolT              = typeNamed_ (haskellName tcName "Symbol")
typeErrorT           = typeNamed_ (haskellName tcName "TypeError")

apOp :: HsQOp
apOp  = uvar_ "<*>"

fmapOp :: HsQOp
fmapOp  = uvar_ "<$>"

composeOp :: HsQOp
composeOp = var_ (haskellName varName ".")

fractionOp :: HsQOp
fractionOp = var_ (haskellName varName "/")

bindOp :: HsQOp
bindOp = var_ (haskellName varName ">>=")

altOp :: HsQOp
altOp = uvar_ "<|>"

toJSONPBOp :: HsQOp
toJSONPBOp = uvar_ ".="

parseJSONPBOp :: HsQOp
parseJSONPBOp = uvar_ ".:"

neConsOp :: HsQOp
neConsOp = var_ (haskellName varName ":|")

intE :: Integral a => a -> HsExp
intE x = noLocA $ HsOverLit synDef $ mkHsIntegral $ IL
  { il_text = NoSourceText
  , il_neg = x < 0
  , il_value = abs (toInteger x)
  }

intP :: Integral a => a -> HsPat
intP x = noLocA $ NPat synDef overlit Nothing NoExtField
  where
    overlit = L synDef $ mkHsIntegral $ IL
      { il_text = NoSourceText
      , il_neg = x < 0
      , il_value = abs (toInteger x)
      }

natTLit :: Integral a => a -> HsType
natTLit = noLocA . HsTyLit synDef . HsNumTy NoSourceText . toInteger

floatE :: forall f . RealFloat f => f -> HsExp
floatE x
    | isNaN x = opApp zero fractionOp zero
    | isInfinite x = opApp (if x < 0 then minusOne else plusOne) fractionOp zero
    | otherwise = scientific x
  where
    zero = scientific (0 :: f)
    minusOne = scientific (-1 :: f)
    plusOne = scientific (1 :: f)
    scientific y = noLocA $ HsOverLit synDef overlit
      where
        (_s, _e) = decodeFloat (abs y)
        overlit = mkHsFractional $ FL
          { fl_text = NoSourceText
          , fl_neg = y < 0
#if MIN_VERSION_ghc(9,2,0)
          , fl_signi = _s % 1
          , fl_exp = toInteger _e
          , fl_exp_base = case floatRadix y of
              2 -> Base2
              10 -> Base10
              b -> error $ "doubleE: unsupported floatRadix " ++ show b
#else
          , fl_value = toRational (abs y)
#endif
          }

do_ :: [ExprLStmt GhcPs] -> HsExp
do_ = noLocA . mkHsDo (DoExpr Nothing)
#if MIN_VERSION_ghc(9,2,0)
                                       . noLocA
#endif

letStmt_ :: [HsBind] -> ExprLStmt GhcPs
letStmt_ locals = noLocA $ LetStmt synDef
#if !MIN_VERSION_ghc(9,2,0)
                    $ noLocA
#endif
                      binds
  where
    binds = HsValBinds synDef (ValBinds synDef (listToBag locals) [])

bindStmt_ :: HsPat -> HsExp -> ExprLStmt GhcPs
bindStmt_ p e = noLocA $ mkPsBindStmt
#if MIN_VERSION_ghc(9,2,0)
                                      synDef
#endif
                                             p e

lastStmt_ :: HsExp -> ExprLStmt GhcPs
lastStmt_ = noLocA . mkLastStmt

bodyStmt_ :: HsExp -> ExprLStmt GhcPs
bodyStmt_ = noLocA . mkBodyStmt
