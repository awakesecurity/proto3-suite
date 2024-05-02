{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-| Utilities to manipulate Haskell AST -}
module Proto3.Suite.DotProto.Generate.Syntax where

import Data.Functor ((<&>))
import Data.Maybe (maybeToList)
import Data.Ratio ((%))
import GHC.Data.Bag (listToBag)
import GHC.Data.FastString (mkFastString)
import GHC.Exts (considerAccessible)
import GHC.Hs
         (AmbiguousFieldOcc(..), ClsInstDecl(..), ConDecl(..), ConDeclField(..),
          DerivClauseTys(..), EpAnn(..), ExprLStmt, FieldOcc(..), GhcPs, GRHS(..),
          GRHSs(..), HsArrow(..), HsBindLR(..), HsConDetails(..), HsDataDefn(..),
          HsDoFlavour(..), HsExpr(..), HsFieldBind(..), HsLit(..), HsLocalBindsLR(..),
          HsMatchContext(..), HsModule(..), HsOuterTyVarBndrs(..), HsOverLit(..),
          HsRecFields(..), HsScaled(..), HsSigType(..), HsSrcBang(..), HsToken(..),
          HsTupArg(..), HsTupleSort(..), HsUniToken(..), HsValBindsLR(..),
          HsWildCardBndrs(..), IE(..), IEWrappedName(..), ImportDecl(..),
          ImportDeclQualifiedStyle(..), InstDecl(..), LBangType, LConDecl,
          LDerivStrategy, LGRHS, LHsBind, LHsDecl, LHsDerivingClause, LHsExpr,
          LHsQTyVars(..), LHsRecField, LHsRecUpdField, LHsTyVarBndr, LHsType, LIE, LIdP,
          LImportDecl, LMatch, LPat, LSig, Match(..), MatchGroup(..), NewOrData(..),
          NoExtField(..), OverLitVal(..), Pat(..), Sig(..), SrcStrictness(..),
          SrcUnpackedness(..), StmtLR(..), TokenLocation(..), TyClDecl(..),
          dataConCantHappen, emptyComments, noAnn, noLocA)
import qualified GHC.Hs as GHC (HsDecl(..), HsDerivingClause(..),
                                HsOuterSigTyVarBndrs, HsTyVarBndr(..), HsType(..))
import GHC.Types.Basic (Boxity(..), Origin(..), PromotionFlag(..))
import GHC.Types.Fixity (LexicalFixity(..))
import GHC.Types.Name.Occurrence (NameSpace, dataName, mkOccName, tcName, tvName, varName)
import GHC.Types.Name.Reader (mkRdrQual, mkRdrUnqual, rdrNameSpace)
import GHC.Types.PkgQual (RawPkgQual(..))
import GHC.Types.SourceText
         (IntegralLit(..), FractionalExponentBase(..), FractionalLit(..), SourceText(..))
import GHC.Types.SrcLoc (GenLocated(..), LayoutInfo(..), generatedSrcSpan)
import GHC.Types.Var (Specificity)
import GHC.Unit.Module.Name (ModuleName, mkModuleName)
import GHC.Unit (IsBootInterface(..))

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
type HsOuterSigTyVarBndrs = GHC.HsOuterSigTyVarBndrs GhcPs
type HsPat = LPat GhcPs
type HsQName = LIdP GhcPs
type HsQOp = LHsExpr GhcPs
type HsSig = LSig GhcPs
type HsType = LHsType GhcPs
type Module = ModuleName

haskellName, jsonpbName, grpcName, lrName, protobufName, protobufASTName, proxyName ::
  NameSpace -> String -> HsQName
haskellName     = qual_ haskellNS
jsonpbName      = qual_ (mkModuleName "HsJSONPB")
grpcName        = qual_ (mkModuleName "HsGRPC")
lrName          = qual_ (mkModuleName "LR")
protobufName    = qual_ (mkModuleName "HsProtobuf")
protobufASTName = qual_ (mkModuleName "HsProtobufAST")
proxyName       = qual_ (mkModuleName "Proxy")

haskellNS :: ModuleName
haskellNS = mkModuleName "Hs"

--------------------------------------------------------------------------------
--
-- * Wrappers around ghc constructors
--

app :: HsExp -> HsExp -> HsExp
app f x = noLocA (HsApp noAnn f (paren x))

apply :: HsExp -> [HsExp] -> HsExp
apply f = paren . foldl app f

appAt :: HsExp -> HsType -> HsExp
appAt f t = noLocA (HsAppType generatedSrcSpan f (HsWC NoExtField (parenTy t)))

applyAt :: HsExp -> [HsType] -> HsExp
applyAt f = paren . foldl appAt f

opApp :: HsExp -> HsQOp -> HsExp -> HsExp
opApp x op y = noLocA $ OpApp noAnn x op y

maybeModify :: HsExp -> Maybe HsExp -> HsExp
maybeModify x Nothing = x
maybeModify x (Just f) = paren (app f x)

paren :: HsExp -> HsExp
paren e@(L _ e') = case e' of
    HsPar _ _ _ _ -> e
    HsVar _ _ -> e
    HsUnboundVar _ _ -> e
    HsOverLit _ (OverLit _ (HsIntegral x)) | nonnegativeI x -> e
    HsOverLit _ (OverLit _ (HsFractional x)) | nonnegativeF x -> e
    HsOverLit _ (OverLit _ (HsIsString _ _)) -> e
    HsLit _ (HsChar _ _) -> e
    HsLit _ (HsCharPrim _ _) -> e
    HsLit _ (HsString _ _) -> e
    HsLit _ (HsStringPrim _ _) -> e
    HsLit _ (HsInt _ x) | nonnegativeI x -> e
    HsLit _ (HsIntPrim _ v) | 0 <= v -> e
    HsLit _ (HsWordPrim _ v) | 0 <= v -> e
    HsLit _ (HsInt64Prim _ v) | 0 <= v -> e
    HsLit _ (HsWord64Prim _ v) | 0 <= v -> e
    HsLit _ (HsInteger _ v _) | 0 <= v -> e
    HsLit _ (HsRat _ x _) | nonnegativeF x -> e
    HsLit _ (HsFloatPrim _ x) | nonnegativeF x -> e
    HsLit _ (HsDoublePrim _ x) | nonnegativeF x -> e
    ExplicitTuple _ _ _ -> e
    ExplicitSum _ _ _ _ -> e
    ExplicitList _ _ -> e
    ArithSeq _ _ _ -> e
    _ -> noLocA $ HsPar noAnn tok e tok
  where
    tok = L NoTokenLoc HsTok
    nonnegativeI (IL _ n v) = not n && 0 <= v
    nonnegativeF (FL _ n s _ _) = not n && 0 <= s

parenPat :: HsPat -> HsPat
parenPat p@(L _ (ParPat _ _ _ _)) = p
parenPat p@(L _ (VarPat _ _)) = p
parenPat p = noLocA $ ParPat noAnn tok p tok
  where
    tok = L NoTokenLoc HsTok

parenTy :: HsType -> HsType
parenTy t@(L _ (GHC.HsParTy _ _)) = t
parenTy t@(L _ (GHC.HsTyVar _ _ _)) = t
parenTy t@(L _ (GHC.HsTupleTy _ _ _)) = t
parenTy t = noLocA $ GHC.HsParTy noAnn t

applicativeApply :: HsExp -> [HsExp] -> HsExp
applicativeApply f = foldl snoc nil
  where
    nil = apply pureE [f]
    snoc g x = noLocA (OpApp mempty g apOp x)

tyApp :: HsType -> HsType -> HsType
tyApp f a = noLocA $ GHC.HsAppTy NoExtField f (parenTy a)

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
funTy a b = noLocA (GHC.HsFunTy EpAnnNotUsed unrestrictedArrow_ a b)

unrestrictedArrow_ :: HsArrow GhcPs
unrestrictedArrow_ = HsUnrestrictedArrow (L NoTokenLoc HsNormalTok)

unbangedTy_ :: HsType -> HsBangType
unbangedTy_ =
  noLocA . GHC.HsBangTy noAnn (HsSrcBang NoSourceText NoSrcUnpack NoSrcStrict) . parenTy

module_ :: ModuleName -> Maybe [HsExportSpec] -> [HsImportDecl] -> [HsDecl] -> HsModule
module_ moduleName maybeExports imports decls = HsModule
  { hsmodAnn = noAnn
  , hsmodLayout = VirtualBraces 2
        -- ^ Layout info for the module.
        -- For incomplete modules (e.g. the output of parseHeader), it is NoLayoutInfo.
  , hsmodName = Just $ noLocA moduleName
  , hsmodExports = noLocA <$> maybeExports
  , hsmodImports = imports
  , hsmodDecls = decls
  , hsmodDeprecMessage = Nothing
  , hsmodHaddockModHeader = Nothing
  }

importDecl_ ::
  ModuleName ->
  Bool ->
  Maybe ModuleName ->
  Maybe (Bool, [HsImportSpec]) ->
  HsImportDecl
importDecl_ moduleName qualified maybeAs details = noLocA ImportDecl
  { ideclExt = noAnn
  , ideclSourceSrc = NoSourceText
  , ideclName = noLocA moduleName
  , ideclPkgQual = NoRawPkgQual
  , ideclSource = NotBoot
  , ideclSafe = False
  , ideclQualified = if qualified then QualifiedPre else NotQualified
  , ideclImplicit = False
  , ideclAs = noLocA <$> maybeAs
  , ideclHiding = fmap noLocA <$> details
  }

ieName_ :: HsName -> HsImportSpec
ieName_ = noLocA . IEVar NoExtField . noLocA . IEName

ieNameAll_ :: HsName -> HsImportSpec
ieNameAll_ = noLocA . IEThingAll noAnn . noLocA . IEName

dataDecl_ :: String -> [LHsTyVarBndr () GhcPs] -> [HsConDecl] -> [HsQName] -> HsDecl
dataDecl_ messageName bndrs constructors derivedInstances = noLocA $ GHC.TyClD NoExtField DataDecl
  { tcdDExt = mempty
  , tcdLName = unqual_ tcName messageName
  , tcdTyVars = HsQTvs NoExtField bndrs
  , tcdFixity = Prefix
  , tcdDataDefn = HsDataDefn
      { dd_ext = NoExtField
      , dd_ND = case constructors of
          [ L _ ( ConDeclH98 { con_forall = False
                             , con_ex_tvs = []
                             , con_mb_cxt = Nothing
                             , con_args = args
                             } ) ] -> case args of
            PrefixCon [] [_] -> NewType
            RecCon (L _ [L _ ConDeclField { cd_fld_names = [_] }]) -> NewType
            _ -> DataType
          _ -> DataType
      , dd_ctxt = Nothing
      , dd_cType = Nothing
      , dd_kindSig = Nothing
      , dd_cons = constructors
      , dd_derivs = maybeToList $ derivingClause_ Nothing $ derivedInstances <&> \className ->
          (implicitTyVarBinders_, typeNamed_ className)
      }
  }

recDecl_ :: HsName -> [([HsName], HsBangType)] -> HsConDecl
recDecl_ name fields = noLocA ConDeclH98
  { con_ext = mempty
  , con_name = name
  , con_forall = False
  , con_ex_tvs = []
  , con_mb_cxt = Nothing
  , con_args = RecCon $ noLocA $ fields <&> \(names, bangTy) -> noLocA ConDeclField
      { cd_fld_ext = mempty
      , cd_fld_names = noLocA . FieldOcc NoExtField <$> names
      , cd_fld_type = bangTy
      , cd_fld_doc = Nothing
      }
  , con_doc = Nothing
  }

conDecl_ :: HsName -> [HsBangType] -> HsConDecl
conDecl_ name fields = noLocA ConDeclH98
  { con_ext = mempty
  , con_name = name
  , con_forall = False
  , con_ex_tvs = []
  , con_mb_cxt = Nothing
  , con_args = PrefixCon [] (HsScaled unrestrictedArrow_ <$> fields)
  , con_doc = Nothing
  }

derivingClause_ ::
  Maybe HsDerivStrategy ->
  [(HsOuterSigTyVarBndrs, HsType)] ->
  Maybe HsDerivingClause
derivingClause_ _ [] = Nothing
derivingClause_ strategy classTypes = Just $ noLocA GHC.HsDerivingClause
  { GHC.deriv_clause_ext = mempty
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
      XHsSigType impossible | considerAccessible -> dataConCantHappen impossible
        -- We use 'considerAccessible' because GHC 9.4.6 will issue the warning
        -- "Pattern match is redundant" (-Woverlapping-patterns) if we provide
        -- this match *and* use its strict field 'impossible', and yet
        -- if we omit this match then GHC 9.4.6 will issue the warning
        -- "Pattern match(es) are non-exhaustive" (-Wincomplete-patterns).
        -- We cannot avoid the warning without either 'considerAccessible'
        -- or avoiding any use of 'impossible', which would require 'error'
        -- or similar to handle this impossible case match.

instDecl_ :: HsQName -> [HsType] -> [HsBind] -> HsDecl
instDecl_ className classArgs binds = noLocA $ GHC.InstD NoExtField ClsInstD
  { cid_d_ext = NoExtField
  , cid_inst = ClsInstDecl
      { cid_ext = mempty
      , cid_poly_ty = noLocA HsSig
          { sig_ext = NoExtField
          , sig_bndrs = implicitTyVarBinders_
          , sig_body = tyConApply className classArgs
          }
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
                         { cid_poly_ty = L _ HsSig
                           { sig_bndrs = binders, sig_body = classType } } } ) ) =
  Just (binders, classType)
typeOfInstDecl _ =
  Nothing

-- | 'HsBind' includes a location, and this is one of the few places
-- where we do not need a location.  Rather than distinguishing in
-- the type between bindings that have a location and those that
-- do not, we simply ignore any binding location given here.
valDecl_ :: HsBind -> HsDecl
valDecl_ (L _ b) = noLocA (GHC.ValD NoExtField b)

unguardedRhs_ :: HsExp -> HsGrhs
unguardedRhs_ e = noLocA (GRHS noAnn [] e)

unguardedRhss_ :: HsExp -> HsGuardedAlts
unguardedRhss_ e = grhss_ [unguardedRhs_ e] [] []

grhss_ :: [HsGrhs] -> [HsBind] -> [HsSig] -> HsGuardedAlts
grhss_ rs locals sigs = GRHSs
  { grhssExt = emptyComments
  , grhssGRHSs = rs
  , grhssLocalBinds = case locals of
      [] -> EmptyLocalBinds NoExtField
      _ -> HsValBinds mempty (ValBinds mempty (listToBag locals) sigs)
  }

patBind_ :: HsPat -> HsGuardedAlts -> HsBind
patBind_ (L _ (VarPat _ nm)) rhss =
  function_ nm [([], rhss)]  -- The comments at 'HsBindLR' say to use 'FunBind'.
patBind_ (L _ (BangPat _ (L _ (VarPat _ nm)))) rhss =
  function_ nm [([], rhss)]  -- The comments at 'HsBindLR' say to use 'FunBind'.
patBind_ pat rhss = noLocA PatBind
  { pat_ext = noAnn
  , pat_lhs = pat
  , pat_rhs = rhss
  , pat_ticks = mempty
  }

-- | @'functionS_' = 'function_' . 'unqual_' 'varName'@
functionS_ :: String -> [([HsPat], HsGuardedAlts)] -> HsBind
functionS_ = function_ . unqual_ varName

-- | A function with prefix syntax (as opposed to infix).
function_ :: HsName -> [([HsPat], HsGuardedAlts)] -> HsBind
function_ name alts = noLocA FunBind
    { fun_ext = NoExtField
    , fun_id = name
    , fun_matches = MG
        { mg_ext = NoExtField
        , mg_alts = noLocA (map match alts)
        , mg_origin = Generated
        }
    , fun_tick = []
    }
  where
    -- | For matching the arguments to a function
    -- with prefix syntax (as opposed to infix).
    match :: ([HsPat], HsGuardedAlts) -> HsMatch
    match (pats, rhss) = noLocA Match
      { m_ext = mempty
      , m_ctxt = FunRhs
          { mc_fun = name
          , mc_fixity = Prefix
          , mc_strictness = NoSrcStrict  -- This is a function binding, not a value binding.
          }
      , m_pats = pats
      , m_grhss = rhss
      }

typeSig_ :: [HsName] -> HsOuterSigTyVarBndrs -> HsType -> HsDecl
typeSig_ nms bndrs ty = noLocA $ GHC.SigD NoExtField $ TypeSig noAnn nms $
  HsWC NoExtField $ noLocA HsSig
    { sig_ext = NoExtField
    , sig_bndrs = bndrs
    , sig_body = ty
    }

implicitTyVarBinders_ :: HsOuterSigTyVarBndrs
implicitTyVarBinders_ = HsOuterImplicit NoExtField

explicitTyVarBinders_ :: [LHsTyVarBndr Specificity GhcPs] -> HsOuterSigTyVarBndrs
explicitTyVarBinders_ = HsOuterExplicit noAnn

userTyVar_ :: flag -> HsName -> LHsTyVarBndr flag GhcPs
userTyVar_ flag nm = noLocA $ GHC.UserTyVar noAnn flag nm

kindedTyVar_ :: flag -> HsName -> HsType -> LHsTyVarBndr flag GhcPs
kindedTyVar_ flag nm ty = noLocA $ GHC.KindedTyVar noAnn flag nm ty

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
fieldBind_ nm val = noLocA HsFieldBind
  { hfbAnn = mempty
  , hfbLHS = noLocA $ FieldOcc NoExtField nm
  , hfbRHS = val
  , hfbPun = False
  }

recordCtor_ :: HsName -> [LHsRecField GhcPs HsExp] -> HsExp
recordCtor_ nm fields = noLocA RecordCon
  { rcon_ext = mempty
  , rcon_con = nm
  , rcon_flds = HsRecFields
      { rec_flds = fields
      , rec_dotdot = Nothing
      }
  }

fieldUpd_ :: HsName -> HsExp -> LHsRecUpdField GhcPs
fieldUpd_ nm val = noLocA HsFieldBind
  { hfbAnn = mempty
  , hfbLHS = noLocA $ Ambiguous NoExtField nm
  , hfbRHS = val
  , hfbPun = False
  }

recordUpd_ :: HsExp -> [LHsRecUpdField GhcPs] -> HsExp
recordUpd_ r fields = noLocA RecordUpd
  { rupd_ext = mempty
  , rupd_expr = r
  , rupd_flds = Left fields
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
typeNamed_ nm@(L _ r) = noLocA $ GHC.HsTyVar noAnn promotion nm
  where
    promotion
      | rdrNameSpace r == dataName = IsPromoted
      | otherwise = NotPromoted

type_ :: String -> HsType
type_ = typeNamed_ . unqual_ tcName

tvar_ :: String -> HsType
tvar_ = typeNamed_ . unqual_ tvName

tupleType_ :: [HsType] -> HsType
tupleType_ = noLocA . GHC.HsTupleTy noAnn HsBoxedOrConstraintTuple

patVar :: String -> HsPat
patVar = noLocA . VarPat NoExtField . unqual_ varName

conPat :: HsQName -> [HsPat] -> HsPat
conPat ctor fields = noLocA $ ConPat mempty ctor (PrefixCon [] fields)

recPat :: HsQName -> [LHsRecField GhcPs HsPat] -> HsPat
recPat ctor fields = noLocA $ ConPat mempty ctor $ RecCon $ HsRecFields
  { rec_flds = fields
  , rec_dotdot = Nothing
  }

fieldPunPat :: HsName -> LHsRecField GhcPs HsPat
fieldPunPat nm = noLocA HsFieldBind
  { hfbAnn = mempty
  , hfbLHS = noLocA $ FieldOcc NoExtField nm
  , hfbRHS = noLocA $ VarPat NoExtField nm
  , hfbPun = True
  }

alt_ :: HsPat -> HsGuardedAlts -> HsAlt
alt_ pat rhss = noLocA Match
  { m_ext = mempty
  , m_ctxt = CaseAlt
  , m_pats = [pat]
  , m_grhss = rhss
  }

case_ :: HsExp -> [HsAlt] -> HsExp
case_ e alts = noLocA $ HsCase EpAnnNotUsed e MG
  { mg_ext = NoExtField
  , mg_alts = noLocA alts
  , mg_origin = Generated
  }

-- | Simple let expression for ordinary bindings.
let_ :: [HsBind] -> HsExp -> HsExp
let_ locals e = noLocA $ HsLet EpAnnNotUsed tok binds tok e
  where
    tok = L NoTokenLoc HsTok
    binds = HsValBinds mempty (ValBinds mempty (listToBag locals) [])

-- | Lambda abstraction.
lambda_ :: [HsPat] -> HsGuardedAlts -> HsExp
lambda_ pats rhss = noLocA $ HsLam NoExtField MG
  { mg_ext = NoExtField
  , mg_alts = noLocA $ (: []) $ noLocA Match
      { m_ext = mempty
      , m_ctxt = LambdaExpr
      , m_pats = pats
      , m_grhss = rhss
      }
  , mg_origin = Generated
  }

if_ :: HsExp -> HsExp -> HsExp -> HsExp
if_ c t f = noLocA $ HsIf EpAnnNotUsed c t f

-- | A boxed tuple with all components present.
tuple_ :: [HsExp] -> HsExp
tuple_ xs = noLocA $ ExplicitTuple EpAnnNotUsed (map (Present EpAnnNotUsed) xs) Boxed

list_ :: [HsExp] -> HsExp
list_ = noLocA . ExplicitList mempty

str_ :: String -> HsExp
str_ = noLocA . HsLit noAnn . HsString NoSourceText . mkFastString

strPat :: String -> HsPat
strPat = noLocA . LitPat NoExtField . HsString NoSourceText . mkFastString

--------------------------------------------------------------------------------
--
-- * Common Haskell expressions, constructors, and operators
--

nothingN, justN :: HsQName

dotProtoFieldC, primC, repeatedC, nestedRepeatedC, namedC, mapC,
  fieldNumberC, singleC, dotsC, pathC, qualifiedC, anonymousC, dotProtoOptionC,
  identifierC, stringLitC, intLitC, floatLitC, boolLitC, trueC, falseC, nothingC,
  justC, forceEmitC,  encodeMessageFieldE, fromStringE, decodeMessageFieldE,
  pureE, returnE, mappendE, memptyE, msumE, atE, oneofE, fmapE :: HsExp

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
nothingN             =       haskellName dataName "Nothing"
justC                = var_ justN
justN                =       haskellName dataName "Just"
fromStringE          = var_ (haskellName varName "fromString")
pureE                = var_ (haskellName varName "pure")
returnE              = var_ (haskellName varName "return")
mappendE             = var_ (haskellName varName "mappend")
memptyE              = var_ (haskellName varName "mempty")
msumE                = var_ (haskellName varName "msum")
fmapE                = var_ (haskellName varName "fmap")

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
intE x = noLocA $ HsOverLit noAnn overlit
  where
    overlit = OverLit NoExtField $ HsIntegral $ IL
      { il_text = NoSourceText
      , il_neg = x < 0
      , il_value = abs (toInteger x)
      }

intP :: Integral a => a -> HsPat
intP x = noLocA $ NPat mempty overlit Nothing NoExtField
  where
    overlit = noLocA $ OverLit NoExtField $ HsIntegral $ IL
      { il_text = NoSourceText
      , il_neg = x < 0
      , il_value = abs (toInteger x)
      }

floatE :: forall f . RealFloat f => f -> HsExp
floatE x
    | isNaN x = opApp zero fractionOp zero
    | isInfinite x = opApp (if x < 0 then minusOne else plusOne) fractionOp zero
    | otherwise = scientific x
  where
    zero = scientific (0 :: f)
    minusOne = scientific (-1 :: f)
    plusOne = scientific (1 :: f)
    scientific y = noLocA $ HsOverLit noAnn overlit
      where
        (s, e) = decodeFloat y
        overlit = OverLit NoExtField $ HsFractional $ FL
          { fl_text = NoSourceText
          , fl_neg = y < 0
          , fl_signi = s % 1
          , fl_exp = toInteger e
          , fl_exp_base = case floatRadix y of
              2 -> Base2
              10 -> Base10
              b -> error $ "doubleE: unsupported floatRadix " ++ show b
          }

do_ :: [ExprLStmt GhcPs] -> HsExp
do_ = noLocA . HsDo mempty (DoExpr Nothing) . noLocA

letStmt_ :: [HsBind] -> ExprLStmt GhcPs
letStmt_ locals = noLocA $ LetStmt mempty binds
  where
    binds = HsValBinds mempty (ValBinds mempty (listToBag locals) [])

bindStmt_ :: HsPat -> HsExp -> ExprLStmt GhcPs
bindStmt_ p e = noLocA $ BindStmt mempty p e

lastStmt_ :: HsExp -> ExprLStmt GhcPs
lastStmt_ e = noLocA $ LastStmt NoExtField e Nothing NoExtField

bodyStmt_ :: HsExp -> ExprLStmt GhcPs
bodyStmt_ e = noLocA $ BodyStmt NoExtField e NoExtField NoExtField
