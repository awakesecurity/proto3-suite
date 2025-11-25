{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, TupleSections #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module Proto3.Suite.Haskell.Syntax
  ( -- * HsDecl
    hsDeclToDec
    -- * InstDecl
  , instDeclToDec
    -- * ClsInstDecl
  , clsInstDeclToDec
    -- * HsBindLR
  , hsBindLR
    -- * MatchGroup
  , matchGroupToClause  
    -- * Match
  , hsMatchToClause
    -- * HsExpr
  , hsExprToExp
    -- * PatSynBind
  , patSynBindToDec 
    -- * HsLit
  , hsLitToLit
    -- * IntegralLit
  , integralLitToInteger
    -- * Pat
  , hsPatToPat
    -- * HsConPatTyArg
  , hsConPatTyArgToType
    -- * HsRecFields
  , hsRecFieldsToFieldPat
    -- * HsFieldBind
  , hsFieldBindToFieldPat
    -- * HsPatSigType
  , hsPatSigTypeToType
    -- * HsTyPat
  , hsTyPatToType 
    -- * OverlapMode
  , overlapModeToOverlap
    -- * HsSigType
  , hsSigTypeToType
    -- * HsType
  , hsTypeToType
    -- * RdrName
  , rdrNameToName
  ) where

import Data.Char (isUpper)
import Data.ByteString.Char8 qualified as ByteString

import "template-haskell" Language.Haskell.TH.Syntax qualified as TH

import "ghc-lib-parser" Language.Haskell.Syntax.Extension
import "ghc-lib-parser" Language.Haskell.Syntax.Pat
import "ghc-lib-parser" GHC.Types.SrcLoc
import "ghc-lib-parser" Language.Haskell.Syntax.Binds qualified as GHC
  ( PatSynBind (..)
  )
-- import GHC.Hs.Type qualified as GHC 
--   ( HsBndrVar (..)
--   )
import GHC.Hs.Pat ()
import GHC.Hs.Decls qualified as GHC 
  ( DerivClauseTys (..)
  )
import GHC.Hs.Type qualified as GHC 
  ( SrcStrictness (..)
  , SrcUnpackedness (..)
  )
import "ghc-lib-parser" Language.Haskell.Syntax.Decls qualified as GHC
  ( HsDataDefn (..)
  , DataDefnCons (..)
  , ConDecl (..)
  , HsConDeclH98Details
  )
import "ghc-lib-parser" Language.Haskell.Syntax.Type qualified as GHC
  ( FieldOcc (..)
  , HsForAllTelescope (..)
  , HsTyVarBndr (..)
  , ConDeclField (..)
  , hsScaledThing
  )
-- import GHC.Utils.Outputable qualified as GHC (Outputable (..), runSDoc)
import "ghc-lib-parser" Language.Haskell.Syntax.Binds qualified as GHC
  ( HsLocalBindsLR (..)
  , HsValBindsLR (..)
  , HsIPBinds (..)
  )
import "ghc-lib-parser" GHC.Types.Var qualified as GHC
  ( Specificity (..)
  )
-- import "ghc-lib-parser" GHC.Unit.Types qualified as GHC (Module, moduleName)
import "ghc-lib-parser" Language.Haskell.Syntax.Expr qualified as GHC
  ( GRHSs (..)
  , GRHS (..)
  , StmtLR (..)
  )
import "ghc-lib-parser" Language.Haskell.Syntax qualified as GHC
  ( ClsInstDecl (..)
  , HsDerivingClause (..)
  , Sig (..)
  , HsBndrVis (..)
  , LHsQTyVars (..)
  , TyClDecl (..)
  , HsSrcBang (..)
  , DerivStrategy (..)
  , HsBindLR (..)
  , DerivDecl (..)
  , FunDep (..)
  , HsConPatTyArg (..)
  , HsRecFields (..)
  , HsTyLit (..)
  , HsFieldBind (..)
  , HsDecl (..)
  , HsTupleSort (..)
  , HsExpr (..)
  , HsArrow (..)
  , HsWildCardBndrs (..)
  , LHsExpr 
  , HsConDetails (..)
  , RecordPatSynField (..)
  , HsPatSynDir (..)
  , HsTyPat (..)
  , LPat
  , HsSigType (..)
  , HsPatSigType (..)
  , HsType (..)
  , HsPatSynDetails 
  , LFieldOcc
  , HsLit (..)
  , InstDecl (..)
  , Match (..)
  , MatchGroup (..)
  , PromotionFlag (..)
  , Pat (..)
  , moduleNameString
  )
import "ghc-lib-parser" GHC.Types.Name.Reader qualified as GHC (RdrName (..), nameRdrName)
import "ghc-lib-parser" GHC.Types.Basic qualified as GHC (OverlapMode (..))
import "ghc-lib-parser" GHC.Types.Name.Occurrence qualified as GHC (occNameString)
import GHC.Types.SourceText qualified as GHC 
  ( IntegralLit (..)
  , rationalFromFractionalLit
  )
import GHC.Types.SrcLoc ()
import "ghc-lib-parser" GHC.Data.FastString qualified as FastString

#if MIN_VERSION_ghc_lib_parser(9,6,0)
import "ghc-lib-parser" GHC.Hs.Extension qualified as GHC
  ( GhcPs 
  )
#endif

--- HsDecl ---------------------------------------------------------------------

hsDeclToDec :: GHC.HsDecl GHC.GhcPs -> TH.Dec
hsDeclToDec hsDecl = case hsDecl of 
  GHC.InstD _x instDecl -> instDeclToDec instDecl
  GHC.DerivD _x deriveDecl -> hsDerivDeclToDec deriveDecl
  GHC.TyClD _x tyClDecl -> hsTyClDeclToDec tyClDecl 
  _other -> error "unsupported HsDecl in hsDeclToDec"

--- TyClDecl -------------------------------------------------------------------

hsTyClDeclToDec :: GHC.TyClDecl GHC.GhcPs -> TH.Dec
hsTyClDeclToDec (GHC.DataDecl _x idt tyVars _fixity defn) = case GHC.dd_cons defn of 
  GHC.DataTypeCons _ cons -> 
    TH.DataD
      (maybe [] (map (hsTypeToType . unLoc) . unLoc) (GHC.dd_ctxt defn))
      (rdrNameToName (unLoc idt))
      (map (hsTyVarBndrToTyVarBndr . unLoc) (GHC.hsq_explicit tyVars)) -- [TyVarBndr]
      (fmap (hsTypeToType . unLoc) (GHC.dd_kindSig defn))
      (map (conDeclToDec . unLoc) cons)
      [] -- (map (_ . unLoc) (GHC.dd_derivs defn))
  GHC.NewTypeCon con ->
    TH.NewtypeD
      (maybe [] (map (hsTypeToType . unLoc) . unLoc) (GHC.dd_ctxt defn))
      (rdrNameToName (unLoc idt))
      (map (hsTyVarBndrToTyVarBndr . unLoc) (GHC.hsq_explicit tyVars)) -- [TyVarBndr]
      (fmap (hsTypeToType . unLoc) (GHC.dd_kindSig defn))
      (conDeclToDec (unLoc con))
      (foldr ((++) . hsDerivingClauseToDerivClause . unLoc) (GHC.dd_derivs defn))
hsTyClDeclToDec (GHC.ClassDecl _x ctxt idt tyVars _fixity fds sigs meths _ats _atds _docs) =
  TH.ClassD
    (maybe [] (map (hsTypeToType . unLoc) . unLoc) ctxt)
    (rdrNameToName (unLoc idt))
    (map (hsTyVarBndrToTyVarBndr . unLoc) (GHC.hsq_explicit tyVars)) -- [TyVarBndr]
    (map (hsFunDepToFunDep . unLoc) fds)
    (mconcat 
      [ foldr ((++) . sigToDec . unLoc) [] sigs
      , foldr ((:) . hsBindLRToDec . unLoc) [] meths
      ]
    ) -- [Dec]
hsTyClDeclToDec _other = 
  error "hsTyClDeclToDec: only ClassDecl is supported so far"

  
hsDerivingClauseToDerivClause :: GHC.HsDerivingClause GHC.GhcPs -> [TH.DerivClause]
hsDerivingClauseToDerivClause GHC.HsDerivingClause {..} = 
  TH.DerivClause
    (_ deriv_clause_strategy)
    _

derivStrategyToDerivStrategy :: GHC.DerivStrategy GHC.GhcPs -> TH.DerivStrategy
derivStrategyToDerivStrategy GHC.StockStrategy {} = TH.StockStrategy
derivStrategyToDerivStrategy GHC.NewtypeStrategy {} = TH.NewtypeStrategy
derivStrategyToDerivStrategy GHC.AnyclassStrategy {} = TH.AnyclassStrategy
derivStrategyToDerivStrategy (GHC.ViaStrategy sig) = _


conDeclToDec :: GHC.ConDecl GHC.GhcPs -> TH.Con
conDeclToDec (GHC.ConDeclGADT _x _idts _bndrs _ctxt _args _resTy _doc) = 
  error "conDeclToDec: GADT not yet implemented"

conDeclToDec (GHC.ConDeclH98 _x idt fa _exs ctxt args _doc) 
  | fa = 
    TH.ForallC
      []
      (maybe [] (map (hsTypeToType . unLoc) . unLoc) ctxt)
      (TH.NormalC 
        (rdrNameToName (unLoc idt)) 
        (hsConDetailsToBangTypes args)   
      )
  | otherwise = case args of 
    GHC.PrefixCon _ conArgs -> 
      TH.NormalC 
        (rdrNameToName (unLoc idt)) 
        (map (hsTypeToBangType . unLoc . GHC.hsScaledThing) conArgs)
    GHC.RecCon r -> 
      TH.RecC 
        (rdrNameToName (unLoc idt))
        (foldr ((++) . conDeclFieldToVarBangType . unLoc) [] (unLoc r))
    GHC.InfixCon a b -> 
      TH.InfixC 
        (hsTypeToBangType (unLoc (GHC.hsScaledThing a)))
        (rdrNameToName (unLoc idt))
        (hsTypeToBangType (unLoc (GHC.hsScaledThing b)))
  where 

conDeclFieldToVarBangType :: GHC.ConDeclField GHC.GhcPs -> [TH.VarBangType]
conDeclFieldToVarBangType GHC.ConDeclField {..} = 
  map (makeVarBangType . unLoc . GHC.foLabel . unLoc) cd_fld_names
  where 
    makeVarBangType :: GHC.RdrName -> TH.VarBangType
    makeVarBangType idt = case hsTypeToBangType (unLoc cd_fld_type) of
      (bang, ty) -> (rdrNameToName idt, bang, ty)

hsConDetailsToBangTypes :: GHC.HsConDeclH98Details GHC.GhcPs -> [TH.BangType]
hsConDetailsToBangTypes (GHC.PrefixCon _tyArgs args) = 
  map (hsTypeToBangType . unLoc . GHC.hsScaledThing) args
hsConDetailsToBangTypes (GHC.InfixCon lhs rhs) = 
  [ (hsTypeToBangType (unLoc (GHC.hsScaledThing lhs)))
  , (hsTypeToBangType (unLoc (GHC.hsScaledThing rhs)))
  ]
hsConDetailsToBangTypes (GHC.RecCon recFields) =
  foldr ((++) . map adapt . conDeclFieldToBangType . unLoc) [] (unLoc recFields)
  where 
    adapt :: TH.VarBangType -> TH.BangType
    adapt (_, bang, ty) = (bang, ty)

conDeclFieldToBangType :: GHC.ConDeclField GHC.GhcPs -> [TH.VarBangType]
conDeclFieldToBangType GHC.ConDeclField {..} = 
  map (makeBangType . rdrNameToName . unLoc . GHC.foLabel . unLoc) cd_fld_names 
  where 
    makeBangType :: TH.Name -> TH.VarBangType
    makeBangType name = case hsTypeToBangType (unLoc cd_fld_type) of 
      (bang, ty) -> (name, bang, ty)

sigToDec :: GHC.Sig GHC.GhcPs -> [TH.Dec]
sigToDec sig = case sig of 
  GHC.TypeSig _x idts hsSigType -> 
    map (\idt -> TH.SigD (rdrNameToName (unLoc idt)) (hsSigTypeToType (unLoc (GHC.hswc_body hsSigType)))) idts
  _other -> error "sigToDec: only TypeSig is supported so far"

hsBindLRToDec :: GHC.HsBindLR GHC.GhcPs GHC.GhcPs -> TH.Dec
hsBindLRToDec bind = case bind of 
  GHC.FunBind _x idt matches -> 
    TH.FunD
      (rdrNameToName (unLoc idt))
      (matchGroupToClause matches)
  _other -> error "hsBindLRToDec: only FunBind is supported so far"

hsTyVarBndrToTyVarBndr :: GHC.HsTyVarBndr (GHC.HsBndrVis GHC.GhcPs) GHC.GhcPs -> TH.TyVarBndr TH.BndrVis
hsTyVarBndrToTyVarBndr (GHC.UserTyVar _x specificity idt) =
    TH.PlainTV 
      (rdrNameToName (unLoc idt)) 
      (hsBndrVisToBndrVis specificity)
hsTyVarBndrToTyVarBndr (GHC.KindedTyVar _x specificity idt ki) =
  TH.KindedTV 
    (rdrNameToName (unLoc idt)) 
    (hsBndrVisToBndrVis specificity)
    (hsTypeToType (unLoc ki))

hsBndrVisToBndrVis :: GHC.HsBndrVis GHC.GhcPs -> TH.BndrVis
hsBndrVisToBndrVis (GHC.HsBndrRequired _x) = TH.BndrReq
hsBndrVisToBndrVis (GHC.HsBndrInvisible _x) = TH.BndrInvis

hsFunDepToFunDep :: GHC.FunDep GHC.GhcPs -> TH.FunDep
hsFunDepToFunDep (GHC.FunDep _x xs ys) = 
  TH.FunDep 
    (map (rdrNameToName . unLoc) xs) 
    (map (rdrNameToName . unLoc) ys)

--- DerivDecl ------------------------------------------------------------------

hsDerivDeclToDec :: GHC.DerivDecl GHC.GhcPs -> TH.Dec
hsDerivDeclToDec GHC.DerivDecl{deriv_type = GHC.HsWC _x body, ..} =
  TH.StandaloneDerivD
    (fmap (hsDerivStrategyToDerivStrategy . unLoc) deriv_strategy)
    []
    (hsSigTypeToType (unLoc body))

hsDerivStrategyToDerivStrategy :: GHC.DerivStrategy GHC.GhcPs -> TH.DerivStrategy
hsDerivStrategyToDerivStrategy GHC.StockStrategy {} = TH.StockStrategy
hsDerivStrategyToDerivStrategy GHC.NewtypeStrategy {} = TH.NewtypeStrategy
hsDerivStrategyToDerivStrategy GHC.AnyclassStrategy {} = TH.AnyclassStrategy
hsDerivStrategyToDerivStrategy GHC.ViaStrategy {} = error "ViaStrategy not yet implemented"

-- --- InstDecl -------------------------------------------------------------------

instDeclToDec :: GHC.InstDecl GHC.GhcPs -> TH.Dec
instDeclToDec instDecl = case instDecl of
  GHC.ClsInstD {..} -> clsInstDeclToDec cid_inst
  GHC.DataFamInstD {} -> error "DataFamInstD not yet implemented"
  GHC.TyFamInstD {} -> error "TyFamInstD not yet implemented"

--- ClsInstDecl ----------------------------------------------------------------

clsInstDeclToDec :: GHC.ClsInstDecl GHC.GhcPs -> TH.Dec
clsInstDeclToDec GHC.ClsInstDecl{..} = 
  TH.InstanceD
    (overlapModeToOverlap . unLoc =<< cid_overlap_mode) 
    [] 
    (hsSigTypeToType (unLoc cid_poly_ty)) 
    (map hsBindLR (foldr ((:) . unLoc) [] cid_binds))

--- HsBindLR -------------------------------------------------------------------

hsBindLR :: GHC.HsBindLR GHC.GhcPs GHC.GhcPs -> TH.Dec
hsBindLR (GHC.PatSynBind _x bind) = patSynBindToDec bind
hsBindLR (GHC.FunBind _x idt matches) = 
  TH.FunD
    (rdrNameToName (unLoc idt))
    (matchGroupToClause matches)
hsBindLR (GHC.PatBind _x _lhs _mult _rhs) = 
  error "hsBindLR: PatBind not yet implemented"
hsBindLR (GHC.VarBind _x idt rhs) = 
  TH.ValD
    (TH.VarP (rdrNameToName idt))
    (TH.NormalB (hsExprToExp (unLoc rhs)))
    []

--- MatchGroup -----------------------------------------------------------------

matchGroupToClause :: GHC.MatchGroup GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> [TH.Clause]
matchGroupToClause GHC.MG {..} = map (hsMatchToClause . unLoc) (unLoc mg_alts)

--- Match ----------------------------------------------------------------------

hsMatchToClause :: GHC.Match GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> TH.Clause
hsMatchToClause (GHC.Match _x _ctxt pats grRHS) = 
  TH.Clause 
    thPats 
    (TH.GuardedB (foldr ((++) . conv . unLoc) [] (GHC.grhssGRHSs grRHS)))
    (bindsToDecs (GHC.grhssLocalBinds grRHS))
  where 
    thPats :: [TH.Pat]
    thPats = map (hsPatToPat . unLoc) pats

    conv :: GHC.GRHS GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> [(TH.Guard, TH.Exp)]
    conv (GHC.GRHS _x guards body) = 
      [(stmtLRToGuard (unLoc guard), hsExprToExp (unLoc body)) | guard <- guards]

    bindsToDecs :: GHC.HsLocalBindsLR GHC.GhcPs GHC.GhcPs -> [TH.Dec]
    bindsToDecs (GHC.HsValBinds _x (GHC.ValBinds _x2 binds _sigs)) = 
      map hsBindLR (foldr ((:) . unLoc) [] binds)
    bindsToDecs (GHC.HsIPBinds _x (GHC.IPBinds _x2 _hsIPBinds)) = 
      error "hsMatchToClause: HsIPBinds not yet implemented"
    bindsToDecs (GHC.HsValBinds _x (GHC.XValBindsLR _x2)) = 
      error "hsMatchToClause: XValBindsLR impossible!"
    bindsToDecs (GHC.EmptyLocalBinds _x) = 
      []

-- gRhsToGuardedExp :: GHC.GRHS GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> ([TH.Guard], TH.Exp)
-- gRhsToGuardedExp (GHC.GRHS _x guards body) = (thGuards, thExp) 
--   where 
--     thGuards :: [TH.Guard]
--     thGuards = map (stmtLRToGuard . unLoc) guards

--     thExp :: TH.Exp
--     thExp = hsExprToExp (unLoc body)

stmtLRToGuard :: GHC.StmtLR GHC.GhcPs GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> TH.Guard
stmtLRToGuard (GHC.BindStmt _x pat body) = 
  TH.PatG [TH.BindS (hsPatToPat (unLoc pat)) (hsExprToExp (unLoc body))]
stmtLRToGuard (GHC.BodyStmt _x body _stxl _stxr) = 
  TH.NormalG (hsExprToExp (unLoc body))
stmtLRToGuard (GHC.LetStmt _x _locals) = 
  error ("'LetStmt' statement in guard: not supported; no body expression")
stmtLRToGuard (GHC.LastStmt _x _body _mb _stx) = 
  error ("'LastStmt' statement in guard: not supported; no body expression")
stmtLRToGuard (GHC.ParStmt _x _parStmtBlock _hsExpr _stx) = 
  error ("'ParStmt' statement in guard: not supported")
stmtLRToGuard (GHC.TransStmt _x _form _stmts _bndrs _usingExpr _byExpr _stxret _stxbind _fmapExpr) = 
  error ("'TransStmt' statement in guard: not supported")
stmtLRToGuard (GHC.RecStmt _x _stmts _later_ids _rec_ids _stxbind_fn _stxret_fn _stxmfix_fn) = 
  error ("'RecStmt' statement in guard: not supported")
stmtLRToGuard (GHC.ApplicativeStmt _x _args _stx) = 
  error ("'ApplicativeStmt' statement in guard: not supported")

--- HsExpr ---------------------------------------------------------------------

hsExprToExp :: GHC.HsExpr GHC.GhcPs -> TH.Exp
hsExprToExp (GHC.HsVar _ idt) = TH.VarE (rdrNameToName (unLoc idt))
hsExprToExp (GHC.HsLit _ lit) = TH.LitE (hsLitToLit lit)
hsExprToExp (GHC.HsApp _ func arg) = 
  TH.AppE 
    (hsExprToExp (unLoc func)) 
    (hsExprToExp (unLoc arg))
hsExprToExp (GHC.HsUnboundVar _x idt) = 
  TH.VarE (rdrNameToName idt)
hsExprToExp (GHC.HsPar _x expr) = 
  TH.ParensE (hsExprToExp (unLoc expr))
hsExprToExp (GHC.HsOverLit _x _overLit) =
  error "hsExprToExp: HsOverLit not yet implemented"
hsExprToExp (GHC.HsRecSel _x fldOcc) =
  TH.VarE (rdrNameToName (unLoc (GHC.foLabel fldOcc)))
hsExprToExp (GHC.HsAppType _x expr ty) =
  TH.AppTypeE 
    (hsExprToExp (unLoc expr)) 
    (hsTypeToType (unLoc (GHC.hswc_body ty)))
hsExprToExp (GHC.OpApp _ lhs op rhs) = 
  TH.InfixE 
    (Just (hsExprToExp (unLoc lhs))) 
    (hsExprToExp (unLoc op))
    (Just (hsExprToExp (unLoc rhs)))
hsExprToExp (GHC.NegApp _x expr _) = 
  TH.UInfixE 
    (TH.LitE (TH.IntegerL 0)) 
    (TH.VarE (TH.mkName "-")) 
    (hsExprToExp (unLoc expr))
hsExprToExp (GHC.SectionL _x expr op) =
  TH.InfixE 
    (Just (hsExprToExp (unLoc expr)))
    (hsExprToExp (unLoc op)) 
    Nothing
hsExprToExp (GHC.SectionR _x expr op) =
  TH.InfixE 
    Nothing
    (hsExprToExp (unLoc op)) 
    (Just (hsExprToExp (unLoc expr)))
hsExprToExp _other = 
  error ("hsExprToExp: unsupported HsExpr: ")

-- HsPatSynDir -----------------------------------------------------------------

hsPatSynDirToPatSynDir :: GHC.HsPatSynDir GHC.GhcPs -> TH.PatSynDir
hsPatSynDirToPatSynDir GHC.Unidirectional = TH.Unidir
hsPatSynDirToPatSynDir GHC.ImplicitBidirectional = TH.ImplBidir
hsPatSynDirToPatSynDir (GHC.ExplicitBidirectional expr) = TH.ExplBidir (map (hsMatchToClause . unLoc) (unLoc (GHC.mg_alts expr)))

--- HsPatSynDetails ------------------------------------------------------------

hsPatSynDetailsToPatSynDetails :: GHC.HsPatSynDetails GHC.GhcPs -> TH.PatSynArgs
hsPatSynDetailsToPatSynDetails (GHC.PrefixCon _tyArgs args) = TH.PrefixPatSyn (map (rdrNameToName . unLoc) args)
hsPatSynDetailsToPatSynDetails (GHC.InfixCon lhs rhs) = TH.InfixPatSyn (rdrNameToName (unLoc lhs)) (rdrNameToName (unLoc rhs))
hsPatSynDetailsToPatSynDetails (GHC.RecCon r) = TH.RecordPatSyn (map (rdrNameToName . unLoc . GHC.recordPatSynPatVar) r)

--- PatSynBind -----------------------------------------------------------------

patSynBindToDec :: GHC.PatSynBind GHC.GhcPs GHC.GhcPs -> TH.Dec
patSynBindToDec GHC.PSB {..} = 
  TH.PatSynD
    (rdrNameToName (unLoc psb_id))
    (hsPatSynDetailsToPatSynDetails psb_args)
    (hsPatSynDirToPatSynDir psb_dir)
    (hsPatToPat (unLoc psb_def))

hsLitToLit :: GHC.HsLit GHC.GhcPs -> TH.Lit
hsLitToLit (GHC.HsChar _x c) = TH.CharL c
hsLitToLit (GHC.HsCharPrim _x c) = TH.CharL c
hsLitToLit (GHC.HsString _x s) = TH.StringL (FastString.unpackFS s)
hsLitToLit (GHC.HsStringPrim _x bs) = TH.StringL (ByteString.unpack bs)
hsLitToLit (GHC.HsInteger _x i _) = TH.IntegerL i
hsLitToLit (GHC.HsInt _x i) = TH.IntegerL (integralLitToInteger i)
hsLitToLit (GHC.HsInt8Prim _x n) = TH.IntegerL (fromIntegral n)
hsLitToLit (GHC.HsInt16Prim _x n) = TH.IntegerL (fromIntegral n)
hsLitToLit (GHC.HsInt32Prim _x n) = TH.IntegerL (fromIntegral n)
hsLitToLit (GHC.HsInt64Prim _x n) = TH.IntegerL (fromIntegral n)
hsLitToLit (GHC.HsIntPrim _x i) = TH.IntPrimL i
hsLitToLit (GHC.HsRat _x r _) = TH.RationalL (GHC.rationalFromFractionalLit r)
hsLitToLit (GHC.HsWordPrim _x w) = TH.IntegerL w
hsLitToLit (GHC.HsWord8Prim _x n) = TH.IntegerL (fromIntegral n)
hsLitToLit (GHC.HsWord16Prim _x n) = TH.IntegerL (fromIntegral n)
hsLitToLit (GHC.HsWord32Prim _x n) = TH.IntegerL (fromIntegral n)
hsLitToLit (GHC.HsWord64Prim _x n) = TH.IntegerL (fromIntegral n)
hsLitToLit (GHC.HsFloatPrim _x r) = TH.RationalL (GHC.rationalFromFractionalLit r)
hsLitToLit (GHC.HsDoublePrim _x r) = TH.RationalL (GHC.rationalFromFractionalLit r)

--- IntegralLit ----------------------------------------------------------------

integralLitToInteger :: GHC.IntegralLit -> Integer
integralLitToInteger GHC.IL {..} = if il_neg then negate il_value else il_value

--- Pat ------------------------------------------------------------------------

hsPatToPat :: GHC.Pat GHC.GhcPs -> TH.Pat
hsPatToPat (GHC.WildPat _x) = TH.WildP
hsPatToPat (GHC.VarPat _x idt) = TH.VarP (rdrNameToName (unLoc idt))
hsPatToPat (GHC.LazyPat _x pat) = TH.TildeP (hsPatToPat (unLoc pat)) 
hsPatToPat (GHC.AsPat _x idt pat) = TH.AsP (rdrNameToName (unLoc idt)) (hsPatToPat (unLoc pat))
hsPatToPat (GHC.ParPat _x pat) = TH.ParensP (hsPatToPat (unLoc pat))
hsPatToPat (GHC.BangPat _x pat) = TH.BangP (hsPatToPat (unLoc pat))
hsPatToPat (GHC.ListPat _x pats) = TH.ListP (map (hsPatToPat . unLoc) pats)
hsPatToPat (GHC.TuplePat _x pats _boxity) = TH.TupP (map (hsPatToPat . unLoc) pats)
-- hsPatToPat (GHC.OrPat _x pats) = _
hsPatToPat (GHC.SumPat _x _pat _conTag _sumWith) = error "hsPatToPat: SumPat not yet implemented"
-- hsPatToPat (GHC.SumPat _x pat conTag sumWith) = TH.SumP (hsPatToPat (unLoc pat)) conTag sumWith
hsPatToPat (GHC.ConPat _x conPat details) = case details of 
  GHC.PrefixCon tyArgs args ->
    TH.ConP
      name
      (map hsConPatTyArgToType tyArgs) 
      (map (hsPatToPat . unLoc) args)
  GHC.InfixCon lhs rhs ->
    TH.InfixP
      (hsPatToPat (unLoc lhs))
      name
      (hsPatToPat (unLoc rhs))
  GHC.RecCon r ->
    TH.RecP
      name
      (hsRecFieldsToFieldPat r) 
  where 
    name :: TH.Name
    name = conPatToName conPat 
hsPatToPat (GHC.ViewPat _x lhs rhs) = TH.ViewP (hsExprToExp (unLoc lhs)) (hsPatToPat (unLoc rhs))
hsPatToPat (GHC.SplicePat _x _spl) = error "hsPatToPat: SplicePat not yet implemented"
hsPatToPat (GHC.LitPat _x lit) = TH.LitP (hsLitToLit lit)
hsPatToPat (GHC.NPat _x _hsOverLit _optStxExpr _stxExpr) = error "hsPatToPat: n pattern not supportedf"
hsPatToPat (GHC.NPlusKPat _x _idt _hsOverLit _optStxExpr _stxExpr _v) = error "hsPatToPat: n plus k pattern not supported"
hsPatToPat (GHC.SigPat _x pat sig) = TH.SigP (hsPatToPat (unLoc pat)) (hsPatSigTypeToType sig)
hsPatToPat (GHC.EmbTyPat _x tyPat) = TH.TypeP (hsTyPatToType tyPat)
hsPatToPat (GHC.InvisPat _x tyPat) = TH.InvisP (hsTyPatToType tyPat)

conPatToName :: XRec GHC.GhcPs (ConLikeP GHC.GhcPs) -> TH.Name
conPatToName = rdrNameToName . unLoc

--- HsConPatTyArg --------------------------------------------------------------

hsConPatTyArgToType :: GHC.HsConPatTyArg GHC.GhcPs -> TH.Type
hsConPatTyArgToType (GHC.HsConPatTyArg _x tyPat) = hsTyPatToType tyPat

--- HsRecFields ----------------------------------------------------------------

hsRecFieldsToFieldPat :: GHC.HsRecFields GHC.GhcPs (GHC.LPat GHC.GhcPs) -> [TH.FieldPat]
hsRecFieldsToFieldPat = map (hsFieldBindToFieldPat . unLoc) . GHC.rec_flds 

--- HsFieldBind ----------------------------------------------------------------

hsFieldBindToFieldPat :: GHC.HsFieldBind (GHC.LFieldOcc GHC.GhcPs) (GHC.LPat GHC.GhcPs) -> TH.FieldPat
hsFieldBindToFieldPat hsFieldBind = (fieldIdt, fieldPat)
  where 
    fieldIdt :: TH.Name
    fieldIdt = rdrNameToName (unLoc (GHC.foLabel (unLoc (GHC.hfbLHS hsFieldBind))))

    fieldPat :: TH.Pat
    fieldPat = hsPatToPat (unLoc (GHC.hfbRHS hsFieldBind))

--- HsPatSigType ---------------------------------------------------------------

hsPatSigTypeToType :: GHC.HsPatSigType GHC.GhcPs -> TH.Type
hsPatSigTypeToType = hsTypeToType . unLoc . GHC.hsps_body 

--- HsTyPat --------------------------------------------------------------------

hsTyPatToType :: GHC.HsTyPat GHC.GhcPs -> TH.Type
hsTyPatToType (GHC.HsTP _x body) = hsTypeToType (unLoc body)

--- OverlapMode ----------------------------------------------------------------

overlapModeToOverlap :: GHC.OverlapMode -> Maybe TH.Overlap
overlapModeToOverlap GHC.NoOverlap {} = Nothing
overlapModeToOverlap GHC.Overlappable {} = Just TH.Overlappable
overlapModeToOverlap GHC.Overlapping {} = Just TH.Overlapping
overlapModeToOverlap GHC.Overlaps {} = Just TH.Overlappable
overlapModeToOverlap GHC.Incoherent {} = Just TH.Incoherent
overlapModeToOverlap GHC.NonCanonical {} = Nothing

--- HsSigType ------------------------------------------------------------------

hsSigTypeToType :: GHC.HsSigType GHC.GhcPs -> TH.Type
hsSigTypeToType = hsTypeToType . unLoc . GHC.sig_body

--- HsType ---------------------------------------------------------------------

-- hsTypeToVarBangType :: GHC.HsType GHC.GhcPs -> TH.VarBangType
-- hsTypeToVarBangType hsType = case hsType of

-- hsTypeToVarType :: GHC.HsType GHC.GhcPs -> (TH.Name, TH.Type)
-- hsTypeToVarType (GHC.HsRecTy _x fields) = (_, _)

hsTypeToBangType :: GHC.HsType GHC.GhcPs -> TH.BangType
hsTypeToBangType hsType = case hsType of 
  GHC.HsBangTy _x bang ty -> (hsSrcBangToBang bang, hsTypeToType (unLoc ty))
  other -> (TH.Bang TH.NoSourceUnpackedness TH.NoSourceStrictness, hsTypeToType other)

hsSrcBangToBang :: GHC.HsSrcBang -> TH.Bang
hsSrcBangToBang (GHC.HsSrcBang _x s u) = TH.Bang (toSrcPackedness s) (toSrcStrictness u)
  where 
    toSrcPackedness GHC.NoSrcUnpack = TH.NoSourceUnpackedness
    toSrcPackedness GHC.SrcUnpack = TH.SourceUnpack
    toSrcPackedness GHC.SrcNoUnpack = TH.SourceNoUnpack

    toSrcStrictness GHC.NoSrcStrict = TH.NoSourceStrictness
    toSrcStrictness GHC.SrcStrict = TH.SourceStrict
    toSrcStrictness GHC.SrcLazy = TH.SourceLazy

hsTypeToType :: GHC.HsType GHC.GhcPs -> TH.Type
hsTypeToType (GHC.HsTyVar _x promotionFlag idt)
  | promotionFlag == GHC.IsPromoted = TH.PromotedT (rdrNameToName (unLoc idt))
  | otherwise = case TH.nameBase name of 
    "" -> error "hsTypeToType: empty name"
    c : _ 
      | isUpper c -> TH.ConT name
      | otherwise -> TH.VarT name
  where 
    name :: TH.Name 
    name = rdrNameToName (unLoc idt)
hsTypeToType (GHC.HsForAllTy _x telescope body) = case telescope of
  GHC.HsForAllVis {..} -> 
    TH.ForallT 
      (map (hsTyVarBndrToTyVarBndrWith (const TH.SpecifiedSpec) . unLoc) hsf_vis_bndrs)
      []
      (hsTypeToType (unLoc body))
  GHC.HsForAllInvis {..} -> 
    TH.ForallT 
      (map (hsTyVarBndrToTyVarBndrWith hsSpecifityToSpecificity . unLoc) hsf_invis_bndrs) 
      []
      (hsTypeToType (unLoc body))
hsTypeToType (GHC.HsQualTy _x ctxt body) =
  TH.ForallT 
    []
    (map (hsTypeToType . unLoc) (unLoc ctxt))
    (hsTypeToType (unLoc body))
hsTypeToType (GHC.HsAppTy _x a b) =
  TH.AppT (hsTypeToType (unLoc a))  (hsTypeToType (unLoc b))
hsTypeToType (GHC.HsAppKindTy _x a b) =
  TH.AppKindT (hsTypeToType (unLoc a)) (hsTypeToType (unLoc b))
hsTypeToType (GHC.HsFunTy _x hsArrow a b) = case hsArrow of 
  GHC.HsUnrestrictedArrow _x -> 
    TH.ArrowT 
      `TH.AppT` hsTypeToType (unLoc a)
      `TH.AppT` hsTypeToType (unLoc b)
  GHC.HsLinearArrow _x -> 
    TH.MulArrowT 
      `TH.AppT` TH.LitT (TH.NumTyLit 1)
      `TH.AppT` hsTypeToType (unLoc a)
      `TH.AppT` hsTypeToType (unLoc b)
  GHC.HsExplicitMult _x mult ->
    TH.MulArrowT 
      `TH.AppT` hsTypeToType (unLoc mult)
      `TH.AppT` hsTypeToType (unLoc a)
      `TH.AppT` hsTypeToType (unLoc b)
hsTypeToType (GHC.HsListTy _x a) =
  TH.AppT 
    TH.ListT 
    (hsTypeToType (unLoc a))
hsTypeToType (GHC.HsTupleTy _x boxity args) =
  let tup :: TH.Type
      tup = case boxity of 
        GHC.HsUnboxedTuple -> TH.UnboxedTupleT (length args)
        GHC.HsBoxedOrConstraintTuple -> TH.TupleT (length args)
    in foldl (\x y -> x `TH.AppT` hsTypeToType (unLoc y)) tup args
hsTypeToType (GHC.HsSumTy _x _args) =
  error "hsTypeToType: SumTy not yet implemented"
hsTypeToType (GHC.HsOpTy _x promotion lhsTy opIdt rhsTy) = case promotion of
  GHC.IsPromoted ->
    TH.InfixT 
      (hsTypeToType (unLoc lhsTy)) 
      (rdrNameToName (unLoc opIdt)) 
      (hsTypeToType (unLoc rhsTy))
  GHC.NotPromoted ->
    TH.InfixT 
      (hsTypeToType (unLoc lhsTy)) 
      (rdrNameToName (unLoc opIdt)) 
      (hsTypeToType (unLoc rhsTy))
hsTypeToType (GHC.HsParTy _x ty) =
  TH.ParensT (hsTypeToType (unLoc ty))
hsTypeToType (GHC.HsIParamTy _x _hsIPName _ty) =
  error "hsTypeToType: HsIParamTy impossible"
hsTypeToType (GHC.HsStarTy _x _p) =
  TH.StarT
hsTypeToType (GHC.HsKindSig _x ty ki) =
  TH.SigT (hsTypeToType (unLoc ty)) (hsTypeToType (unLoc ki))
hsTypeToType (GHC.HsSpliceTy _x _s) =
  error "hsTypeToType: HsSpliceTy impossible!"
hsTypeToType (GHC.HsDocTy _x _ty _doc) =
  error "hsTypeToType: HsDocTy not yet implemented"
hsTypeToType (GHC.HsBangTy _x _bangType _ty) =
  error "hsTypeToType: HsBangTy impossible"
hsTypeToType (GHC.HsRecTy _x _fields) =
  error "hsTypeToType: HsRecTy impossible"
hsTypeToType (GHC.HsExplicitListTy _x _promotion _tys) =
  error "hsTypeToType: HsExplicitListTy impossible"
hsTypeToType (GHC.HsExplicitTupleTy _x _tys) =
  error "hsTypeToType: HsExplicitTupleTy impossible"
hsTypeToType (GHC.HsTyLit _x tyLit) =
  TH.LitT (hsTyLitToType tyLit)
hsTypeToType (GHC.HsWildCardTy _x) =
  TH.WildCardT
hsTypeToType (GHC.XHsType _x) =
  error "hsTypeToType: XHsType impossible"

hsTyLitToType :: GHC.HsTyLit GHC.GhcPs -> TH.TyLit
hsTyLitToType (GHC.HsNumTy _x n) = TH.NumTyLit n
hsTyLitToType (GHC.HsStrTy _x s) = TH.StrTyLit (FastString.unpackFS s)
hsTyLitToType (GHC.HsCharTy _x c) = TH.CharTyLit c

hsTyVarBndrToTyVarBndrWith :: 
  (flag -> flag') -> 
  GHC.HsTyVarBndr flag GHC.GhcPs -> 
  TH.TyVarBndr flag'
hsTyVarBndrToTyVarBndrWith  f (GHC.KindedTyVar _x specificity binder kind) =
  TH.KindedTV (rdrNameToName (unLoc binder)) (f specificity) (hsTypeToType (unLoc kind))
hsTyVarBndrToTyVarBndrWith f (GHC.UserTyVar _x specificity binder) =
  TH.PlainTV (rdrNameToName (unLoc binder)) (f specificity)

hsSpecifityToSpecificity :: GHC.Specificity -> TH.Specificity
hsSpecifityToSpecificity GHC.SpecifiedSpec = TH.SpecifiedSpec
hsSpecifityToSpecificity GHC.InferredSpec = TH.InferredSpec

--- RdrName --------------------------------------------------------------------



rdrNameToName :: GHC.RdrName -> TH.Name
rdrNameToName rdrName = case rdrName of
  GHC.Unqual occName -> 
    TH.mkName (GHC.occNameString occName) 
  GHC.Exact name -> 
    rdrNameToName (GHC.nameRdrName name)
  GHC.Orig _mdl occName -> 
    error ("rdrNameToName: Orig not yet implemented" ++ GHC.occNameString occName)
    -- TH.Name
    --   (TH.OccName (GHC.occNameString occName)) 
    --   (TH.NameG TH.VarName (TH.PkgName "proto3-suite") (ghcModuleNameToTHModuleName mdl))
  GHC.Qual moduleName occName -> 
    TH.Name 
      (TH.OccName (GHC.occNameString occName)) 
      (TH.NameQ (TH.ModName (GHC.moduleNameString moduleName)))
  -- where 
  --   ghcModuleNameToTHModuleName :: GHC.Module -> TH.ModName
  --   ghcModuleNameToTHModuleName mdl = TH.ModName (GHC.moduleNameString (GHC.moduleName mdl))
