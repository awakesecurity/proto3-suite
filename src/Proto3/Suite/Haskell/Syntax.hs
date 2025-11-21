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

import Data.ByteString.Char8 qualified as ByteString
import Data.Maybe (isJust)

import "template-haskell" Language.Haskell.TH.Syntax qualified as TH

import "ghc-lib-parser" Language.Haskell.Syntax.Binds qualified as GHC
  ( PatSynBind (..)
  )
import "ghc-lib-parser" Language.Haskell.Syntax.Type qualified as GHC
  ( FieldOcc (..)
  )
import "ghc-lib-parser" Language.Haskell.Syntax.Binds qualified as GHC
  ( HsLocalBindsLR (..)
  , HsValBindsLR (..)
  , HsIPBinds (..)
  )
import "ghc-lib-parser" GHC.Unit.Types qualified as GHC
  ( Module (..)
  , moduleName
  , moduleUnitId
  )
import "ghc-lib-parser" Language.Haskell.Syntax.Expr qualified as GHC
  ( GRHSs (..)
  , GRHS (..)
  , StmtLR (..)
  )
import "ghc-lib-parser" Language.Haskell.Syntax qualified as GHC
  ( ClsInstDecl (..)
  , HsBindLR (..)
  , HsLamVariant (..)
  , HsArrowMatchContext (..)
  , HsMatchContext (..) 
  , HsConPatTyArg (..)
  , HsTyPat (..)
  , HsRecFields (..)
  , HsFieldBind (..)
  , HsDecl (..)
  , HsExpr (..)
  , LHsExpr 
  , HsModule (..)
  , HsConDetails (..)
  , HsConPatDetails (..)
  , RecordPatSynField (..)
  , HsPatSynDir (..)
  , HsTyPat (..)
  , HsSigType (..)
  , HsPatSigType (..)
  , HsType (..)
  , HsPatSynDetails 
  , HsUntypedSplice (..)
  , LHsFieldBind
  , LFieldOcc
  , HsLit (..)
  , GRHSs
  , IdP (..)
  , InstDecl (..)
  , Match (..)
  , MatchGroup (..)
  , NoExtField (..)
  , PromotionFlag (..)
  , Pat (..)
  , TyClDecl (..)
  , XRec (..)
  , moduleNameString
  )
import "ghc-lib-parser" GHC.Parser.Annotation qualified as GHC
  ( SrcSpanAnnA
  )
import "ghc-lib-parser" GHC.Types.Name.Reader qualified as GHC
import "ghc-lib-parser" GHC.Types.Name.Reader qualified as GHC
  ( RdrName (..)
  , rdrNameOcc
  , isExact
  , isUnqual
  , nameRdrName
  )
import "ghc-lib-parser" GHC.Types.Basic qualified as GHC
  ( OverlapMode (..)
  )
import "ghc-lib-parser" GHC.Types.Name.Occurrence qualified as GHC
  ( OccName (..)
  , occNameString
  )
import GHC.Types.SourceText qualified as GHC 
  ( IntegralLit (..)
  , rationalFromFractionalLit
  )
import GHC.Types.SrcLoc 
  ( GenLocated(..)
  , SrcSpan
  , generatedSrcSpan
  , unLoc
  )
import "ghc-lib-parser" GHC.Data.FastString qualified as FastString

#if MIN_VERSION_ghc_lib_parser(9,6,0)
import "ghc-lib-parser" GHC.Hs.Extension qualified as GHC
  ( GhcPs 
  )
#endif

--------------------------------------------------------------------------------




--- HsModule -------------------------------------------------------------------


#if MIN_VERSION_ghc_lib_parser(9,6,0)
type HsModule = GHC.HsModule GHC.GhcPs 
#else
type HsModule = GHC.HsModule
#endif 

--- HsDecl ---------------------------------------------------------------------

hsDeclToDec :: GHC.HsDecl GHC.GhcPs -> TH.Dec
hsDeclToDec hsDecl = case hsDecl of 
  GHC.InstD xInstD instDecl -> instDeclToDec instDecl
  other -> error "unsupported HsDecl in hsDeclToDec"

--- InstDecl -------------------------------------------------------------------

instDeclToDec :: GHC.InstDecl GHC.GhcPs -> TH.Dec
instDeclToDec instDecl = case instDecl of
  GHC.DataFamInstD {..} -> error "DataFamInstD not yet implemented"
  GHC.TyFamInstD {..} -> error "TyFamInstD not yet implemented"
  GHC.ClsInstD {..} -> clsInstDeclToDec cid_inst

--- ClsInstDecl ----------------------------------------------------------------

clsInstDeclToDec :: GHC.ClsInstDecl GHC.GhcPs -> TH.Dec
clsInstDeclToDec GHC.ClsInstDecl{..} = 
  TH.InstanceD
    (overlapModeToOverlap . unLoc =<< cid_overlap_mode) 
    [] 
    (hsSigTypeToType (unLoc cid_poly_ty)) 
    instDecs
  where 
    instDecs :: [TH.Dec]
    instDecs = map hsBindLR (foldr ((:) . unLoc) [] cid_binds)

--- HsBindLR -------------------------------------------------------------------

hsBindLR :: GHC.HsBindLR GHC.GhcPs GHC.GhcPs -> TH.Dec
hsBindLR (GHC.PatSynBind _x bind) = patSynBindToDec bind
hsBindLR (GHC.FunBind _x idt matches) = 
  TH.FunD
    (rdrNameToName (unLoc idt))
    (matchGroupToClause matches)
hsBindLR (GHC.PatBind _x lhs mult rhs) = 
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
hsMatchToClause (GHC.Match _x ctxt pats grRHS) = 
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
    bindsToDecs (GHC.HsValBinds _x (GHC.ValBinds _x2 binds sigs)) = 
      map hsBindLR (foldr ((:) . unLoc) [] binds)
    bindsToDecs (GHC.HsIPBinds _x (GHC.IPBinds _x2 hsIPBinds)) = 
      error "hsMatchToClause: HsIPBinds not yet implemented"
    bindsToDecs (GHC.EmptyLocalBinds _x) = 
      []

    -- thDecs :: [TH.Dec]
    -- thDecs = case GHC.grhssLocalBinds grRHS of 
    --   GHC.HsValBinds _x1 (GHC.ValBinds _x2 binds sigs) -> 
    --     _
    --   GHC.HsIPBinds _x1 (GHC.IPBinds _x2 hsIPBinds) -> 
    --     _
    --   GHC.EmptyLocalBinds _x -> 
    --     []

gRhsToGuardedExp :: GHC.GRHS GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> ([TH.Guard], TH.Exp)
gRhsToGuardedExp (GHC.GRHS _x guards body) = (thGuards, thExp) 
  where 
    thGuards :: [TH.Guard]
    thGuards = map (stmtLRToGuard . unLoc) guards

    thExp :: TH.Exp
    thExp = hsExprToExp (unLoc body)

stmtLRToGuard :: GHC.StmtLR GHC.GhcPs GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> TH.Guard
stmtLRToGuard (GHC.BindStmt _x pat body) = 
  TH.PatG [TH.BindS (hsPatToPat (unLoc pat)) (hsExprToExp (unLoc body))]
stmtLRToGuard (GHC.BodyStmt _x body _stxl _stxr) = 
  TH.NormalG (hsExprToExp (unLoc body))
stmtLRToGuard (GHC.LetStmt _x locals) = 
  error ("'LetStmt' statement in guard: not supported; no body expression")
stmtLRToGuard (GHC.LastStmt _x body mb _stx) = 
  error ("'LastStmt' statement in guard: not supported; no body expression")
stmtLRToGuard (GHC.ParStmt _x parStmtBlock hsExpr _stx) = 
  error ("'ParStmt' statement in guard: not supported")
stmtLRToGuard (GHC.TransStmt _x form stmts bndrs usingExpr byExpr _stxret _stxbind fmapExpr) = 
  error ("'TransStmt' statement in guard: not supported")
stmtLRToGuard (GHC.RecStmt _x stmts later_ids rec_ids _stxbind_fn _stxret_fn _stxmfix_fn) = 
  error ("'RecStmt' statement in guard: not supported")

-- hsMatchToClause GHC.Match {..} = case m_ctxt of 
--   GHC.FunRhs fun fixity strictness -> 
--     _
--   GHC.CaseAlt -> 
--     _
--   GHC.LamAlt variant -> case variant of 
--     GHC.LamSingle -> 
--       _
--     GHC.LamCase -> 
--       _
--     GHC.LamCases -> 
--       _
--   GHC.IfAlt -> 
--     _
--   GHC.ArrowMatchCtxt arrowMatchCtxt -> case arrowMatchCtxt of
--     GHC.ProcExpr -> 
--       _
--     GHC.ArrowCaseAlt -> 
--       _ 
--     GHC.ArrowLamAlt variant -> case variant of 
--       GHC.LamSingle -> 
--         _
--       GHC.LamCase -> 
--         _
--       GHC.LamCases -> 
--         _
--   where 
    -- hsMatchToClause GHC.Match {..} =

  -- TH.Clause 
  --   (map (hsPatToPat . unLoc) m_pats)
  --   (_ m_ctxt)
  --   (gRHSsToDec m_grhss)

--- HsMatchContext -------------------------------------------------------------

-- hsMatchContextToMatchContext :: GHC.MatchContext -> TH.MatchContext


--- GRHSs ----------------------------------------------------------------------

-- gRHSsToDec :: GHC.GRHSs GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> [TH.Dec]
-- gRHSsToDec GHC.GRHSs {..} = _

--- HsExpr ---------------------------------------------------------------------

hsExprToExp :: GHC.HsExpr GHC.GhcPs -> TH.Exp
hsExprToExp (GHC.HsVar _ idt) = TH.VarE (rdrNameToName (unLoc idt))
hsExprToExp (GHC.HsLit _ lit) = TH.LitE (hsLitToLit lit)

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
hsLitToLit (GHC.HsInt _x i) = TH.IntegerL (integralLitToInteger i)
hsLitToLit (GHC.HsIntPrim _x i) = TH.IntPrimL i
hsLitToLit (GHC.HsRat _x r _) = TH.RationalL (GHC.rationalFromFractionalLit r)
hsLitToLit (GHC.HsWordPrim _x w) = TH.IntegerL w
-- hsLitToLit (GHC.HsMultiLineString _x s) = TH.StringL s
-- hsLitToLit (GHC.HsFloat _x r _) = TH.RationalL (_ r)
-- hsLitToLit (GHC.HsDouble _x r _) = TH.RationalL (_ r)

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
hsPatToPat (GHC.TuplePat _x pats boxity) = TH.TupP (map (hsPatToPat . unLoc) pats)
-- hsPatToPat (GHC.OrPat _x pats) = _
hsPatToPat (GHC.SumPat _x pat conTag sumWith) = error "hsPatToPat: SumPat not yet implemented"
-- hsPatToPat (GHC.SumPat _x pat conTag sumWith) = TH.SumP (hsPatToPat (unLoc pat)) conTag sumWith
hsPatToPat (GHC.ConPat _x con details) = case details of 
  GHC.PrefixCon tyArgs args ->
    TH.ConP
      (rdrNameToName (unLoc con))
      (map hsConPatTyArgToType tyArgs) 
      (map (hsPatToPat . unLoc) args)
  GHC.InfixCon lhs rhs ->
    TH.InfixP
      (hsPatToPat (unLoc lhs))
      (rdrNameToName (unLoc con)) 
      (hsPatToPat (unLoc rhs))
  GHC.RecCon r ->
    TH.RecP
      (rdrNameToName (unLoc con))
      (error "RecP: NYI") -- (_ r) 
hsPatToPat (GHC.ViewPat _x lhs rhs) = TH.ViewP (hsExprToExp (unLoc lhs)) (hsPatToPat (unLoc rhs))
hsPatToPat (GHC.SplicePat _x _spl) = error "hsPatToPat: SplicePat not yet implemented"
hsPatToPat (GHC.LitPat _x lit) = TH.LitP (hsLitToLit lit)
hsPatToPat (GHC.NPat _x _hsOverLit _optStxExpr _stxExpr) = error "hsPatToPat: n pattern not supportedf"
hsPatToPat (GHC.NPlusKPat _x _idt _hsOverLit _optStxExpr _stxExpr _v) = error "hsPatToPat: n plus k pattern not supported"
hsPatToPat (GHC.SigPat _x pat sig) = TH.SigP (hsPatToPat (unLoc pat)) (hsPatSigTypeToType sig)
hsPatToPat (GHC.EmbTyPat _x tyPat) = TH.TypeP (hsTyPatToType tyPat)
hsPatToPat (GHC.InvisPat _x tyPat) = TH.InvisP (hsTyPatToType tyPat)

--- HsConPatTyArg --------------------------------------------------------------

hsConPatTyArgToType :: GHC.HsConPatTyArg GHC.GhcPs -> TH.Type
hsConPatTyArgToType (GHC.HsConPatTyArg _x tyPat) = hsTyPatToType tyPat

--- HsRecFields ----------------------------------------------------------------

hsRecFieldsToFieldPat :: GHC.HsRecFields GHC.GhcPs (GHC.Pat GHC.GhcPs) -> [TH.FieldPat]
hsRecFieldsToFieldPat = map (hsFieldBindToFieldPat . unLoc) . GHC.rec_flds

--- HsFieldBind ----------------------------------------------------------------

hsFieldBindToFieldPat :: GHC.HsFieldBind (GHC.LFieldOcc GHC.GhcPs) (GHC.Pat GHC.GhcPs) -> TH.FieldPat
hsFieldBindToFieldPat hsFieldBind = (fieldIdt, fieldPat)
  where 
    fieldIdt :: TH.Name
    fieldIdt = rdrNameToName (unLoc (GHC.foLabel (unLoc (GHC.hfbLHS hsFieldBind))))

    fieldPat :: TH.Pat
    fieldPat = hsPatToPat (GHC.hfbRHS hsFieldBind)

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

--- HsSigType ------------------------------------------------------------------

hsSigTypeToType :: GHC.HsSigType GHC.GhcPs -> TH.Type
hsSigTypeToType = hsTypeToType . unLoc . GHC.sig_body

--- HsType ---------------------------------------------------------------------

hsTypeToType :: GHC.HsType GHC.GhcPs -> TH.Type
hsTypeToType hsType = case hsType of 
  GHC.HsTyVar _ promotionFlag (L _ idt) 
    | promotionFlag == GHC.IsPromoted -> TH.PromotedT (rdrNameToName idt)
    | otherwise -> TH.VarT (rdrNameToName idt)

--- RdrName --------------------------------------------------------------------

rdrNameToName :: GHC.RdrName -> TH.Name
rdrNameToName rdrName = case rdrName of
  GHC.Unqual occName -> TH.mkName (GHC.occNameString occName) 
  GHC.Exact name -> rdrNameToName (GHC.nameRdrName name)
  GHC.Orig mdl occName -> 
    TH.Name
      (TH.OccName (GHC.occNameString occName)) 
      (TH.NameG TH.VarName _ (ghcModuleNameToTHModuleName mdl))
  GHC.Qual moduleName occName -> 
    TH.Name 
      (TH.OccName (GHC.occNameString occName)) 
      (TH.NameQ (TH.ModName (GHC.moduleNameString moduleName)))
  where 
    ghcModuleNameToUnit :: GHC.Module -> TH.PkgName
    ghcModuleNameToUnit mdl = TH.PkgName (_ (GHC.moduleUnitId mdl))

    ghcModuleNameToTHModuleName :: GHC.Module -> TH.ModName
    ghcModuleNameToTHModuleName mdl = TH.ModName (GHC.moduleNameString (GHC.moduleName mdl))