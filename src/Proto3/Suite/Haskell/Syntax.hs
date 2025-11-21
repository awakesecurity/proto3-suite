{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module Proto3.Suite.Haskell.Syntax
  ( -- * HsModule
    HsModule
    -- * HsDecl
  , HsDecl
    -- * ClsInstDecl
  , ClsInstDecl
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
import "ghc-lib-parser" Language.Haskell.Syntax qualified as GHC
  ( ClsInstDecl (..)
  , HsBindLR (..)
  , HsRecFields (..)
  , HsFieldBind (..)
  , HsDecl (..)
  , HsExpr (..)
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
    (_ matches)
hsBindLR (GHC.PatBind _x lhs mult rhs) = 
  _
hsBindLR (GHC.VarBind _x idt rhs) = 
  TH.ValD
    (TH.VarP (rdrNameToName idt))
    (TH.NormalB (hsExprToExp (unLoc rhs)))
    []

--- HsExpr ---------------------------------------------------------------------

hsExprToExp :: GHC.HsExpr GHC.GhcPs -> TH.Exp
hsExprToExp (GHC.HsVar _ idt) = TH.VarE (rdrNameToName (unLoc idt))
hsExprToExp (GHC.HsLit _ lit) = TH.LitE (hsLitToLit lit)

-- HsPatSynDir -----------------------------------------------------------------

hsPatSynDirToPatSynDir :: GHC.HsPatSynDir GHC.GhcPs -> TH.PatSynDir
hsPatSynDirToPatSynDir GHC.Unidirectional = TH.Unidir
hsPatSynDirToPatSynDir GHC.ImplicitBidirectional = TH.ImplBidir
hsPatSynDirToPatSynDir (GHC.ExplicitBidirectional expr) = TH.ExplBidir (map (hsMatchToClause . unLoc) (unLoc (GHC.mg_alts expr)))

--- HsConDetails ------------------------------------------------------------

hsConPatDetailsToPatSynDetails :: GHC.HsConPatDetails GHC.GhcPs -> TH.PatSynArgs
hsConPatDetailsToPatSynDetails (GHC.PrefixCon _tyArgs args) = 
  TH.PrefixPatSyn 
    (map (rdrNameToName . unLoc) args)
hsConPatDetailsToPatSynDetails (GHC.InfixCon lhs rhs) = 
  TH.InfixPatSyn
    (_ lhs)
    (_ rhs)
hsConPatDetailsToPatSynDetails (GHC.RecCon r) 
  | isWildCard = 
    TH.RecordPatSyn (map (_ . unLoc) (GHC.rec_flds r))
  | otherwise =
    TH.RecordPatSyn (map (_ . unLoc) (GHC.rec_flds r))
  where 
    isWildCard :: Bool
    isWildCard = isJust (GHC.rec_dotdot r)

--- HsPatSynDetails ------------------------------------------------------------

hsPatSynDetailsToPatSynDetails :: GHC.HsPatSynDetails GHC.GhcPs -> TH.PatSynArgs
hsPatSynDetailsToPatSynDetails (GHC.PrefixCon _tyArgs args) = TH.PrefixPatSyn (map (rdrNameToName . unLoc) args)
hsPatSynDetailsToPatSynDetails (GHC.InfixCon lhs rhs) = TH.InfixPatSyn (rdrNameToName (unLoc lhs)) (rdrNameToName (unLoc rhs))
hsPatSynDetailsToPatSynDetails (GHC.RecCon r) = TH.RecordPatSyn (map (rdrNameToName . unLoc . GHC.recordPatSynPatVar) r)

--- GRHSs -----------------------------------------------------------------------

hsMatchToClause :: GHC.Match GHC.GhcPs (GenLocated GHC.SrcSpanAnnA (GHC.HsExpr GHC.GhcPs)) -> TH.Clause
hsMatchToClause = _


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
      (map _ tyArgs) 
      (map (hsPatToPat . unLoc) args)
  GHC.InfixCon lhs rhs ->
    TH.InfixP
      (hsPatToPat (unLoc lhs))
      (rdrNameToName (unLoc con)) 
      (hsPatToPat (unLoc rhs))
  GHC.RecCon r ->
    TH.RecP
      (rdrNameToName (unLoc con))
      (hsRecFieldsToFieldPat (fmap unLoc r))
hsPatToPat (GHC.ViewPat _x lhs rhs) = TH.ViewP (hsExprToExp (unLoc lhs)) (hsPatToPat (unLoc rhs))
hsPatToPat (GHC.SplicePat _x _spl) = error "hsPatToPat: SplicePat not yet implemented"
hsPatToPat (GHC.LitPat _x lit) = TH.LitP (hsLitToLit lit)
hsPatToPat (GHC.NPat _x hsOverLit optStxExpr stxExpr) = error "hsPatToPat: n pattern not supportedf"
hsPatToPat (GHC.NPlusKPat _x idt hsOverLit optStxExpr stxExpr x) = error "hsPatToPat: n plus k pattern not supported"
hsPatToPat (GHC.SigPat _x pat sig) = TH.SigP (hsPatToPat (unLoc pat)) (hsPatSigTypeToType sig)
hsPatToPat (GHC.EmbTyPat _x tyPat) = TH.TypeP (hsTyPatToType tyPat)
hsPatToPat (GHC.InvisPat _x tyPat) = TH.InvisP (hsTyPatToType tyPat)

--- HsRecFields ----------------------------------------------------------------

hsRecFieldsToFieldPat :: GHC.HsRecFields GHC.GhcPs (GHC.Pat GHC.GhcPs) -> [TH.FieldPat]
hsRecFieldsToFieldPat hsRecFields = map (conv . unLoc) (GHC.rec_flds hsRecFields)

conv :: GHC.HsFieldBind (GenLocated GHC.SrcSpanAnnA (GHC.FieldOcc GHC.GhcPs)) (GHC.Pat GHC.GhcPs) -> TH.FieldPat
conv hsFieldBind = (fieldIdt, fieldPat)
  where 
    fieldIdt :: TH.Name
    fieldIdt = rdrNameToName (unLoc (GHC.foLabel (unLoc (GHC.hfbLHS hsFieldBind))))

    fieldPat :: TH.Pat
    fieldPat = hsPatToPat (GHC.hfbRHS hsFieldBind)

--- HsFieldBind ----------------------------------------------------------------

-- hsFieldBindToFieldPat :: LHsFieldBind -> TH.FieldPat
-- hsFieldBindToFieldPat GHC.HsFieldBind {..} = (fieldIdt, fieldPat)
--   where 
--     fieldIdt :: TH.Name
--     fieldIdt = _ 

--     fieldPat :: TH.Pat
--     fieldPat = _ 

--- HsConDetails ---------------------------------------------------------------

hsConDetailsToPat :: GHC.HsConPatDetails GHC.GhcPs -> TH.Pat
hsConDetailsToPat (GHC.PrefixCon _tys args) = TH.TupP (map (hsPatToPat . unLoc) args)

--- HsPatSigType ---------------------------------------------------------------

hsPatSigTypeToType :: GHC.HsPatSigType GHC.GhcPs -> TH.Type
hsPatSigTypeToType (GHC.HsPS _x body) = hsTypeToType (unLoc body)

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
  GHC.Unqual occName -> 
    TH.mkName occNameStr
  GHC.Exact name -> 
    TH.mkName occNameStr
  GHC.Qual moduleName occName -> 
    TH.Name (TH.OccName occNameStr) (TH.NameQ (TH.ModName (GHC.moduleNameString moduleName)))
  where 
    occNameStr :: String
    occNameStr = GHC.occNameString (GHC.rdrNameOcc rdrName)