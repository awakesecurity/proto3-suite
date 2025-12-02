{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskellQuotes      #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Proto3.Suite.Haskell.Syntax
  ( -- * HsDecl
    hsDeclToDec
    -- * InstDecl
  , instDeclToDec
    -- * ClsInstDecl
  , clsInstDeclToDec
    -- * MatchGroup
  , matchGroupToClause  
    -- * Match
  , matchToClause
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
  , toName
  ) where

import Data.Char (isUpper)
import Data.ByteString.Char8 qualified as ByteString
import Data.List qualified as List

import "template-haskell" Language.Haskell.TH.Syntax qualified as TH

import "ghc-lib-parser" GHC.Data.FastString qualified as FastString
import "ghc-lib-parser" GHC.Types.Basic qualified as GHC (OverlapMode (..))
import "ghc-lib-parser" GHC.Types.Name.Reader qualified as GHC 
  ( RdrName (..)
  , nameRdrName
  )
import "ghc-lib-parser" GHC.Types.Name.Occurrence qualified as GHC 
  ( occNameString
  )
import GHC.Types.SourceText qualified as GHC 
  ( IntegralLit (..)
  , rationalFromFractionalLit
  )
import "ghc-lib-parser" GHC.Types.SrcLoc qualified as GHC (GenLocated (..))
import "ghc-lib-parser" GHC.Hs.Decls qualified as GHC 
  ( DerivClauseTys (..)
  , InstDecl (..)
  , XViaStrategyPs (..)
  )
import "ghc-lib-parser" GHC.Hs.Type qualified as GHC (SrcStrictness (..), SrcUnpackedness (..))
import "ghc-lib-parser" GHC.Types.SrcLoc qualified as GHC (unLoc)
import "ghc-lib-parser" GHC.Types.Var qualified as GHC (Specificity (..))
import "ghc-lib-parser" GHC.Utils.Outputable qualified as GHC 
  ( Outputable (..)
  , runSDoc
  , defaultSDocContext
  )
import "ghc-lib-parser" Language.Haskell.Syntax.Binds qualified as GHC
  ( HsLocalBindsLR (..)
  , HsValBindsLR (..)
  , HsIPBinds (..)
  , PatSynBind (..)
  , Sig (..)
  )
import "ghc-lib-parser" Language.Haskell.Syntax.Decls qualified as GHC
  ( DataDefnCons (..)
  , DataFamInstDecl (..)
  , ConDecl (..)
  , HsConDeclH98Details
  , HsDataDefn (..)
  , TyFamInstDecl (..)
  )
import "ghc-lib-parser" Language.Haskell.Syntax.Type qualified as GHC
  ( ConDeclField (..)
  , FieldOcc (..)
  , HsForAllTelescope (..)
  , HsTyVarBndr (..)
  , hsScaledThing
  )
import "ghc-lib-parser" Language.Haskell.Syntax.Basic qualified as GHC
  ( Boxity (..)
  , FieldLabelString (..)
  )
import "ghc-lib-parser" Language.Haskell.Syntax.Expr qualified as GHC
  ( DotFieldOcc (..)
  , FieldLabelStrings (..)
  , GRHS (..)
  , GRHSs (..)
  , HsDoFlavour (..)
  , HsTupArg (..)
  , LHsRecUpdFields (..)
  , StmtLR (..)
  )
import "ghc-lib-parser" Language.Haskell.Syntax qualified as GHC
  ( ClsInstDecl (..)
  , ModuleName (..)
  , HsDerivingClause (..)
  , AmbiguousFieldOcc (..)
  , HsOverLit (..)
  , OverLitVal (..)
  , HsOuterTyVarBndrs (..)
  , HsBndrVis (..)
  , LHsType
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
  , HsArg (..)
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
  , FamEqn (..)
  , Match (..)
  , MatchGroup (..)
  , PromotionFlag (..)
  , Pat (..)
  , moduleNameString
  )

#if MIN_VERSION_ghc_lib_parser(9,6,0)
import "ghc-lib-parser" GHC.Hs.Extension qualified as GHC
  ( GhcPs 
  )
#endif

--- 


matchGroupFirstBody :: 
  GHC.MatchGroup GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> 
  GHC.GRHS GHC.GhcPs (GHC.LHsExpr GHC.GhcPs)
matchGroupFirstBody matchGroup = case List.uncons (GHC.unLoc (GHC.mg_alts matchGroup)) of 
  Nothing -> error "impossible"
  Just (x, _) -> case List.uncons (GHC.grhssGRHSs (GHC.m_grhss (GHC.unLoc x))) of 
    Nothing -> error "impossible"
    Just (y, _) -> GHC.unLoc y

--- TyClDecl -------------------------------------------------------------------

hsTyClDeclToDec :: GHC.TyClDecl GHC.GhcPs -> TH.Dec
hsTyClDeclToDec (GHC.DataDecl _x idt tyVars _fixity defn) = case GHC.dd_cons defn of 
  GHC.DataTypeCons _ cons -> 
    TH.DataD
      (maybe [] (map (hsTypeToType . GHC.unLoc) . GHC.unLoc) (GHC.dd_ctxt defn))
      (toName (GHC.unLoc idt))
      (map (hsTyVarBndrToTyVarBndr . GHC.unLoc) (GHC.hsq_explicit tyVars)) -- [TyVarBndr]
      (fmap (hsTypeToType . GHC.unLoc) (GHC.dd_kindSig defn))
      (map (conDeclToDec . GHC.unLoc) cons)
      (foldr ((:) . hsDerivingClauseToDerivClause . GHC.unLoc) [] (GHC.dd_derivs defn))
  GHC.NewTypeCon con ->
    TH.NewtypeD
      (maybe [] (map (hsTypeToType . GHC.unLoc) . GHC.unLoc) (GHC.dd_ctxt defn))
      (toName (GHC.unLoc idt))
      (map (hsTyVarBndrToTyVarBndr . GHC.unLoc) (GHC.hsq_explicit tyVars)) -- [TyVarBndr]
      (fmap (hsTypeToType . GHC.unLoc) (GHC.dd_kindSig defn))
      (conDeclToDec (GHC.unLoc con))
      (foldr ((:) . hsDerivingClauseToDerivClause . GHC.unLoc) [] (GHC.dd_derivs defn))
hsTyClDeclToDec (GHC.ClassDecl _x ctxt idt tyVars _fixity fds sigs meths _ats _atds _docs) =
  TH.ClassD
    (maybe [] (map (hsTypeToType . GHC.unLoc) . GHC.unLoc) ctxt)
    (toName (GHC.unLoc idt))
    (map (hsTyVarBndrToTyVarBndr . GHC.unLoc) (GHC.hsq_explicit tyVars)) -- [TyVarBndr]
    (map (hsFunDepToFunDep . GHC.unLoc) fds)
    (mconcat 
      [ foldr ((++) . sigToDec . GHC.unLoc) [] sigs
      , foldr ((:) . hsBindLRToDec . GHC.unLoc) [] meths
      ]
    ) -- [Dec]
hsTyClDeclToDec _other = 
  error "hsTyClDeclToDec: only ClassDecl is supported so far"

  
hsDerivingClauseToDerivClause :: GHC.HsDerivingClause GHC.GhcPs -> TH.DerivClause
hsDerivingClauseToDerivClause (GHC.HsDerivingClause _x strategy tys) = 
  TH.DerivClause 
    (fmap (derivStrategyToDerivStrategy . GHC.unLoc) strategy)
    types
  where 
    types :: [TH.Type]
    types = case GHC.unLoc tys of 
      GHC.DctSingle _ t -> [hsSigTypeToType (GHC.unLoc t)]
      GHC.DctMulti _ ts -> map (hsSigTypeToType . GHC.unLoc) ts

derivStrategyToDerivStrategy :: GHC.DerivStrategy GHC.GhcPs -> TH.DerivStrategy
derivStrategyToDerivStrategy (GHC.StockStrategy _) = TH.StockStrategy
derivStrategyToDerivStrategy (GHC.NewtypeStrategy _) = TH.NewtypeStrategy
derivStrategyToDerivStrategy (GHC.AnyclassStrategy _) = TH.AnyclassStrategy
derivStrategyToDerivStrategy (GHC.ViaStrategy (GHC.XViaStrategyPs _ ty)) = TH.ViaStrategy (hsSigTypeToType (GHC.unLoc ty))

conDeclToDec :: GHC.ConDecl GHC.GhcPs -> TH.Con
conDeclToDec (GHC.ConDeclGADT _x _idts _bndrs _ctxt _args _resTy _doc) = 
  error "conDeclToDec: GADT not yet implemented"
conDeclToDec (GHC.ConDeclH98 _x idt fa _exs ctxt args _doc) 
  | fa = 
    TH.ForallC
      []
      (maybe [] (map (hsTypeToType . GHC.unLoc) . GHC.unLoc) ctxt)
      (TH.NormalC 
        (toName (GHC.unLoc idt)) 
        (hsConDetailsToBangTypes args)   
      )
  | otherwise = case args of 
    GHC.PrefixCon _ conArgs -> 
      TH.NormalC 
        (toName (GHC.unLoc idt)) 
        (map (hsTypeToBangType . GHC.unLoc . GHC.hsScaledThing) conArgs)
    GHC.RecCon r -> 
      TH.RecC 
        (toName (GHC.unLoc idt))
        (foldr ((++) . conDeclFieldToVarBangType . GHC.unLoc) [] (GHC.unLoc r))
    GHC.InfixCon a b -> 
      TH.InfixC 
        (hsTypeToBangType (GHC.unLoc (GHC.hsScaledThing a)))
        (toName (GHC.unLoc idt))
        (hsTypeToBangType (GHC.unLoc (GHC.hsScaledThing b)))
  where 

conDeclFieldToVarBangType :: GHC.ConDeclField GHC.GhcPs -> [TH.VarBangType]
conDeclFieldToVarBangType GHC.ConDeclField {..} = 
  map (makeVarBangType . GHC.unLoc . GHC.foLabel . GHC.unLoc) cd_fld_names
  where 
    makeVarBangType :: GHC.RdrName -> TH.VarBangType
    makeVarBangType idt = case hsTypeToBangType (GHC.unLoc cd_fld_type) of
      (bang, ty) -> (toName idt, bang, ty)

hsConDetailsToBangTypes :: GHC.HsConDeclH98Details GHC.GhcPs -> [TH.BangType]
hsConDetailsToBangTypes (GHC.PrefixCon _tyArgs args) = 
  map (hsTypeToBangType . GHC.unLoc . GHC.hsScaledThing) args
hsConDetailsToBangTypes (GHC.InfixCon lhs rhs) = 
  [ (hsTypeToBangType (GHC.unLoc (GHC.hsScaledThing lhs)))
  , (hsTypeToBangType (GHC.unLoc (GHC.hsScaledThing rhs)))
  ]
hsConDetailsToBangTypes (GHC.RecCon recFields) =
  foldr ((++) . map adapt . conDeclFieldToBangType . GHC.unLoc) [] (GHC.unLoc recFields)
  where 
    adapt :: TH.VarBangType -> TH.BangType
    adapt (_, bang, ty) = (bang, ty)

conDeclFieldToBangType :: GHC.ConDeclField GHC.GhcPs -> [TH.VarBangType]
conDeclFieldToBangType GHC.ConDeclField {..} = 
  map (makeBangType . toName . GHC.unLoc . GHC.foLabel . GHC.unLoc) cd_fld_names 
  where 
    makeBangType :: TH.Name -> TH.VarBangType
    makeBangType name = case hsTypeToBangType (GHC.unLoc cd_fld_type) of 
      (bang, ty) -> (name, bang, ty)

sigToDec :: GHC.Sig GHC.GhcPs -> [TH.Dec]
sigToDec sig = case sig of 
  GHC.TypeSig _x idts hsSigType -> 
    map (\idt -> TH.SigD (toName (GHC.unLoc idt)) (hsSigTypeToType (GHC.unLoc (GHC.hswc_body hsSigType)))) idts
  _other -> error "sigToDec: only TypeSig is supported so far"

matchGroupToPat :: GHC.MatchGroup GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> [TH.Pat]
matchGroupToPat (GHC.MG _x alts) = 
  foldr ((++) . hsMatchToPat . GHC.unLoc) [] (GHC.unLoc alts)
  where 
    hsMatchToPat :: GHC.Match GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> [TH.Pat]
    hsMatchToPat = map (hsPatToPat . GHC.unLoc) . GHC.m_pats 

-- matchGroupExprToClause :: 
--   GHC.MatchGroup GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> 
--   [TH.Clause]
-- matchGroupExprToClause (GHC.MG _x alts) = 
--   foldr ((++) . matchToClause . GHC.unLoc) [] (GHC.unLoc alts)

matchToClause :: 
  GHC.Match GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> 
  [TH.Clause]
matchToClause (GHC.Match _x _ pats grhs) =
  [ TH.Clause 
      (map (hsPatToPat . GHC.unLoc) pats)
      body
      locals
  ]
  where 
    locals :: [TH.Dec]
    locals = case GHC.grhssLocalBinds grhs of 
      GHC.EmptyLocalBinds _x -> []
      GHC.HsValBinds _ (GHC.ValBinds _ binds sigs) ->
        foldr ((:) . hsBindLRToDec . GHC.unLoc) [] binds 
          ++ foldr ((++) . sigToDec . GHC.unLoc) [] sigs
      GHC.HsIPBinds _ _ipBinds -> 
        error ("matchToClause: HsIPBinds (not yet implemented)")
    
    body :: TH.Body
    body = TH.GuardedB (foldr ((:) . gRHSExprToGuardedExpr . GHC.unLoc) [] (GHC.grhssGRHSs grhs))

-- gRHSsExprToBody :: GHC.GRHSs GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> TH.Body
-- gRHSsExprToBody (GHC.GRHSs _ grhss _) = TH.GuardedB guardedExps 
--   where 
--     guardedExps :: [(TH.Guard, TH.Exp)] 
--     guardedExps = foldr ((:) . gRHSExprToGuardedExpr . GHC.unLoc) [] grhss

gRHSExprToGuardedExpr :: GHC.GRHS GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> (TH.Guard, TH.Exp)
gRHSExprToGuardedExpr (GHC.GRHS _x stmts body) = (TH.PatG stmts', body')
  where 
    body' :: TH.Exp
    body' = hsExprToExp (GHC.unLoc body)

    stmts' :: [TH.Stmt]
    stmts' = map (stmtLRToStmt . GHC.unLoc) stmts

hsTyVarBndrToTyVarBndr :: GHC.HsTyVarBndr (GHC.HsBndrVis GHC.GhcPs) GHC.GhcPs -> TH.TyVarBndr TH.BndrVis
hsTyVarBndrToTyVarBndr (GHC.UserTyVar _x specificity idt) =
    TH.PlainTV 
      (toName (GHC.unLoc idt)) 
      (hsBndrVisToBndrVis specificity)
hsTyVarBndrToTyVarBndr (GHC.KindedTyVar _x specificity idt ki) =
  TH.KindedTV 
    (toName (GHC.unLoc idt)) 
    (hsBndrVisToBndrVis specificity)
    (hsTypeToType (GHC.unLoc ki))

hsBndrVisToBndrVis :: GHC.HsBndrVis GHC.GhcPs -> TH.BndrVis
hsBndrVisToBndrVis (GHC.HsBndrRequired _x) = TH.BndrReq
hsBndrVisToBndrVis (GHC.HsBndrInvisible _x) = TH.BndrInvis

hsFunDepToFunDep :: GHC.FunDep GHC.GhcPs -> TH.FunDep
hsFunDepToFunDep (GHC.FunDep _x xs ys) = 
  TH.FunDep 
    (map (toName . GHC.unLoc) xs) 
    (map (toName . GHC.unLoc) ys)

--- DerivDecl ------------------------------------------------------------------

hsDerivDeclToDec :: GHC.DerivDecl GHC.GhcPs -> TH.Dec
hsDerivDeclToDec GHC.DerivDecl{deriv_type = GHC.HsWC _x body, ..} =
  TH.StandaloneDerivD
    (fmap (derivStrategyToDerivStrategy . GHC.unLoc) deriv_strategy)
    []
    (hsSigTypeToType (GHC.unLoc body))

--- InstDecl -------------------------------------------------------------------

instDeclToDec :: GHC.InstDecl GHC.GhcPs -> TH.Dec
instDeclToDec instDecl = case instDecl of
  GHC.ClsInstD {..} -> clsInstDeclToDec cid_inst
  GHC.DataFamInstD {} ->
    -- @ 
    -- 
    -- @
    error "DataFamInstD not yet implemented"
  GHC.TyFamInstD {} ->
    -- @ 
    -- 
    -- @
    error "TyFamInstD not yet implemented"

--- ClsInstDecl ----------------------------------------------------------------

-- | TODO: docs
--
-- @ 
-- class [overlapMode] (polyTy :: 'Type') where 
--    [typeFamInsts :: [TyFamInstDecl]]
--    
--    [dataFamInsts :: [DataFamInstDecl]]
--
--    [sigs :: [Sig]]
--
--    [binds :: [HsBindLR]]
-- @
clsInstDeclToDec :: GHC.ClsInstDecl GHC.GhcPs -> TH.Dec
clsInstDeclToDec (GHC.ClsInstDecl _x polyTy binds sigs tyFamInsts dataFamInsts overlapMode) = 
  TH.InstanceD
    (overlapModeToOverlap . GHC.unLoc =<< overlapMode) 
    []
    (hsSigTypeToType (GHC.unLoc polyTy)) 
    (mconcat 
      [ map (TH.TySynInstD . dataFamInstDecl . GHC.unLoc) dataFamInsts
      , map (TH.TySynInstD . tyFamInstDeclToTySynEqn . GHC.unLoc) tyFamInsts
      , foldr ((++) . sigToDec . GHC.unLoc) [] sigs
      , map hsBindLRToDec (foldr ((:) . GHC.unLoc) [] binds)
      ])

dataFamInstDecl :: GHC.DataFamInstDecl GHC.GhcPs -> TH.TySynEqn 
dataFamInstDecl (GHC.DataFamInstDecl eqn) = dataFamEqnToTySynEqn eqn
    
dataFamEqnToTySynEqn :: GHC.FamEqn GHC.GhcPs (GHC.HsDataDefn GHC.GhcPs) -> TH.TySynEqn
dataFamEqnToTySynEqn (GHC.FamEqn _x _tyCon bndrs pats _fixity rhs) = 
  TH.TySynEqn
    (hsOuterTyVarBndrs bndrs)
    (hsArgToTypeFamilyLHS pats)
    (hsDataDefnToType rhs)

hsDataDefnToType :: GHC.HsDataDefn GHC.GhcPs -> TH.Type
hsDataDefnToType (GHC.HsDataDefn _x _ctxt _ctype _kindSig _cons _derivs) = 
  error "hsDataDefnToType: not yet implemented"

-- | TODO: docs
--
-- @ 
-- 
-- @ 
tyFamInstDeclToTySynEqn :: GHC.TyFamInstDecl GHC.GhcPs -> TH.TySynEqn
tyFamInstDeclToTySynEqn (GHC.TyFamInstDecl _x eqn) = 
  TH.TySynEqn
    (hsOuterTyVarBndrs (GHC.feqn_bndrs eqn))
    -- lhs eqn
    (hsArgToTypeFamilyLHS (GHC.feqn_pats eqn))
    -- rhs eqn
    (hsTypeToType (GHC.unLoc (GHC.feqn_rhs eqn)))

--- HsBindLR -------------------------------------------------------------------

hsArgToTypeFamilyLHS :: 
  [GHC.HsArg GHC.GhcPs (GHC.LHsType GHC.GhcPs) (GHC.LHsType GHC.GhcPs)] -> 
  TH.Type
hsArgToTypeFamilyLHS [] = error "hsArgToTypeFamilyLHS: empty args"
hsArgToTypeFamilyLHS (arg : args) = 
  foldl 
    (\acc x -> TH.AppT acc (hsArgToTypePat x)) 
    (hsArgToTypePat arg) 
    args

hsOuterTyVarBndrs :: GHC.HsOuterTyVarBndrs () GHC.GhcPs -> Maybe [TH.TyVarBndr ()]
hsOuterTyVarBndrs (GHC.HsOuterImplicit _x) = Nothing
hsOuterTyVarBndrs (GHC.HsOuterExplicit _x binders) = Just (map (hsHsTyVarBndrToTyVarBndr . GHC.unLoc) binders)

hsHsTyVarBndrToTyVarBndr :: GHC.HsTyVarBndr () GHC.GhcPs -> TH.TyVarBndr ()
hsHsTyVarBndrToTyVarBndr (GHC.UserTyVar _x () var) = 
  TH.PlainTV (toName (GHC.unLoc var)) ()
hsHsTyVarBndrToTyVarBndr (GHC.KindedTyVar _x () var ki) = 
  TH.KindedTV (toName (GHC.unLoc var)) () (hsTypeToType (GHC.unLoc ki))

hsArgToTypePat :: GHC.HsArg GHC.GhcPs (GHC.LHsType GHC.GhcPs) (GHC.LHsType GHC.GhcPs) -> TH.Type
hsArgToTypePat (GHC.HsValArg _ x) = hsTypeToType (GHC.unLoc x)
hsArgToTypePat (GHC.HsTypeArg _ x) = hsTypeToType (GHC.unLoc x)

hsBindLRToDec :: GHC.HsBindLR GHC.GhcPs GHC.GhcPs -> TH.Dec
hsBindLRToDec (GHC.FunBind _x idt matches) = 
  TH.FunD
    (toName (GHC.unLoc idt))
    (matchGroupToClause matches)
hsBindLRToDec (GHC.PatBind _x lhs _mult rhs) = 
  TH.ValD 
    (hsPatToPat (GHC.unLoc lhs))
    (TH.GuardedB (foldr ((:) . gRHSExprToGuardedExpr . GHC.unLoc) [] (GHC.grhssGRHSs rhs)))
    (bindsToDecs (GHC.grhssLocalBinds rhs))
hsBindLRToDec (GHC.VarBind _x idt rhs) = 
  TH.ValD
    (TH.VarP (toName idt))
    (TH.NormalB (hsExprToExp (GHC.unLoc rhs)))
    []
hsBindLRToDec (GHC.PatSynBind _x bind) = 
    patSynBindToDec bind

--- MatchGroup -----------------------------------------------------------------

matchGroupToClause :: GHC.MatchGroup GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> [TH.Clause]
matchGroupToClause (GHC.MG _ alts) = foldMap (matchToClause . GHC.unLoc) (GHC.unLoc alts)

--- Match ----------------------------------------------------------------------

stmtLRToStmt :: GHC.StmtLR GHC.GhcPs GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> TH.Stmt
stmtLRToStmt (GHC.BindStmt _x pat body) = 
  TH.BindS (hsPatToPat (GHC.unLoc pat)) (hsExprToExp (GHC.unLoc body))
stmtLRToStmt (GHC.BodyStmt _x body _stxl _stxr) = 
  TH.NoBindS (hsExprToExp (GHC.unLoc body))
stmtLRToStmt (GHC.LetStmt _x locals) = 
  TH.LetS (bindsToDecs locals)
stmtLRToStmt (GHC.LastStmt _x body _mb _stx) = 
  TH.NoBindS (hsExprToExp (GHC.unLoc body))
stmtLRToStmt (GHC.ParStmt _x _parStmtBlock _hsExpr _stx) = 
  error ("'ParStmt' statement in guard: not supported")
stmtLRToStmt (GHC.TransStmt _x _form _stmts _bndrs _usingExpr _byExpr _stxret _stxbind _fmapExpr) = 
  error ("'TransStmt' statement in guard: not supported")
stmtLRToStmt (GHC.RecStmt _x stmts _later_ids _rec_ids _stxbind_fn _stxret_fn _stxmfix_fn) = 
  TH.RecS (map (stmtLRToStmt . GHC.unLoc) (GHC.unLoc stmts))
stmtLRToStmt (GHC.ApplicativeStmt _x _args _stx) = 
  error ("'ApplicativeStmt' statement in guard: not supported")


hsRecFieldsToFieldExps :: GHC.HsRecFields GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> [TH.FieldExp]
hsRecFieldsToFieldExps (GHC.HsRecFields fields _) = map (hsFieldBindToFieldExp . GHC.unLoc) fields

hsFieldBindToFieldExp :: IsName a => GHC.HsFieldBind a (GHC.LHsExpr GHC.GhcPs) -> (TH.Name, TH.Exp) -- ~ TH.FieldExp
hsFieldBindToFieldExp (GHC.HsFieldBind _ lhs rhs _) = (toName lhs, hsExprToExp (GHC.unLoc rhs))

bindsToDecs :: GHC.HsLocalBindsLR GHC.GhcPs GHC.GhcPs -> [TH.Dec]
bindsToDecs (GHC.HsValBinds _x (GHC.ValBinds _x2 binds _sigs)) = 
  map hsBindLRToDec (foldr ((:) . GHC.unLoc) [] binds)
bindsToDecs (GHC.HsIPBinds _x (GHC.IPBinds _x2 _hsIPBinds)) = 
  error "hsExprToExp: HsIPBinds not yet implemented"
bindsToDecs (GHC.EmptyLocalBinds _x) = 
  []

hsDoFlavorToMaybeModName :: GHC.HsDoFlavour -> Maybe TH.ModName
hsDoFlavorToMaybeModName (GHC.DoExpr modName) = fmap moduleNameToModName modName 
hsDoFlavorToMaybeModName (GHC.MDoExpr modName) = fmap moduleNameToModName modName
hsDoFlavorToMaybeModName _ = Nothing

moduleNameToModName :: GHC.ModuleName -> TH.ModName
moduleNameToModName (GHC.ModuleName fs) = TH.ModName (FastString.unpackFS fs)

clauseToMatch :: TH.Clause -> [TH.Match]
clauseToMatch (TH.Clause pats body decs) = map (\pat -> TH.Match pat body decs) pats

-- HsPatSynDir -----------------------------------------------------------------

hsPatSynDirToPatSynDir :: GHC.HsPatSynDir GHC.GhcPs -> TH.PatSynDir
hsPatSynDirToPatSynDir GHC.Unidirectional = TH.Unidir
hsPatSynDirToPatSynDir GHC.ImplicitBidirectional = TH.ImplBidir
hsPatSynDirToPatSynDir (GHC.ExplicitBidirectional expr) = TH.ExplBidir (foldr ((++) . matchToClause . GHC.unLoc) [] (GHC.unLoc (GHC.mg_alts expr)))

--- HsPatSynDetails ------------------------------------------------------------

hsPatSynDetailsToPatSynDetails :: GHC.HsPatSynDetails GHC.GhcPs -> TH.PatSynArgs
hsPatSynDetailsToPatSynDetails (GHC.PrefixCon _tyArgs args) = TH.PrefixPatSyn (map (toName . GHC.unLoc) args)
hsPatSynDetailsToPatSynDetails (GHC.InfixCon lhs rhs) = TH.InfixPatSyn (toName (GHC.unLoc lhs)) (toName (GHC.unLoc rhs))
hsPatSynDetailsToPatSynDetails (GHC.RecCon r) = TH.RecordPatSyn (map (toName . GHC.unLoc . GHC.recordPatSynPatVar) r)

--- PatSynBind -----------------------------------------------------------------

patSynBindToDec :: GHC.PatSynBind GHC.GhcPs GHC.GhcPs -> TH.Dec
patSynBindToDec GHC.PSB {..} = 
  TH.PatSynD
    (toName (GHC.unLoc psb_id))
    (hsPatSynDetailsToPatSynDetails psb_args)
    (hsPatSynDirToPatSynDir psb_dir)
    (hsPatToPat (GHC.unLoc psb_def))

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
hsPatToPat (GHC.VarPat _x idt) = TH.VarP (toName (GHC.unLoc idt))
hsPatToPat (GHC.LazyPat _x pat) = TH.TildeP (hsPatToPat (GHC.unLoc pat)) 
hsPatToPat (GHC.AsPat _x idt pat) = TH.AsP (toName (GHC.unLoc idt)) (hsPatToPat (GHC.unLoc pat))
hsPatToPat (GHC.ParPat _x pat) = TH.ParensP (hsPatToPat (GHC.unLoc pat))
hsPatToPat (GHC.BangPat _x pat) = TH.BangP (hsPatToPat (GHC.unLoc pat))
hsPatToPat (GHC.ListPat _x pats) = TH.ListP (map (hsPatToPat . GHC.unLoc) pats)
hsPatToPat (GHC.TuplePat _x pats _boxity) = TH.TupP (map (hsPatToPat . GHC.unLoc) pats)
-- hsPatToPat (GHC.OrPat _x pats) = _
hsPatToPat (GHC.SumPat _x _pat _conTag _sumWith) = error "hsPatToPat: SumPat not yet implemented"
-- hsPatToPat (GHC.SumPat _x pat conTag sumWith) = TH.SumP (hsPatToPat (GHC.unLoc pat)) conTag sumWith
hsPatToPat (GHC.ConPat _x conPat details) = case details of 
  GHC.PrefixCon tyArgs args ->
    TH.ConP
      (toName conPat)
      (map hsConPatTyArgToType tyArgs) 
      (map (hsPatToPat . GHC.unLoc) args)
  GHC.InfixCon lhs rhs ->
    TH.InfixP
      (hsPatToPat (GHC.unLoc lhs))
      (toName conPat)
      (hsPatToPat (GHC.unLoc rhs))
  GHC.RecCon r ->
    TH.RecP
      (toName conPat)
      (hsRecFieldsToFieldPat r) 
hsPatToPat (GHC.ViewPat _x lhs rhs) = TH.ViewP (hsExprToExp (GHC.unLoc lhs)) (hsPatToPat (GHC.unLoc rhs))
hsPatToPat (GHC.SplicePat _x _spl) = error "hsPatToPat: SplicePat not yet implemented"
hsPatToPat (GHC.LitPat _x lit) = TH.LitP (hsLitToLit lit)
hsPatToPat (GHC.NPat _x hsOverLit _optStxExpr _stxExpr) = TH.LitP (hsOverLitToLit (GHC.unLoc hsOverLit))
hsPatToPat (GHC.NPlusKPat _x _idt _hsOverLit _optStxExpr _stxExpr _v) = error "hsPatToPat: n plus k pattern not supported"
hsPatToPat (GHC.SigPat _x pat sig) = TH.SigP (hsPatToPat (GHC.unLoc pat)) (hsPatSigTypeToType sig)
hsPatToPat (GHC.EmbTyPat _x tyPat) = TH.TypeP (hsTyPatToType tyPat)
hsPatToPat (GHC.InvisPat _x tyPat) = TH.InvisP (hsTyPatToType tyPat)

hsOverLitToLit :: GHC.HsOverLit GHC.GhcPs -> TH.Lit
hsOverLitToLit = overLitValToLit . GHC.ol_val

overLitValToLit :: GHC.OverLitVal -> TH.Lit
overLitValToLit (GHC.HsIntegral n) = TH.IntegerL (GHC.il_value n)
overLitValToLit (GHC.HsFractional q) = TH.RationalL (GHC.rationalFromFractionalLit q)
overLitValToLit (GHC.HsIsString _ s) = TH.StringL (FastString.unpackFS s)

hsRecUpdFields :: GHC.LHsRecUpdFields GHC.GhcPs -> [TH.FieldExp]
hsRecUpdFields (GHC.RegularRecUpdFields _ fields) = map (hsFieldBindToFieldExp . GHC.unLoc) fields
hsRecUpdFields (GHC.OverloadedRecUpdFields _ fields) = map (hsFieldBindToFieldExp . GHC.unLoc) fields

--- HsConPatTyArg --------------------------------------------------------------

hsConPatTyArgToType :: GHC.HsConPatTyArg GHC.GhcPs -> TH.Type
hsConPatTyArgToType (GHC.HsConPatTyArg _x tyPat) = hsTyPatToType tyPat

--- HsRecFields ----------------------------------------------------------------

hsRecFieldsToFieldPat :: GHC.HsRecFields GHC.GhcPs (GHC.LPat GHC.GhcPs) -> [TH.FieldPat]
hsRecFieldsToFieldPat = map (hsFieldBindToFieldPat . GHC.unLoc) . GHC.rec_flds 

--- HsFieldBind ----------------------------------------------------------------

hsFieldBindToFieldPat :: GHC.HsFieldBind (GHC.LFieldOcc GHC.GhcPs) (GHC.LPat GHC.GhcPs) -> TH.FieldPat
hsFieldBindToFieldPat hsFieldBind = (fieldIdt, fieldPat)
  where 
    fieldIdt :: TH.Name
    fieldIdt = toName (GHC.unLoc (GHC.foLabel (GHC.unLoc (GHC.hfbLHS hsFieldBind))))

    fieldPat :: TH.Pat
    fieldPat = hsPatToPat (GHC.unLoc (GHC.hfbRHS hsFieldBind))

--- HsPatSigType ---------------------------------------------------------------

hsPatSigTypeToType :: GHC.HsPatSigType GHC.GhcPs -> TH.Type
hsPatSigTypeToType = hsTypeToType . GHC.unLoc . GHC.hsps_body 

--- HsTyPat --------------------------------------------------------------------

hsTyPatToType :: GHC.HsTyPat GHC.GhcPs -> TH.Type
hsTyPatToType (GHC.HsTP _x body) = hsTypeToType (GHC.unLoc body)

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
hsSigTypeToType = hsTypeToType . GHC.unLoc . GHC.sig_body

--- HsType ---------------------------------------------------------------------

-- hsTypeToVarBangType :: GHC.HsType GHC.GhcPs -> TH.VarBangType
-- hsTypeToVarBangType hsType = case hsType of

-- hsTypeToVarType :: GHC.HsType GHC.GhcPs -> (TH.Name, TH.Type)
-- hsTypeToVarType (GHC.HsRecTy _x fields) = (_, _)

hsTypeToBangType :: GHC.HsType GHC.GhcPs -> TH.BangType
hsTypeToBangType hsType = case hsType of 
  GHC.HsBangTy _x bang ty -> (hsSrcBangToBang bang, hsTypeToType (GHC.unLoc ty))
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


hsTyLitToType :: GHC.HsTyLit GHC.GhcPs -> TH.TyLit
hsTyLitToType (GHC.HsNumTy _x n) = TH.NumTyLit n
hsTyLitToType (GHC.HsStrTy _x s) = TH.StrTyLit (FastString.unpackFS s)
hsTyLitToType (GHC.HsCharTy _x c) = TH.CharTyLit c

hsTyVarBndrToTyVarBndrWith :: 
  (flag -> flag') -> 
  GHC.HsTyVarBndr flag GHC.GhcPs -> 
  TH.TyVarBndr flag'
hsTyVarBndrToTyVarBndrWith  f (GHC.KindedTyVar _x specificity binder kind) =
  TH.KindedTV (toName (GHC.unLoc binder)) (f specificity) (hsTypeToType (GHC.unLoc kind))
hsTyVarBndrToTyVarBndrWith f (GHC.UserTyVar _x specificity binder) =
  TH.PlainTV (toName (GHC.unLoc binder)) (f specificity)

hsSpecifityToSpecificity :: GHC.Specificity -> TH.Specificity
hsSpecifityToSpecificity GHC.SpecifiedSpec = TH.SpecifiedSpec
hsSpecifityToSpecificity GHC.InferredSpec = TH.InferredSpec

--- HsDecl ---------------------------------------------------------------------

hsDeclToDec :: GHC.HsDecl GHC.GhcPs -> [TH.Dec]
hsDeclToDec (GHC.InstD _x instDecl) = pure (instDeclToDec instDecl)
hsDeclToDec (GHC.DerivD _x deriveDecl) = pure (hsDerivDeclToDec deriveDecl)
hsDeclToDec (GHC.TyClD _x tyClDecl) = pure (hsTyClDeclToDec tyClDecl )
hsDeclToDec (GHC.SigD _x sig) = sigToDec sig
hsDeclToDec (GHC.ValD _x bind) = pure (hsBindLRToDec bind)
hsDeclToDec _other = []

--- HsType ---------------------------------------------------------------------

hsTypeToType :: GHC.HsType GHC.GhcPs -> TH.Type
hsTypeToType (GHC.HsListTy _x a) = TH.ListT `TH.AppT` hsTypeToType (GHC.unLoc a)
hsTypeToType (GHC.HsStarTy _ _p) = TH.StarT
hsTypeToType (GHC.HsTyLit _ tyLit) = TH.LitT (hsTyLitToType tyLit)
hsTypeToType (GHC.HsWildCardTy _) = TH.WildCardT
hsTypeToType (GHC.HsAppTy _x a b) = hsTypeToType (GHC.unLoc a) `TH.AppT` hsTypeToType (GHC.unLoc b)
hsTypeToType (GHC.HsAppKindTy _x a b) = hsTypeToType (GHC.unLoc a) `TH.AppKindT` hsTypeToType (GHC.unLoc b)
hsTypeToType (GHC.HsTyVar _x promotionFlag idt)
  | promotionFlag == GHC.IsPromoted = TH.PromotedT (toName (GHC.unLoc idt))
  | otherwise = case TH.nameBase name of 
    "" -> error "hsTypeToType: empty name"
    c : _ 
      | isUpper c -> TH.ConT name 
      | otherwise -> TH.VarT name 
    where 
      name :: TH.Name 
      name = toName (GHC.unLoc idt)
hsTypeToType (GHC.HsForAllTy _x telescope body) = case telescope of
  GHC.HsForAllVis {..} -> 
    TH.ForallT 
      (map (hsTyVarBndrToTyVarBndrWith (const TH.SpecifiedSpec) . GHC.unLoc) hsf_vis_bndrs)
      []
      (hsTypeToType (GHC.unLoc body))
  GHC.HsForAllInvis {..} -> 
    TH.ForallT 
      (map (hsTyVarBndrToTyVarBndrWith hsSpecifityToSpecificity . GHC.unLoc) hsf_invis_bndrs) 
      []
      (hsTypeToType (GHC.unLoc body))
hsTypeToType (GHC.HsQualTy _x ctxt body) =
  TH.ForallT 
    []
    (map (hsTypeToType . GHC.unLoc) (GHC.unLoc ctxt))
    (hsTypeToType (GHC.unLoc body))
hsTypeToType (GHC.HsFunTy _x hsArrow a b) = case hsArrow of 
  GHC.HsUnrestrictedArrow _x -> 
    TH.ArrowT 
      `TH.AppT` hsTypeToType (GHC.unLoc a)
      `TH.AppT` hsTypeToType (GHC.unLoc b)
  GHC.HsLinearArrow _x -> 
    TH.MulArrowT 
      `TH.AppT` TH.LitT (TH.NumTyLit 1)
      `TH.AppT` hsTypeToType (GHC.unLoc a)
      `TH.AppT` hsTypeToType (GHC.unLoc b)
  GHC.HsExplicitMult _x mult ->
    TH.MulArrowT 
      `TH.AppT` hsTypeToType (GHC.unLoc mult)
      `TH.AppT` hsTypeToType (GHC.unLoc a)
      `TH.AppT` hsTypeToType (GHC.unLoc b)
hsTypeToType (GHC.HsTupleTy _x boxity args) =
  let tup :: TH.Type
      tup = case boxity of 
        GHC.HsUnboxedTuple -> TH.UnboxedTupleT (length args)
        GHC.HsBoxedOrConstraintTuple -> TH.TupleT (length args)
    in foldl (\x y -> x `TH.AppT` hsTypeToType (GHC.unLoc y)) tup args
hsTypeToType (GHC.HsOpTy _x promotion lhsTy opIdt rhsTy) = case promotion of
  GHC.IsPromoted ->
    TH.PromotedInfixT 
      (hsTypeToType (GHC.unLoc lhsTy)) 
      (toName (GHC.unLoc opIdt)) 
      (hsTypeToType (GHC.unLoc rhsTy))
  GHC.NotPromoted ->
    TH.InfixT 
      (hsTypeToType (GHC.unLoc lhsTy)) 
      (toName (GHC.unLoc opIdt)) 
      (hsTypeToType (GHC.unLoc rhsTy))
hsTypeToType (GHC.HsParTy _x ty) =
  TH.ParensT (hsTypeToType (GHC.unLoc ty))
hsTypeToType (GHC.HsIParamTy _x _hsIPName _ty) =
  error "hsTypeToType: HsIParamTy impossible"
hsTypeToType (GHC.HsKindSig _x ty ki) =
  TH.SigT (hsTypeToType (GHC.unLoc ty)) (hsTypeToType (GHC.unLoc ki))
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
hsTypeToType (GHC.XHsType _x) =
  error "hsTypeToType: XHsType impossible"
hsTypeToType (GHC.HsSumTy _x _args) =
  error "hsTypeToType: SumTy not yet implemented"

--- HsExpr ---------------------------------------------------------------------
isConName :: TH.Name -> Bool
isConName name = case TH.nameBase name of 
  [] -> False
  (c : _) -> isUpper c || c == ':'

hsExprToExp :: GHC.HsExpr GHC.GhcPs -> TH.Exp
hsExprToExp (GHC.HsVar _ idt) 
  | isConName name = TH.ConE name 
  | otherwise = TH.VarE name
  where 
    name :: TH.Name
    name = toName (GHC.unLoc idt)
hsExprToExp (GHC.HsUnboundVar _ idt) 
  | isConName name = TH.ConE name
  | otherwise = TH.VarE name 
  where 
    name :: TH.Name
    name = toName idt
hsExprToExp (GHC.HsLit _ lit) = TH.LitE (hsLitToLit lit)
hsExprToExp (GHC.HsApp _ func arg) = 
  TH.AppE 
    (hsExprToExp (GHC.unLoc func)) 
    (hsExprToExp (GHC.unLoc arg))
hsExprToExp (GHC.HsPar _x expr) = 
  TH.ParensE (hsExprToExp (GHC.unLoc expr))
hsExprToExp (GHC.HsOverLit _x overLit) =
  TH.LitE (hsOverLitToLit overLit)
hsExprToExp (GHC.HsRecSel _x fldOcc) =
  TH.VarE (toName (GHC.unLoc (GHC.foLabel fldOcc)))
hsExprToExp (GHC.HsAppType _x expr ty) =
  TH.AppTypeE 
    (hsExprToExp (GHC.unLoc expr)) 
    (hsTypeToType (GHC.unLoc (GHC.hswc_body ty)))
hsExprToExp (GHC.OpApp _ lhs op rhs) = 
  TH.InfixE 
    (Just (hsExprToExp (GHC.unLoc lhs))) 
    (hsExprToExp (GHC.unLoc op))
    (Just (hsExprToExp (GHC.unLoc rhs)))
hsExprToExp (GHC.NegApp _x expr _) = 
  TH.UInfixE 
    (TH.LitE (TH.IntegerL 0)) 
    (TH.VarE (TH.mkName "-")) 
    (hsExprToExp (GHC.unLoc expr))
hsExprToExp (GHC.SectionL _x expr op) =
  TH.InfixE 
    (Just (hsExprToExp (GHC.unLoc expr)))
    (hsExprToExp (GHC.unLoc op)) 
    Nothing
hsExprToExp (GHC.SectionR _x expr op) =
  TH.InfixE 
    Nothing
    (hsExprToExp (GHC.unLoc op)) 
    (Just (hsExprToExp (GHC.unLoc expr)))
hsExprToExp (GHC.HsLet _x locals expr) =
  TH.LetE 
    (bindsToDecs locals)
    (hsExprToExp (GHC.unLoc expr))
hsExprToExp (GHC.HsCase _x expr matchGroup) = 
  TH.CaseE 
    (hsExprToExp (GHC.unLoc expr))
    (foldr ((++) . clauseToMatch) [] (matchGroupToClause matchGroup))
hsExprToExp (GHC.ExplicitList _x exprs) =
  TH.ListE (map (hsExprToExp . GHC.unLoc) exprs)
hsExprToExp (GHC.HsLam _x _ matchGroup) =
  case matchGroupFirstBody matchGroup of 
    GHC.GRHS _x [] bodyExpr -> 
      TH.LamE 
        (matchGroupToPat matchGroup)
        (hsExprToExp (GHC.unLoc bodyExpr))
    _other -> 
      error "hsExprToExp: only single-clause lambdas without guards are supported so far"
hsExprToExp (GHC.HsDo _x modName stmts) =
  TH.DoE 
    (hsDoFlavorToMaybeModName modName) 
    (map (stmtLRToStmt . GHC.unLoc) 
    (GHC.unLoc stmts))
hsExprToExp (GHC.RecordCon _x con fields) = 
  TH.RecConE 
    (toName (GHC.unLoc con))
    (hsRecFieldsToFieldExps fields)
hsExprToExp (GHC.RecordUpd _x expr fields) = 
  TH.RecUpdE 
    (hsExprToExp (GHC.unLoc expr))
    (hsRecUpdFields fields)
hsExprToExp (GHC.ExplicitTuple _ args boxity)
  | boxity == GHC.Boxed = 
      TH.TupE (map hsTupArgToExp args)
  | boxity == GHC.Unboxed = 
      TH.UnboxedTupE (map hsTupArgToExp args)
  where 
    hsTupArgToExp :: GHC.HsTupArg GHC.GhcPs -> Maybe TH.Exp
    hsTupArgToExp (GHC.Present _x expr) = Just (hsExprToExp (GHC.unLoc expr))
    hsTupArgToExp (GHC.Missing _x) = Nothing
hsExprToExp _other = 
  error ("hsExprToExp: unsupported HsExpr: " ++ show (GHC.runSDoc (GHC.ppr _other) GHC.defaultSDocContext))

--- IsName ---------------------------------------------------------------------

class IsName a where 
  toName :: a -> TH.Name

instance IsName FastString.FastString where 
  toName = TH.mkName . FastString.unpackFS

instance IsName GHC.RdrName where
  toName (GHC.Exact name) = toName (GHC.nameRdrName name)
  toName (GHC.Unqual occName) = TH.mkName (GHC.occNameString occName) 
  toName (GHC.Orig _mdl occName) =
    error ("toName: Orig not yet implemented" ++ GHC.occNameString occName)
  toName (GHC.Qual moduleName occName) 
    | null modNameStr = TH.mkName occNameStr
    | otherwise =
      TH.Name 
        (TH.OccName (GHC.occNameString occName)) 
        (TH.NameQ (TH.ModName modNameStr))
    where 
      occNameStr :: String
      occNameStr = GHC.occNameString occName

      modNameStr :: String
      modNameStr = GHC.moduleNameString moduleName

instance IsName (GHC.FieldOcc GHC.GhcPs) where 
  toName = toName . GHC.unLoc . GHC.foLabel

instance IsName GHC.FieldLabelString where
  toName = toName . GHC.field_label

instance IsName (GHC.DotFieldOcc GHC.GhcPs) where
  toName = toName . GHC.dfoLabel

instance IsName (GHC.FieldLabelStrings GHC.GhcPs) where
  toName (GHC.FieldLabelStrings occs) = case List.uncons occs of 
    Nothing -> error "impossible" 
    Just (o, _) -> toName o

instance IsName (GHC.AmbiguousFieldOcc GHC.GhcPs) where
  toName (GHC.Unambiguous _ idt) = toName idt
  toName (GHC.Ambiguous _ idt) = toName idt

instance IsName a => IsName (GHC.GenLocated s a) where 
  toName = toName . GHC.unLoc
