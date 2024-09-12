{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DerivingStrategies        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE ImplicitParams            #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiWayIf                #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE ViewPatterns              #-}

{-| This module provides functions to generate Haskell declarations for protobuf
    messages
-}

module Proto3.Suite.DotProto.Generate
  ( CompileError(..)
  , StringType(..)
  , RecordStyle(..)
  , parseStringType
  , TypeContext
  , CompileArgs(..)
  , compileDotProtoFile
  , compileDotProtoFileOrDie
  , renameProtoFile
  , hsModuleForDotProto
  , renderHsModuleForDotProto
  , readDotProtoWithContext
  ) where

import           Control.Applicative
import           Control.Lens                   ((&), ix, over, has, filtered)
import           Control.Monad                  (when)
import           Control.Monad.Except           (MonadError(..), runExceptT)
import           Control.Monad.IO.Class         (MonadIO(..))
import           Control.Monad.Writer           (WriterT, runWriterT, tell)
import           Data.Char
import           Data.Coerce
import           Data.Either                    (partitionEithers)
import           Data.Foldable                  (fold)
import           Data.Function                  (on)
import           Data.Functor                   ((<&>))
import           Data.List                      (find, intercalate, nub, sort, sortBy, stripPrefix)
import qualified Data.List.NonEmpty             as NE
import           Data.List.Split                (splitOn)
import           Data.List.NonEmpty             (NonEmpty (..))
import qualified Data.Map                       as M
import           Data.Maybe
import           Data.Monoid
import           Data.Ord                       (comparing)
import qualified Data.Set                       as S
import           Data.String                    (fromString)
import qualified Data.Text                      as T
import qualified GHC.Data.FastString            as GHC
import qualified GHC.Data.StringBuffer          as GHC
import qualified GHC.Hs                         as GHC
import qualified GHC.Types.Name                 as GHC
import           GHC.Types.Name.Occurrence      (dataName, tcName, varName)
import qualified GHC.Types.Name.Reader          as GHC
import qualified GHC.Types.SrcLoc               as GHC
import qualified GHC.Utils.Outputable           as GHC
import qualified NeatInterpolation              as Neat
import           Prelude                        hiding (FilePath)
import           Proto3.Suite.DotProto
import           Proto3.Suite.DotProto.AST.Lens
import qualified Proto3.Suite.DotProto.Generate.LargeRecord as LargeRecord
import qualified Proto3.Suite.DotProto.Generate.Record as RegularRecord
import           Proto3.Suite.DotProto.Generate.Syntax
import           Proto3.Suite.Haskell.Parser    (Logger, parseModule, renderSDoc)
import           Proto3.Suite.DotProto.Internal
import           Proto3.Wire.Types              (FieldNumber (..))
import Text.Parsec (Parsec, alphaNum, eof, parse, satisfy, try)
import qualified Text.Parsec as Parsec
import qualified Turtle hiding (encodeString)
import qualified Turtle.Compat as Turtle (encodeString)
import           Turtle                         (FilePath, (</>), (<.>))

#if !MIN_VERSION_ghc(9,6,0)
import qualified GHC.Unit.Module.Name           as GHC
import qualified GHC.Types.Basic                as GHC (PromotionFlag(..))
#endif

#if MIN_VERSION_ghc(9,2,0)
import           GHC.Hs                         (HsSigType(..))
import           GHC.Parser.Annotation          (noLocA)
#if !MIN_VERSION_ghc(9,6,0)
import qualified GHC.Types.SourceText           as GHC
#endif
#else
import           GHC.Types.Basic                as GHC (SourceText(..))
#endif

-- $setup
-- >>> :set -XTypeApplications

--------------------------------------------------------------------------------

--
-- * Public interface
--
data CompileArgs = CompileArgs
  { includeDir         :: [FilePath]
  , extraInstanceFiles :: [FilePath]
  , inputProto         :: FilePath
  , outputDir          :: FilePath
  , stringType         :: StringType
  , recordStyle        :: RecordStyle
  , typeLevelFormat    :: Bool
  }

data StringType = StringType String String
  -- ^ Qualified module name, then unqualified type name.

data RecordStyle = RegularRecords | LargeRecords
  deriving stock (Eq, Show, Read)

parseStringType :: String -> Either String StringType
parseStringType str = case splitOn "." str of
  xs@(_ : _ : _) -> Right $ StringType (intercalate "." $ init xs) (last xs)
  _ -> Left "must be in the form Module.Type"

-- | Generate a Haskell module corresponding to a @.proto@ file
compileDotProtoFile :: Logger -> CompileArgs -> IO (Either CompileError ())
compileDotProtoFile logger CompileArgs{..} = runExceptT $ do
  (dotProto, importTypeContext) <- readDotProtoWithContext includeDir inputProto
  modulePathPieces <- traverse renameProtoFile (toModuleComponents dotProto)

  let relativePath = foldr combine mempty (map fromString $ NE.toList modulePathPieces) <.> "hs"
      combine p1 p2 | p2 == mempty = p1
      combine p1 p2 = p1 </> p2
  let modulePath = outputDir </> relativePath

  Turtle.mktree (Turtle.directory modulePath)

  extraInstances <- foldMapM (getExtraInstances logger) extraInstanceFiles
  haskellModule <-
    let ?recordStyle = recordStyle
        ?stringType = stringType
        ?typeLevelFormat = typeLevelFormat
    in renderHsModuleForDotProto extraInstances dotProto importTypeContext

  liftIO (writeFile (Turtle.encodeString modulePath) haskellModule)
  where
    toModuleComponents :: DotProto -> NonEmpty String
    toModuleComponents = components . metaModulePath . protoMeta

-- | Same as 'compileDotProtoFile', except terminates the program with an error
-- message on failure.
compileDotProtoFileOrDie :: Logger -> CompileArgs -> IO ()
compileDotProtoFileOrDie logger args = compileDotProtoFile logger args >>= \case
  Left e -> do
    -- TODO: pretty print the error messages
    let errText          = Turtle.format Turtle.w  e
    let dotProtoPathText = Turtle.format Turtle.fp (inputProto args)
    dieLines [Neat.text|
      Error: failed to compile "${dotProtoPathText}":

      ${errText}
    |]
  _ -> pure ()

-- | Renaming protobuf file names to valid Haskell module names.
--
-- By convention, protobuf filenames are snake case. 'rnProtoFile' renames
-- snake-cased protobuf filenames by:
--
-- * Replacing occurrences of one or more underscores followed by an
-- alphabetical character with one less underscore.
--
-- * Capitalizing the first character following the string of underscores.
--
-- ==== __Examples__
--
-- >>> renameProtoFile @(Either CompileError) "abc_xyz"
-- Right "AbcXyz"
--
-- >>> renameProtoFile @(Either CompileError) "abc_1bc"
-- Left (InvalidModuleName "abc_1bc")
--
-- >>> renameProtoFile @(Either CompileError) "_"
-- Left (InvalidModuleName "_")
renameProtoFile :: MonadError CompileError m => String -> m String
renameProtoFile filename =
  case parse parser "" filename of
    Left {} -> throwError (InvalidModuleName filename)
    Right (nm, ps, sn) -> pure (toUpperFirst nm ++ rename ps ++ sn)
  where
    rename :: [(String, String)] -> String
    rename = foldMap $ \(us, nm) ->
      drop 1 us ++ toUpperFirst nm

    parser :: Parsec String () (String, [(String, String)], String)
    parser = do
      nm <- pName
      ps <- Parsec.many (try pNamePart)
      sn <- Parsec.many (satisfy (== '_'))
      pure (nm, ps, sn) <* eof

    pNamePart :: Parsec String () (String, String)
    pNamePart = liftA2 (,) (Parsec.many1 (satisfy (== '_'))) pName

    pName :: Parsec String () String
    pName = liftA2 (:) (satisfy isAlpha) (Parsec.many alphaNum)

-- | Compile a 'DotProto' AST into a 'String' representing the Haskell
--   source of a module implementing types and instances for the .proto
--   messages and enums.
renderHsModuleForDotProto ::
  ( MonadError CompileError m
  , (?recordStyle :: RecordStyle)
  , (?stringType :: StringType)
  , (?typeLevelFormat :: Bool)
  ) =>
  ([HsImportDecl],[HsDecl]) ->
  DotProto ->
  TypeContext ->
  m String
renderHsModuleForDotProto extraInstanceFiles dotProto importCtxt = do
    haskellModule <- hsModuleForDotProto extraInstanceFiles dotProto importCtxt

    let languagePragmas = textUnlines $ map (\extn -> "{-# LANGUAGE " <> extn <> " #-}") $ sort extensions
        ghcOptionPragmas = textUnlines $ map (\opt -> "{-# OPTIONS_GHC " <> opt <> " #-}") $ sort options

        extensions :: [T.Text]
        extensions = nub $
          [ "DataKinds"
          , "DeriveAnyClass"
          , "DeriveGeneric"
          , "GADTs"
          , "NamedFieldPuns"
          , "NegativeLiterals"
          , "OverloadedStrings"
          , "TypeApplications"
          , "TypeOperators"
          ] ++
          (if ?typeLevelFormat then [ "TypeFamilies", "UndecidableInstances" ] else []) ++
          case ?recordStyle of
            RegularRecords -> []
            LargeRecords -> [ "ConstraintKinds"
                            , "FlexibleInstances"
                            , "MultiParamTypeClasses"
                            , "ScopedTypeVariables"
                            , "TypeFamilies"
                            , "UndecidableInstances"
                            ]

        options :: [T.Text]
        options = [ "-fno-warn-unused-imports"
                  , "-fno-warn-name-shadowing"
                  , "-fno-warn-unused-matches"
                  , "-fno-warn-missing-export-lists"
                  ] ++
                  case ?recordStyle of
                    RegularRecords -> []
                    LargeRecords -> [ "-fplugin=Data.Record.Plugin" ]

        mkLRAnnotation :: HsDecl -> Maybe HsDecl
        mkLRAnnotation decl = mkAnn <$> LargeRecord.typeNameIfLargeRecord decl
          where
            mkAnn :: HsName -> HsDecl
            mkAnn recName = noLocA $ GHC.AnnD GHC.NoExtField $ GHC.HsAnnotation
              synDef
#if !MIN_VERSION_ghc(9,6,0)
              GHC.NoSourceText
#endif
              (GHC.TypeAnnProvenance recName)
              (uvar_ "largeRecord")

        annotatedHaskellModule :: GHC.HsModule
#if MIN_VERSION_ghc(9,6,0)
                                               GHC.GhcPs
#endif
        annotatedHaskellModule =
          case (?recordStyle, haskellModule) of
            (RegularRecords, _) -> haskellModule
            (LargeRecords, GHC.HsModule{hsmodDecls = moduleDecls}) ->
              haskellModule{GHC.hsmodDecls = moduleDecls ++ mapMaybe mkLRAnnotation moduleDecls}

        moduleContent :: T.Text
        moduleContent = T.pack $ renderSDoc $ GHC.ppr annotatedHaskellModule

        textUnlines :: [T.Text] -> T.Text
        textUnlines = T.intercalate "\n"

    pure $ T.unpack $ [Neat.text|
      $languagePragmas
      $ghcOptionPragmas

      -- | Generated by Haskell protocol buffer compiler. DO NOT EDIT!
      $moduleContent
    |]

-- | Compile a Haskell module AST given a 'DotProto' package AST.
-- Instances given in @eis@ override those otherwise generated.
hsModuleForDotProto ::
  ( MonadError CompileError m
  , (?recordStyle :: RecordStyle)
  , (?stringType :: StringType)
  , (?typeLevelFormat :: Bool)
  ) =>
  -- | Extra user-define instances that override default generated instances
  ([HsImportDecl], [HsDecl]) ->
  -- |
  DotProto ->
  -- |
  TypeContext ->
  m (GHC.HsModule
#if MIN_VERSION_ghc(9,6,0)
                  GHC.GhcPs
#endif
                           )
hsModuleForDotProto
    (extraImports, extraInstances)
    dotProto@DotProto{ protoMeta = DotProtoMeta { metaModulePath = modulePath }
                     , protoPackage
                     , protoDefinitions
                     }
    importTypeContext
  = do
       moduleName <- modulePathModName modulePath

       typeContextImports <- ctxtImports importTypeContext

       let icUsesGrpc = has (traverse._DotProtoService) protoDefinitions

       let importDeclarations = concat
              [ defaultImports icUsesGrpc
              , extraImports
              , typeContextImports ]

       typeContext <- dotProtoTypeContext dotProto

       let toDotProtoDeclaration =
             dotProtoDefinitionD protoPackage (typeContext <> importTypeContext)

       let extraInstances' = instancesForModule moduleName extraInstances

       decls <- replaceHsInstDecls extraInstances' <$>
                foldMapM toDotProtoDeclaration protoDefinitions

       pure (module_ moduleName Nothing importDeclarations decls)

getExtraInstances
    :: (MonadIO m, MonadError CompileError m)
    => Logger -> FilePath -> m ([HsImportDecl], [HsDecl])
getExtraInstances logger (Turtle.encodeString -> extraInstanceFile) = do
  contents <- liftIO $ GHC.hGetStringBuffer extraInstanceFile
  let location = GHC.mkRealSrcLoc (GHC.mkFastString extraInstanceFile) 1 1
  maybeModule <- liftIO $ parseModule logger location contents
  case maybeModule of
    Nothing ->
      internalError (T.unpack "Error: Failed to parse instance file")
    Just (GHC.L _ m) -> do
      let isInstDecl (GHC.L _ GHC.InstD{}) = True
          isInstDecl _                     = False
      pure (GHC.hsmodImports m, filter isInstDecl (GHC.hsmodDecls m))

-- | This very specific function will only work for the qualification on the very first type
-- in the object of an instance declaration. Those are the only sort of instance declarations
-- generated within this code, so it suffices.
instancesForModule :: Module -> [HsDecl] -> [HsDecl]
instancesForModule m = mapMaybe go
  where
    go ( GHC.L instX
         ( GHC.InstD clsInstX
           ( GHC.ClsInstD clsInstDeclX clsInstDecl@GHC.ClsInstDecl
             { cid_poly_ty =
#if MIN_VERSION_ghc(9,2,0)
                 GHC.L tyX
#endif
                   (HsSig ext bndrs ty) } ) ) )
      | Just (tc, GHC.L _ (GHC.HsTyVar _ GHC.NotPromoted (GHC.L _ (GHC.Qual tm i))) : ts) <-
          splitTyConApp ty, m == tm =
        Just ( GHC.L instX
               ( GHC.InstD clsInstX
                 ( GHC.ClsInstD clsInstDeclX clsInstDecl
                   { GHC.cid_poly_ty =
#if MIN_VERSION_ghc(9,2,0)
                       GHC.L tyX
#endif
                         (HsSig ext bndrs (tyConApply tc (typeNamed_ (noLocA (GHC.Unqual i)) : ts)))
                   } ) ) )
    go _ = Nothing

-- | For each thing in @base@ replaces it if it finds a matching @override@.
--
-- Current Limitations: The type of the type class instance and the corresponding
-- override must both be monomorphic; otherwise they will not match each other.
-- Furthermore, comparison is based on unqualified names, so please ensure that
-- those unqualified names are unambiguous or false matches may occur.
replaceHsInstDecls :: [HsDecl] -> [HsDecl] -> [HsDecl]
replaceHsInstDecls overrides base = concatMap (mbReplace) base
  where
    -- instances defined separately from data type definition:
    mbReplace :: HsDecl -> [HsDecl]
    mbReplace hid@(typeOfInstDecl -> Just classSig) =
        [fromMaybe hid (search classSig)]

    -- instances listed in "deriving" clause of data type or newtype definition:
    mbReplace ( GHC.L tyClDX
                ( GHC.TyClD dataDeclX
                  ( dataDecl@GHC.DataDecl
                    { tcdLName = tyn
                    , tcdDataDefn = dd@GHC.HsDataDefn
                      { dd_derivs =
#if !MIN_VERSION_ghc(9,2,0)
                          GHC.L _
#endif
                                  clauses
                      }
                    } ) ) ) =
        let ty = typeNamed_ tyn
            (uncustomized, customized) = partitionEithers (concatMap (clause ty) clauses)
        in ( GHC.L tyClDX
             ( GHC.TyClD dataDeclX
               ( dataDecl { GHC.tcdDataDefn = dd
                            { GHC.dd_derivs =
#if !MIN_VERSION_ghc(9,2,0)
                                noLocA
#endif
                                  uncustomized
                            } } ) ) )
           : customized

    -- irrelevant declarations remain unchanged:
    mbReplace hid = [hid]

    clause :: HsType -> HsDerivingClause -> [Either HsDerivingClause HsDecl]
    clause ty (splitDerivingClause -> (strategy, classSigs)) =
      let (uncustomized, customized) = partitionEithers (map (deriv ty) classSigs)
      in maybe id ((:) . Left) (derivingClause_ strategy uncustomized) (map Right customized)

    deriv ::
      HsType ->
      (HsOuterSigTyVarBndrs, HsType) ->
      Either (HsOuterSigTyVarBndrs, HsType) HsDecl
    deriv ty classSig@(bindings, classType) =
        maybe (Left classSig) Right (search (bindings, tyApp classType ty))

    -- | NOTE: 'getSig' must return 'Just' for *both* the goal and the override
    -- in order for there to be a match: 'Nothing' for both means no match.
    search :: (HsOuterSigTyVarBndrs, HsType) -> Maybe HsDecl
    search y = do
      desired <- getSig y
      find (\x -> Just desired == (getSig =<< typeOfInstDecl x)) overrides

    getSig :: (HsOuterSigTyVarBndrs, HsType) -> Maybe SimpleTypeName
#if MIN_VERSION_ghc(9,2,0)
    getSig (GHC.HsOuterImplicit _, x) = simpleType x
    getSig _ = empty
#else
    getSig ((), x) = simpleType x
#endif

-- | A simplified representation of certain Haskell types.
data SimpleTypeName = SimpleTypeName GHC.OccName [SimpleTypeName]
  deriving Eq

-- | Types are difficult to compare in general, but for many
-- of the simpler types we can find a corresponding simple
-- description that is practical for us to compare.
--
-- WARNING: As legacy behavior we remove all qualifiers, rather than
-- normalizing them in some way that considers equivalence of module
-- qualifiers.  This behavior could potentially cause incorrect
-- results if two modules provide the same type name.
simpleType :: HsType -> Maybe SimpleTypeName
simpleType (GHC.L _ (GHC.HsParTy _ x)) = simpleType x
simpleType x = do
    (tc, as) <- splitTyConApp x
    sas <- traverse simpleType as
    pure (SimpleTypeName (unQual tc) sas)
  where
    unQual :: HsName -> GHC.OccName
    unQual (GHC.L _ (GHC.Unqual n)) = n
    unQual (GHC.L _ (GHC.Qual _ n)) = n
    unQual (GHC.L _ (GHC.Orig _ n)) = n
    unQual (GHC.L _ (GHC.Exact n)) = GHC.nameOccName n

-- | If both types are sufficiently simple,
-- then return the result of an equality test.
--
-- WARNING: As legacy behavior we remove all qualifiers, rather than
-- normalizing them in some way that considers equivalence of module
-- qualifiers.  This behavior could potentially cause incorrect
-- results if two modules provide the same type name.
simpleTypeEq :: HsType -> HsType -> Maybe Bool
simpleTypeEq a b = (==) <$> simpleType a <*> simpleType b

-- | Parses the file at the given path and produces an AST along with a
-- 'TypeContext' representing all types from imported @.proto@ files, using the
-- first parameter as a list of paths to search for imported files. Terminates
-- with exit code 1 when an included file cannot be found in the search path.
readDotProtoWithContext
    :: (MonadError CompileError m, MonadIO m)
    => [FilePath]
    -> FilePath
    -> m (DotProto, TypeContext)
readDotProtoWithContext [] toplevelProto = do
  -- If we're not given a search path, default to using the current working
  -- directory, as `protoc` does
  cwd <- Turtle.pwd
  readDotProtoWithContext [cwd] toplevelProto

readDotProtoWithContext searchPaths toplevelProto = do
  dp <- importProto searchPaths toplevelProto toplevelProto
  let importIt = readImportTypeContext searchPaths toplevelProto (S.singleton toplevelProto)
  tc <- foldMapM importIt (protoImports dp)
  pure (dp, tc)

-- | Build the type context for an import, resolving transitive imports.
readImportTypeContext
    :: (MonadError CompileError m, MonadIO m)
    => [FilePath]
    -> FilePath
    -> S.Set FilePath
    -> DotProtoImport
    -> m TypeContext
readImportTypeContext searchPaths toplevelFP alreadyRead (DotProtoImport _ path)
  | path `S.member` alreadyRead = throwError (CircularImport path)
  | otherwise = do
      import_ <- importProto searchPaths toplevelFP path
      let importPkgSpec = protoPackage import_

      let fixImportTyInfo tyInfo =
             tyInfo { dotProtoTypeInfoPackage    = importPkgSpec
                    , dotProtoTypeInfoModulePath = metaModulePath . protoMeta $ import_
                    }
      importTypeContext <- fmap fixImportTyInfo <$> dotProtoTypeContext import_

      let prefixWithPackageName =
            case importPkgSpec of
              DotProtoPackageSpec packageName -> concatDotProtoIdentifier packageName
              DotProtoNoPackage -> pure

      qualifiedTypeContext <- mapKeysM prefixWithPackageName importTypeContext

      let isPublic (DotProtoImport q _) = q == DotProtoImportPublic
      transitiveImportsTC <-
        foldMapOfM (traverse . filtered isPublic)
                   (readImportTypeContext searchPaths toplevelFP (S.insert path alreadyRead))
                   (protoImports import_)

      pure $ importTypeContext <> qualifiedTypeContext <> transitiveImportsTC

-- | Given a type context, generates the Haskell import statements necessary to
--   import all the required types.  Excludes module "Google.Protobuf.Wrappers"
--   because the generated code does not actually make use of wrapper types
--   as such; instead it uses @Maybe a@, where @a@ is the wrapped type.
ctxtImports :: MonadError CompileError m => TypeContext -> m [HsImportDecl]
ctxtImports =
    fmap (map mkImport . nub . filter (GHC.mkModuleName "Google.Protobuf.Wrappers" /=))
    . traverse (modulePathModName . dotProtoTypeInfoModulePath)
    . M.elems
  where
    mkImport modName = importDecl_ modName True Nothing Nothing

--------------------------------------------------------------------------------
--
-- * Helper functions for Haskell code generation
--

-- ** Names

-- | Generate the Haskell type name for a 'DotProtoTypeInfo' for a message /
--   enumeration being compiled. NB: We ignore the 'dotProtoTypeInfoPackage'
--   field of the 'DotProtoTypeInfo' parameter, instead demanding that we have
--   been provided with a valid module path in its 'dotProtoTypeInfoModulePath'
--   field. The latter describes the name of the Haskell module being generated.
msgTypeFromDpTypeInfo :: MonadError CompileError m
                      => TypeContext -> DotProtoTypeInfo -> DotProtoIdentifier -> m HsType
msgTypeFromDpTypeInfo ctxt DotProtoTypeInfo{..} ident = do
    modName   <- modulePathModName dotProtoTypeInfoModulePath
    identName <- qualifiedMessageTypeName ctxt dotProtoTypeInfoParent ident
    pure $ typeNamed_ $ qual_ modName tcName identName

modulePathModName :: MonadError CompileError m => Path -> m Module
modulePathModName (Path comps) =
  GHC.mkModuleName . intercalate "." <$> traverse typeLikeName (NE.toList comps)

_pkgIdentModName :: MonadError CompileError m => DotProtoIdentifier -> m Module
_pkgIdentModName (Single s)  = GHC.mkModuleName <$> typeLikeName s
_pkgIdentModName (Dots path) = modulePathModName path
_pkgIdentModName x           = throwError (InvalidPackageName x)


-- ** Dhall

#ifdef DHALL
hsDhallPB :: String
hsDhallPB = "HsDhallPb"

dhallPBName :: GHC.NameSpace -> String -> HsQName
dhallPBName = qual_ (GHC.mkModuleName hsDhallPB)

-- *** Generate Dhall Interpret and Inject generic instances

fromDhall, toDhall :: String
(fromDhall, toDhall) =
#if MIN_VERSION_dhall(1,27,0)
  ("FromDhall", "ToDhall")
#else
  ("Interpret", "Inject")
#endif

dhallInterpretInstDecl :: String -> HsDecl
dhallInterpretInstDecl typeName =
  instDecl_ (dhallPBName tcName fromDhall)
            [ type_ typeName ]
            [ ]

dhallInjectInstDecl :: String -> HsDecl
dhallInjectInstDecl typeName =
  instDecl_ (dhallPBName tcName toDhall)
            [ type_ typeName ]
            [ ]
#endif

-- ** Helpers to wrap/unwrap types for protobuf (de-)serialization

data FieldContext = WithinMessage | WithinOneOf
  deriving (Eq, Show)

coerceE :: Bool -> Bool -> HsType -> HsType -> Maybe HsExp
coerceE _ _ from to | Just True <- simpleTypeEq from to = Nothing
coerceE overTyCon unsafe from to =
    Just $ applyAt coerceF [from, to]
  where
    coerceF | unsafe = var_ (name "unsafeCoerce")
            | otherwise  = var_ (name "coerce")
    name | overTyCon = protobufName varName . (<> "Over")
         | otherwise = haskellName varName

wrapFunE ::
  ( MonadError CompileError m
  , (?stringType :: StringType)
  ) =>
  Bool ->
  FieldContext ->
  TypeContext ->
  [DotProtoOption] ->
  DotProtoType ->
  m (Maybe HsExp)
wrapFunE overTyCon fc ctxt opts dpt =
  coerceE overTyCon (isMap dpt)
    <$> dptToHsType fc ctxt dpt
    <*> dptToHsTypeWrapped fc opts ctxt dpt

wrapE ::
  ( MonadError CompileError m
  , (?stringType :: StringType)
  ) =>
  FieldContext ->
  TypeContext ->
  [DotProtoOption] ->
  DotProtoType ->
  HsExp ->
  m HsExp
wrapE fc ctxt opts dpt e =
  maybeModify e <$> wrapFunE False fc ctxt opts dpt

unwrapFunE ::
  ( MonadError CompileError m
  , (?stringType :: StringType)
  ) =>
  Bool ->
  FieldContext ->
  TypeContext ->
  [DotProtoOption] ->
  DotProtoType ->
  m (Maybe HsExp)
unwrapFunE overTyCon fc ctxt opts dpt =
  coerceE overTyCon (isMap dpt)
    <$> dptToHsTypeWrapped fc opts ctxt dpt
    <*> dptToHsType fc ctxt dpt

unwrapE ::
  ( MonadError CompileError m
  , (?stringType :: StringType)
  ) =>
  FieldContext ->
  TypeContext ->
  [DotProtoOption] ->
  DotProtoType ->
  HsExp ->
  m HsExp
unwrapE fc ctxt opts dpt e = do
  maybeModify e <$> unwrapFunE True fc ctxt opts dpt

--------------------------------------------------------------------------------
--
-- * Functions to convert 'DotProtoType' into Haskell types
--

-- | Convert a dot proto type to a Haskell type
dptToHsType ::
  ( MonadError CompileError m
  , (?stringType :: StringType)
  ) =>
  FieldContext ->
  TypeContext ->
  DotProtoType ->
  m HsType
dptToHsType fc = foldDPT (dptToHsContType fc) dpptToHsType

-- | Convert a dot proto type to a wrapped Haskell type
dptToHsTypeWrapped ::
  ( MonadError CompileError m
  , (?stringType :: StringType)
  ) =>
  FieldContext ->
  [DotProtoOption] ->
  TypeContext ->
  DotProtoType ->
  m HsType
dptToHsTypeWrapped fc opts =
  foldDPT
    -- The wrapper for the collection type replaces the native haskell
    -- collection type, so try that first.
    (\ctxt ty -> maybe (dptToHsContType fc ctxt ty) id (dptToHsWrappedContType fc ctxt opts ty))
    -- Always wrap the primitive type.
    dpptToHsTypeWrapped

-- | Like 'dptToHsTypeWrapped' but without use of
-- 'dptToHsContType' or 'dptToHsWrappedContType'.
dpptToHsTypeWrapped ::
  ( MonadError CompileError m
  , (?stringType :: StringType)
  ) =>
  TypeContext ->
  DotProtoPrimType ->
  m HsType
dpptToHsTypeWrapped ctxt | StringType _ stringType <- ?stringType = \case
  Int32 ->
    pure $ primType_ "Int32"
  Int64 ->
    pure $ primType_ "Int64"
  SInt32 ->
    pure $ protobufSignedType_ $ primType_ "Int32"
  SInt64 ->
    pure $ protobufSignedType_ $ primType_ "Int64"
  UInt32 ->
    pure $ primType_ "Word32"
  UInt64 ->
    pure $ primType_ "Word64"
  Fixed32 ->
    pure $ protobufFixedType_ $ primType_ "Word32"
  Fixed64 ->
    pure $ protobufFixedType_ $ primType_ "Word64"
  SFixed32 ->
    pure $ protobufSignedType_ $ protobufFixedType_ $ primType_ "Int32"
  SFixed64 ->
    pure $ protobufSignedType_ $ protobufFixedType_ $ primType_ "Int64"
  String ->
    pure $ protobufStringType_ stringType
  Bytes  ->
    pure $ protobufBytesType_ "ByteString"
  Bool ->
    pure $ primType_ "Bool"
  Float ->
    pure $ primType_ "Float"
  Double ->
    pure $ primType_ "Double"
  Named (Dots (Path ("google" :| ["protobuf", x])))
    | x == "Int32Value" ->
        pure $ protobufWrappedType_ $ primType_ "Int32"
    | x == "Int64Value" ->
        pure $ protobufWrappedType_ $ primType_ "Int64"
    | x == "UInt32Value" ->
        pure $ protobufWrappedType_ $ primType_ "Word32"
    | x == "UInt64Value" ->
        pure $ protobufWrappedType_ $ primType_ "Word64"
    | x == "StringValue" ->
        pure $ protobufWrappedType_ $ protobufStringType_ stringType
    | x == "BytesValue" ->
        pure $ protobufWrappedType_ $ protobufBytesType_ "ByteString"
    | x == "BoolValue" ->
        pure $ protobufWrappedType_ $ primType_ "Bool"
    | x == "FloatValue" ->
        pure $ protobufWrappedType_ $ primType_ "Float"
    | x == "DoubleValue" ->
        pure $ protobufWrappedType_ $ primType_ "Double"
  Named msgName ->
    case M.lookup msgName ctxt of
      Just ty@(DotProtoTypeInfo { dotProtoTypeInfoKind = DotProtoKindEnum }) ->
        tyApp (protobufType_ "Enumerated") <$> msgTypeFromDpTypeInfo ctxt ty msgName
      Just ty -> msgTypeFromDpTypeInfo ctxt ty msgName
      Nothing -> noSuchTypeError msgName

foldDPT :: MonadError CompileError m
        => (TypeContext -> DotProtoType -> HsType -> HsType)
        -> (TypeContext -> DotProtoPrimType -> m HsType)
        -> TypeContext
        -> DotProtoType
        -> m HsType
foldDPT dptToHsCont foldPrim ctxt dpt =
  let
      prim = foldPrim ctxt
      go = foldDPT dptToHsCont foldPrim ctxt
      cont = dptToHsCont ctxt dpt
  in
    case dpt of
      Prim pType           -> cont <$> prim pType
      Repeated pType       -> cont <$> prim pType
      NestedRepeated pType -> cont <$> prim pType
      Map k v  | validMapKey k -> tyApp . cont <$> prim k <*> go (Prim v) -- need to 'Nest' message types
               | otherwise -> throwError $ InvalidMapKeyType (show $ pPrint k)

-- | Translate DotProtoType constructors to wrapped Haskell container types
-- (for Message serde instances).
--
-- When the given 'FieldContext' is 'WithinOneOf' we do not wrap submessages
-- in "Maybe" because the entire oneof is already wrapped in a "Maybe".
dptToHsWrappedContType :: FieldContext -> TypeContext -> [DotProtoOption] -> DotProtoType -> Maybe (HsType -> HsType)
dptToHsWrappedContType fc ctxt opts = \case
  Prim (Named tyName)
    | WithinMessage <- fc, isMessage ctxt tyName
                            -> Just $ tyApp (protobufType_ "Nested")
  Repeated (Named tyName)
    | isMessage ctxt tyName -> Just $ tyApp (protobufType_ "NestedVec")
  Repeated ty
    | isUnpacked opts       -> Just $ tyApp (protobufType_ "UnpackedVec")
    | isPacked opts         -> Just $ tyApp (protobufType_ "PackedVec")
    | isPackable ctxt ty    -> Just $ tyApp (protobufType_ "PackedVec")
    | otherwise             -> Just $ tyApp (protobufType_ "UnpackedVec")
  _ -> Nothing

-- | Translate DotProtoType to Haskell container types.
--
-- When the given 'FieldContext' is 'WithinOneOf' we do not wrap submessages
-- in "Maybe" because the entire oneof is already wrapped in a "Maybe".
dptToHsContType :: FieldContext -> TypeContext -> DotProtoType -> HsType -> HsType
dptToHsContType fc ctxt = \case
  Prim (Named tyName) | WithinMessage <- fc, isMessage ctxt tyName
                     -> tyApp $ primType_ "Maybe"
  Repeated _         -> tyApp $ primType_ "Vector"
  NestedRepeated _   -> tyApp $ primType_ "Vector"
  Map _ _            -> tyApp $ primType_ "Map"
  _                  -> id

-- | Convert a dot proto prim type to an unwrapped Haskell type
dpptToHsType ::
  ( MonadError CompileError m
  , (?stringType :: StringType)
  ) =>
  TypeContext ->
  DotProtoPrimType ->
  m HsType
dpptToHsType ctxt | StringType _ stringType <- ?stringType = \case
  Int32    -> pure $ primType_ "Int32"
  Int64    -> pure $ primType_ "Int64"
  SInt32   -> pure $ primType_ "Int32"
  SInt64   -> pure $ primType_ "Int64"
  UInt32   -> pure $ primType_ "Word32"
  UInt64   -> pure $ primType_ "Word64"
  Fixed32  -> pure $ primType_ "Word32"
  Fixed64  -> pure $ primType_ "Word64"
  SFixed32 -> pure $ primType_ "Int32"
  SFixed64 -> pure $ primType_ "Int64"
  String   -> pure $ primType_ stringType
  Bytes    -> pure $ primType_ "ByteString"
  Bool     -> pure $ primType_ "Bool"
  Float    -> pure $ primType_ "Float"
  Double   -> pure $ primType_ "Double"
  Named (Dots (Path ("google" :| ["protobuf", x])))
    | x == "Int32Value" -> pure $ primType_ "Int32"
    | x == "Int64Value" -> pure $ primType_ "Int64"
    | x == "UInt32Value" -> pure $ primType_ "Word32"
    | x == "UInt64Value" -> pure $ primType_ "Word64"
    | x == "StringValue" -> pure $ primType_ stringType
    | x == "BytesValue" -> pure $ primType_ "ByteString"
    | x == "BoolValue" -> pure $ primType_ "Bool"
    | x == "FloatValue" -> pure $ primType_ "Float"
    | x == "DoubleValue" -> pure $ primType_ "Double"
  Named msgName ->
    case M.lookup msgName ctxt of
      Just ty@(DotProtoTypeInfo { dotProtoTypeInfoKind = DotProtoKindEnum }) ->
        tyApp (protobufType_ "Enumerated") <$> msgTypeFromDpTypeInfo ctxt ty msgName
      Just ty -> msgTypeFromDpTypeInfo ctxt ty msgName
      Nothing -> noSuchTypeError msgName

validMapKey :: DotProtoPrimType -> Bool
validMapKey = (`elem` [ Int32, Int64, SInt32, SInt64, UInt32, UInt64
                      , Fixed32, Fixed64, SFixed32, SFixed64
                      , String, Bool])

-- | Convert a dot proto type to a Haskell type of kind `Proto3.Suite.Form.Repetition`.
-- It is ASSUMED that the field with this type is NOT part of a @oneof@.
dptToFormRepetition ::
  MonadError CompileError m => [DotProtoOption] -> TypeContext -> DotProtoType -> m HsType
dptToFormRepetition opts ctxt = \case
  Prim (Named tyName)
    | isMessage ctxt tyName -> pure formOptionalT
  Prim _                    -> pure $ tyApp formSingularT formImplicitT
    -- TO DO: When the @optional@ keyword is supported, check for it here.
  Repeated (Named tyName)
    | isMessage ctxt tyName -> pure unpacked
  Repeated pType
    | isUnpacked opts       -> pure unpacked
    | isPacked opts         -> pure packed
    | isPackable ctxt pType -> pure packed
    | otherwise             -> pure unpacked
  NestedRepeated pType      -> internalError $ "unexpected NestedRepeated on " ++ show pType
  Map k _
    | validMapKey k         -> pure unpacked
    | otherwise             -> throwError $ InvalidMapKeyType (show $ pPrint k)
  where
    packed = tyApp formRepeatedT formPackedT
    unpacked = tyApp formRepeatedT formUnpackedT

-- | Convert a dot proto type to a Haskell type of kind `Proto3.Suite.Form.ProtoType`,
-- with `Proto3.Suite.Form.Optional` replacing wrapper types.
dptToFormType :: MonadError CompileError m => TypeContext -> DotProtoType -> m HsType
dptToFormType ctxt = \case
  Prim pType -> dpptToFormType ctxt pType
  Repeated pType -> dpptToFormType ctxt pType
  NestedRepeated pType -> internalError $ "unexpected NestedRepeated on " ++ show pType
  Map k v
    | validMapKey k -> do
        k2 <- dpptToFormType ctxt k
        v2 <- dptToFormType ctxt (Prim v)
        pure $ tyApply formMapT [k2, v2]
    | otherwise ->
        throwError $ InvalidMapKeyType (show $ pPrint k)

-- | Like 'dptToFormType' but for primitive types.
dpptToFormType ::
  forall m .
  MonadError CompileError m =>
  TypeContext ->
  DotProtoPrimType ->
  m HsType
dpptToFormType ctxt = \case
    Int32 ->
      pure formInt32T
    Int64 ->
      pure formInt64T
    SInt32 ->
      pure formSInt32T
    SInt64 ->
      pure formSInt64T
    UInt32 ->
      pure formUInt32T
    UInt64 ->
      pure formUInt64T
    Fixed32 ->
      pure formFixed32T
    Fixed64 ->
      pure formFixed64T
    SFixed32 ->
      pure formSFixed32T
    SFixed64 ->
      pure formSFixed64T
    String ->
      pure formStringT
    Bytes ->
      pure formBytesT
    Bool ->
      pure formBoolT
    Float ->
      pure formFloatT
    Double ->
      pure formDoubleT
    Named (Dots (Path ("google" :| ["protobuf", x])))
      | x == "Int32Value" ->
          wrapper formInt32T
      | x == "Int64Value" ->
          wrapper formInt64T
      | x == "UInt32Value" ->
          wrapper formUInt32T
      | x == "UInt64Value" ->
          wrapper formUInt64T
      | x == "StringValue" ->
          wrapper formStringT
      | x == "BytesValue" ->
          wrapper formBytesT
      | x == "BoolValue" ->
          wrapper formBoolT
      | x == "FloatValue" ->
          wrapper formFloatT
      | x == "DoubleValue" ->
          wrapper formDoubleT
    Named msgName ->
      case M.lookup msgName ctxt of
        Just ty@(DotProtoTypeInfo { dotProtoTypeInfoKind = DotProtoKindEnum }) ->
          tyApp formEnumerationT <$> msgTypeFromDpTypeInfo ctxt ty msgName
        Just ty -> tyApp formMessageT <$> msgTypeFromDpTypeInfo ctxt ty msgName
        Nothing -> noSuchTypeError msgName
  where
    wrapper :: HsType -> m HsType
    wrapper = pure . tyApp formMessageT . tyApp formWrapperT

--------------------------------------------------------------------------------
--
-- * Code generation
--

-- ** Generate instances for a 'DotProto' package

dotProtoDefinitionD ::
  ( MonadError CompileError m
  , (?recordStyle :: RecordStyle)
  , (?stringType :: StringType)
  , (?typeLevelFormat :: Bool)
  ) =>
  DotProtoPackageSpec ->
  TypeContext ->
  DotProtoDefinition ->
  m [HsDecl]
dotProtoDefinitionD pkgSpec ctxt = \case
  DotProtoMessage _ messageName messageParts ->
    dotProtoMessageD ctxt Anonymous messageName messageParts

  DotProtoEnum _ enumName enumParts ->
    dotProtoEnumD Anonymous enumName enumParts

  DotProtoService _ serviceName serviceParts ->
    dotProtoServiceD pkgSpec ctxt serviceName serviceParts

-- | Generate 'Named' instance for a type in this package
namedInstD :: String -> HsDecl
namedInstD messageName =
    instDecl_ (protobufName tcName "Named")
              [ type_ messageName ]
              [ functionS_ "nameOf" [nameOf] ]
  where
    nameOf = ([wild_], apply fromStringE [ str_ messageName ])

hasDefaultInstD :: String -> HsDecl
hasDefaultInstD messageName =
  instDecl_ (protobufName tcName "HasDefault")
            [ type_ messageName ]
            [ ]

-- ** Generate types and instances for .proto messages

-- | Generate data types, 'Bounded', 'Enum', 'FromJSONPB', 'Named', 'Message',
--   'ToJSONPB' instances as appropriate for the given 'DotProtoMessagePart's
dotProtoMessageD ::
  forall m .
  ( MonadError CompileError m
  , (?recordStyle :: RecordStyle)
  , (?stringType :: StringType)
  , (?typeLevelFormat :: Bool)
  ) =>
  TypeContext ->
  DotProtoIdentifier ->
  DotProtoIdentifier ->
  [DotProtoMessagePart] ->
  m [HsDecl]
dotProtoMessageD ctxt parentIdent messageIdent messageParts = do
    messageName <- qualifiedMessageName parentIdent messageIdent

    let mkDataDecl flds =
          dataDecl_ messageName
                    []
                    [ recDecl_ (unqual_ varName messageName) flds ]
                    defaultMessageDeriving

#ifdef SWAGGER
    let getName = \case
          DotProtoMessageField fld -> (: []) <$> getFieldNameForSchemaInstanceDeclaration fld
          DotProtoMessageOneOf ident _ -> (: []) . (Nothing, ) <$> dpIdentUnqualName ident
          _ -> pure []
#endif

    messageDataDecl <- mkDataDecl <$> foldMapM (messagePartFieldD messageName) messageParts

    foldMapM id
      [ sequence
          [ pure messageDataDecl
          , pure (nfDataInstD messageDataDecl messageName)
          , pure (namedInstD messageName)
          , pure (hasDefaultInstD messageName)
          , messageInstD ctxt' parentIdent messageIdent messageParts

          , toJSONPBMessageInstD ctxt' parentIdent messageIdent messageParts
          , fromJSONPBMessageInstD ctxt' parentIdent messageIdent messageParts

            -- Generate Aeson instances in terms of JSONPB instances
          , pure (toJSONInstDecl messageName)
          , pure (fromJSONInstDecl messageName)

#ifdef SWAGGER
          -- And the Swagger ToSchema instance corresponding to JSONPB encodings
          , toSchemaInstanceDeclaration ctxt' messageName Nothing
              =<< foldMapM getName messageParts
#endif

#ifdef DHALL
          -- Generate Dhall instances
          , pure (dhallInterpretInstDecl messageName)
          , pure (dhallInjectInstDecl messageName)
#endif
          ]

      , if ?typeLevelFormat
          then typeLevelInstsD ctxt' parentIdent messageIdent messageParts
          else pure []

      -- Nested regular and oneof message decls
      , foldMapOfM (traverse . _DotProtoMessageDefinition)
                   nestedDecls
                   messageParts

      , foldMapOfM (traverse . _DotProtoMessageOneOf)
                   (uncurry $ nestedOneOfDecls messageName)
                   messageParts
      ]

  where
    ctxt' :: TypeContext
    ctxt' = maybe mempty dotProtoTypeChildContext (M.lookup messageIdent ctxt)
                <> ctxt

    nfDataInstD = case ?recordStyle of
                    RegularRecords -> RegularRecord.nfDataInstD
                    LargeRecords -> LargeRecord.nfDataInstD

    messagePartFieldD :: String -> DotProtoMessagePart -> m [([HsName], HsBangType)]
    messagePartFieldD messageName (DotProtoMessageField DotProtoField{..}) = do
      fullName <- prefixedFieldName messageName =<< dpIdentUnqualName dotProtoFieldName
      fullTy <- dptToHsType WithinMessage ctxt' dotProtoFieldType
      pure [ ([unqual_ varName fullName], unbangedTy_ fullTy) ]

    messagePartFieldD messageName (DotProtoMessageOneOf fieldName _) = do
      fullName <- prefixedFieldName messageName =<< dpIdentUnqualName fieldName
      qualTyName <- prefixedConName messageName =<< dpIdentUnqualName fieldName
      let fullTy = tyConApp (haskellName tcName "Maybe") . type_ $ qualTyName
      pure [ ([unqual_ varName fullName], unbangedTy_ fullTy) ]

    messagePartFieldD _ _ = pure []

    nestedDecls :: DotProtoDefinition -> m [HsDecl]
    nestedDecls (DotProtoMessage _ subMsgName subMessageDef) = do
      parentIdent' <- concatDotProtoIdentifier parentIdent messageIdent
      dotProtoMessageD ctxt' parentIdent' subMsgName subMessageDef

    nestedDecls (DotProtoEnum _ subEnumName subEnumDef) = do
      parentIdent' <- concatDotProtoIdentifier parentIdent messageIdent
      dotProtoEnumD parentIdent' subEnumName subEnumDef

    nestedDecls _ = pure []

    nestedOneOfDecls :: String -> DotProtoIdentifier -> [DotProtoField] -> m [HsDecl]
    nestedOneOfDecls messageName identifier fields = do
      fullName <- prefixedConName messageName =<< dpIdentUnqualName identifier

      (cons, _idents) <- fmap unzip (mapM (oneOfCons fullName) fields)

#ifdef SWAGGER
      toSchemaInstance <- toSchemaInstanceDeclaration ctxt' fullName (Just _idents)
                            =<< mapM getFieldNameForSchemaInstanceDeclaration fields
#endif

      let nestedDecl = dataDecl_ fullName [] cons defaultMessageDeriving
      pure [ nestedDecl
           , nfDataInstD nestedDecl fullName
           , namedInstD fullName
#ifdef SWAGGER
           , toSchemaInstance
#endif

#ifdef DHALL
           , dhallInterpretInstDecl fullName
           , dhallInjectInstDecl fullName
#endif
           ]

    oneOfCons :: String -> DotProtoField -> m (HsConDecl, HsName)
    oneOfCons fullName DotProtoField{..} = do
       consTy <- dptToHsType WithinOneOf ctxt' dotProtoFieldType
       consName <- prefixedConName fullName =<< dpIdentUnqualName dotProtoFieldName
       let ident = unqual_ dataName consName
       pure (conDecl_ ident [unbangedTy_ consTy], ident)

    oneOfCons _ DotProtoEmptyField = internalError "field type : empty field"


-- *** Generate type family instances providing type-level information about protobuf formats.

type FieldOccurrences = (Histogram FieldName, Histogram FieldNumber)

data FieldSpec = FieldSpec
  { fieldSpecName :: FieldName
  , fieldSpecNumber :: FieldNumber
  , fieldSpecOneOf :: Maybe FieldName
  , fieldSpecRepetition :: HsType
  , fieldSpecProtoType :: HsType
  }

typeLevelInstsD ::
  forall m .
  ( MonadError CompileError m
  , (?stringType :: StringType)
  ) =>
  TypeContext ->
  DotProtoIdentifier->
  DotProtoIdentifier ->
  [DotProtoMessagePart]->
  m [HsDecl]
typeLevelInstsD ctxt parentIdent msgIdent messageParts = do
    msgName <- qualifiedMessageName parentIdent msgIdent

    qualifiedFields <- getQualifiedFields msgName messageParts

    (fieldSpecLists, (fieldNames, fieldNumbers)) <-
      runWriterT (mapM mkFieldSpecs qualifiedFields)

    let (sort -> oneOfs, sortBy (compare `on` fieldSpecName) -> fieldSpecs) = fold fieldSpecLists
        repeatedFieldNames = mulipleOccurrencesOnly fieldNames
        repeatedFieldNumbers = mulipleOccurrencesOnly fieldNumbers

    when (repeatedFieldNames /= mempty || repeatedFieldNumbers /= mempty) $
      throwError $ RedefinedFields repeatedFieldNames repeatedFieldNumbers

    when (let Histogram m = fieldNames in M.member "" m) $
      internalError $ "empty field name within message " ++ show msgIdent

    let msgNameT = type_ msgName
        msgNumberOf = unqual_ tcName (msgName ++ "_NumberOf")
        msgProtoTypeOf = unqual_ tcName (msgName ++ "_ProtoTypeOf")
        msgOneOfOf = unqual_ tcName (msgName ++ "_OneOfOf")
        msgRepetitionOf = unqual_ tcName (msgName ++ "_RepetitionOf")
        fieldNameVar = tvarn_ "name"
        fieldNameVarT = typeNamed_ fieldNameVar
        fieldNameVarB = kindedTyVar_ synDef fieldNameVar symbolT
        err msg =
          [(Nothing, [fieldNameVarT], tyApp typeErrorT (tyApply msg [msgNameT, fieldNameVarT]))]
        toSym = symT . getFieldName
        fieldNameT = toSym . fieldSpecName
        fieldNumberT = natTLit . getFieldNumber . fieldSpecNumber
        oneOfT = maybe (symT "") toSym . fieldSpecOneOf

    let onFields :: (FieldSpec -> HsType) -> [(Maybe [HsTyVarBndr], [HsType], HsType)]
        onFields rhs = fieldSpecs <&> \f -> (Nothing, [ fieldNameT f ], rhs f)

        onOneOfs :: (FieldName -> HsType) -> [(Maybe [HsTyVarBndr], [HsType], HsType)]
        onOneOfs rhs = oneOfs <&> \o -> (Nothing, [ toSym o ], rhs o)

    let namesOf :: HsDecl
        numberOf, protoTypeOf, oneOfOf, repetitionOf :: [HsDecl]
        namesOf = tyFamInstDecl_ formNamesOf Nothing [ msgNameT ]
          (listT_ (map fieldNameT fieldSpecs))
        numberOf =
          [ tyFamInstDecl_ formNumberOf Nothing [ msgNameT, fieldNameVarT ]
              (tyApp (typeNamed_ msgNumberOf) fieldNameVarT)
          , closedTyFamDecl_ msgNumberOf [ fieldNameVarB ] natT
              (onFields fieldNumberT ++ err formFieldNotFound)
          ]
        protoTypeOf =
          [ tyFamInstDecl_ formProtoTypeOf Nothing [ msgNameT, fieldNameVarT ]
              (tyApp (typeNamed_ msgProtoTypeOf) fieldNameVarT)
          , closedTyFamDecl_ msgProtoTypeOf [ fieldNameVarB ] formProtoTypeT
              (onFields fieldSpecProtoType ++ err formFieldNotFound)
          ]
        oneOfOf =
          [ tyFamInstDecl_ formOneOfOf Nothing [ msgNameT, fieldNameVarT ]
              (tyApp (typeNamed_ msgOneOfOf) fieldNameVarT)
          , closedTyFamDecl_ msgOneOfOf [ fieldNameVarB ] symbolT
              (onFields oneOfT ++ onOneOfs toSym ++ err formFieldOrOneOfNotFound)
          ]
        repetitionOf =
          [ tyFamInstDecl_ formRepetitionOf Nothing [ msgNameT, fieldNameVarT ]
              (tyApp (typeNamed_ msgRepetitionOf) fieldNameVarT)
          , closedTyFamDecl_ msgRepetitionOf [ fieldNameVarB ] formRepetitionT
              ( onFields fieldSpecRepetition ++
                onOneOfs (const (tyApp formSingularT formAlternativeT)) ++
                err formFieldOrOneOfNotFound
              )
          ]

    pure $ namesOf : numberOf ++ protoTypeOf ++ oneOfOf ++ repetitionOf
  where
    mkFieldSpecs :: QualifiedField -> WriterT FieldOccurrences m ([FieldName], [FieldSpec])
    mkFieldSpecs QualifiedField{fieldInfo} = case fieldInfo of
      FieldNormal fieldName fieldNum dpType options -> do
        tell (oneOccurrence fieldName, oneOccurrence fieldNum)
        repetition <- dptToFormRepetition options ctxt dpType
        protoType <- dptToFormType ctxt dpType
        pure ( [], [ FieldSpec
                       { fieldSpecName = fieldName
                       , fieldSpecNumber = fieldNum
                       , fieldSpecOneOf = Nothing
                       , fieldSpecRepetition = repetition
                       , fieldSpecProtoType = protoType
                       } ] )

      FieldOneOf oneofName OneofField{subfields} -> do
          tell (oneOccurrence oneofName, mempty)
          ([oneofName], ) <$> mapM mkSubfieldSpec subfields
        where
          mkSubfieldSpec :: OneofSubfield -> WriterT FieldOccurrences m FieldSpec
          mkSubfieldSpec (OneofSubfield
                            { subfieldNumber = subfieldNum
                            , subfieldName = subfieldName
                            , subfieldType = dpType
                            }) = do
            tell (oneOccurrence subfieldName, oneOccurrence subfieldNum)
            protoType <- dptToFormType ctxt dpType
            pure FieldSpec
                   { fieldSpecName = subfieldName
                   , fieldSpecNumber = subfieldNum
                   , fieldSpecOneOf = Just oneofName
                   , fieldSpecRepetition = tyApp formSingularT formAlternativeT
                   , fieldSpecProtoType = protoType
                   }

-- *** Generate Protobuf 'Message' type class instances

messageInstD ::
  forall m .
  ( MonadError CompileError m
  , (?stringType :: StringType)
  ) =>
  TypeContext ->
  DotProtoIdentifier ->
  DotProtoIdentifier ->
  [DotProtoMessagePart] ->
  m HsDecl
messageInstD ctxt parentIdent msgIdent messageParts = do
     msgName         <- qualifiedMessageName parentIdent msgIdent
     qualifiedFields <- getQualifiedFields msgName messageParts

     encodedFields   <- mapM encodeMessageField qualifiedFields
     decodedFields   <- mapM decodeMessageField qualifiedFields

     let encodeMessageBind :: HsBind
         encodeMessageBind =
           functionS_ "encodeMessage" [([wild_, recordPattern], encodeMessageE)]

         encodeMessageE :: HsExp
         encodeMessageE = case encodedFields of
           [] -> memptyE
           (field : fields) -> foldl op (paren field) fields
             where op fs f = apply (apply mappendE [fs]) [paren f]
             -- NOTE: We use a left fold because this way the leftmost field
             -- is the most nested and the rightmost field--the one to be written
             -- first by the right-to-left builder--is the one that is least nested.

         recordPattern :: HsPat
         recordPattern = recPat (unqual_ dataName msgName) punnedFieldsP

         punnedFieldsP :: [GHC.LHsRecField GHC.GhcPs HsPat]
         punnedFieldsP = map (fp . coerce . recordFieldName) qualifiedFields
           where
             fp = fieldPunPat . unqual_ varName

     let decodeMessageBind :: HsBind
         decodeMessageBind = functionS_ "decodeMessage" [([wild_], decodeMessageE)]

         decodeMessageE :: HsExp
         decodeMessageE = foldl (\f -> opApp f apOp)
                                (apply pureE [ uvar_ msgName ])
                                decodedFields

     let dotProtoBind :: HsBind
         dotProtoBind = functionS_ "dotProto" [([wild_], dotProtoE)]

         dotProtoE :: HsExp
         dotProtoE = list_ $ do
           DotProtoMessageField DotProtoField{..} <- messageParts
           pure $ apply dotProtoFieldC
                        [ fieldNumberE dotProtoFieldNumber
                        , dpTypeE dotProtoFieldType
                        , dpIdentE dotProtoFieldName
                        , list_ (map optionE dotProtoFieldOptions)
                        , str_ dotProtoFieldComment
                        ]

     pure $ instDecl_ (protobufName tcName "Message")
                      [ type_ msgName ]
                      [ encodeMessageBind
                      , decodeMessageBind
                      , dotProtoBind
                      ]
  where
    encodeMessageField :: QualifiedField -> m HsExp
    encodeMessageField QualifiedField{recordFieldName, fieldInfo} =
      let recordFieldName' = uvar_ (coerce recordFieldName) in
      case fieldInfo of
        FieldNormal _fieldName fieldNum dpType options -> do
            fieldE <- wrapE WithinMessage ctxt options dpType recordFieldName'
            pure $ apply encodeMessageFieldE [ fieldNumberE fieldNum, fieldE ]

        FieldOneOf _ OneofField{subfields} -> do
            alts <- mapM mkAlt subfields
            pure $ case_ recordFieldName'
                    [ alt_ (conPat nothingN []) memptyE
                    , alt_ (conPat justN [patVar "x"]) (case_ (uvar_ "x") alts)
                    ]
          where
            -- Create all pattern match & expr for each constructor:
            --    Constructor y -> encodeMessageField num (Nested (Just y)) -- for embedded messages
            --    Constructor y -> encodeMessageField num (ForceEmit y)     -- for everything else
            mkAlt (OneofSubfield
                     { subfieldNumber = fieldNum
                     , subfieldConsName = conName
                     , subfieldType = dpType
                     , subfieldOptions = options
                     }) = do
              let isMaybe
                     | Prim (Named tyName) <- dpType
                     = isMessage ctxt tyName
                     | otherwise
                     = False

              let wrapJust = paren . app justC

              xE <- (if isMaybe then id else fmap forceEmitE)
                     . wrapE WithinMessage ctxt options dpType
                         -- For now we use 'WithinMessage' to preserve
                         -- the historical approach of treating this field
                         -- as if it were an ordinary non-oneof field that
                         -- just happens to be present, then forcing it to
                         -- be emitted.
                     . (if isMaybe then wrapJust else id)
                     $ uvar_ "y"

              pure $ alt_ (conPat (unqual_ dataName conName) [patVar "y"])
                          (apply encodeMessageFieldE [fieldNumberE fieldNum, xE])

    decodeMessageField :: QualifiedField -> m HsExp
    decodeMessageField QualifiedField{fieldInfo} =
      case fieldInfo of
        FieldNormal _fieldName fieldNum dpType options ->
            unwrapE WithinMessage ctxt options dpType $
              apply atE [ decodeMessageFieldE, fieldNumberE fieldNum ]

        FieldOneOf _ OneofField{subfields} -> do
            parsers <- mapM subfieldParserE subfields
            pure $ apply oneofE [ nothingC, list_ parsers ]
          where
            -- create a list of (fieldNumber, Cons <$> parser)
            subfieldParserE (OneofSubfield
                               { subfieldNumber = fieldNumber
                               , subfieldConsName = consName
                               , subfieldType = dpType
                               , subfieldOptions = options
                               }) = do
              let fE | Prim (Named tyName) <- dpType, isMessage ctxt tyName =
                         paren (app fmapE (uvar_ consName))
                     | otherwise =
                         paren (opApp justC composeOp (uvar_ consName))

              -- For now we continue the historical practice of parsing
              -- submessages within oneofs as if were outside of oneofs,
              -- and replacing the "Just . Ctor" with "fmap . Ctor".
              -- That is why we do not pass WithinOneOf.
              alts <- unwrapE WithinMessage ctxt options dpType decodeMessageFieldE

              pure $ tuple_
                   [ fieldNumberE fieldNumber
                   , opApp (apply pureE [ fE ]) apOp alts
                   ]


-- *** Generate ToJSONPB/FromJSONPB instances

toJSONPBMessageInstD ::
  forall m .
  ( MonadError CompileError m
  , (?stringType :: StringType)
  ) =>
  TypeContext ->
  DotProtoIdentifier ->
  DotProtoIdentifier ->
  [DotProtoMessagePart] ->
  m HsDecl
toJSONPBMessageInstD ctxt parentIdent msgIdent messageParts = do
    msgName    <- qualifiedMessageName parentIdent msgIdent
    qualFields <- getQualifiedFields msgName messageParts

    let applyE nm oneofNm = do
          fs <- traverse (encodeMessageField oneofNm) qualFields
          pure $ apply (var_ (jsonpbName varName nm)) [list_ fs]

    let patBinder = foldQF (const fieldBinder) (oneofSubDisjunctBinder . subfields)
    let matchE nm appNm oneofAppNm = do
          rhs <- applyE appNm oneofAppNm
          pure $ functionS_ nm
            [ ( [conPat (unqual_ dataName msgName) (patVar . patBinder <$> qualFields)]
              , rhs
              )
            ]

    toJSONPB <- matchE "toJSONPB" "object" "objectOrNull"
    toEncoding <- matchE "toEncodingPB" "pairs" "pairsOrNull"

    pure $ instDecl_ (jsonpbName tcName "ToJSONPB")
                     [ type_ msgName ]
                     [ toJSONPB
                     , toEncoding
                     ]

  where
    encodeMessageField :: String -> QualifiedField -> m HsExp
    encodeMessageField oneofNm (QualifiedField _ fieldInfo) =
      case fieldInfo of
        FieldNormal fldName fldNum dpType options ->
          defPairE fldName fldNum dpType options
        FieldOneOf _ oo ->
          oneofCaseE oneofNm oo

    -- E.g.
    -- "another" .= f2 -- always succeeds (produces default value on missing field)
    defPairE fldName fldNum dpType options = do
      w <- wrapE WithinMessage ctxt options dpType (uvar_ (fieldBinder fldNum))
      pure $ opApp (str_ (coerce fldName)) toJSONPBOp w

    -- E.g.
    -- HsJSONPB.pair "name" f4 -- fails on missing field
    oneOfPairE fldNm varNm options dpType = do
      w <- wrapE WithinOneOf ctxt options dpType (uvar_ varNm)
      pure $ apply (var_ (jsonpbName varName "pair")) [str_ (coerce fldNm), w]

    -- Suppose we have a sum type Foo, nested inside a message Bar.
    -- We want to generate the following:
    --
    -- > toJSONPB (Bar foo more stuff) =
    -- >   HsJSONPB.object
    -- >     [ (let encodeFoo = (<case expr scrutinising foo> :: Options -> Value)
    -- >        in \option -> if optEmitNamedOneof option
    -- >                      then ("Foo" .= (PB.objectOrNull [encodeFoo] option)) option
    -- >                      else encodeFoo option
    -- >       )
    -- >     , <encode more>
    -- >     , <encode stuff>
    -- >     ]
    oneofCaseE :: String -> OneofField -> m HsExp
    oneofCaseE retJsonCtor (OneofField typeName subfields) = do
        altEs <- traverse altE subfields
        pure $ paren
          $ let_ [ functionS_ caseName [([], caseExpr altEs)] ]
          $ lambda_ [patVar optsStr] (if_ dontInline noInline yesInline)
      where
        optsStr = "options"
        opts    = uvar_ optsStr

        caseName = "encode" <> over (ix 0) toUpper typeName
        caseBnd = uvar_ caseName

        dontInline = app (var_ (jsonpbName varName "optEmitNamedOneof")) opts

        noInline = app (paren (opApp (str_ typeName)
                                     toJSONPBOp
                                     (apply (var_ (jsonpbName varName retJsonCtor))
                                            [ list_ [caseBnd], opts ])))
                       opts

        yesInline = app caseBnd opts

        altE sub@(OneofSubfield
                    { subfieldConsName = conName
                    , subfieldName = pbFldNm
                    , subfieldType = dpType
                    , subfieldOptions = options
                    }) = do
          let patVarNm = oneofSubBinder sub
          p <- oneOfPairE pbFldNm patVarNm options dpType
          pure $
            alt_ (conPat justN [parenPat (conPat (unqual_ dataName conName) [patVar patVarNm])]) p

        -- E.g.
        -- case f4_or_f9 of
        --   Just (SomethingPickOneName f4)
        --     -> HsJSONPB.pair "name" f4
        --   Just (SomethingPickOneSomeid f9)
        --     -> HsJSONPB.pair "someid" f9
        --   Nothing
        --     -> mempty
        caseExpr altEs = paren $
            case_ disjunctName (altEs <> [fallthroughE])
          where
            disjunctName = uvar_ (oneofSubDisjunctBinder subfields)
            fallthroughE = alt_ (conPat nothingN []) memptyE

fromJSONPBMessageInstD ::
  forall m .
  ( MonadError CompileError m
  , (?stringType :: StringType)
  ) =>
  TypeContext ->
  DotProtoIdentifier ->
  DotProtoIdentifier ->
  [DotProtoMessagePart] ->
  m HsDecl
fromJSONPBMessageInstD ctxt parentIdent msgIdent messageParts = do
    msgName    <- qualifiedMessageName parentIdent msgIdent
    qualFields <- getQualifiedFields msgName messageParts

    fieldParsers <- traverse parseField qualFields

    let parseJSONPBE =
          apply (var_ (jsonpbName varName "withObject"))
                [ str_ msgName
                , paren (lambda_ [lambdaPVar] fieldAps)
                ]
          where
            fieldAps = foldl (\f -> opApp f apOp)
                             (apply pureE [ uvar_ msgName ])
                             fieldParsers

    let parseJSONPBBind = functionS_ "parseJSONPB" [([], parseJSONPBE)]

    pure (instDecl_ (jsonpbName tcName "FromJSONPB")
                    [ type_ msgName ]
                    [ parseJSONPBBind ])
  where
    lambdaPVar = patVar "obj"
    lambdaVar  = uvar_ "obj"

    parseField (QualifiedField _ (FieldNormal fldName _ dpType options)) =
      normalParserE fldName dpType options
    parseField (QualifiedField _ (FieldOneOf _ fld)) =
      oneofParserE fld

    -- E.g., for message
    --   message Something { oneof name_or_id { string name = _; int32 someid = _; } }
    --
    -- ==>
    --
    -- (let parseSomethingNameOrId parseObj = <FUNCTION, see tryParseDisjunctsE>
    --  in ((obj .: "nameOrId") Hs.>>=
    --      (HsJSONPB.withObject "nameOrId" parseSomethingNameOrId))
    --     <|>
    --     (parseSomethingNameOrId obj)
    -- )
    oneofParserE :: OneofField -> m HsExp
    oneofParserE (OneofField oneofType fields) = do
        ds <- tryParseDisjunctsE
        pure $ paren $
          let_ [ functionS_ letBndStr [([patVar letArgStr], ds)] ]
               (opApp parseWrapped altOp parseUnwrapped)
      where
        oneofTyLit = str_ oneofType -- FIXME

        letBndStr  = "parse" <> over (ix 0) toUpper oneofType
        letBndName = uvar_ letBndStr
        letArgStr  = "parseObj"
        letArgName = uvar_ letArgStr

        parseWrapped = paren $
          opApp (opApp lambdaVar parseJSONPBOp oneofTyLit)
                bindOp
                (apply (var_ (jsonpbName varName "withObject")) [ oneofTyLit , letBndName ])

        parseUnwrapped = paren (app letBndName lambdaVar)

        -- parseSomethingNameOrId parseObj =
        --   Hs.msum
        --     [ (Just . SomethingPickOneName) <$> (HsJSONPB.parseField parseObj "name")
        --     , (Just . SomethingPickOneSomeid) <$> (HsJSONPB.parseField parseObj "someid")
        --     , pure Nothing
        --     ]
        tryParseDisjunctsE = do
          fs <- traverse subParserE fields
          pure $ app msumE (list_ (fs <> fallThruE))

        fallThruE = [ app pureE nothingC ]

        subParserE OneofSubfield{subfieldConsName, subfieldName,
                                 subfieldType, subfieldOptions} = do
          maybeCoercion <-
            unwrapFunE False WithinOneOf ctxt subfieldOptions subfieldType
          let inject = opApp justC composeOp (uvar_ subfieldConsName)
          pure $ opApp
              (maybe inject (opApp inject composeOp) maybeCoercion)
              fmapOp
              (apply (var_ (jsonpbName varName "parseField"))
                     [ letArgName
                     , str_ (coerce subfieldName)])

    -- E.g. obj .: "someid"
    normalParserE :: FieldName -> DotProtoType -> [DotProtoOption] -> m HsExp
    normalParserE fldName dpType options =
      unwrapE WithinMessage ctxt options dpType $
        opApp lambdaVar
                   parseJSONPBOp
                   (str_(coerce fldName))

-- *** Generate default Aeson To/FromJSON and Swagger ToSchema instances
-- (These are defined in terms of ToJSONPB)

toJSONInstDecl :: String -> HsDecl
toJSONInstDecl typeName =
  instDecl_ (jsonpbName tcName "ToJSON")
            [ type_ typeName ]
            [ functionS_ "toJSON"
                         [([], var_ (jsonpbName varName "toAesonValue"))]
            , functionS_ "toEncoding"
                         [([], var_ (jsonpbName varName "toAesonEncoding"))]
            ]

fromJSONInstDecl :: String -> HsDecl
fromJSONInstDecl typeName =
  instDecl_ (jsonpbName tcName "FromJSON")
            [ type_ typeName ]
            [ functionS_ "parseJSON"
                         [([], var_ (jsonpbName varName "parseJSONPB"))]
            ]


-- *** Generate `ToSchema` instance

#ifdef SWAGGER
getFieldNameForSchemaInstanceDeclaration
  :: MonadError CompileError m
  => DotProtoField
  -> m (Maybe ([DotProtoOption], DotProtoType), String)
getFieldNameForSchemaInstanceDeclaration fld = do
  unqual <- dpIdentUnqualName (dotProtoFieldName fld)
  let optsType = (dotProtoFieldOptions fld, dotProtoFieldType fld)
  pure (Just optsType, unqual)

toSchemaInstanceDeclaration ::
  ( MonadError CompileError m
  , (?stringType :: StringType)
  ) =>
  TypeContext ->
  -- | Name of the message type to create an instance for
  String ->
  -- | Oneof constructors
  Maybe [HsName] ->
  -- | Field names, with every field that is not actually a oneof
  -- combining fields paired with its options and protobuf type
  [(Maybe ([DotProtoOption], DotProtoType), String)] ->
  m HsDecl
toSchemaInstanceDeclaration ctxt messageName maybeConstructors fieldNamesEtc = do
  let fieldNames = map snd fieldNamesEtc

  qualifiedFieldNames <- mapM (prefixedFieldName messageName) fieldNames

  let messageConstructor = uvar_ messageName

  let _namedSchemaNameExpression = app justC (str_ messageName)

#ifdef SWAGGER
      -- { _paramSchemaType = HsJSONPB.SwaggerObject
      -- }
  let paramSchemaUpdates =
        [ fieldUpd_ _paramSchemaType _paramSchemaTypeExpression
        ]
        where
          _paramSchemaType = jsonpbName varName "_paramSchemaType"

#if MIN_VERSION_swagger2(2,4,0)
          _paramSchemaTypeExpression = app justC (var_ (jsonpbName dataName "SwaggerObject"))
#else
          _paramSchemaTypeExpression = var_ (jsonpbName dataName "SwaggerObject")
#endif
#else
  let paramSchemaUpdates = []
#endif

  let _schemaParamSchemaExpression = recordUpd_ memptyE paramSchemaUpdates

      -- [ ("fieldName0", qualifiedFieldName0)
      -- , ("fieldName1", qualifiedFieldName1)
      -- ...
      -- ]
  let properties = list_ $ do
        (fieldName, qualifiedFieldName) <- zip fieldNames qualifiedFieldNames
        pure (tuple_ [ str_  fieldName, uvar_ qualifiedFieldName ])

  let _schemaPropertiesExpression =
        app (var_ (jsonpbName varName "insOrdFromList")) properties

      -- { _schemaParamSchema = ...
      -- , _schemaProperties  = ...
      -- , ...
      -- }
  let schemaUpdates = normalUpdates ++ extraUpdates
        where
          normalUpdates =
            [ fieldUpd_ _schemaParamSchema _schemaParamSchemaExpression
            , fieldUpd_ _schemaProperties  _schemaPropertiesExpression
            ]

          extraUpdates =
            case maybeConstructors of
                Just _ ->
                  [ fieldUpd_ _schemaMinProperties justOne
                  , fieldUpd_ _schemaMaxProperties justOne
                  ]
                Nothing ->
                  []

          _schemaParamSchema    = jsonpbName varName "_schemaParamSchema"
          _schemaProperties     = jsonpbName varName "_schemaProperties"
          _schemaMinProperties  = jsonpbName varName "_schemaMinProperties"
          _schemaMaxProperties  = jsonpbName varName "_schemaMaxProperties"

          justOne = app justC (intE (1 :: Integer))

  let _namedSchemaSchemaExpression = recordUpd_ memptyE schemaUpdates

      -- { _namedSchemaName   = ...
      -- , _namedSchemaSchema = ...
      -- }
  let namedSchemaBinds =
        [ fieldBind_ _namedSchemaName   _namedSchemaNameExpression
        , fieldBind_ _namedSchemaSchema _namedSchemaSchemaExpression
        ]
        where
          _namedSchemaName   = jsonpbName varName "_namedSchemaName"
          _namedSchemaSchema = jsonpbName varName "_namedSchemaSchema"

  let namedSchema = recordCtor_ (jsonpbName dataName "NamedSchema") namedSchemaBinds

  let toDeclareName fieldName = "declare_" ++ fieldName

  let toArgument fc (maybeOptsType, fieldName) =
          maybe pure (uncurry (unwrapE fc ctxt)) maybeOptsType $
            app asProxy declare
        where
          declare = uvar_ (toDeclareName fieldName)
          asProxy = var_ (jsonpbName varName "asProxy")

      -- do let declare_fieldName0 = HsJSONPB.declareSchemaRef
      --    qualifiedFieldName0 <- declare_fieldName0 Proxy.Proxy
      --    let declare_fieldName1 = HsJSONPB.declareSchemaRef
      --    qualifiedFieldName1 <- declare_fieldName1 Proxy.Proxy
      --    ...
      --    let _ = pure MessageName <*> HsJSONPB.asProxy declare_fieldName0 <*> HsJSONPB.asProxy declare_fieldName1 <*> ...
      --    return (...)
  let expressionForMessage = do
        let bindingStatements = do
              (fieldName, qualifiedFieldName) <- zip fieldNames qualifiedFieldNames

              let declareIdentifier = unqual_ varName (toDeclareName fieldName)

              let stmt0 = letStmt_
                    [ function_ declareIdentifier
                        [([], var_ (jsonpbName varName "declareSchemaRef"))] ]

              let stmt1 = bindStmt_ (patVar qualifiedFieldName)
                                    (app (var_ declareIdentifier)
                                         (var_ (proxyName dataName "Proxy")))
              [ stmt0, stmt1]

        inferenceStatement <- do
          arguments <- traverse (toArgument WithinMessage) fieldNamesEtc
          let patternBind = patBind_ wild_ (applicativeApply messageConstructor arguments)
          pure $ if null fieldNames then [] else [ letStmt_ [ patternBind ] ]

        let returnStatement = lastStmt_ (app returnE (paren namedSchema))

        pure $ do_ (bindingStatements ++ inferenceStatement ++ [ returnStatement ])

      -- do let declare_fieldName0 = HsJSONPB.declareSchemaRef
      --    let _ = pure ConstructorName0 <*> HsJSONPB.asProxy declare_fieldName0
      --    qualifiedFieldName0 <- declare_fieldName0 Proxy.Proxy
      --    let declare_fieldName1 = HsJSONPB.declareSchemaRef
      --    let _ = pure ConstructorName1 <*> HsJSONPB.asProxy declare_fieldName1
      --    qualifiedFieldName1 <- declare_fieldName1 Proxy.Proxy
      --    ...
      --    return (...)
  let expressionForOneOf constructors = do
        let bindingStatement (fieldNameEtc, qualifiedFieldName, constructor) = do
              let declareIdentifier = unqual_ varName (toDeclareName (snd fieldNameEtc))

              let stmt0 = letStmt_
                    [ function_ declareIdentifier
                        [([], var_ (jsonpbName varName "declareSchemaRef"))] ]

              let stmt1 = bindStmt_ (patVar qualifiedFieldName)
                                    (app (var_ declareIdentifier)
                                         (var_ (proxyName dataName "Proxy")))

              inferenceStatement <- do
                argument <- toArgument WithinOneOf fieldNameEtc
                let patternBind = patBind_ wild_ (applicativeApply (var_ constructor) [ argument ])
                pure $ if null fieldNames then [] else [ letStmt_ [ patternBind ] ]

              pure $ [stmt0, stmt1] ++ inferenceStatement

        bindingStatements <- foldMapM bindingStatement $
          zip3 fieldNamesEtc qualifiedFieldNames constructors

        let returnStatement = lastStmt_ (app returnE (paren namedSchema))

        pure $ do_ (bindingStatements ++ [ returnStatement ])

  expression <- case maybeConstructors of
    Nothing           -> expressionForMessage
    Just constructors -> expressionForOneOf constructors

  let instanceDeclaration =
          instDecl_ className [ classArgument ] [ classDeclaration ]
        where
          className = jsonpbName tcName "ToSchema"

          classArgument = type_ messageName

          classDeclaration =
            functionS_ "declareNamedSchema" [([ wild_ ], expression)]

  pure instanceDeclaration
#endif


-- ** Generate types and instances for .proto enums

dotProtoEnumD
    :: MonadError CompileError m
    => DotProtoIdentifier
    -> DotProtoIdentifier
    -> [DotProtoEnumPart]
    -> m [HsDecl]
dotProtoEnumD parentIdent enumIdent enumParts = do
  enumName <- qualifiedMessageName parentIdent enumIdent

  let enumeratorDecls =
        [ (i, conIdent) | DotProtoEnumField conIdent i _options <- enumParts ]

  enumeratorDeclsNE <- case enumeratorDecls of
    [] -> throwError $ EmptyEnumeration enumName
    h@(i, conIdent) : t
      | i == 0 -> pure (h :| t)
      | otherwise -> throwError $ NonzeroFirstEnumeration enumName conIdent i

  enumCons <- NE.sortBy (comparing fst) <$>
    traverse (traverse (fmap (prefixedEnumFieldName enumName) . dpIdentUnqualName))
             enumeratorDeclsNE

  let enumConNames = fmap snd enumCons

      minBoundD :: HsBind
      minBoundD = functionS_ "minBound" [([], uvar_ (NE.head enumConNames))]

      maxBoundD :: HsBind
      maxBoundD = functionS_ "maxBound" [([], uvar_ (NE.last enumConNames))]

      compareD :: HsBind
      compareD = functionS_ "compare"
          [ ( [ patVar "x", patVar "y" ]
            , app
                  (app
                      (var_ (haskellName varName "compare"))
                      (paren
                          (app (var_ (protobufName varName "fromProtoEnum"))
                                 (uvar_ "x")
                          )
                      )
                  )
                  (paren
                      (app (var_ (protobufName varName "fromProtoEnum"))
                             (uvar_ "y")
                      )
                  )
            )
          ]

      fromProtoEnumD :: HsBind
      fromProtoEnumD = functionS_ "fromProtoEnum"
          [ ([ conPat (unqual_ dataName conName) [] ], intE conIdx)
          | (conIdx, conName) <- NE.toList enumCons
          ]

      toProtoEnumMayD :: HsBind
      toProtoEnumMayD = functionS_ "toProtoEnumMay" $
          [ ([ intP conIdx ], app justC (uvar_ conName))
          | (conIdx, conName) <- NE.toList enumCons ] ++
          [ ([ wild_ ], nothingC) ]

      parseJSONPBDecl :: HsBind
      parseJSONPBDecl = functionS_ "parseJSONPB" $
          foldr ((:) . matchConName) [mismatch] enumConNames
        where
          matchConName conName = ([pat conName], app pureE (uvar_ conName))

          pat nm = conPat (jsonpbName dataName "String") [ strPat (tryStripEnumName nm) ]

          tryStripEnumName = fromMaybe <*> stripPrefix enumName

          mismatch =
            ( [patVar "v"]
            , apply (var_ (jsonpbName varName "typeMismatch"))
                    [ str_ enumName, uvar_ "v" ]
            )

      toJSONPBDecl :: HsBind
      toJSONPBDecl =
        functionS_ "toJSONPB"
          [( [ patVar "x", wild_ ]
           , app (var_ (jsonpbName varName "enumFieldString")) (uvar_ "x")
           )]

      toEncodingPBDecl :: HsBind
      toEncodingPBDecl =
        functionS_ "toEncodingPB"
          [([ patVar "x", wild_ ]
           , app (var_ (jsonpbName varName "enumFieldEncoding")) (uvar_ "x")
           )]

  pure [ dataDecl_ enumName
                   []
                   [ conDecl_ (unqual_ dataName con) [] | con <- NE.toList enumConNames ]
                   defaultEnumDeriving
       , namedInstD enumName
       , hasDefaultInstD enumName
       , instDecl_ (haskellName tcName "Bounded") [ type_ enumName ]
                   [ minBoundD
                   , maxBoundD
                   ]
       , instDecl_ (haskellName tcName "Ord") [ type_ enumName ]
                   [ compareD ]
       , instDecl_ (protobufName tcName "ProtoEnum") [ type_ enumName ]
                   [ toProtoEnumMayD
                   , fromProtoEnumD
                   ]
       , instDecl_ (jsonpbName tcName "ToJSONPB") [ type_ enumName ]
                   [ toJSONPBDecl
                   , toEncodingPBDecl
                   ]
       , instDecl_ (jsonpbName tcName "FromJSONPB") [ type_ enumName ]
                   [ parseJSONPBDecl ]
       -- Generate Aeson instances in terms of JSONPB instances
       , toJSONInstDecl enumName
       , fromJSONInstDecl enumName

#ifdef DHALL
       -- Generate Dhall instances
       , dhallInterpretInstDecl enumName
       , dhallInjectInstDecl enumName
#endif

       -- And the Finite instance, used to infer a Swagger ToSchema instance
       -- for this enumerated type.
       , instDecl_ (protobufName tcName "Finite") [ type_ enumName ] []
       ]

-- ** Generate code for dot proto services

dotProtoServiceD ::
  ( MonadError CompileError m
  , (?stringType :: StringType)
  ) =>
  DotProtoPackageSpec ->
  TypeContext ->
  DotProtoIdentifier ->
  [DotProtoServicePart] ->
  m [HsDecl]
dotProtoServiceD pkgSpec ctxt serviceIdent service = do
     serviceName <- typeLikeName =<< dpIdentUnqualName serviceIdent

     endpointPrefix <-
       case pkgSpec of
         DotProtoPackageSpec pkgIdent -> do
           packageName <- dpIdentQualName pkgIdent
           pure $ "/" ++ packageName ++ "." ++ serviceName ++ "/"
         DotProtoNoPackage -> pure $ "/" ++ serviceName ++ "/"

     let serviceFieldD (DotProtoServiceRPCMethod RPCMethod{..}) = do
           fullName <- prefixedMethodName serviceName =<< dpIdentUnqualName rpcMethodName

           methodName <- case rpcMethodName of
                           Single nm -> pure nm
                           _ -> invalidMethodNameError rpcMethodName

           requestTy  <- dpptToHsType ctxt (Named rpcMethodRequestType)

           responseTy <- dpptToHsType ctxt (Named rpcMethodResponseType)

           let streamingType =
                 case (rpcMethodRequestStreaming, rpcMethodResponseStreaming) of
                   (Streaming, Streaming)       -> biDiStreamingC
                   (Streaming, NonStreaming)    -> clientStreamingC
                   (NonStreaming, Streaming)    -> serverStreamingC
                   (NonStreaming, NonStreaming) -> normalC

           pure [ ( endpointPrefix ++ methodName
                  , fullName, rpcMethodRequestStreaming, rpcMethodResponseStreaming
                  , unbangedTy_ $
                    funTy (tyApply (tvar_ "request") [streamingType, requestTy, responseTy])
                          (tyApply ioT [tyApply (tvar_ "response") [streamingType, responseTy]])
                  )
                ]

         serviceFieldD _ = pure []

     fieldsD <- foldMapM serviceFieldD service

     serverFuncName <- prefixedFieldName serviceName "server"
     clientFuncName <- prefixedFieldName serviceName "client"

     let conDecl = recDecl_ (unqual_ dataName serviceName)
                            [ ([unqual_ varName hsName], ty) | (_, hsName, _, _, ty) <- fieldsD ]

     let serverT = tyApply (typeNamed_ (unqual_ tcName serviceName))
                           [ serverRequestT, serverResponseT ]

     let serviceServerTypeD =
           typeSig_ [ unqual_ varName serverFuncName ] implicitOuterSigTyVarBinders_
                    (funTy serverT (funTy serviceOptionsC ioActionT))

     let serviceServerD = valDecl_ $
             functionS_ serverFuncName [(serverFuncPats, serverFuncRhs)]
           where
             serverFuncPats =
                 [ recPat (unqual_ dataName serviceName)
                          [ fieldPunPat (unqual_ varName methodName)
                          | (_, methodName, _, _, _) <- fieldsD
                          ]
                 , conPat (unqual_ dataName "ServiceOptions")
                          [ patVar "serverHost"
                          , patVar "serverPort"
                          , patVar "useCompression"
                          , patVar "userAgentPrefix"
                          , patVar "userAgentSuffix"
                          , patVar "initialMetadata"
                          , patVar "sslConfig"
                          , patVar "logger"
                          , patVar "serverMaxReceiveMessageLength"
                          , patVar "serverMaxMetadataSize"
                          ]
                 ]

             serverFuncRhs = apply serverLoopE [ serverOptsE ]

             handlerE handlerC adapterE methodName hsName =
                 apply handlerC [ apply methodNameC [ str_ methodName ]
                                , apply adapterE [ uvar_ hsName ]
                                ]

             update u v = fieldUpd_ (unqual_ varName u) (uvar_ v)

             serverOptsE = recordUpd_ defaultOptionsE
                 [ fieldUpd_ (grpcName varName "optNormalHandlers") $
                       list_ [ handlerE unaryHandlerC convertServerHandlerE endpointName hsName
                             | (endpointName, hsName, NonStreaming, NonStreaming, _) <- fieldsD
                             ]

                 , fieldUpd_ (grpcName varName "optClientStreamHandlers") $
                       list_ [ handlerE clientStreamHandlerC convertServerReaderHandlerE endpointName hsName
                             | (endpointName, hsName, Streaming, NonStreaming, _) <- fieldsD
                             ]

                 , fieldUpd_ (grpcName varName "optServerStreamHandlers") $
                       list_ [ handlerE serverStreamHandlerC convertServerWriterHandlerE endpointName hsName
                             | (endpointName, hsName, NonStreaming, Streaming, _) <- fieldsD
                             ]

                 , fieldUpd_ (grpcName varName "optBiDiStreamHandlers") $
                       list_ [ handlerE biDiStreamHandlerC convertServerRWHandlerE endpointName hsName
                             | (endpointName, hsName, Streaming, Streaming, _) <- fieldsD
                             ]

                 , update "optServerHost" "serverHost"
                 , update "optServerPort" "serverPort"
                 , update "optUseCompression" "useCompression"
                 , update "optUserAgentPrefix" "userAgentPrefix"
                 , update "optUserAgentSuffix" "userAgentSuffix"
                 , update "optInitialMetadata" "initialMetadata"
                 , update "optSSLConfig" "sslConfig"
                 , update "optLogger" "logger"
                 , update "optMaxReceiveMessageLength" "serverMaxReceiveMessageLength"
                 , update "optMaxMetadataSize" "serverMaxMetadataSize"
                 ]

     let clientT = tyApply (type_ serviceName) [ clientRequestT, clientResultT ]

     let serviceClientTypeD =
            typeSig_ [ unqual_ varName clientFuncName ] implicitOuterSigTyVarBinders_
                     (funTy grpcClientT (tyApp ioT clientT))

     let serviceClientD = valDecl_ $
              functionS_ clientFuncName [([patVar "client"], clientRecE)]
            where
              clientRecE = foldl
                (\f -> opApp f apOp)
                (apply pureE [ uvar_ serviceName ])
                [ paren (opApp clientRequestE' apOp (registerClientMethodE endpointName))
                | (endpointName, _, _, _, _) <- fieldsD
                ]

              clientRequestE' = apply pureE [ apply clientRequestE [ uvar_ "client" ] ]

              registerClientMethodE endpoint =
                apply clientRegisterMethodE [ uvar_ "client"
                                            , apply methodNameC [ str_ endpoint ]
                                            ]

     pure [ dataDecl_ serviceName
                      [ userTyVar_ synDef (unqual_ GHC.tvName "request")
                      , userTyVar_ synDef (unqual_ GHC.tvName "response")
                      ]
                      [ conDecl ]
                      defaultServiceDeriving

          , serviceServerTypeD
          , serviceServerD

          , serviceClientTypeD
          , serviceClientD
          ]

--------------------------------------------------------------------------------

--
-- * Common Haskell expressions, constructors, and operators
--

unaryHandlerC, clientStreamHandlerC, serverStreamHandlerC, biDiStreamHandlerC,
  methodNameC, defaultOptionsE, serverLoopE, convertServerHandlerE,
  convertServerReaderHandlerE, convertServerWriterHandlerE,
  convertServerRWHandlerE, clientRegisterMethodE, clientRequestE :: HsExp

unaryHandlerC               = var_ (grpcName dataName "UnaryHandler")
clientStreamHandlerC        = var_ (grpcName dataName "ClientStreamHandler")
serverStreamHandlerC        = var_ (grpcName dataName "ServerStreamHandler")
biDiStreamHandlerC          = var_ (grpcName dataName "BiDiStreamHandler")
methodNameC                 = var_ (grpcName tcName "MethodName")
defaultOptionsE             = var_ (grpcName varName "defaultOptions")
serverLoopE                 = var_ (grpcName varName "serverLoop")
convertServerHandlerE       = var_ (grpcName varName "convertGeneratedServerHandler")
convertServerReaderHandlerE = var_ (grpcName varName "convertGeneratedServerReaderHandler")
convertServerWriterHandlerE = var_ (grpcName varName "convertGeneratedServerWriterHandler")
convertServerRWHandlerE     = var_ (grpcName varName "convertGeneratedServerRWHandler")
clientRegisterMethodE       = var_ (grpcName varName "clientRegisterMethod")
clientRequestE              = var_ (grpcName varName "clientRequest")

biDiStreamingC, serverStreamingC, clientStreamingC, normalC, serviceOptionsC,
  ioActionT, serverRequestT, serverResponseT, clientRequestT, clientResultT,
  ioT, grpcClientT :: HsType
biDiStreamingC   = typeNamed_ (qual_ grpcModule dataName "BiDiStreaming")
serverStreamingC = typeNamed_ (qual_ grpcModule dataName "ServerStreaming")
clientStreamingC = typeNamed_ (qual_ grpcModule dataName "ClientStreaming")
normalC          = typeNamed_ (qual_ grpcModule dataName "Normal")
serviceOptionsC  = typeNamed_ (qual_ grpcModule tcName "ServiceOptions")
serverRequestT   = typeNamed_ (grpcName tcName "ServerRequest")
serverResponseT  = typeNamed_ (grpcName tcName "ServerResponse")
clientRequestT   = typeNamed_ (grpcName tcName "ClientRequest")
clientResultT    = typeNamed_ (grpcName tcName "ClientResult")
grpcClientT      = typeNamed_ (grpcName tcName "Client")
ioActionT        = tyApp ioT (tupleType_ [])
ioT              = typeNamed_ (haskellName tcName "IO")

grpcModule :: GHC.ModuleName
grpcModule = GHC.mkModuleName "HsGRPC"

-- ** Expressions for protobuf-wire types

forceEmitE :: HsExp -> HsExp
forceEmitE = paren . app forceEmitC

fieldNumberE :: FieldNumber -> HsExp
fieldNumberE = paren . app fieldNumberC . intE . getFieldNumber

dpIdentE :: DotProtoIdentifier -> HsExp
dpIdentE (Single n) = apply singleC [ str_ n ]
dpIdentE (Dots (Path (n NE.:| ns))) =
  apply dotsC [ apply pathC [ paren (opApp (str_ n) neConsOp (list_ (map str_ ns))) ] ]
dpIdentE (Qualified a b)  = apply qualifiedC [ dpIdentE a, dpIdentE b ]
dpIdentE Anonymous        = anonymousC

dpValueE :: DotProtoValue -> HsExp
dpValueE (Identifier nm) = apply identifierC [ dpIdentE nm ]
dpValueE (StringLit s)   = apply stringLitC  [ str_ s ]
dpValueE (IntLit i)      = apply intLitC     [ intE i ]
dpValueE (FloatLit f)    = apply floatLitC   [ floatE f ]
dpValueE (BoolLit True)  = apply boolLitC    [ trueC ]
dpValueE (BoolLit False) = apply boolLitC    [ falseC ]

optionE :: DotProtoOption -> HsExp
optionE (DotProtoOption name value) =
  apply dotProtoOptionC [ dpIdentE name, dpValueE value ]

-- | Translate a dot proto type to its Haskell AST type
dpTypeE :: DotProtoType -> HsExp
dpTypeE (Prim p)           = apply primC           [ dpPrimTypeE p ]
dpTypeE (Repeated p)       = apply repeatedC       [ dpPrimTypeE p ]
dpTypeE (NestedRepeated p) = apply nestedRepeatedC [ dpPrimTypeE p ]
dpTypeE (Map k v)          = apply mapC            [ dpPrimTypeE k, dpPrimTypeE v]


-- | Translate a dot proto primitive type to a Haskell AST primitive type.
dpPrimTypeE :: DotProtoPrimType -> HsExp
dpPrimTypeE ty =
    let wrap = var_ . protobufASTName dataName in
    case ty of
        Named n  -> apply namedC [ dpIdentE n ]
        Int32    -> wrap "Int32"
        Int64    -> wrap "Int64"
        SInt32   -> wrap "SInt32"
        SInt64   -> wrap "SInt64"
        UInt32   -> wrap "UInt32"
        UInt64   -> wrap "UInt64"
        Fixed32  -> wrap "Fixed32"
        Fixed64  -> wrap "Fixed64"
        SFixed32 -> wrap "SFixed32"
        SFixed64 -> wrap "SFixed64"
        String   -> wrap "String"
        Bytes    -> wrap "Bytes"
        Bool     -> wrap "Bool"
        Float    -> wrap "Float"
        Double   -> wrap "Double"

defaultImports ::
  ( (?recordStyle :: RecordStyle)
  , (?stringType :: StringType)
  , (?typeLevelFormat :: Bool)
  ) =>
  -- | Uses GRPC?
  Bool ->
  [HsImportDecl]
defaultImports icUsesGrpc | StringType stringModule stringType <- ?stringType =
    [ importDecl_ (m "Prelude")               & qualified haskellNS  & everything
    , importDecl_ (m "Proto3.Suite.Class")    & qualified protobufNS & everything
#ifdef DHALL
    , importDecl_ (m "Proto3.Suite.DhallPB")  & qualified (m hsDhallPB) & everything
#endif
    , importDecl_ (m "Proto3.Suite.DotProto") & qualified protobufASTNS & everything
    , importDecl_ (m "Proto3.Suite.JSONPB")   & qualified jsonpbNS   & everything
    , importDecl_ (m "Proto3.Suite.JSONPB")   & unqualified          & selecting  [s".=", s".:"]
    , importDecl_ (m "Proto3.Suite.Types")    & qualified protobufNS & everything
    , importDecl_ (m "Proto3.Wire")           & qualified protobufNS & everything
    , importDecl_ (m "Proto3.Wire.Decode")    & qualified protobufNS & selecting  [i"Parser", i"RawField"]
    , importDecl_ (m "Control.Applicative")   & qualified haskellNS  & everything
    , importDecl_ (m "Control.Applicative")   & unqualified          & selecting  [s"<*>", s"<|>", s"<$>"]
    , importDecl_ (m "Control.DeepSeq")       & qualified haskellNS  & everything
    , importDecl_ (m "Control.Monad")         & qualified haskellNS  & everything
    , importDecl_ (m "Data.ByteString")       & qualified haskellNS  & everything
    , importDecl_ (m "Data.Coerce")           & qualified haskellNS  & everything
    , importDecl_ (m "Data.Int")              & qualified haskellNS  & selecting  [i"Int16", i"Int32", i"Int64"]
    , importDecl_ (m "Data.List.NonEmpty")    & qualified haskellNS  & selecting  [ieNameAll_ (unqual_ tcName "NonEmpty")]
    , importDecl_ (m "Data.Map")              & qualified haskellNS  & selecting  [i"Map", i"mapKeysMonotonic"]
    , importDecl_ (m "Data.Proxy")            & qualified proxyNS    & everything
    , importDecl_ (m "Data.String")           & qualified haskellNS  & selecting  [i"fromString"]
    , importDecl_ (m stringModule)            & qualified haskellNS  & selecting  [i stringType]
    , importDecl_ (m "Data.Vector")           & qualified haskellNS  & selecting  [i"Vector"]
    , importDecl_ (m "Data.Word")             & qualified haskellNS  & selecting  [i"Word16", i"Word32", i"Word64"]
    , importDecl_ (m "GHC.Enum")              & qualified haskellNS  & everything
    , importDecl_ (m "GHC.Generics")          & qualified haskellNS  & everything
    , importDecl_ (m "Google.Protobuf.Wrappers.Polymorphic") & qualified protobufNS & selecting [ieNameAll_ (unqual_ tcName "Wrapped")]
    , importDecl_ (m "Unsafe.Coerce")         & qualified haskellNS  & everything
    ]
    <>
    (if not icUsesGrpc then [] else
    [ importDecl_ (m "Network.GRPC.HighLevel.Generated")           & alias grpcNS & everything
    , importDecl_ (m "Network.GRPC.HighLevel.Client")              & alias grpcNS & everything
    , importDecl_ (m "Network.GRPC.HighLevel.Server")              & alias grpcNS & hiding    [i"serverLoop"]
    , importDecl_ (m "Network.GRPC.HighLevel.Server.Unregistered") & alias grpcNS & selecting [i"serverLoop"]
    ])
    <>
    ( if not ?typeLevelFormat then [] else
    [ importDecl_ (m "Proto3.Suite.Form") & qualified protobufFormNS & everything
    , importDecl_ (m "GHC.TypeLits")      & qualified haskellNS      & selecting [i"Nat", i"Symbol", i"TypeError"]
    ])
    <>
    case ?recordStyle of
      RegularRecords -> []
      LargeRecords ->
        [ importDecl_ (m "Data.Record.Generic")              & qualified lrNS  & everything
        -- "large-records" stopped exporting "grnf"; we try
        -- to get it directly from "large-generics" if we can.
        --
        -- Ideally we would generate CPP conditionals so that
        -- the version check happens when the generated code
        -- is built, but as yet it is unclear how to do that.
        --
        -- As a result, we have to enable the version check
        -- based on the package version available when building
        -- compile-proto-file, and only if large record support
        -- is enabled in the library code.
#ifdef LARGE_RECORDS
#if MIN_VERSION_large_generics(0,2,1)
        , importDecl_ (m "Data.Record.Generic.NFData")       & qualified lrNS  & everything
#endif
#endif
        , importDecl_ (m "Data.Record.Generic.Rep")          & qualified lrNS  & everything
        , importDecl_ (m "Data.Record.Generic.Rep.Internal") & qualified lrNS  & everything
        , importDecl_ (m "Data.Record.Plugin.Runtime")       & qualified lrNS  & everything
        -- <https://hackage.haskell.org/package/large-records-0.4/changelog>
        -- says that as of large-records-0.4 the plugin does not generate
        -- imports, and that "code must now import Data.Record.Plugin to
        -- bring largeRecord into scope (necessary for ANN annotations)."
        -- We also seem to need to import some Prelude identifiers.
        --
        -- Ideally we would generate CPP conditionals so that
        -- the version check happens when the generated code
        -- is built, but as yet it is unclear how to do that.
        --
        -- As a result, we have to enable the version check
        -- based on the package version available when building
        -- compile-proto-file, and only if large record support
        -- is enabled in the library code.
#ifdef LARGE_RECORDS
#if MIN_VERSION_large_records(0,4,0)
        , importDecl_ (m "Data.Record.Plugin")               & unqualified     & selecting [i"largeRecord"]
        , importDecl_ (m "Prelude")                          & unqualified     & selecting [i"Eq", i"Int", i"Ord", i"Show", i"error"]
#endif
#endif
        ]
  where
    m = GHC.mkModuleName
    i n = ieName_ (unqual_ (if foldr (const . isLower) True n then varName else tcName) n)
    s n = ieName_ (unqual_ varName n)

    grpcNS                    = m "HsGRPC"
    jsonpbNS                  = m "HsJSONPB"
    lrNS                      = m "LR"
    protobufNS                = m "HsProtobuf"
    protobufASTNS             = m "HsProtobufAST"
    proxyNS                   = m "Proxy"

    -- staged constructors for importDecl
    qualified :: Module -> (Bool -> Maybe Module -> a)  -> a
    qualified m' f = f True (Just m')

    unqualified :: (Bool -> Maybe Module -> a) -> a
    unqualified f = f False Nothing

    -- import unqualified AND also under a namespace
    alias :: Module -> (Bool -> Maybe Module -> a) -> a
    alias m' f = f False (Just m')

    selecting :: [HsImportSpec] -> (Maybe (Bool, [HsImportSpec]) -> a) -> a
    selecting is f = f (Just (False, is))

    hiding :: [HsImportSpec] -> (Maybe (Bool, [HsImportSpec]) -> a) -> a
    hiding is f =  f (Just (True, is))

    everything :: (Maybe (Bool, [HsImportSpec]) -> a) -> a
    everything f = f Nothing

defaultMessageDeriving :: [HsQName]
defaultMessageDeriving = map (haskellName tcName) [ "Show", "Eq", "Ord", "Generic" ]

defaultEnumDeriving :: [HsQName]
defaultEnumDeriving = map (haskellName tcName) [ "Show", "Eq", "Generic", "NFData" ]

defaultServiceDeriving :: [HsQName]
defaultServiceDeriving = map (haskellName tcName) [ "Generic" ]
