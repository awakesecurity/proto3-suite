{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DerivingStrategies        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
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
  , RecordStyle (..)
  , IsPrefixed(..)
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
import           Control.Monad.Except
import           Data.Char
import           Data.Coerce
import           Data.Either                    (partitionEithers)
import           Data.List                      (find, intercalate, nub, sort, sortBy, stripPrefix)
import qualified Data.List.NonEmpty             as NE
import           Data.List.Split                (splitOn)
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map                       as M
import           Data.Maybe
import           Data.Monoid
import           Data.Ord                       (comparing)
import qualified Data.Set                       as S
import           Data.String                    (fromString)
import qualified Data.Text                      as T
import           Language.Haskell.Parser        (ParseResult(..), parseModule)
import           Language.Haskell.Pretty
import           Language.Haskell.Syntax
import qualified NeatInterpolation              as Neat
import           Prelude                        hiding (FilePath)
import           Proto3.Suite.DotProto
import           Proto3.Suite.DotProto.AST.Lens
import qualified Proto3.Suite.DotProto.Generate.LargeRecord as LargeRecord
import qualified Proto3.Suite.DotProto.Generate.Record as RegularRecord
import           Proto3.Suite.DotProto.Generate.Syntax
import           Proto3.Suite.DotProto.Internal
import           Proto3.Wire.Types              (FieldNumber (..))
import Text.Parsec (Parsec, alphaNum, eof, parse, satisfy, try)
import qualified Text.Parsec as Parsec
import qualified Turtle hiding (encodeString)
import qualified Turtle.Compat as Turtle (encodeString)
import           Turtle                         (FilePath, (</>), (<.>))

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
  , isPrefixed         :: IsPrefixed
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
compileDotProtoFile :: CompileArgs -> IO (Either CompileError ())
compileDotProtoFile CompileArgs{..} = runExceptT $ do
  (dotProto, importTypeContext) <- readDotProtoWithContext includeDir inputProto
  modulePathPieces <- traverse renameProtoFile (toModuleComponents dotProto)

  let relativePath = foldr combine mempty (map fromString $ NE.toList modulePathPieces) <.> "hs"
      combine p1 p2 | p2 == mempty = p1
      combine p1 p2 = p1 </> p2
  let modulePath = outputDir </> relativePath

  Turtle.mktree (Turtle.directory modulePath)

  extraInstances <- foldMapM getExtraInstances extraInstanceFiles
  haskellModule <- renderHsModuleForDotProto stringType recordStyle isPrefixed extraInstances dotProto importTypeContext

  liftIO (writeFile (Turtle.encodeString modulePath) haskellModule)
  where
    toModuleComponents :: DotProto -> NonEmpty String
    toModuleComponents = components . metaModulePath . protoMeta

-- | Same as 'compileDotProtoFile', except terminates the program with an error
-- message on failure.
compileDotProtoFileOrDie :: CompileArgs -> IO ()
compileDotProtoFileOrDie args = compileDotProtoFile args >>= \case
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
renderHsModuleForDotProto
    :: MonadError CompileError m
    => StringType
    -> RecordStyle
    -> IsPrefixed
    -> ([HsImportDecl],[HsDecl]) -> DotProto -> TypeContext -> m String
renderHsModuleForDotProto stringType recordStyle isPrefixed extraInstanceFiles dotProto importCtxt = do
    haskellModule <- hsModuleForDotProto stringType recordStyle isPrefixed extraInstanceFiles dotProto importCtxt

    let languagePragmas = textUnlines $ map (\extn -> "{-# LANGUAGE " <> extn <> " #-}") $ sort extensions
        ghcOptionPragmas = textUnlines $ map (\opt -> "{-# OPTIONS_GHC " <> opt <> " #-}") $ sort options

        extensions :: [T.Text]
        extensions =
          [ "DataKinds"
          , "DeriveAnyClass"
          , "DeriveGeneric"
          , "GADTs"
          , "OverloadedStrings"
          , "TypeApplications"
          ] ++
          case recordStyle of
            RegularRecords -> []
            LargeRecords -> [ "ConstraintKinds"
                           , "FlexibleInstances"
                           , "MultiParamTypeClasses"
                           , "ScopedTypeVariables"
                           , "TypeFamilies"
                           , "UndecidableInstances"
                           ]
          ++ case isPrefixed of
            IsPrefixed True -> []
            IsPrefixed False -> ["DuplicateRecordFields"]

        options :: [T.Text]
        options = [ "-fno-warn-unused-imports"
                  , "-fno-warn-name-shadowing"
                  , "-fno-warn-unused-matches"
                  , "-fno-warn-missing-export-lists"
                  ] ++
                  case recordStyle of
                    RegularRecords -> []
                    LargeRecords -> [ "-fplugin=Data.Record.Plugin" ]

        mkLRAnnotation :: HsDecl -> Maybe T.Text
        mkLRAnnotation (HsDataDecl _ _ (HsIdent recName) _ [HsRecDecl _ _ (_fld1:_fld2:_)] _) =
          Just ("{-# ANN type " <> T.pack recName <> " largeRecord #-}")
        mkLRAnnotation _ = Nothing

        lrAnnotations :: T.Text
        lrAnnotations =
          case (recordStyle, haskellModule) of
            (RegularRecords, _) -> ""
            (LargeRecords, HsModule _ _ _ _ moduleDecls) ->
              textUnlines (mapMaybe mkLRAnnotation moduleDecls)

        moduleContent :: T.Text
        moduleContent = T.pack (prettyPrint haskellModule)

        textUnlines :: [T.Text] -> T.Text
        textUnlines = T.intercalate "\n"

    pure $ T.unpack $ [Neat.text|
      $languagePragmas
      $ghcOptionPragmas

      -- | Generated by Haskell protocol buffer compiler. DO NOT EDIT!
      $moduleContent

      $lrAnnotations
    |]

-- | Compile a Haskell module AST given a 'DotProto' package AST.
-- Instances given in @eis@ override those otherwise generated.
hsModuleForDotProto
    :: MonadError CompileError m
    => StringType
    -- ^ the module and the type for string
    -> RecordStyle
    -- ^ kind of records to generate
    -> IsPrefixed
    -- ^ flag for prefix of field names
    -> ([HsImportDecl], [HsDecl])
    -- ^ Extra user-define instances that override default generated instances
    -> DotProto
    -- ^
    -> TypeContext
    -- ^
    -> m HsModule
hsModuleForDotProto
    stringType
    recordStyle
    isPrefixed
    (extraImports, extraInstances)
    dotProto@DotProto{ protoMeta = DotProtoMeta { metaModulePath = modulePath }
                     , protoPackage
                     , protoDefinitions
                     }
    importTypeContext
  = do
       moduleName <- modulePathModName modulePath

       typeContextImports <- ctxtImports importTypeContext

       let hasService = has (traverse._DotProtoService) protoDefinitions

       let importDeclarations = concat
              [ defaultImports recordStyle
                               ImportCustomisation
                               { icUsesGrpc = hasService
                               , icStringType = stringType
                               }
              , extraImports
              , typeContextImports ]

       typeContext <- dotProtoTypeContext dotProto

       let toDotProtoDeclaration =
             dotProtoDefinitionD stringType recordStyle isPrefixed protoPackage (typeContext <> importTypeContext)

       let extraInstances' = instancesForModule moduleName extraInstances

       decls <- replaceHsInstDecls extraInstances' <$>
                foldMapM toDotProtoDeclaration protoDefinitions

       return (module_ moduleName Nothing importDeclarations decls)

getExtraInstances
    :: (MonadIO m, MonadError CompileError m)
    => FilePath -> m ([HsImportDecl], [HsDecl])
getExtraInstances extraInstanceFile = do

  contents <- liftIO (readFile (Turtle.encodeString extraInstanceFile))

  case parseModule contents of
    ParseOk (HsModule _srcloc _mod _es idecls decls) -> do
      let isInstDecl HsInstDecl{} = True
          isInstDecl _            = False

      return (idecls, filter isInstDecl decls) --TODO give compile result

    ParseFailed srcLoc err -> do
      let srcLocText = Turtle.format Turtle.w srcLoc

      let errText = T.pack err

      let message = [Neat.text|
            Error: Failed to parse instance file

            ${srcLocText}: ${errText}
          |]

      internalError (T.unpack message)

-- | This very specific function will only work for the qualification on the very first type
-- in the object of an instance declaration. Those are the only sort of instance declarations
-- generated within this code, so it suffices.
instancesForModule :: Module -> [HsDecl] -> [HsDecl]
instancesForModule m = foldr go []
   where go x xs = case x of
             HsInstDecl a b c (HsTyCon (Qual tm  i):ts) d ->
                        if m == tm then HsInstDecl a b c (HsTyCon (UnQual i):ts) d:xs else xs
             _ -> xs

-- | For each thing in @base@ replaces it if it finds a matching @override@
replaceHsInstDecls :: [HsDecl] -> [HsDecl] -> [HsDecl]
replaceHsInstDecls overrides base = concatMap mbReplace base
  where
    -- instances defined separately from data type definition:
    mbReplace hid@(HsInstDecl _ _ qn tys _) =
        (: []) . fromMaybe hid $ search qn tys

    -- instances listed in "deriving" clause of data type definition:
    mbReplace (HsDataDecl loc ctx tyn names def insts) =
        let (uncustomized, customized) = partitionEithers (map (deriv tyn) insts)
        in HsDataDecl loc ctx tyn names def uncustomized : customized

    -- instances listed in "deriving" clause of newtype definition:
    mbReplace (HsNewTypeDecl loc ctx tyn names def insts) =
        let (uncustomized, customized) = partitionEithers (map (deriv tyn) insts)
        in HsNewTypeDecl loc ctx tyn names def uncustomized : customized

    -- irrelevant declarations remain unchanged:
    mbReplace hid = [hid]

    deriv tyn qn = maybe (Left qn) Right $ search qn [HsTyCon (UnQual tyn)]

    search qn tys = find (\x -> Just (unQual qn,tys) == getSig x) overrides

    getSig (HsInstDecl _ _ qn tys _) = Just (unQual qn,tys)
    getSig _ = Nothing

    unQual (Qual _ n) = Just n
    unQual (UnQual n) = Just n
    unQual (Special _) = Nothing

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
    fmap (map mkImport . nub . filter (Module "Google.Protobuf.Wrappers" /=))
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
    pure $ HsTyCon (Qual modName (HsIdent identName))

modulePathModName :: MonadError CompileError m => Path -> m Module
modulePathModName (Path comps) = Module . intercalate "." <$> traverse typeLikeName (NE.toList comps)

_pkgIdentModName :: MonadError CompileError m => DotProtoIdentifier -> m Module
_pkgIdentModName (Single s)  = Module <$> typeLikeName s
_pkgIdentModName (Dots path) = modulePathModName path
_pkgIdentModName x           = throwError (InvalidPackageName x)


-- ** Dhall

#ifdef DHALL
hsDhallPB :: String
hsDhallPB = "HsDhallPb"

dhallPBName :: String -> HsQName
dhallPBName name = Qual (Module hsDhallPB) (HsIdent name)

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
  instDecl_ (dhallPBName fromDhall)
            [ type_ typeName ]
            [ ]

dhallInjectInstDecl :: String -> HsDecl
dhallInjectInstDecl typeName =
  instDecl_ (dhallPBName toDhall)
            [ type_ typeName ]
            [ ]
#endif

-- ** Helpers to wrap/unwrap types for protobuf (de-)serialization

data FieldContext = WithinMessage | WithinOneOf
  deriving (Eq, Show)

typeApp :: HsType -> HsExp
typeApp ty = uvar_ ("@("++ pp ty ++ ")")
  where
    -- Do not add linebreaks to typeapps as that causes parse errors
    pp = prettyPrintStyleMode style{mode=OneLineMode} defaultMode

coerceE :: Bool -> Bool -> HsType -> HsType -> Maybe HsExp
coerceE _ _ from to | from == to = Nothing
coerceE overTyCon unsafe from to =
    Just $ HsApp (HsApp coerceF (typeApp from)) (typeApp to)
  where
    coerceF | unsafe = HsVar (name "unsafeCoerce")
            | otherwise  = HsVar (name "coerce")
    name | overTyCon = protobufName . (<> "Over")
         | otherwise = haskellName

wrapFunE :: MonadError CompileError m => Bool -> FieldContext -> StringType -> TypeContext -> [DotProtoOption] -> DotProtoType -> m (Maybe HsExp)
wrapFunE overTyCon fc stringType ctxt opts dpt =
  coerceE overTyCon (isMap dpt)
    <$> dptToHsType fc stringType ctxt dpt
    <*> dptToHsTypeWrapped fc stringType opts ctxt dpt

wrapE :: MonadError CompileError m => FieldContext -> StringType -> TypeContext -> [DotProtoOption] -> DotProtoType -> HsExp -> m HsExp
wrapE fc stringType ctxt opts dpt e =
  maybeModify e <$> wrapFunE False fc stringType ctxt opts dpt

unwrapFunE :: MonadError CompileError m => Bool -> FieldContext -> StringType -> TypeContext -> [DotProtoOption] -> DotProtoType -> m (Maybe HsExp)
unwrapFunE overTyCon fc stringType ctxt opts dpt =
  coerceE overTyCon (isMap dpt)
    <$> dptToHsTypeWrapped fc stringType opts ctxt dpt
    <*> dptToHsType fc stringType ctxt dpt

unwrapE :: MonadError CompileError m => FieldContext -> StringType -> TypeContext -> [DotProtoOption] -> DotProtoType -> HsExp -> m HsExp
unwrapE fc stringType ctxt opts dpt e = do
  maybeModify e <$> unwrapFunE True fc stringType ctxt opts dpt

--------------------------------------------------------------------------------
--
-- * Functions to convert 'DotProtoType' into Haskell types
--

-- | Convert a dot proto type to a Haskell type
dptToHsType :: MonadError CompileError m => FieldContext -> StringType -> TypeContext -> DotProtoType -> m HsType
dptToHsType fc = foldDPT (dptToHsContType fc) . dpptToHsType

-- | Convert a dot proto type to a wrapped Haskell type
dptToHsTypeWrapped
  :: MonadError CompileError m
  => FieldContext
  -> StringType
  -> [DotProtoOption]
  -> TypeContext
  -> DotProtoType
  -> m HsType
dptToHsTypeWrapped fc stringType opts =
  foldDPT
    -- The wrapper for the collection type replaces the native haskell
    -- collection type, so try that first.
    (\ctxt ty -> maybe (dptToHsContType fc ctxt ty) id (dptToHsWrappedContType fc ctxt opts ty))
    -- Always wrap the primitive type.
    (dpptToHsTypeWrapped stringType)

-- | Like 'dptToHsTypeWrapped' but without use of
-- 'dptToHsContType' or 'dptToHsWrappedContType'.
dpptToHsTypeWrapped
  :: MonadError CompileError m
  => StringType
  -> TypeContext
  -> DotProtoPrimType
  -> m HsType
dpptToHsTypeWrapped (StringType _ stringType) ctxt =  \case
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
          HsTyApp (protobufType_ "Enumerated") <$> msgTypeFromDpTypeInfo ctxt ty msgName
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
      Map k v  | validMapKey k -> HsTyApp . cont <$> prim k <*> go (Prim v) -- need to 'Nest' message types
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
                            -> Just $ HsTyApp (protobufType_ "Nested")
  Repeated (Named tyName)
    | isMessage ctxt tyName -> Just $ HsTyApp (protobufType_ "NestedVec")
  Repeated ty
    | isUnpacked opts       -> Just $ HsTyApp (protobufType_ "UnpackedVec")
    | isPacked opts         -> Just $ HsTyApp (protobufType_ "PackedVec")
    | isPackable ctxt ty    -> Just $ HsTyApp (protobufType_ "PackedVec")
    | otherwise             -> Just $ HsTyApp (protobufType_ "UnpackedVec")
  _ -> Nothing

-- | Translate DotProtoType to Haskell container types.
--
-- When the given 'FieldContext' is 'WithinOneOf' we do not wrap submessages
-- in "Maybe" because the entire oneof is already wrapped in a "Maybe".
dptToHsContType :: FieldContext -> TypeContext -> DotProtoType -> HsType -> HsType
dptToHsContType fc ctxt = \case
  Prim (Named tyName) | WithinMessage <- fc, isMessage ctxt tyName
                     -> HsTyApp $ primType_ "Maybe"
  Repeated _         -> HsTyApp $ primType_ "Vector"
  NestedRepeated _   -> HsTyApp $ primType_ "Vector"
  Map _ _            -> HsTyApp $ primType_ "Map"
  _                  -> id

-- | Convert a dot proto prim type to an unwrapped Haskell type
dpptToHsType :: MonadError CompileError m
             => StringType
             -> TypeContext
             -> DotProtoPrimType
             -> m HsType
dpptToHsType (StringType _ stringType) ctxt = \case
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
          HsTyApp (protobufType_ "Enumerated") <$> msgTypeFromDpTypeInfo ctxt ty msgName
      Just ty -> msgTypeFromDpTypeInfo ctxt ty msgName
      Nothing -> noSuchTypeError msgName

validMapKey :: DotProtoPrimType -> Bool
validMapKey = (`elem` [ Int32, Int64, SInt32, SInt64, UInt32, UInt64
                      , Fixed32, Fixed64, SFixed32, SFixed64
                      , String, Bool])


--------------------------------------------------------------------------------
--
-- * Code generation
--

-- ** Generate instances for a 'DotProto' package

dotProtoDefinitionD :: MonadError CompileError m
                    => StringType
                    -> RecordStyle
                    -> IsPrefixed
                    -> DotProtoPackageSpec
                    -> TypeContext
                    -> DotProtoDefinition
                    -> m [HsDecl]
dotProtoDefinitionD stringType recordStyle isPrefixed pkgSpec ctxt = \case
  DotProtoMessage _ messageName messageParts ->
    dotProtoMessageD stringType recordStyle isPrefixed ctxt Anonymous messageName messageParts

  DotProtoEnum _ enumName enumParts ->
    dotProtoEnumD Anonymous enumName enumParts

  DotProtoService _ serviceName serviceParts ->
    dotProtoServiceD stringType isPrefixed pkgSpec ctxt serviceName serviceParts

-- | Generate 'Named' instance for a type in this package
namedInstD :: String -> HsDecl
namedInstD messageName =
  instDecl_ (protobufName "Named")
      [ type_ messageName ]
      [ HsFunBind [nameOfDecl] ]
  where
    nameOfDecl = match_ (HsIdent "nameOf") [HsPWildCard]
                        (HsUnGuardedRhs (apply fromStringE
                                               [ str_ messageName ]))
                        []

hasDefaultInstD :: String -> HsDecl
hasDefaultInstD messageName =
  instDecl_ (protobufName "HasDefault")
      [ type_ messageName ]
      [ ]

-- ** Generate types and instances for .proto messages

-- | Generate data types, 'Bounded', 'Enum', 'FromJSONPB', 'Named', 'Message',
--   'ToJSONPB' instances as appropriate for the given 'DotProtoMessagePart's
dotProtoMessageD
    :: forall m
     . MonadError CompileError m
    => StringType
    -> RecordStyle
    -> IsPrefixed
    -> TypeContext
    -> DotProtoIdentifier
    -> DotProtoIdentifier
    -> [DotProtoMessagePart]
    -> m [HsDecl]
dotProtoMessageD stringType recordStyle isPrefixed ctxt parentIdent messageIdent messageParts = do
    messageName <- qualifiedMessageName parentIdent messageIdent

    let mkDataDecl flds =
          dataDecl_ messageName
            [ recDecl_ (HsIdent messageName) flds ]
            defaultMessageDeriving

    let getName = \case
          DotProtoMessageField fld -> (: []) <$> getFieldNameForSchemaInstanceDeclaration fld
          DotProtoMessageOneOf ident _ -> (: []) . (Nothing, ) <$> dpIdentUnqualName ident
          _ -> pure []

    messageDataDecl <- mkDataDecl <$> foldMapM (messagePartFieldD messageName) messageParts

    foldMapM id
      [ sequence
          [ pure messageDataDecl
          , pure (nfDataInstD messageDataDecl messageName)
          , pure (namedInstD messageName)
          , pure (hasDefaultInstD messageName)
          , messageInstD stringType isPrefixed ctxt' parentIdent messageIdent messageParts

          , toJSONPBMessageInstD stringType isPrefixed ctxt' parentIdent messageIdent messageParts
          , fromJSONPBMessageInstD stringType isPrefixed ctxt' parentIdent messageIdent messageParts

            -- Generate Aeson instances in terms of JSONPB instances
          , pure (toJSONInstDecl messageName)
          , pure (fromJSONInstDecl messageName)

#ifdef SWAGGER
          -- And the Swagger ToSchema instance corresponding to JSONPB encodings
          , toSchemaInstanceDeclaration stringType isPrefixed ctxt' messageName Nothing
              =<< foldMapM getName messageParts
#endif

#ifdef DHALL
          -- Generate Dhall instances
          , pure (dhallInterpretInstDecl messageName)
          , pure (dhallInjectInstDecl messageName)
#endif
          ]

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

    nfDataInstD = case recordStyle of
                    RegularRecords -> RegularRecord.nfDataInstD
                    LargeRecords -> LargeRecord.nfDataInstD

    messagePartFieldD :: String -> DotProtoMessagePart -> m [([HsName], HsBangType)]
    messagePartFieldD messageName (DotProtoMessageField DotProtoField{..}) = do
      fullName <- prefixedFieldNameWithFlag isPrefixed messageName =<< dpIdentUnqualName dotProtoFieldName
      fullTy <- dptToHsType WithinMessage stringType ctxt' dotProtoFieldType
      pure [ ([HsIdent fullName], HsUnBangedTy fullTy ) ]

    messagePartFieldD messageName (DotProtoMessageOneOf fieldName _) = do
      fullName <- prefixedFieldNameWithFlag isPrefixed messageName =<< dpIdentUnqualName fieldName
      qualTyName <- prefixedConName messageName =<< dpIdentUnqualName fieldName
      let fullTy = HsTyApp (HsTyCon (haskellName "Maybe")) . type_ $ qualTyName
      pure [ ([HsIdent fullName], HsUnBangedTy fullTy) ]

    messagePartFieldD _ _ = pure []

    nestedDecls :: DotProtoDefinition -> m [HsDecl]
    nestedDecls (DotProtoMessage _ subMsgName subMessageDef) = do
      parentIdent' <- concatDotProtoIdentifier parentIdent messageIdent
      dotProtoMessageD stringType recordStyle isPrefixed ctxt' parentIdent' subMsgName subMessageDef

    nestedDecls (DotProtoEnum _ subEnumName subEnumDef) = do
      parentIdent' <- concatDotProtoIdentifier parentIdent messageIdent
      dotProtoEnumD parentIdent' subEnumName subEnumDef

    nestedDecls _ = pure []

    nestedOneOfDecls :: String -> DotProtoIdentifier -> [DotProtoField] -> m [HsDecl]
    nestedOneOfDecls messageName identifier fields = do
      fullName <- prefixedConName messageName =<< dpIdentUnqualName identifier

      (cons, idents) <- fmap unzip (mapM (oneOfCons fullName) fields)

#ifdef SWAGGER
      toSchemaInstance <- toSchemaInstanceDeclaration stringType isPrefixed ctxt' fullName (Just idents)
                            =<< mapM getFieldNameForSchemaInstanceDeclaration fields
#endif

      let nestedDecl = dataDecl_ fullName cons defaultMessageDeriving
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
       consTy <- dptToHsType WithinOneOf stringType ctxt' dotProtoFieldType
       consName <- prefixedConName fullName =<< dpIdentUnqualName dotProtoFieldName
       let ident = HsIdent consName
       pure (conDecl_ ident [HsUnBangedTy consTy], ident)

    oneOfCons _ DotProtoEmptyField = internalError "field type : empty field"

-- *** Generate Protobuf 'Message' instances

messageInstD
    :: forall m
     . MonadError CompileError m
    => StringType
    -> IsPrefixed
    -> TypeContext
    -> DotProtoIdentifier
    -> DotProtoIdentifier
    -> [DotProtoMessagePart]
    -> m HsDecl
messageInstD stringType isPrefixed ctxt parentIdent msgIdent messageParts = do
     msgName         <- qualifiedMessageName parentIdent msgIdent
     qualifiedFields <- getQualifiedFields isPrefixed msgName messageParts

     encodedFields   <- mapM encodeMessageField qualifiedFields
     decodedFields   <- mapM decodeMessageField qualifiedFields

     let encodeMessageDecl = match_ (HsIdent "encodeMessage")
                                    [HsPWildCard, HsPRec (unqual_ msgName) punnedFieldsP]
                                    (HsUnGuardedRhs encodeMessageE) []

         encodeMessageE = apply mconcatE [HsList encodedFields]

         punnedFieldsP = map (fp . coerce . recordFieldName) qualifiedFields
           where fp nm = HsPFieldPat (unqual_ nm) (HsPVar (HsIdent nm))


     let decodeMessageDecl = match_ (HsIdent "decodeMessage") [ HsPWildCard ]
                                    (HsUnGuardedRhs decodeMessageE) []

         decodeMessageE = foldl (\f -> HsInfixApp f apOp)
                                (apply pureE [ uvar_ msgName ])
                                decodedFields

     let dotProtoDecl = match_ (HsIdent "dotProto") [HsPWildCard]
                               (HsUnGuardedRhs dotProtoE) []

         dotProtoE = HsList $ do
           DotProtoMessageField DotProtoField{..} <- messageParts
           pure $ apply dotProtoFieldC
                        [ fieldNumberE dotProtoFieldNumber
                        , dpTypeE dotProtoFieldType
                        , dpIdentE dotProtoFieldName
                        , HsList (map optionE dotProtoFieldOptions)
                        , str_ dotProtoFieldComment
                        ]


     pure $ instDecl_ (protobufName "Message")
                      [ type_ msgName ]
                      [ HsFunBind [ encodeMessageDecl ]
                      , HsFunBind [ decodeMessageDecl ]
                      , HsFunBind [ dotProtoDecl ]
                      ]
  where
    encodeMessageField :: QualifiedField -> m HsExp
    encodeMessageField QualifiedField{recordFieldName, fieldInfo} =
      let recordFieldName' = uvar_ (coerce recordFieldName) in
      case fieldInfo of
        FieldNormal _fieldName fieldNum dpType options -> do
            fieldE <- wrapE WithinMessage stringType ctxt options dpType recordFieldName'
            pure $ apply encodeMessageFieldE [ fieldNumberE fieldNum, fieldE ]

        FieldOneOf OneofField{subfields} -> do
            alts <- mapM mkAlt subfields
            pure $ HsCase recordFieldName'
                    [ alt_ (HsPApp (haskellName "Nothing") [])
                           (HsUnGuardedAlt memptyE)
                           []
                    , alt_ (HsPApp (haskellName "Just") [patVar "x"])
                           (HsUnGuardedAlt (HsCase (uvar_ "x") alts))
                           []
                    ]
          where
            -- Create all pattern match & expr for each constructor:
            --    Constructor y -> encodeMessageField num (Nested (Just y)) -- for embedded messages
            --    Constructor y -> encodeMessageField num (ForceEmit y)     -- for everything else
            mkAlt (OneofSubfield fieldNum conName _ dpType options) = do
              let isMaybe
                     | Prim (Named tyName) <- dpType
                     = isMessage ctxt tyName
                     | otherwise
                     = False

              let wrapJust = HsParen . HsApp (HsVar (haskellName "Just"))

              xE <- (if isMaybe then id else fmap forceEmitE)
                     . wrapE WithinMessage stringType ctxt options dpType
                         -- For now we use 'WithinMessage' to preserve
                         -- the historical approach of treating this field
                         -- as if it were an ordinary non-oneof field that
                         -- just happens to be present.
                     . (if isMaybe then wrapJust else id)
                     $ uvar_ "y"

              pure $ alt_ (HsPApp (unqual_ conName) [patVar "y"])
                          (HsUnGuardedAlt (apply encodeMessageFieldE [fieldNumberE fieldNum, xE]))
                          []


    decodeMessageField :: QualifiedField -> m HsExp
    decodeMessageField QualifiedField{fieldInfo} =
      case fieldInfo of
        FieldNormal _fieldName fieldNum dpType options ->
            unwrapE WithinMessage stringType ctxt options dpType $
              apply atE [ decodeMessageFieldE, fieldNumberE fieldNum ]

        FieldOneOf OneofField{subfields} -> do
            parsers <- mapM subfieldParserE subfields
            pure $  apply oneofE [ HsVar (haskellName "Nothing")
                                 , HsList parsers
                                 ]
          where
            -- create a list of (fieldNumber, Cons <$> parser)
            subfieldParserE (OneofSubfield fieldNumber consName _ dpType options) = do
              let fE | Prim (Named tyName) <- dpType, isMessage ctxt tyName
                     = HsParen (HsApp fmapE (uvar_ consName))
                     | otherwise
                     = HsParen (HsInfixApp (HsVar (haskellName "Just"))
                                           composeOp
                                           (uvar_ consName))

              -- For now we continue the historical practice of parsing
              -- submessages within oneofs as if were outside of oneofs,
              -- and replacing the "Just . Ctor" with "fmap . Ctor".
              -- That is why we do not pass WithinOneOf.
              alts <- unwrapE WithinMessage stringType ctxt options dpType decodeMessageFieldE

              pure $ HsTuple
                   [ fieldNumberE fieldNumber
                   , HsInfixApp (apply pureE [ fE ]) apOp alts
                   ]


-- *** Generate ToJSONPB/FromJSONPB instances

toJSONPBMessageInstD
    :: forall m
     . MonadError CompileError m
    => StringType
    -> IsPrefixed
    -> TypeContext
    -> DotProtoIdentifier
    -> DotProtoIdentifier
    -> [DotProtoMessagePart]
    -> m HsDecl
toJSONPBMessageInstD stringType isPrefixed ctxt parentIdent msgIdent messageParts = do
    msgName    <- qualifiedMessageName parentIdent msgIdent
    qualFields <- getQualifiedFields isPrefixed msgName messageParts

    let applyE nm oneofNm = do
          fs <- traverse (encodeMessageField oneofNm) qualFields
          pure $ apply (HsVar (jsonpbName nm)) [HsList fs]

    let patBinder = foldQF (const fieldBinder) (oneofSubDisjunctBinder . subfields)
    let matchE nm appNm oneofAppNm = do
          rhs <- applyE appNm oneofAppNm
          pure $ match_
            (HsIdent nm)
            [ HsPApp (unqual_ msgName)
                     (patVar . patBinder <$> qualFields) ]
            (HsUnGuardedRhs rhs)
            []

    toJSONPB <- matchE "toJSONPB" "object" "objectOrNull"
    toEncoding <- matchE "toEncodingPB" "pairs" "pairsOrNull"

    pure $ instDecl_ (jsonpbName "ToJSONPB")
                     [ type_ msgName ]
                     [ HsFunBind [toJSONPB]
                     , HsFunBind [toEncoding]
                     ]

  where
    encodeMessageField :: String -> QualifiedField -> m HsExp
    encodeMessageField oneofNm (QualifiedField _ fieldInfo) =
      case fieldInfo of
        FieldNormal fldName fldNum dpType options ->
          defPairE fldName fldNum dpType options
        FieldOneOf oo ->
          oneofCaseE oneofNm oo

    -- E.g.
    -- "another" .= f2 -- always succeeds (produces default value on missing field)
    defPairE fldName fldNum dpType options = do
      w <- wrapE WithinMessage stringType ctxt options dpType (uvar_ (fieldBinder fldNum))
      pure $ HsInfixApp (str_ (coerce fldName)) toJSONPBOp w

    -- E.g.
    -- HsJSONPB.pair "name" f4 -- fails on missing field
    oneOfPairE fldNm varNm options dpType = do
      w <- wrapE WithinOneOf stringType ctxt options dpType (uvar_ varNm)
      pure $ apply (HsVar (jsonpbName "pair")) [str_ (coerce fldNm), w]

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
        pure $ HsParen
          $ HsLet [ HsFunBind [ match_ (HsIdent caseName) [] (HsUnGuardedRhs (caseExpr altEs)) [] ] ]
          $ HsLambda defaultSrcLoc [patVar optsStr] (HsIf dontInline noInline yesInline)
      where
        optsStr = "options"
        opts    = uvar_ optsStr

        caseName = "encode" <> over (ix 0) toUpper typeName
        caseBnd = uvar_ caseName

        dontInline = HsApp (HsVar (jsonpbName "optEmitNamedOneof")) opts

        noInline = HsApp (HsParen (HsInfixApp (str_ typeName)
                                              toJSONPBOp
                                              (apply (HsVar (jsonpbName retJsonCtor))
                                                     [ HsList [caseBnd], opts ])))
                         opts

        yesInline = HsApp caseBnd opts

        altE sub@(OneofSubfield _ conName pbFldNm dpType options) = do
          let patVarNm = oneofSubBinder sub
          p <- oneOfPairE pbFldNm patVarNm options dpType
          pure $ alt_ (HsPApp (haskellName "Just")
                              [ HsPParen
                                (HsPApp (unqual_ conName) [patVar patVarNm])
                              ]
                      )
                      (HsUnGuardedAlt p)
                      []

        -- E.g.
        -- case f4_or_f9 of
        --   Just (SomethingPickOneName f4)
        --     -> HsJSONPB.pair "name" f4
        --   Just (SomethingPickOneSomeid f9)
        --     -> HsJSONPB.pair "someid" f9
        --   Nothing
        --     -> mempty
        caseExpr altEs = HsParen $
            HsCase disjunctName (altEs <> [fallthroughE])
          where
            disjunctName = uvar_ (oneofSubDisjunctBinder subfields)
            fallthroughE =
              alt_ (HsPApp (haskellName "Nothing") [])
                   (HsUnGuardedAlt memptyE)
                   []

fromJSONPBMessageInstD
    :: forall m
     . MonadError CompileError m
    => StringType
    -> IsPrefixed
    -> TypeContext
    -> DotProtoIdentifier
    -> DotProtoIdentifier
    -> [DotProtoMessagePart]
    -> m HsDecl
fromJSONPBMessageInstD stringType isPrefixed ctxt parentIdent msgIdent messageParts = do
    msgName    <- qualifiedMessageName parentIdent msgIdent
    qualFields <- getQualifiedFields isPrefixed msgName messageParts

    fieldParsers <- traverse parseField qualFields

    let parseJSONPBE =
          apply (HsVar (jsonpbName "withObject"))
                [ str_ msgName
                , HsParen (HsLambda defaultSrcLoc [lambdaPVar] fieldAps)
                ]
          where
            fieldAps = foldl (\f -> HsInfixApp f apOp)
                             (apply pureE [ uvar_ msgName ])
                             fieldParsers

    let parseJSONPBDecl =
          match_ (HsIdent "parseJSONPB") [] (HsUnGuardedRhs parseJSONPBE) []

    pure (instDecl_ (jsonpbName "FromJSONPB")
                   [ type_ msgName ]
                   [ HsFunBind [ parseJSONPBDecl ] ])
  where
    lambdaPVar = patVar "obj"
    lambdaVar  = uvar_ "obj"

    parseField (QualifiedField _ (FieldNormal fldName _ dpType options)) =
      normalParserE fldName dpType options
    parseField (QualifiedField _ (FieldOneOf fld)) =
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
        pure $ HsParen $
          HsLet [ HsFunBind [ match_ (HsIdent letBndStr) [patVar letArgStr ]
                                     (HsUnGuardedRhs ds) []
                            ]
                ]
                (HsInfixApp parseWrapped altOp parseUnwrapped)
      where
        oneofTyLit = str_ oneofType -- FIXME

        letBndStr  = "parse" <> over (ix 0) toUpper oneofType
        letBndName = uvar_ letBndStr
        letArgStr  = "parseObj"
        letArgName = uvar_ letArgStr

        parseWrapped = HsParen $
          HsInfixApp (HsParen (HsInfixApp lambdaVar parseJSONPBOp oneofTyLit))
                     bindOp
                     (apply (HsVar (jsonpbName "withObject")) [ oneofTyLit , letBndName ])

        parseUnwrapped = HsParen (HsApp letBndName lambdaVar)

        -- parseSomethingNameOrId parseObj =
        --   Hs.msum
        --     [ (Just . SomethingPickOneName) <$> (HsJSONPB.parseField parseObj "name")
        --     , (Just . SomethingPickOneSomeid) <$> (HsJSONPB.parseField parseObj "someid")
        --     , pure Nothing
        --     ]
        tryParseDisjunctsE = do
          fs <- traverse subParserE fields
          pure $ HsApp msumE (HsList (fs <> fallThruE))

        fallThruE = [ HsApp pureE (HsVar (haskellName "Nothing")) ]

        subParserE OneofSubfield{subfieldConsName, subfieldName,
                                 subfieldType, subfieldOptions} = do
          maybeCoercion <-
            unwrapFunE False WithinOneOf stringType ctxt subfieldOptions subfieldType
          let inject = (HsInfixApp (HsVar (haskellName "Just"))
                                   composeOp
                                   (uvar_ subfieldConsName))
          pure $ HsInfixApp
              (maybe inject (HsInfixApp inject composeOp) maybeCoercion)
              fmapOp
              (apply (HsVar (jsonpbName "parseField"))
                     [ letArgName
                     , str_ (coerce subfieldName)])

    -- E.g. obj .: "someid"
    normalParserE :: FieldName -> DotProtoType -> [DotProtoOption] -> m HsExp
    normalParserE fldName dpType options =
      unwrapE WithinMessage stringType ctxt options dpType $
        HsInfixApp lambdaVar
                   parseJSONPBOp
                   (str_(coerce fldName))

-- *** Generate default Aeson To/FromJSON and Swagger ToSchema instances
-- (These are defined in terms of ToJSONPB)

toJSONInstDecl :: String -> HsDecl
toJSONInstDecl typeName =
  instDecl_ (jsonpbName "ToJSON")
            [ type_ typeName ]
            [ HsFunBind [ match_ (HsIdent "toJSON") []
                                 (HsUnGuardedRhs (HsVar (jsonpbName "toAesonValue"))) []
                        ]
            , HsFunBind [ match_ (HsIdent "toEncoding") []
                                 (HsUnGuardedRhs (HsVar (jsonpbName "toAesonEncoding"))) []
                        ]
            ]

fromJSONInstDecl :: String -> HsDecl
fromJSONInstDecl typeName =
  instDecl_ (jsonpbName "FromJSON")
            [ type_ typeName ]
            [ HsFunBind [match_ (HsIdent "parseJSON") []
                                (HsUnGuardedRhs (HsVar (jsonpbName "parseJSONPB"))) []
                        ]
            ]


-- *** Generate `ToSchema` instance

getFieldNameForSchemaInstanceDeclaration
  :: MonadError CompileError m
  => DotProtoField
  -> m (Maybe ([DotProtoOption], DotProtoType), String)
getFieldNameForSchemaInstanceDeclaration fld = do
  unqual <- dpIdentUnqualName (dotProtoFieldName fld)
  let optsType = (dotProtoFieldOptions fld, dotProtoFieldType fld)
  pure (Just optsType, unqual)

toSchemaInstanceDeclaration
    :: MonadError CompileError m
    => StringType
    -> IsPrefixed
    -> TypeContext
    -> String
    -- ^ Name of the message type to create an instance for
    -> Maybe [HsName]
    -- ^ Oneof constructors
    -> [(Maybe ([DotProtoOption], DotProtoType), String)]
    -- ^ Field names, with every field that is not actually a oneof
    -- combining fields paired with its options and protobuf type
    -> m HsDecl
toSchemaInstanceDeclaration stringType isPrefixed ctxt messageName maybeConstructors fieldNamesEtc = do
  let fieldNames = map snd fieldNamesEtc

  qualifiedFieldNames <- mapM (prefixedFieldNameWithFlag isPrefixed messageName) fieldNames

  let messageConstructor = HsCon (UnQual (HsIdent messageName))

  let _namedSchemaNameExpression = HsApp justC (str_ messageName)

#ifdef SWAGGER
      -- { _paramSchemaType = HsJSONPB.SwaggerObject
      -- }
  let paramSchemaUpdates =
        [ HsFieldUpdate _paramSchemaType _paramSchemaTypeExpression
        ]
        where
          _paramSchemaType = jsonpbName "_paramSchemaType"

#if MIN_VERSION_swagger2(2,4,0)
          _paramSchemaTypeExpression = HsApp justC (HsVar (jsonpbName "SwaggerObject"))
#else
          _paramSchemaTypeExpression = HsVar (jsonpbName "SwaggerObject")
#endif
#else
  let paramSchemaUpdates = []
#endif

  let _schemaParamSchemaExpression = HsRecUpdate memptyE paramSchemaUpdates

      -- [ ("fieldName0", qualifiedFieldName0)
      -- , ("fieldName1", qualifiedFieldName1)
      -- ...
      -- ]
  let properties = HsList $ do
        (fieldName, qualifiedFieldName) <- zip fieldNames qualifiedFieldNames
        return (HsTuple [ str_  fieldName, uvar_ qualifiedFieldName ])

  let _schemaPropertiesExpression =
        HsApp (HsVar (jsonpbName "insOrdFromList")) properties

      -- { _schemaParamSchema = ...
      -- , _schemaProperties  = ...
      -- , ...
      -- }
  let schemaUpdates = normalUpdates ++ extraUpdates
        where
          normalUpdates =
            [ HsFieldUpdate _schemaParamSchema _schemaParamSchemaExpression
            , HsFieldUpdate _schemaProperties  _schemaPropertiesExpression
            ]

          extraUpdates =
            case maybeConstructors of
                Just _ ->
                  [ HsFieldUpdate _schemaMinProperties justOne
                  , HsFieldUpdate _schemaMaxProperties justOne
                  ]
                Nothing ->
                  []

          _schemaParamSchema    = jsonpbName "_schemaParamSchema"
          _schemaProperties     = jsonpbName "_schemaProperties"
          _schemaMinProperties  = jsonpbName "_schemaMinProperties"
          _schemaMaxProperties  = jsonpbName "_schemaMaxProperties"

          justOne = HsApp justC (HsLit (HsInt 1))

  let _namedSchemaSchemaExpression = HsRecUpdate memptyE schemaUpdates

      -- { _namedSchemaName   = ...
      -- , _namedSchemaSchema = ...
      -- }
  let namedSchemaUpdates =
        [ HsFieldUpdate _namedSchemaName   _namedSchemaNameExpression
        , HsFieldUpdate _namedSchemaSchema _namedSchemaSchemaExpression
        ]
        where
          _namedSchemaName   = jsonpbName "_namedSchemaName"
          _namedSchemaSchema = jsonpbName "_namedSchemaSchema"

  let namedSchema = HsRecConstr (jsonpbName "NamedSchema") namedSchemaUpdates

  let toDeclareName fieldName = "declare_" ++ fieldName

  let toArgument fc (maybeOptsType, fieldName) =
          maybe pure (uncurry (unwrapE fc stringType ctxt)) maybeOptsType $
            HsApp asProxy declare
        where
          declare = uvar_ (toDeclareName fieldName)
          asProxy = HsVar (jsonpbName "asProxy")

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

              let declareIdentifier = HsIdent (toDeclareName fieldName)

              let stmt0 = HsLetStmt [ HsFunBind
                                      [ HsMatch defaultSrcLoc declareIdentifier []
                                                 (HsUnGuardedRhs (HsVar (jsonpbName "declareSchemaRef"))) []
                                      ]
                                    ]

              let stmt1 = HsGenerator defaultSrcLoc (HsPVar (HsIdent qualifiedFieldName))
                                        (HsApp (HsVar (UnQual declareIdentifier))
                                               (HsCon (proxyName "Proxy")))
              [ stmt0, stmt1]

        inferenceStatement <- do
          arguments <- traverse (toArgument WithinMessage) fieldNamesEtc
          let patternBind = HsPatBind defaultSrcLoc HsPWildCard
                (HsUnGuardedRhs (applicativeApply messageConstructor arguments)) []
          pure $ if null fieldNames then [] else [ HsLetStmt [ patternBind ] ]

        let returnStatement = HsQualifier (HsApp returnE (HsParen namedSchema))

        pure $ HsDo (bindingStatements ++ inferenceStatement ++ [ returnStatement ])

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
              let declareIdentifier = HsIdent (toDeclareName (snd fieldNameEtc))

              let stmt0 = HsLetStmt [ HsFunBind
                                        [ HsMatch defaultSrcLoc declareIdentifier []
                                                   (HsUnGuardedRhs (HsVar (jsonpbName "declareSchemaRef"))) []
                                        ]
                                    ]
              let stmt1 = HsGenerator defaultSrcLoc (HsPVar (HsIdent qualifiedFieldName))
                                        (HsApp (HsVar (UnQual declareIdentifier))
                                               (HsCon (proxyName "Proxy")))
              inferenceStatement <- do
                argument <- toArgument WithinOneOf fieldNameEtc
                let patternBind = HsPatBind defaultSrcLoc HsPWildCard
                      (HsUnGuardedRhs (applicativeApply (HsCon (UnQual constructor)) [ argument ])) []
                pure $ if null fieldNames then [] else [ HsLetStmt [ patternBind ] ]

              pure $ [stmt0, stmt1] ++ inferenceStatement

        bindingStatements <- foldMapM bindingStatement $
          zip3 fieldNamesEtc qualifiedFieldNames constructors

        let returnStatement = HsQualifier (HsApp returnE (HsParen namedSchema))

        pure $ HsDo (bindingStatements ++ [ returnStatement ])

  expression <- case maybeConstructors of
    Nothing           -> expressionForMessage
    Just constructors -> expressionForOneOf constructors

  let instanceDeclaration =
          instDecl_ className [ classArgument ] [ classDeclaration ]
        where
          className = jsonpbName "ToSchema"

          classArgument = HsTyCon (UnQual (HsIdent messageName))

          classDeclaration = HsFunBind [ match ]
            where
              match = match_ matchName [ HsPWildCard ] rightHandSide []
                where
                  rightHandSide = HsUnGuardedRhs expression

                  matchName = HsIdent "declareNamedSchema"

  return instanceDeclaration


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

  case enumeratorDecls of
    [] -> throwError $ EmptyEnumeration enumName
    (i, conIdent) : _
      | i == 0 -> return ()
      | otherwise -> throwError $ NonzeroFirstEnumeration enumName conIdent i

  enumCons <- sortBy (comparing fst) <$> traverse (traverse (fmap (prefixedEnumFieldName enumName) . dpIdentUnqualName)) enumeratorDecls

  let enumConNames = map snd enumCons

      minBoundD =
          [ match_ (HsIdent "minBound")
                   []
                   (HsUnGuardedRhs (uvar_ (head enumConNames)))
                   []
          ]

      maxBoundD =
          [ match_ (HsIdent "maxBound")
                   []
                   (HsUnGuardedRhs (uvar_ (last enumConNames)))
                   []
          ]

      compareD =
          [ match_ (HsIdent "compare")
                   [ patVar "x", patVar "y" ]
                   (HsUnGuardedRhs
                       (HsApp
                           (HsApp
                               (HsVar (haskellName "compare"))
                               (HsParen
                                   (HsApp (HsVar(protobufName "fromProtoEnum"))
                                          (uvar_ "x")
                                   )
                               )
                           )
                           (HsParen
                               (HsApp (HsVar (protobufName "fromProtoEnum"))
                                      (uvar_ "y")
                               )
                           )
                       )
                   )
                   []
          ]

      fromProtoEnumD =
          [ match_ (HsIdent "fromProtoEnum") [ HsPApp (unqual_ conName) [] ]
                   (HsUnGuardedRhs (intE conIdx))
                   []
          | (conIdx, conName) <- enumCons
          ]

      toProtoEnumMayD =
          [ match_ (HsIdent "toProtoEnumMay")
                   [ intP conIdx ]
                   (HsUnGuardedRhs (HsApp justC (uvar_ conName)))
                   []
          | (conIdx, conName) <- enumCons ] ++
          [ match_ (HsIdent "toProtoEnumMay")
                   [ HsPWildCard ]
                   (HsUnGuardedRhs nothingC)
                   []
          ]

      parseJSONPBDecls :: [HsMatch]
      parseJSONPBDecls = foldr ((:) . matchConName) [mismatch] enumConNames
        where
          matchConName conName = match_ (HsIdent "parseJSONPB") [pat conName]
                                        (HsUnGuardedRhs
                                           (HsApp pureE (uvar_ conName)))
                                        []

          pat nm = HsPApp (jsonpbName "String") [ HsPLit (HsString (tryStripEnumName nm)) ]

          tryStripEnumName = fromMaybe <*> stripPrefix enumName

          mismatch = match_ (HsIdent "parseJSONPB") [patVar "v"]
                            (HsUnGuardedRhs
                                  (apply (HsVar (jsonpbName "typeMismatch"))
                                    [ str_ enumName, uvar_ "v" ]))
                            []


      toJSONPBDecl =
        match_ (HsIdent "toJSONPB") [ patVar "x", HsPWildCard ]
          (HsUnGuardedRhs
             (HsApp (HsVar (jsonpbName "enumFieldString"))
                    (uvar_ "x")))
          []

      toEncodingPBDecl =
        match_ (HsIdent "toEncodingPB") [ patVar "x", HsPWildCard ]
          (HsUnGuardedRhs
             (HsApp (HsVar (jsonpbName "enumFieldEncoding"))
                    (uvar_ "x")))
          []

  pure [ dataDecl_ enumName
                   [ conDecl_ (HsIdent con) [] | con <- enumConNames ]
                   defaultEnumDeriving
       , namedInstD enumName
       , hasDefaultInstD enumName
       , instDecl_ (haskellName "Bounded") [ type_ enumName ]
                   [ HsFunBind minBoundD
                   , HsFunBind maxBoundD
                   ]
       , instDecl_ (haskellName "Ord") [ type_ enumName ]
                   [ HsFunBind compareD ]
       , instDecl_ (protobufName "ProtoEnum") [ type_ enumName ]
                   [ HsFunBind toProtoEnumMayD
                   , HsFunBind fromProtoEnumD
                   ]
       , instDecl_ (jsonpbName "ToJSONPB") [ type_ enumName ]
                   [ HsFunBind [toJSONPBDecl]
                   , HsFunBind [toEncodingPBDecl]
                   ]
       , instDecl_ (jsonpbName "FromJSONPB") [ type_ enumName ]
                   [ HsFunBind parseJSONPBDecls ]
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
       , instDecl_ (protobufName "Finite") [ type_ enumName ] []
       ]

-- ** Generate code for dot proto services

dotProtoServiceD
    :: MonadError CompileError m
    => StringType
    -> IsPrefixed
    -> DotProtoPackageSpec
    -> TypeContext
    -> DotProtoIdentifier
    -> [DotProtoServicePart]
    -> m [HsDecl]
dotProtoServiceD stringType isPrefixed pkgSpec ctxt serviceIdent service = do
     serviceName <- typeLikeName =<< dpIdentUnqualName serviceIdent

     endpointPrefix <-
       case pkgSpec of
         DotProtoPackageSpec pkgIdent -> do
           packageName <- dpIdentQualName pkgIdent
           pure $ "/" ++ packageName ++ "." ++ serviceName ++ "/"
         DotProtoNoPackage -> pure $ "/" ++ serviceName ++ "/"

     let serviceFieldD (DotProtoServiceRPCMethod RPCMethod{..}) = do
           fullName <- prefixedMethodNameWithFlag isPrefixed serviceName =<< dpIdentUnqualName rpcMethodName

           methodName <- case rpcMethodName of
                           Single nm -> pure nm
                           _ -> invalidMethodNameError rpcMethodName

           requestTy  <- dpptToHsType stringType ctxt (Named rpcMethodRequestType)

           responseTy <- dpptToHsType stringType ctxt (Named rpcMethodResponseType)

           let streamingType =
                 case (rpcMethodRequestStreaming, rpcMethodResponseStreaming) of
                   (Streaming, Streaming)       -> biDiStreamingC
                   (Streaming, NonStreaming)    -> clientStreamingC
                   (NonStreaming, Streaming)    -> serverStreamingC
                   (NonStreaming, NonStreaming) -> normalC

           pure [ ( endpointPrefix ++ methodName
                  , fullName, rpcMethodRequestStreaming, rpcMethodResponseStreaming
                  , HsUnBangedTy $
                    HsTyFun (tyApp (HsTyVar (HsIdent "request"))
                                   [streamingType, requestTy, responseTy])
                            (tyApp ioT
                                   [tyApp (HsTyVar (HsIdent "response"))
                                          [streamingType, responseTy]
                                   ]
                            )
                  )
                ]

         serviceFieldD _ = pure []

     fieldsD <- foldMapM serviceFieldD service

     serverFuncName <- prefixedFieldName serviceName "server"
     clientFuncName <- prefixedFieldName serviceName "client"

     let conDecl = recDecl_ (HsIdent serviceName)
                            [ ([HsIdent hsName], ty) | (_, hsName, _, _, ty) <- fieldsD ]

     let serverT = tyApp (HsTyCon (unqual_ serviceName))
                         [ serverRequestT, serverResponseT ]

     let serviceServerTypeD =
            HsTypeSig defaultSrcLoc [ HsIdent serverFuncName ]
                        (HsQualType [] (HsTyFun serverT (HsTyFun serviceOptionsC ioActionT)))

     let serviceServerD = HsFunBind [serverFuncD]
           where
             serverFuncD =
               match_ (HsIdent serverFuncName)
                      [ HsPRec (unqual_ serviceName)
                               [ HsPFieldPat (unqual_ methodName) (HsPVar (HsIdent methodName))
                               | (_, methodName, _, _, _) <- fieldsD
                               ]
                      , HsPApp (unqual_ "ServiceOptions")
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
                      (HsUnGuardedRhs (apply serverLoopE [ serverOptsE ]))
                      []

             handlerE handlerC adapterE methodName hsName =
                 apply handlerC [ apply methodNameC [ str_ methodName ]
                                , apply adapterE [ uvar_ hsName ]
                                ]

             update u v = HsFieldUpdate (unqual_ u) (uvar_ v)

             serverOptsE = HsRecUpdate defaultOptionsE
                 [ HsFieldUpdate (grpcName "optNormalHandlers") $
                       HsList [ handlerE unaryHandlerC convertServerHandlerE endpointName hsName
                              | (endpointName, hsName, NonStreaming, NonStreaming, _) <- fieldsD
                              ]

                 , HsFieldUpdate (grpcName "optClientStreamHandlers") $
                       HsList [ handlerE clientStreamHandlerC convertServerReaderHandlerE endpointName hsName
                              | (endpointName, hsName, Streaming, NonStreaming, _) <- fieldsD
                              ]


                 , HsFieldUpdate (grpcName "optServerStreamHandlers") $
                       HsList [ handlerE serverStreamHandlerC convertServerWriterHandlerE endpointName hsName
                              | (endpointName, hsName, NonStreaming, Streaming, _) <- fieldsD
                              ]


                 , HsFieldUpdate (grpcName "optBiDiStreamHandlers") $
                       HsList [ handlerE biDiStreamHandlerC convertServerRWHandlerE endpointName hsName
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

     let clientT = tyApp (HsTyCon (unqual_ serviceName)) [ clientRequestT, clientResultT ]

     let serviceClientTypeD =
             HsTypeSig defaultSrcLoc [ HsIdent clientFuncName ]
                       (HsQualType [] (HsTyFun grpcClientT (HsTyApp ioT clientT)))

     let serviceClientD = HsFunBind [ clientFuncD ]
            where
              clientFuncD = match_ (HsIdent clientFuncName)
                                   [ HsPVar (HsIdent "client") ]
                                   ( HsUnGuardedRhs clientRecE ) []

              clientRecE = foldl (\f -> HsInfixApp f apOp)
                                 (apply pureE [ uvar_ serviceName ])
                                 [ HsParen $ HsInfixApp clientRequestE' apOp (registerClientMethodE endpointName)
                                 | (endpointName, _, _, _, _) <- fieldsD
                                 ]

              clientRequestE' = apply pureE [ apply clientRequestE [ uvar_ "client" ] ]

              registerClientMethodE endpoint =
                apply clientRegisterMethodE [ uvar_ "client"
                                            , apply methodNameC [ str_ endpoint ]
                                            ]

     pure [ HsDataDecl defaultSrcLoc  [] (HsIdent serviceName)
                [ HsIdent "request", HsIdent "response" ]
                [ conDecl ] defaultServiceDeriving

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

unaryHandlerC               = HsVar (grpcName "UnaryHandler")
clientStreamHandlerC        = HsVar (grpcName "ClientStreamHandler")
serverStreamHandlerC        = HsVar (grpcName "ServerStreamHandler")
biDiStreamHandlerC          = HsVar (grpcName "BiDiStreamHandler")
methodNameC                 = HsVar (grpcName "MethodName")
defaultOptionsE             = HsVar (grpcName "defaultOptions")
serverLoopE                 = HsVar (grpcName "serverLoop")
convertServerHandlerE       = HsVar (grpcName "convertGeneratedServerHandler")
convertServerReaderHandlerE = HsVar (grpcName "convertGeneratedServerReaderHandler")
convertServerWriterHandlerE = HsVar (grpcName "convertGeneratedServerWriterHandler")
convertServerRWHandlerE     = HsVar (grpcName "convertGeneratedServerRWHandler")
clientRegisterMethodE       = HsVar (grpcName "clientRegisterMethod")
clientRequestE              = HsVar (grpcName "clientRequest")

biDiStreamingC, serverStreamingC, clientStreamingC, normalC, serviceOptionsC,
  ioActionT, serverRequestT, serverResponseT, clientRequestT, clientResultT,
  ioT, grpcClientT :: HsType
biDiStreamingC   = HsTyCon (Qual (Module "'HsGRPC") (HsIdent "BiDiStreaming"))
serverStreamingC = HsTyCon (Qual (Module "'HsGRPC") (HsIdent "ServerStreaming"))
clientStreamingC = HsTyCon (Qual (Module "'HsGRPC") (HsIdent "ClientStreaming"))
normalC          = HsTyCon (Qual (Module "'HsGRPC") (HsIdent "Normal"))
serviceOptionsC  = HsTyCon (Qual (Module "HsGRPC") (HsIdent "ServiceOptions"))
serverRequestT   = HsTyCon (grpcName "ServerRequest")
serverResponseT  = HsTyCon (grpcName "ServerResponse")
clientRequestT   = HsTyCon (grpcName "ClientRequest")
clientResultT    = HsTyCon (grpcName "ClientResult")
grpcClientT      = HsTyCon (grpcName "Client")
ioActionT        = tyApp ioT [ HsTyTuple [] ]
ioT              = HsTyCon (haskellName "IO")

-- ** Expressions for protobuf-wire types

forceEmitE :: HsExp -> HsExp
forceEmitE = HsParen . HsApp forceEmitC

fieldNumberE :: FieldNumber -> HsExp
fieldNumberE = HsParen . HsApp fieldNumberC . intE . getFieldNumber

dpIdentE :: DotProtoIdentifier -> HsExp
dpIdentE (Single n)       = apply singleC [ str_ n ]
dpIdentE (Dots (Path (n NE.:| ns)))
  = apply dotsC [ apply pathC [ HsParen (HsInfixApp (str_ n) neConsOp (HsList (map str_ ns))) ] ]
dpIdentE (Qualified a b)  = apply qualifiedC [ dpIdentE a, dpIdentE b ]
dpIdentE Anonymous        = anonymousC

dpValueE :: DotProtoValue -> HsExp
dpValueE (Identifier nm) = apply identifierC [ dpIdentE nm ]
dpValueE (StringLit s)   = apply stringLitC  [ str_ s ]
dpValueE (IntLit i)      = apply intLitC     [ HsLit (HsInt (fromIntegral i)) ]
dpValueE (FloatLit f)    = apply floatLitC   [ HsLit (HsFrac (toRational f)) ]
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
    let wrap = HsVar . protobufASTName in
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

data ImportCustomisation = ImportCustomisation
  { icStringType :: StringType
  , icUsesGrpc :: Bool
  }

defaultImports :: RecordStyle -> ImportCustomisation -> [HsImportDecl]
defaultImports recordStyle ImportCustomisation{ icUsesGrpc, icStringType = StringType stringModule stringType} =
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
    , importDecl_ (m "Data.List.NonEmpty")    & qualified haskellNS  & selecting  [HsIThingAll (HsIdent "NonEmpty")]
    , importDecl_ (m "Data.Map")              & qualified haskellNS  & selecting  [i"Map", i"mapKeysMonotonic"]
    , importDecl_ (m "Data.Proxy")            & qualified proxyNS    & everything
    , importDecl_ (m "Data.String")           & qualified haskellNS  & selecting  [i"fromString"]
    , importDecl_ (m stringModule)            & qualified haskellNS  & selecting  [i stringType]
    , importDecl_ (m "Data.Vector")           & qualified haskellNS  & selecting  [i"Vector"]
    , importDecl_ (m "Data.Word")             & qualified haskellNS  & selecting  [i"Word16", i"Word32", i"Word64"]
    , importDecl_ (m "GHC.Enum")              & qualified haskellNS  & everything
    , importDecl_ (m "GHC.Generics")          & qualified haskellNS  & everything
    , importDecl_ (m "Google.Protobuf.Wrappers.Polymorphic") & qualified protobufNS & selecting [HsIThingAll (HsIdent "Wrapped")]
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
    case recordStyle of
      RegularRecords -> []
      LargeRecords ->
        [ importDecl_ (m "Data.Record.Generic")              & qualified lrNS  & everything
        , importDecl_ (m "Data.Record.Generic.Rep")          & qualified lrNS  & everything
        , importDecl_ (m "Data.Record.Generic.Rep.Internal") & qualified lrNS  & everything
        , importDecl_ (m "Data.Record.Plugin.Runtime")       & qualified lrNS  & everything
        ]
  where
    m = Module
    i = HsIVar . HsIdent
    s = HsIVar . HsSymbol

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
defaultMessageDeriving = map haskellName [ "Show", "Eq", "Ord", "Generic" ]

defaultEnumDeriving :: [HsQName]
defaultEnumDeriving = map haskellName [ "Show", "Eq", "Generic", "NFData" ]

defaultServiceDeriving :: [HsQName]
defaultServiceDeriving = map haskellName [ "Generic" ]
