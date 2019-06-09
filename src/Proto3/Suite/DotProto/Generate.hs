{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiWayIf                #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE ViewPatterns              #-}

{-| This module provides functions to generate Haskell declarations for protobuf
    messages
-}

module Proto3.Suite.DotProto.Generate
  ( CompileError(..)
  , TypeContext

  , compileDotProtoFile
  , compileDotProtoFileOrDie
  , hsModuleForDotProto
  , renderHsModuleForDotProto
  , readDotProtoWithContext
  ) where

import           Control.Applicative
import           Control.Arrow                  ((&&&))
import           Control.Monad.Except
import           Control.Lens                   ((^..), (<&>), ix, over, filtered)
import           Data.Bifunctor                 (first)
import           Data.Char
import           Data.Coerce
import           Data.Either                    (partitionEithers)
import           Data.List                      (find, intercalate, nub, sortBy,
                                                 stripPrefix)
import qualified Data.Map                       as M
import           Data.Maybe                     (fromMaybe)
import           Data.Monoid
import           Data.Ord                       (comparing)
import qualified Data.Set                       as S
import           Data.String                    (fromString)
import qualified Data.Text                      as T
import           Filesystem.Path.CurrentOS      ((</>), (<.>))
import qualified Filesystem.Path.CurrentOS      as FP
import           Language.Haskell.Pretty
import           Language.Haskell.Syntax
import           Language.Haskell.Parser        (ParseResult(..), parseModule)
import qualified NeatInterpolation              as Neat
import           Prelude                        hiding (FilePath)
import           Proto3.Suite.DotProto
import           Proto3.Suite.DotProto.Rendering (Pretty(..))
import           Proto3.Suite.DotProto.Internal
import           Proto3.Suite.DotProto.Generate.Common
import           Proto3.Wire.Types              (FieldNumber (..))
import           System.IO                      (writeFile, readFile)
import           Turtle                         (FilePath)
import qualified Turtle
import           Turtle.Format                  ((%))
import qualified Turtle.Format                  as F

--
-- * Public interface
--

-- | Generate a Haskell module corresponding to a @.proto@ file
compileDotProtoFile
    :: [FilePath]
    -- ^ Haskell modules containing instances used to override default generated
    -- instances
    -> FilePath
    -- ^ Output directory
    -> [FilePath]
    -- ^ List of search paths
    -> FilePath
    -- ^ Path to @.proto@ file (relative to search path)
    -> IO (Either CompileError ())
compileDotProtoFile extraInstanceFiles outputDirectory searchPaths dotProtoPath = runExceptT $ do
    (dotProto, importTypeContext) <- ExceptT (readDotProtoWithContext searchPaths dotProtoPath)

    let DotProto     { protoMeta      } = dotProto
    let DotProtoMeta { metaModulePath } = protoMeta
    let Path         { components     } = metaModulePath

    when (null components) (throwError InternalEmptyModulePath)

    typeLikeComponents <- traverse typeLikeName components

    let relativePath = FP.concat (map fromString typeLikeComponents) <.> "hs"
    let modulePath   = outputDirectory </> relativePath

    Turtle.mktree (Turtle.directory modulePath)

    extraInstances <- foldMapM getExtraInstances extraInstanceFiles

    haskellModule <- renderHsModuleForDotProto extraInstances dotProto importTypeContext

    liftIO (writeFile (FP.encodeString modulePath) haskellModule)

-- | As 'compileDotProtoFile', except terminates the program with an error
-- message on failure.
compileDotProtoFileOrDie
    :: [FilePath]
    -- ^ Haskell modules containing instances used to override default generated
    -- instances
    -> FilePath
    -- ^ Output directory
    -> [FilePath]
    -- ^ List of search paths
    -> FilePath
    -- ^ Path to @.proto@ file (relative to search path)
    -> IO ()
compileDotProtoFileOrDie extraInstanceFiles outputDirectory searchPaths dotProtoPath =
     compileDotProtoFile extraInstanceFiles outputDirectory searchPaths dotProtoPath >>= \case
         Left e -> do
           -- TODO: pretty print the error messages
           let errText          = Turtle.format Turtle.w  e
           let dotProtoPathText = Turtle.format Turtle.fp dotProtoPath
           dieLines [Neat.text|
             Error: failed to compile "${dotProtoPathText}":

             ${errText}
           |]
         _ -> pure ()

-- | Compile a 'DotProto' AST into a 'String' representing the Haskell
--   source of a module implementing types and instances for the .proto
--   messages and enums.
renderHsModuleForDotProto
    :: MonadError CompileError m
    => ([HsImportDecl],[HsDecl]) -> DotProto -> TypeContext -> m String
renderHsModuleForDotProto extraInstanceFiles dotProto importCtxt = do
    haskellModule <- hsModuleForDotProto extraInstanceFiles dotProto importCtxt
    return (T.unpack header ++ prettyPrint haskellModule)
  where
    header = [Neat.text|
      {-# LANGUAGE DeriveGeneric     #-}
      {-# LANGUAGE DeriveAnyClass    #-}
      {-# LANGUAGE DataKinds         #-}
      {-# LANGUAGE GADTs             #-}
      {-# LANGUAGE TypeApplications  #-}
      {-# LANGUAGE OverloadedStrings #-}
      {-# OPTIONS_GHC -fno-warn-unused-imports #-}
      {-# OPTIONS_GHC -fno-warn-name-shadowing #-}
      {-# OPTIONS_GHC -fno-warn-unused-matches #-}

      -- | Generated by Haskell protocol buffer compiler. DO NOT EDIT!
    |]

-- | Compile a Haskell module AST given a 'DotProto' package AST.
-- Instances given in @eis@ override those otherwise generated.
hsModuleForDotProto
    :: MonadError CompileError m
    => ([HsImportDecl], [HsDecl])
    -- ^ Extra user-define instances that override default generated instances
    -> DotProto
    -- ^
    -> TypeContext
    -- ^
    -> m HsModule
hsModuleForDotProto
  (extraImports, extraInstances)
  dotProto@DotProto
    { protoPackage
    , protoMeta        = DotProtoMeta { metaModulePath = modulePath }
    , protoDefinitions
    }
  importTypeContext = do
    packageIdentifier <- protoPackageName protoPackage
    moduleName <- modulePathModName modulePath

    typeContextImports <- ctxtImports importTypeContext

    let hasService = any (\case DotProtoService {} -> True; _ -> False) protoDefinitions

    let importDeclarations =
          concat [ defaultImports hasService, extraImports, typeContextImports ]

    typeContext <- dotProtoTypeContext dotProto

    let toDotProtoDeclaration =
          dotProtoDefinitionD packageIdentifier (typeContext <> importTypeContext)

    let extraInstances' = instancesForModule moduleName extraInstances

    decls <- replaceHsInstDecls extraInstances' <$>
             foldMapM toDotProtoDeclaration protoDefinitions

    return (module_ moduleName Nothing importDeclarations decls)


getExtraInstances
    :: (MonadIO m, MonadError CompileError m)
    => FilePath -> m ([HsImportDecl], [HsDecl])
getExtraInstances extraInstanceFile = do

  contents <- liftIO (readFile (FP.encodeString extraInstanceFile))

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
    :: [FilePath]
    -> FilePath
    -> IO (Either CompileError (DotProto, TypeContext))
readDotProtoWithContext [] dotProtoPath = do
  -- If we're not given a search path, default to using the current working
  -- directory, as `protoc` does
  cwd <- Turtle.pwd
  readDotProtoWithContext [cwd] dotProtoPath

readDotProtoWithContext searchPaths toplevelProto = runExceptT $ do
  findProto searchPaths toplevelProto >>= \case
    Found mp fp     -> parse mp fp
    BadModulePath e -> fatalBadModulePath toplevelProto e
    NotFound        -> dieLines [Neat.text|
      Error: failed to find file "${toplevelProtoText}", after looking in
      the following locations (controlled via the --includeDir switch(es)):

      $searchPathsText
    |]
  where
    parse mp fp = parseProtoFile mp fp >>= \case
      Right dp -> do
        let importIt = readImportTypeContext searchPaths toplevelProto (S.singleton toplevelProto)
        tc <- foldMapM importIt (protoImports dp)
        pure (dp, tc)
      Left err -> throwError (CompileParseError err)

    searchPathsText   = T.unlines (Turtle.format ("  "%F.fp) . (</> toplevelProto) <$> searchPaths)
    toplevelProtoText = Turtle.format F.fp toplevelProto

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
      import_ <- liftEither . first CompileParseError =<< importProto searchPaths toplevelFP path
      importPkg <- protoPackageName (protoPackage import_)

      let fixImportTyInfo tyInfo =
             tyInfo { dotProtoTypeInfoPackage    = DotProtoPackageSpec importPkg
                    , dotProtoTypeInfoModulePath = metaModulePath . protoMeta $ import_
                    }
      importTypeContext <- fmap fixImportTyInfo <$> dotProtoTypeContext import_

      qualifiedTypeContext <- mapKeysM (concatDotProtoIdentifier importPkg) importTypeContext

      let recur = readImportTypeContext searchPaths toplevelFP (S.insert path alreadyRead)
      let isPublic (DotProtoImport q _) = q == DotProtoImportPublic
      transitiveImportsTC <- foldMapOfM (traverse . filtered isPublic) recur (protoImports import_)

      pure $ importTypeContext <> qualifiedTypeContext <> transitiveImportsTC

-- | Given a type context, generates the import statements necessary
--   to import all the required types.
ctxtImports :: MonadError CompileError m => TypeContext -> m [HsImportDecl]
ctxtImports = fmap (map mkImport . nub)
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
                      => DotProtoTypeInfo -> DotProtoIdentifier -> m HsType
msgTypeFromDpTypeInfo DotProtoTypeInfo{..} ident = do
    modName   <- modulePathModName dotProtoTypeInfoModulePath
    identName <- qualifiedMessageName dotProtoTypeInfoParent ident
    pure $ HsTyCon (Qual modName (HsIdent identName))

haskellName, jsonpbName, grpcName, protobufName, proxyName :: String -> HsQName
haskellName  name = Qual (Module "Hs")         (HsIdent name)
jsonpbName   name = Qual (Module "HsJSONPB")   (HsIdent name)
grpcName     name = Qual (Module "HsGRPC")     (HsIdent name)
protobufName name = Qual (Module "HsProtobuf") (HsIdent name)
proxyName    name = Qual (Module "Proxy")      (HsIdent name)

modulePathModName :: MonadError CompileError m => Path -> m Module
modulePathModName (Path [])    = throwError InternalEmptyModulePath
modulePathModName (Path comps) = Module . intercalate "." <$> mapM typeLikeName comps

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

dhallInterpretInstDecl :: String -> HsDecl
dhallInterpretInstDecl typeName =
  instDecl_ (dhallPBName "Interpret")
            [ type_ typeName ]
            [ ]

dhallInjectInstDecl :: String -> HsDecl
dhallInjectInstDecl typeName =
  instDecl_ (dhallPBName "Inject")
            [ type_ typeName ]
            [ ]
#endif

-- ** Helpers to wrap/unwrap types for protobuf (de-)serialization

coerceE :: Bool -> HsType -> HsType -> Maybe HsExp
coerceE _ from to | from == to = Nothing
coerceE unsafe from to = Just $ HsApp (HsApp coerceF (typeApp from)) (typeApp to)
  where
    -- Do not add linebreaks to typeapps as that causes parse errors
    pp = prettyPrintStyleMode style{mode=OneLineMode} defaultMode
    typeApp ty = HsVar (UnQual (HsIdent ("@("++ pp ty ++ ")")))
    coerceF | unsafe = HsVar (haskellName "unsafeCoerce")
            | otherwise  = HsVar (haskellName "coerce")

wrapE :: MonadError CompileError m => TypeContext -> [DotProtoOption] -> DotProtoType -> HsExp -> m HsExp
wrapE ctxt opts dpt e = maybe e (\f -> HsParen (HsApp (HsParen f) e)) <$>
  (coerceE (isMap dpt) <$> dptToHsType ctxt dpt <*> dptToHsTypeWrapped opts ctxt dpt)

unwrapE :: MonadError CompileError m => TypeContext -> [DotProtoOption] -> DotProtoType -> HsExp -> m HsExp
unwrapE ctxt opts dpt e = maybe e (\f -> HsParen (HsApp (HsParen f) e)) <$>
   (coerceE (isMap dpt) <$> overParser (dptToHsTypeWrapped opts ctxt dpt) <*> overParser (dptToHsType ctxt dpt))
  where
    overParser = fmap $ HsTyApp (HsTyVar (HsIdent "_"))


--------------------------------------------------------------------------------
--
-- * Functions to convert 'DotProtoType' into Haskell types
--

-- | Convert a dot proto type to a Haskell type
dptToHsType :: MonadError CompileError m => TypeContext -> DotProtoType -> m HsType
dptToHsType = foldDPT dptToHsContType dpptToHsType

-- | Convert a dot proto type to a wrapped Haskell type
dptToHsTypeWrapped :: MonadError CompileError m => [DotProtoOption] -> TypeContext -> DotProtoType -> m HsType
dptToHsTypeWrapped opts =
   foldDPT
     -- The wrapper for the collection type replaces the native haskell
     -- collection type, so try that first.
     (\ctxt ty -> maybe (dptToHsContType ctxt ty) id (dptToHsWrappedContType ctxt opts ty))
     -- Always wrap the primitive type.
     (\ctxt ty -> dpptToHsTypeWrapper ty <$> dpptToHsType ctxt ty)

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
      Optional pType       -> cont <$> prim pType
      Repeated pType       -> cont <$> prim pType
      NestedRepeated pType -> cont <$> prim pType
      Map k v  | validMapKey k -> HsTyApp . cont <$> prim k <*> go (Prim v) -- need to 'Nest' message types
               | otherwise -> throwError $ InvalidMapKeyType (show $ pPrint k)

-- | Translate DotProtoType constructors to wrapped Haskell container types
-- (for Message serde instances).
dptToHsWrappedContType :: TypeContext -> [DotProtoOption] -> DotProtoType -> Maybe (HsType -> HsType)
dptToHsWrappedContType ctxt opts = \case
  Prim (Named tyName)
    | isMessage ctxt tyName -> Just $ HsTyApp (protobufType_ "Nested")
  Repeated (Named tyName)
    | isMessage ctxt tyName -> Just $ HsTyApp (protobufType_ "NestedVec")
  Repeated ty
    | isUnpacked opts       -> Just $ HsTyApp (protobufType_ "UnpackedVec")
    | isPacked opts         -> Just $ HsTyApp (protobufType_ "PackedVec")
    | isPackable ctxt ty    -> Just $ HsTyApp (protobufType_ "PackedVec")
    | otherwise             -> Just $ HsTyApp (protobufType_ "UnpackedVec")
  _ -> Nothing

-- | Translate DotProtoType to Haskell container types.
dptToHsContType :: TypeContext -> DotProtoType -> HsType -> HsType
dptToHsContType ctxt = \case
  Prim (Named tyName) | isMessage ctxt tyName
                     -> HsTyApp $ primType_ "Maybe"
  Optional _         -> HsTyApp $ primType_ "Maybe"
  Repeated _         -> HsTyApp $ primType_ "Vector"
  NestedRepeated _   -> HsTyApp $ primType_ "Vector"
  Map _ _            -> HsTyApp $ primType_ "Map"
  _                  -> id

-- | Haskell wrapper for primitive dot proto types
dpptToHsTypeWrapper :: DotProtoPrimType -> HsType -> HsType
dpptToHsTypeWrapper = \case
  SInt32   -> HsTyApp (protobufType_ "Signed")
  SInt64   -> HsTyApp (protobufType_ "Signed")
  SFixed32 -> HsTyApp (protobufType_ "Signed") . HsTyApp (protobufType_ "Fixed")
  SFixed64 -> HsTyApp (protobufType_ "Signed") . HsTyApp (protobufType_ "Fixed")
  Fixed32  -> HsTyApp (protobufType_ "Fixed")
  Fixed64  -> HsTyApp (protobufType_ "Fixed")
  _        -> id

-- | Convert a dot proto prim type to an unwrapped Haskell type
dpptToHsType :: MonadError CompileError m => TypeContext -> DotProtoPrimType -> m HsType
dpptToHsType ctxt = \case
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
  String   -> pure $ primType_ "Text"
  Bytes    -> pure $ primType_ "ByteString"
  Bool     -> pure $ primType_ "Bool"
  Float    -> pure $ primType_ "Float"
  Double   -> pure $ primType_ "Double"
  Named msgName ->
    case M.lookup msgName ctxt of
      Just ty@(DotProtoTypeInfo { dotProtoTypeInfoKind = DotProtoKindEnum }) ->
          HsTyApp (protobufType_ "Enumerated") <$> msgTypeFromDpTypeInfo ty msgName
      Just ty -> msgTypeFromDpTypeInfo ty msgName
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
                    => DotProtoIdentifier -> TypeContext -> DotProtoDefinition -> m [HsDecl]
dotProtoDefinitionD pkgIdent ctxt = \case
  DotProtoMessage messageName messageParts ->
    dotProtoMessageD ctxt Anonymous messageName messageParts

  DotProtoEnum enumName enumParts ->
    dotProtoEnumD Anonymous enumName enumParts

  DotProtoService serviceName serviceParts ->
    dotProtoServiceD pkgIdent ctxt serviceName serviceParts

-- | Generate 'Named' instance for a type in this package
namedInstD :: String -> HsDecl
namedInstD messageName =
  instDecl_ (protobufName "Named")
      [ type_ messageName ]
      [ HsFunBind [nameOfDecl] ]
  where
    nameOfDecl = match_ (HsIdent "nameOf") [HsPWildCard]
                        (HsUnGuardedRhs (apply fromStringE
                                               [ HsLit (HsString messageName) ]))
                        []

-- ** Generate types and instances for .proto messages

-- | Generate data types, 'Bounded', 'Enum', 'FromJSONPB', 'Named', 'Message',
--   'ToJSONPB' instances as appropriate for the given 'DotProtoMessagePart's
dotProtoMessageD
    :: forall m
     . MonadError CompileError m
    => TypeContext
    -> DotProtoIdentifier
    -> DotProtoIdentifier
    -> [DotProtoMessagePart]
    -> m [HsDecl]
dotProtoMessageD ctxt parentIdent messageIdent messageParts = do
    messageName <- qualifiedMessageName parentIdent messageIdent

    let mkDataDecl flds =
           dataDecl_ messageName
                     [ recDecl_ (HsIdent messageName) flds ]
                     defaultMessageDeriving

    let getName = \case
          DotProtoMessageField fld     -> [dotProtoFieldName fld]
          DotProtoMessageOneOf ident _ -> [ident]
          _                            -> []

    foldMapM id
      [ sequence
          [ mkDataDecl <$> foldMapM (messagePartFieldD messageName) messageParts
          , pure (namedInstD messageName)
          , messageInstD ctxt' parentIdent messageIdent messageParts

          , toJSONPBMessageInstD   ctxt' parentIdent messageIdent messageParts
          , fromJSONPBMessageInstD ctxt' parentIdent messageIdent messageParts

            -- Generate Aeson instances in terms of JSONPB instances
          , pure (toJSONInstDecl messageName)
          , pure (fromJSONInstDecl messageName)

          -- And the Swagger ToSchema instance corresponding to JSONPB encodings
          , toSchemaInstanceDeclaration messageName Nothing
              =<< foldMapM (traverse dpIdentUnqualName . getName) messageParts

#ifdef DHALL
          -- Generate Dhall instances
          , pure (dhallInterpretInstDecl messageName)
          , pure (dhallInjectInstDecl messageName)
#endif
          ]

      -- Nested regular and oneof message decls
      , foldMapOfM (traverse . _messageDefinition)
                   nestedDecls
                   messageParts

      , foldMapOfM (traverse . _messageOneOf)
                   (uncurry $ nestedOneOfDecls messageName)
                   messageParts
      ]

  where
    ctxt' :: TypeContext
    ctxt' = maybe mempty dotProtoTypeChildContext (M.lookup messageIdent ctxt)
                <> ctxt

    messagePartFieldD :: String -> DotProtoMessagePart -> m [([HsName], HsBangType)]
    messagePartFieldD messageName (DotProtoMessageField DotProtoField{..}) = do
      fullName <- prefixedFieldName messageName =<< dpIdentUnqualName dotProtoFieldName
      fullTy <- dptToHsType ctxt' dotProtoFieldType
      pure [ ([HsIdent fullName], HsUnBangedTy fullTy ) ]

    messagePartFieldD messageName (DotProtoMessageOneOf fieldName _) = do
      fullName <- prefixedFieldName messageName =<< dpIdentUnqualName fieldName
      qualTyName <- prefixedConName messageName =<< dpIdentUnqualName fieldName
      let fullTy = HsTyApp (HsTyCon (haskellName "Maybe")) . type_ $ qualTyName
      pure [ ([HsIdent fullName], HsUnBangedTy fullTy) ]

    messagePartFieldD _ _ = pure []

    nestedDecls :: DotProtoDefinition -> m [HsDecl]
    nestedDecls (DotProtoMessage subMsgName subMessageDef) = do
      parentIdent' <- concatDotProtoIdentifier parentIdent messageIdent
      dotProtoMessageD ctxt' parentIdent' subMsgName subMessageDef

    nestedDecls (DotProtoEnum subEnumName subEnumDef) = do
      parentIdent' <- concatDotProtoIdentifier parentIdent messageIdent
      dotProtoEnumD parentIdent' subEnumName subEnumDef

    nestedDecls _ = pure []

    nestedOneOfDecls :: String -> DotProtoIdentifier -> [DotProtoField] -> m [HsDecl]
    nestedOneOfDecls messageName identifier fields = do
      fullName <- prefixedConName messageName =<< dpIdentUnqualName identifier

      (cons, idents) <- fmap unzip (mapM (oneOfCons fullName) fields)

      toSchemaInstance <- toSchemaInstanceDeclaration fullName (Just idents)
                            =<< mapM (dpIdentUnqualName . dotProtoFieldName) fields

      pure [ dataDecl_ fullName cons defaultMessageDeriving
           , namedInstD fullName
           , toSchemaInstance

#ifdef DHALL
           , dhallInterpretInstDecl fullName
           , dhallInjectInstDecl fullName
#endif
           ]

    oneOfCons :: String -> DotProtoField -> m (HsConDecl, HsName)
    oneOfCons fullName DotProtoField{..} = do
       consTy <- case dotProtoFieldType of
            Prim msg@(Named msgName)
              | isMessage ctxt' msgName
                -> -- Do not wrap message summands with Maybe.
                   dpptToHsType ctxt' msg

            _   -> dptToHsType ctxt' dotProtoFieldType

       consName <- prefixedConName fullName =<< dpIdentUnqualName dotProtoFieldName
       let ident = HsIdent consName
       pure (conDecl_ ident [HsUnBangedTy consTy], ident)

    oneOfCons _ DotProtoEmptyField = internalError "field type : empty field"

-- *** Generate Protobuf 'Message' instances

messageInstD
    :: forall m
     . MonadError CompileError m
    => TypeContext
    -> DotProtoIdentifier
    -> DotProtoIdentifier
    -> [DotProtoMessagePart]
    -> m HsDecl
messageInstD ctxt parentIdent msgIdent messageParts = do
     msgName         <- qualifiedMessageName parentIdent msgIdent
     qualifiedFields <- getQualifiedFields msgName messageParts
     encodedFields   <- mapM encodeMessageField qualifiedFields
     decodedFields   <- mapM decodeMessageField qualifiedFields

     let encodeMessageE = apply mconcatE [ HsList encodedFields]
     let decodeMessageE = foldl (\f -> HsInfixApp f apOp)
                                (apply pureE [ HsVar (unqual_ msgName) ])
                                decodedFields

     let punnedFieldsP =
             [ HsPFieldPat (unqual_ fieldName) (HsPVar (HsIdent fieldName))
             | QualifiedField (coerce -> fieldName) _ <- qualifiedFields
             ]

     let dotProtoE = HsList $
           messageParts^..traverse._messageField <&> \DotProtoField{..} ->
             apply dotProtoFieldC
                  [ fieldNumberE dotProtoFieldNumber
                   , dpTypeE dotProtoFieldType
                   , dpIdentE dotProtoFieldName
                   , HsList (map optionE dotProtoFieldOptions)
                   , maybeE (HsLit . HsString) dotProtoFieldComment
                   ]

     let encodeMessageDecl = match_ (HsIdent "encodeMessage")
                                    [HsPWildCard, HsPRec (unqual_ msgName) punnedFieldsP]
                                    (HsUnGuardedRhs encodeMessageE) []
     let decodeMessageDecl = match_ (HsIdent "decodeMessage") [ HsPWildCard ]
                                    (HsUnGuardedRhs decodeMessageE) []

     let dotProtoDecl      = match_ (HsIdent "dotProto") [HsPWildCard]
                                    (HsUnGuardedRhs dotProtoE) []

     pure $ instDecl_ (protobufName "Message")
                      [ type_ msgName ]
                      [ HsFunBind [ encodeMessageDecl ]
                      , HsFunBind [ decodeMessageDecl ]
                      , HsFunBind [ dotProtoDecl ]
                      ]
  where
    encodeMessageField :: QualifiedField -> m HsExp
    encodeMessageField QualifiedField{recordFieldName, fieldInfo} =
      let recordFieldName' = HsVar (unqual_ (coerce recordFieldName)) in
      case fieldInfo of
        FieldNormal _fieldName fieldNum dpType options -> do
            fieldE <- wrapE ctxt options dpType recordFieldName'
            pure $ apply encodeMessageFieldE [ fieldNumberE fieldNum, fieldE ]

        FieldOneOf OneofField{subfields} -> do
            alts <- mapM mkAlt subfields
            pure $ HsCase recordFieldName'
                    [ alt_ (HsPApp (haskellName "Nothing") [])
                           (HsUnGuardedAlt memptyE)
                           []
                    , alt_ (HsPApp (haskellName "Just") [patVar "x"])
                           (HsUnGuardedAlt (HsCase (HsVar (unqual_ "x")) alts))
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
                     . wrapE ctxt options dpType
                     . (if isMaybe then wrapJust else id)
                     $ HsVar (unqual_ "y")

              pure $ alt_ (HsPApp (unqual_ conName) [patVar "y"])
                          (HsUnGuardedAlt (apply encodeMessageFieldE [fieldNumberE fieldNum, xE]))
                          []


    decodeMessageField :: QualifiedField -> m HsExp
    decodeMessageField QualifiedField{fieldInfo} =
      case fieldInfo of
        FieldNormal _fieldName fieldNum dpType options ->
            unwrapE ctxt options dpType $ apply atE [ decodeMessageFieldE, fieldNumberE fieldNum ]

        FieldOneOf OneofField{subfields} -> do
            parsers <- mapM subfieldParserE subfields
            pure $  apply oneofE [ HsVar (haskellName "Nothing")
                                 , HsList parsers
                                 ]
          where
            -- create a list of (fieldNumber, Cons <$> parser)
            subfieldParserE (OneofSubfield fieldNumber consName _ dpType options) = do
              let fE | Prim (Named tyName) <- dpType, isMessage ctxt tyName
                     = HsParen (HsApp fmapE (HsVar (unqual_ consName)))
                     | otherwise
                     = HsParen (HsInfixApp (HsVar (haskellName "Just"))
                                           composeOp
                                           (HsVar (unqual_ consName)))

              alts <- unwrapE ctxt options dpType decodeMessageFieldE

              pure $ HsTuple
                   [ fieldNumberE fieldNumber
                   , HsInfixApp (apply pureE [ fE ]) apOp alts
                   ]


-- *** Generate ToJSONPB/FromJSONPB instances

toJSONPBMessageInstD
    :: MonadError CompileError m
    => TypeContext
    -> DotProtoIdentifier
    -> DotProtoIdentifier
    -> [DotProtoMessagePart]
    -> m HsDecl
toJSONPBMessageInstD _ctxt parentIdent msgIdent messageParts = do
    msgName    <- qualifiedMessageName parentIdent msgIdent
    qualFields <- getQualifiedFields msgName messageParts

    let applyE nm oneofNm =
          apply (HsVar (jsonpbName nm))
                [ HsList (foldQF defPairE (oneofCaseE oneofNm) <$> qualFields) ]

    let patBinder = foldQF (const fieldBinder) (oneofSubDisjunctBinder . subfields)
    let matchE nm appNm oneofAppNm =
          match_
            (HsIdent nm)
            [ HsPApp (unqual_ msgName)
                     (patVar . patBinder <$> qualFields) ]
            (HsUnGuardedRhs (applyE appNm oneofAppNm))
            []

    pure $ instDecl_ (jsonpbName "ToJSONPB")
                     [ type_ msgName ]
                     [ HsFunBind [matchE "toJSONPB"     "object" "objectOrNull"]
                     , HsFunBind [matchE "toEncodingPB" "pairs"  "pairsOrNull" ]
                     ]

  where
    -- E.g.
    -- "another" .= f2 -- always succeeds (produces default value on missing field)
    defPairE fldName fldNum =
      HsInfixApp (HsLit (HsString (coerce fldName)))
                 toJSONPBOp
                 (HsVar (unqual_ (fieldBinder fldNum)))

    -- E.g.
    -- HsJSONPB.pair "name" f4 -- fails on missing field
    pairE fldNm varNm =
      apply (HsVar (jsonpbName "pair"))
            [ HsLit (HsString (coerce fldNm))
            , HsVar (unqual_ varNm)
            ]

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
    oneofCaseE retJsonCtor (OneofField typeName subfields) =
        HsParen
          $ HsLet [ HsFunBind [ match_ (HsIdent caseName) [] (HsUnGuardedRhs caseExpr) [] ] ]
          $ HsLambda l [patVar optsStr] (HsIf dontInline noInline yesInline)
      where
        optsStr = "options"
        opts    = HsVar (unqual_ optsStr)

        caseName = "encode" <> over (ix 0) toUpper typeName
        caseBnd = HsVar (unqual_ caseName)

        dontInline = HsApp (HsVar (jsonpbName "optEmitNamedOneof")) opts

        noInline = HsApp (HsParen (HsInfixApp (HsLit (HsString typeName))
                                              toJSONPBOp
                                              (apply (HsVar (jsonpbName retJsonCtor)) [ HsList [caseBnd], opts ])))
                         opts

        yesInline = HsApp caseBnd opts


        -- E.g.
        -- case f4_or_f9 of
        --   Just (SomethingPickOneName f4)
        --     -> HsJSONPB.pair "name" f4
        --   Just (SomethingPickOneSomeid f9)
        --     -> HsJSONPB.pair "someid" f9
        --   Nothing
        --     -> mempty
        caseExpr = HsParen $
            HsCase disjunctName (altEs <> [fallthroughE])
          where
            disjunctName = HsVar (unqual_ (oneofSubDisjunctBinder subfields))
            altEs = do
              sub@(OneofSubfield _ conName pbFldNm _ _) <- subfields
              let patVarNm = oneofSubBinder sub
              pure $ alt_ (HsPApp (haskellName "Just")
                                  [ HsPParen
                                    (HsPApp (unqual_ conName) [patVar patVarNm])
                                  ]
                          )
                          (HsUnGuardedAlt (pairE pbFldNm patVarNm))
                          []
            fallthroughE =
              alt_ (HsPApp (haskellName "Nothing") [])
                   (HsUnGuardedAlt memptyE)
                   []

fromJSONPBMessageInstD
    :: MonadError CompileError m
    => TypeContext
    -> DotProtoIdentifier
    -> DotProtoIdentifier
    -> [DotProtoMessagePart]
    -> m HsDecl
fromJSONPBMessageInstD _ctxt parentIdent msgIdent messageParts = do
    msgName    <- qualifiedMessageName parentIdent msgIdent
    qualFields <- getQualifiedFields msgName messageParts

    let parseJSONPBE =
          apply (HsVar (jsonpbName "withObject"))
                [ HsLit (HsString msgName)
                , HsParen (HsLambda l [lambdaPVar] fieldAps)
                ]
          where
            fieldAps = foldl (\f -> HsInfixApp f apOp)
                             (apply pureE [ HsVar (unqual_ msgName) ])
                             (foldQF normalParserE oneofParserE <$> qualFields)

    let parseJSONPBDecl =
          match_ (HsIdent "parseJSONPB") [] (HsUnGuardedRhs parseJSONPBE) []

    pure (instDecl_ (jsonpbName "FromJSONPB")
                   [ type_ msgName ]
                   [ HsFunBind [ parseJSONPBDecl ] ])
  where
    lambdaPVar = patVar "obj"
    lambdaVar  = HsVar (unqual_ "obj")

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
    oneofParserE (OneofField oneofType fields) =
        HsParen $
          HsLet [ HsFunBind [ match_ (HsIdent letBndStr) [patVar letArgStr ]
                                     (HsUnGuardedRhs tryParseDisjunctsE) []
                            ]
                ]
                (HsInfixApp parseWrapped altOp parseUnwrapped)
      where
        oneofTyLit = HsLit (HsString oneofType) -- FIXME

        letBndStr  = "parse" <> over (ix 0) toUpper oneofType
        letBndName = HsVar (unqual_ letBndStr)
        letArgStr  = "parseObj"
        letArgName = HsVar (unqual_ letArgStr)

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
        tryParseDisjunctsE =
            HsApp msumE (HsList (map subParserE fields <> fallThruE))
          where
            fallThruE
              = [ HsApp pureE (HsVar (haskellName "Nothing")) ]
            subParserE OneofSubfield{subfieldConsName, subfieldName}
              = HsInfixApp
                  (HsInfixApp (HsVar (haskellName "Just"))
                              composeOp
                              (HsVar (unqual_ subfieldConsName)))
                  fmapOp
                  (apply (HsVar (jsonpbName "parseField"))
                         [ letArgName
                         , HsLit (HsString (coerce subfieldName))])

    -- E.g. obj .: "someid"
    normalParserE fldNm _ =
      HsInfixApp lambdaVar
                 parseJSONPBOp
                 (HsLit (HsString (coerce fldNm)))

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
            [ HsFunBind [match_ (HsIdent "parseJSON") [] (HsUnGuardedRhs (HsVar (jsonpbName "parseJSONPB"))) []
                        ]
            ]


-- *** Generate `ToSchema` instance

toSchemaInstanceDeclaration
    :: MonadError CompileError m
    => String
    -- ^ Name of the message type to create an instance for
    -> Maybe [HsName]
    -- ^ Oneof constructors
    -> [String]
    -- ^ Field names
    -> m HsDecl
toSchemaInstanceDeclaration messageName maybeConstructors fieldNames = do
  qualifiedFieldNames <- mapM (prefixedFieldName messageName) fieldNames

  let messageConstructor = HsCon (UnQual (HsIdent messageName))

  let _namedSchemaNameExpression = HsApp justC (HsLit (HsString messageName))

      -- { _paramSchemaType = HsJSONPB.SwaggerObject
      -- }
  let paramSchemaUpdates =
        [ HsFieldUpdate _paramSchemaType _paramSchemaTypeExpression
        ]
        where
          _paramSchemaType = jsonpbName "_paramSchemaType"

          _paramSchemaTypeExpression = HsVar (jsonpbName "SwaggerObject")

  let _schemaParamSchemaExpression = HsRecUpdate memptyE paramSchemaUpdates

      -- [ ("fieldName0", qualifiedFieldName0)
      -- , ("fieldName1", qualifiedFieldName1)
      -- ...
      -- ]
  let properties = HsList $ do
        (fieldName, qualifiedFieldName) <- zip fieldNames qualifiedFieldNames

        let string = HsLit (HsString fieldName)

        let variable = HsVar (UnQual (HsIdent qualifiedFieldName))

        return (HsTuple [ string, variable ])

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

  let toArgument fieldName = HsApp asProxy declare
        where
          declare = HsVar (UnQual (HsIdent (toDeclareName fieldName)))

          asProxy = HsVar (jsonpbName "asProxy")

      -- do let declare_fieldName0 = HsJSONPB.declareSchemaRef
      --    qualifiedFieldName0 <- declare_fieldName0 Proxy.Proxy
      --    let declare_fieldName1 = HsJSONPB.declareSchemaRef
      --    qualifiedFieldName1 <- declare_fieldName1 Proxy.Proxy
      --    ...
      --    let _ = pure MessageName <*> HsJSONPB.asProxy declare_fieldName0 <*> HsJSONPB.asProxy declare_fieldName1 <*> ...
      --    return (...)
  let expressionForMessage =
          HsDo (bindingStatements ++ inferenceStatement ++ [ returnStatement ])
        where
          bindingStatements = do
            (fieldName, qualifiedFieldName) <- zip fieldNames qualifiedFieldNames

            let declareIdentifier = HsIdent (toDeclareName fieldName)

            let stmt0 = HsLetStmt [ HsFunBind
                                    [ HsMatch l declareIdentifier []
                                               (HsUnGuardedRhs (HsVar (jsonpbName "declareSchemaRef")))
                                               []
                                    ]
                                  ]

            let stmt1 = HsGenerator l (HsPVar (HsIdent qualifiedFieldName))
                                      (HsApp (HsVar (UnQual declareIdentifier))
                                             (HsCon (proxyName "Proxy")))
            [ stmt0, stmt1]


          inferenceStatement =
              if null fieldNames then [] else [ HsLetStmt [ patternBind ] ]
            where
              arguments = map toArgument fieldNames

              patternBind = HsPatBind l HsPWildCard
                                        (HsUnGuardedRhs (applicativeApply messageConstructor arguments))
                                        []

          returnStatement = HsQualifier (HsApp returnE (HsParen namedSchema))

      -- do let declare_fieldName0 = HsJSONPB.declareSchemaRef
      --    let _ = pure ConstructorName0 <*> HsJSONPB.asProxy declare_fieldName0
      --    qualifiedFieldName0 <- declare_fieldName0 Proxy.Proxy
      --    let declare_fieldName1 = HsJSONPB.declareSchemaRef
      --    let _ = pure ConstructorName1 <*> HsJSONPB.asProxy declare_fieldName1
      --    qualifiedFieldName1 <- declare_fieldName1 Proxy.Proxy
      --    ...
      --    return (...)
  let expressionForOneOf constructors =
          HsDo (bindingStatements ++ [ returnStatement ])
        where
          bindingStatements = do
            (fieldName, qualifiedFieldName, constructor)
                <- zip3 fieldNames qualifiedFieldNames constructors

            let declareIdentifier = HsIdent (toDeclareName fieldName)

            let stmt0 = HsLetStmt [ HsFunBind
                                      [ HsMatch l declareIdentifier []
                                                 (HsUnGuardedRhs (HsVar (jsonpbName "declareSchemaRef")))
                                                 []
                                      ]
                                  ]
            let stmt1 = HsGenerator l (HsPVar (HsIdent qualifiedFieldName))
                                      (HsApp (HsVar (UnQual declareIdentifier))
                                             (HsCon (proxyName "Proxy")))
            let inferenceStatement =
                    if null fieldNames then [] else [ HsLetStmt [ patternBind ] ]
                  where
                    arguments = [ toArgument fieldName ]

                    rightHandSide =
                      HsUnGuardedRhs (applicativeApply (HsCon (UnQual constructor)) arguments)

                    patternBind = HsPatBind l HsPWildCard rightHandSide []

            [stmt0, stmt1] ++ inferenceStatement


          returnStatement = HsQualifier (HsApp returnE (HsParen namedSchema))

  let instanceDeclaration =
          instDecl_ className [ classArgument ] [ classDeclaration ]
        where
          className = jsonpbName "ToSchema"

          classArgument = HsTyCon (UnQual (HsIdent messageName))

          classDeclaration = HsFunBind [ match ]
            where
              match = match_ matchName [ HsPWildCard ] rightHandSide []
                where
                  expression = case maybeConstructors of
                      Nothing           -> expressionForMessage
                      Just constructors -> expressionForOneOf constructors

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

  enumCons <- fmap (sortBy (comparing fst))
              $ traverse (traverse (fmap (prefixedEnumFieldName enumName) . dpIdentUnqualName))
                         [ (i, conIdent) | DotProtoEnumField conIdent i _options <- enumParts ]

  let enumNameE = HsLit (HsString enumName)
      -- TODO assert that there is more than one enumeration constructor
      ((minEnumVal, maxEnumVal), enumConNames) = first (minimum &&& maximum) $ unzip enumCons

      boundsE = HsTuple
                  [ HsExpTypeSig l (intE minEnumVal) (HsQualType [] (HsTyCon (haskellName "Int")))
                  , intE maxEnumVal
                  ]

      toEnumD = toEnumDPatterns <> [ toEnumFailure ]
      fromEnumD =
          [ match_ (HsIdent "fromEnum") [ HsPApp (unqual_ conName) [] ]
                   (HsUnGuardedRhs (intE conIdx))
                   []
          | (conIdx, conName) <- enumCons
          ]
      succD = zipWith succDPattern enumConNames (tail enumConNames) <> [ succFailure ]
      predD = zipWith predDPattern (tail enumConNames) enumConNames <> [ predFailure ]

      toEnumDPatterns =
          [ match_ (HsIdent "toEnum")
                   [ intP conIdx ]
                   (HsUnGuardedRhs (HsVar (unqual_ conName))) []
          | (conIdx, conName) <- enumCons ]

      succDPattern thisCon nextCon =
          match_ (HsIdent "succ") [ HsPApp (unqual_ thisCon) [] ]
                 (HsUnGuardedRhs (HsVar (unqual_ nextCon))) []
      predDPattern thisCon prevCon =
          match_ (HsIdent "pred") [ HsPApp (unqual_ thisCon) [] ]
                 (HsUnGuardedRhs (HsVar (unqual_ prevCon))) []

      toEnumFailure   = match_ (HsIdent "toEnum") [ HsPVar (HsIdent "i") ]
                               (HsUnGuardedRhs
                                  (apply toEnumErrorE [enumNameE , HsVar (unqual_ "i") , boundsE]))
                               []
      succFailure     = match_ (HsIdent "succ") [ HsPWildCard ]
                               (HsUnGuardedRhs (HsApp succErrorE enumNameE)) []
      predFailure     = match_ (HsIdent "pred") [ HsPWildCard ]
                               (HsUnGuardedRhs (HsApp predErrorE enumNameE)) []

      parseJSONPBDecls :: [HsMatch]
      parseJSONPBDecls = foldr ((:) . matchConName) [mismatch] enumConNames
        where
          matchConName conName = match_ (HsIdent "parseJSONPB") [pat conName]
                                        (HsUnGuardedRhs
                                           (HsApp pureE (HsVar (unqual_ conName))))
                                        []

          pat nm = HsPApp (jsonpbName "String") [ HsPLit (HsString (tryStripEnumName nm)) ]

          tryStripEnumName = fromMaybe <*> stripPrefix enumName

          mismatch = match_ (HsIdent "parseJSONPB") [patVar "v"]
                            (HsUnGuardedRhs
                                  (apply (HsVar (jsonpbName "typeMismatch"))
                                    [ HsLit (HsString enumName), HsVar (unqual_ "v") ]))
                            []


      toJSONPBDecl =
        match_ (HsIdent "toJSONPB") [ patVar "x", HsPWildCard ]
          (HsUnGuardedRhs
             (HsApp (HsVar (jsonpbName "enumFieldString"))
                    (HsVar (unqual_ "x"))))
          []

      toEncodingPBDecl =
        match_ (HsIdent "toEncodingPB") [ patVar "x", HsPWildCard ]
          (HsUnGuardedRhs
             (HsApp (HsVar (jsonpbName "enumFieldEncoding"))
                    (HsVar (unqual_ "x"))))
          []

  pure [ dataDecl_ enumName
                   [ conDecl_ (HsIdent con) [] | con <- enumConNames ]
                   defaultEnumDeriving
       , namedInstD enumName
       , instDecl_ (haskellName "Enum") [ type_ enumName ]
                   [ HsFunBind toEnumD, HsFunBind fromEnumD
                   , HsFunBind succD, HsFunBind predD ]
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
    => DotProtoIdentifier
    -> TypeContext
    -> DotProtoIdentifier
    -> [DotProtoServicePart]
    -> m [HsDecl]
dotProtoServiceD pkgIdent ctxt serviceIdent service = do
     serviceNameUnqual <- dpIdentUnqualName serviceIdent
     packageName <- dpIdentQualName pkgIdent

     serviceName <- typeLikeName serviceNameUnqual

     let endpointPrefix = "/" ++ packageName ++ "." ++ serviceName ++ "/"

     let serviceFieldD (DotProtoServiceRPC DotProtoServiceRPCGuts{..}) = do
           fullName <- prefixedFieldName serviceName =<< dpIdentUnqualName rpcGutsName

           methodName <- case rpcGutsName of
                           Single nm -> pure nm
                           _ -> invalidMethodNameError rpcGutsName

           requestTy  <- dpptToHsType ctxt (Named rpcGutsRequestType)
           responseTy <- dpptToHsType ctxt (Named rpcGutsResponseType)

           let streamingType =
                 case (rpcGutsRequestStreaming, rpcGutsResponseStreaming) of
                   (Streaming, Streaming)       -> biDiStreamingC
                   (Streaming, NonStreaming)    -> clientStreamingC
                   (NonStreaming, Streaming)    -> serverStreamingC
                   (NonStreaming, NonStreaming) -> normalC

           pure [ ( endpointPrefix ++ methodName
                  , fullName, rpcGutsRequestStreaming, rpcGutsResponseStreaming
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
            HsTypeSig l [ HsIdent serverFuncName ]
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
                               ]
                      ]
                      (HsUnGuardedRhs (apply serverLoopE [ serverOptsE ]))
                      []

             handlerE handlerC adapterE methodName hsName =
                 apply handlerC [ apply methodNameC [ HsLit (HsString methodName) ]
                                , apply adapterE [ HsVar (unqual_ hsName) ]
                                ]

             update u v = HsFieldUpdate (unqual_ u) (HsVar (unqual_ v))

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
                 ]


     let clientT = tyApp (HsTyCon (unqual_ serviceName)) [ clientRequestT, clientResultT ]

     let serviceClientTypeD =
             HsTypeSig l [ HsIdent clientFuncName ]
                       (HsQualType [] (HsTyFun grpcClientT (HsTyApp ioT clientT)))

     let serviceClientD = HsFunBind [ clientFuncD ]
            where
              clientFuncD = match_ (HsIdent clientFuncName)
                                   [ HsPVar (HsIdent "client") ]
                                   ( HsUnGuardedRhs clientRecE ) []

              clientRecE = foldl (\f -> HsInfixApp f apOp)
                                 (apply pureE [ HsVar (unqual_ serviceName) ])
                                 [ HsParen $ HsInfixApp clientRequestE' apOp (registerClientMethodE endpointName)
                                 | (endpointName, _, _, _, _) <- fieldsD
                                 ]

              clientRequestE' = apply pureE [ apply clientRequestE [ HsVar (unqual_ "client") ] ]

              registerClientMethodE endpoint =
                apply clientRegisterMethodE [ HsVar (unqual_ "client")
                                            , apply methodNameC [ HsLit (HsString endpoint) ]
                                            ]

     pure [ HsDataDecl l  [] (HsIdent serviceName)
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

dotProtoFieldC, primC, optionalC, repeatedC, nestedRepeatedC, namedC, mapC,
  fieldNumberC, singleC, dotsC, pathC, nestedC, anonymousC, dotProtoOptionC,
  identifierC, stringLitC, intLitC, floatLitC, boolLitC, trueC, falseC,
  unaryHandlerC, clientStreamHandlerC, serverStreamHandlerC, biDiStreamHandlerC,
  methodNameC, nothingC, justC, forceEmitC, mconcatE, encodeMessageFieldE,
  fromStringE, decodeMessageFieldE, pureE, returnE, memptyE, msumE, atE, oneofE,
  succErrorE, predErrorE, toEnumErrorE, fmapE, defaultOptionsE, serverLoopE,
  convertServerHandlerE, convertServerReaderHandlerE, convertServerWriterHandlerE,
  convertServerRWHandlerE, clientRegisterMethodE, clientRequestE :: HsExp

dotProtoFieldC       = HsVar (protobufName "DotProtoField")
primC                = HsVar (protobufName "Prim")
optionalC            = HsVar (protobufName "Optional")
repeatedC            = HsVar (protobufName "Repeated")
nestedRepeatedC      = HsVar (protobufName "NestedRepeated")
namedC               = HsVar (protobufName "Named")
mapC                 = HsVar (protobufName "Map")
fieldNumberC         = HsVar (protobufName "FieldNumber")
singleC              = HsVar (protobufName "Single")
pathC                = HsVar (protobufName "Path")
dotsC                = HsVar (protobufName "Dots")
nestedC              = HsVar (protobufName "Nested")
anonymousC           = HsVar (protobufName "Anonymous")
dotProtoOptionC      = HsVar (protobufName "DotProtoOption")
identifierC          = HsVar (protobufName "Identifier")
stringLitC           = HsVar (protobufName "StringLit")
intLitC              = HsVar (protobufName "IntLit")
floatLitC            = HsVar (protobufName "FloatLit")
boolLitC             = HsVar (protobufName "BoolLit")
forceEmitC           = HsVar (protobufName "ForceEmit")
encodeMessageFieldE  = HsVar (protobufName "encodeMessageField")
decodeMessageFieldE  = HsVar (protobufName "decodeMessageField")
atE                  = HsVar (protobufName "at")
oneofE               = HsVar (protobufName "oneof")

trueC                       = HsVar (haskellName "True")
falseC                      = HsVar (haskellName "False")
nothingC                    = HsVar (haskellName "Nothing")
justC                       = HsVar (haskellName "Just")
mconcatE                    = HsVar (haskellName "mconcat")
fromStringE                 = HsVar (haskellName "fromString")
pureE                       = HsVar (haskellName "pure")
returnE                     = HsVar (haskellName "return")
memptyE                     = HsVar (haskellName "mempty")
msumE                       = HsVar (haskellName "msum")
succErrorE                  = HsVar (haskellName "succError")
predErrorE                  = HsVar (haskellName "predError")
toEnumErrorE                = HsVar (haskellName "toEnumError")
fmapE                       = HsVar (haskellName "fmap")

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

apOp :: HsQOp
apOp  = HsQVarOp (UnQual (HsSymbol "<*>"))

fmapOp :: HsQOp
fmapOp  = HsQVarOp (UnQual (HsSymbol "<$>"))

composeOp :: HsQOp
composeOp = HsQVarOp (Qual haskellNS (HsSymbol "."))

bindOp :: HsQOp
bindOp = HsQVarOp (Qual haskellNS (HsSymbol ">>="))

altOp :: HsQOp
altOp = HsQVarOp (UnQual (HsSymbol "<|>"))

toJSONPBOp :: HsQOp
toJSONPBOp = HsQVarOp (UnQual (HsSymbol ".="))

parseJSONPBOp :: HsQOp
parseJSONPBOp = HsQVarOp (UnQual (HsSymbol ".:"))

intE :: Integral a => a -> HsExp
intE x = (if x < 0 then HsParen else id) . HsLit . HsInt . fromIntegral $ x

intP :: Integral a => a -> HsPat
intP x = (if x < 0 then HsPParen else id) . HsPLit . HsInt . fromIntegral $ x

-- ** Expressions for protobuf-wire types

forceEmitE :: HsExp -> HsExp
forceEmitE = HsParen . HsApp forceEmitC

fieldNumberE :: FieldNumber -> HsExp
fieldNumberE = HsParen . HsApp fieldNumberC . intE . getFieldNumber

maybeE :: (a -> HsExp) -> Maybe a -> HsExp
maybeE _ Nothing = nothingC
maybeE f (Just a) = HsApp justC (f a)

dpIdentE :: DotProtoIdentifier -> HsExp
dpIdentE (Single n)       = apply singleC [ HsLit (HsString n) ]
dpIdentE (Dots (Path ns)) = apply dotsC [apply pathC [ HsList (map (HsLit . HsString) ns) ] ]
dpIdentE (Qualified a b)  = apply nestedC [ dpIdentE a, dpIdentE b ]
dpIdentE Anonymous        = anonymousC

dpValueE :: DotProtoValue -> HsExp
dpValueE (Identifier nm) = apply identifierC [ dpIdentE nm ]
dpValueE (StringLit s)   = apply stringLitC  [ HsLit (HsString s) ]
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
dpTypeE (Optional p)       = apply optionalC       [ dpPrimTypeE p ]
dpTypeE (Repeated p)       = apply repeatedC       [ dpPrimTypeE p ]
dpTypeE (NestedRepeated p) = apply nestedRepeatedC [ dpPrimTypeE p ]
dpTypeE (Map k v)          = apply mapC            [ dpPrimTypeE k, dpPrimTypeE v]


-- | Translate a dot proto primitive type to a Haskell AST primitive type.
dpPrimTypeE :: DotProtoPrimType -> HsExp
dpPrimTypeE ty =
    let wrap = HsVar . protobufName in
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

defaultImports :: Bool -> [HsImportDecl]
defaultImports usesGrpc =
    [ importDecl_ preludeM                  True  (Just haskellNS)  Nothing

#ifdef DHALL
    , importDecl_ proto3SuiteDhallPBM       True  (Just (Module hsDhallPB)) Nothing
#endif

    , importDecl_ dataProtobufWireDotProtoM True  (Just protobufNS) Nothing
    , importDecl_ dataProtobufWireTypesM    True  (Just protobufNS) Nothing
    , importDecl_ dataProtobufWireClassM    True  (Just protobufNS) Nothing
    , importDecl_ proto3SuiteJSONPBM        True  (Just jsonpbNS) Nothing
    , importDecl_ proto3SuiteJSONPBM        False  Nothing
                  (Just (False, [ HsIAbs (HsSymbol ".=")
                                , HsIAbs (HsSymbol ".:")
                                ]
                        )
                  )
    , importDecl_ proto3WireM               True  (Just protobufNS) Nothing
    , importDecl_ controlApplicativeM       False Nothing
                  (Just (False, [ HsIAbs (HsSymbol "<*>")
                                , HsIAbs (HsSymbol "<|>")
                                , HsIAbs (HsSymbol "<$>")
                                ]
                        )
                  )
    , importDecl_ controlApplicativeM       True  (Just haskellNS) Nothing
    , importDecl_ controlDeepSeqM           True  (Just haskellNS) Nothing
    , importDecl_ controlMonadM             True  (Just haskellNS) Nothing
    , importDecl_ dataTextM                 True
                  (Just haskellNS) (Just (False, [ importSym "Text" ]))
    , importDecl_ dataByteStringM           True  (Just haskellNS) Nothing
    , importDecl_ dataCoerceM               True  (Just haskellNS) Nothing
    , importDecl_ dataStringM               True  (Just haskellNS)
                  (Just (False, [ importSym "fromString" ]))
    , importDecl_ dataVectorM               True  (Just haskellNS)
                  (Just (False, [ importSym "Vector" ]))
    , importDecl_ dataMapM               True  (Just haskellNS)
                  (Just (False, [ importSym "Map", importSym "mapKeysMonotonic" ]))
    , importDecl_ dataIntM                  True  (Just haskellNS)
                  (Just (False, [ importSym "Int16", importSym "Int32", importSym "Int64" ]))
    , importDecl_ dataWordM                 True  (Just haskellNS)
                  (Just (False, [ importSym "Word16", importSym "Word32", importSym "Word64" ]))
    , importDecl_ dataProxy                 True (Just proxyNS)   Nothing
    , importDecl_ ghcGenericsM              True (Just haskellNS) Nothing
    , importDecl_ ghcEnumM                  True (Just haskellNS) Nothing
    , importDecl_ unsafeCoerceM             True (Just haskellNS) Nothing
    ]
    <>
    if usesGrpc
      then [ importDecl_ networkGrpcHighLevelGeneratedM   False (Just grpcNS) Nothing
           , importDecl_ networkGrpcHighLevelClientM      False (Just grpcNS) Nothing
           , importDecl_ networkGrpcHighLevelServerM      False (Just grpcNS)
                 (Just (True, [ importSym "serverLoop" ]))
           , importDecl_ networkGrpcHighLevelServerUnregM False (Just grpcNS)
                 (Just (False, [ importSym "serverLoop" ]))
           ]
      else []
  where
    preludeM                  = Module "Prelude"
    dataProtobufWireDotProtoM = Module "Proto3.Suite.DotProto"
    dataProtobufWireClassM    = Module "Proto3.Suite.Class"
    dataProtobufWireTypesM    = Module "Proto3.Suite.Types"
    proto3SuiteJSONPBM        = Module "Proto3.Suite.JSONPB"
    proto3WireM               = Module "Proto3.Wire"
    controlApplicativeM       = Module "Control.Applicative"
    controlDeepSeqM           = Module "Control.DeepSeq"
    controlMonadM             = Module "Control.Monad"
    dataCoerceM               = Module "Data.Coerce"
    dataTextM                 = Module "Data.Text.Lazy"
    dataByteStringM           = Module "Data.ByteString"
    dataStringM               = Module "Data.String"
    dataIntM                  = Module "Data.Int"
    dataVectorM               = Module "Data.Vector"
    dataMapM                  = Module "Data.Map"
    dataWordM                 = Module "Data.Word"
    dataProxy                 = Module "Data.Proxy"
    ghcGenericsM              = Module "GHC.Generics"
    ghcEnumM                  = Module "GHC.Enum"
    unsafeCoerceM             = Module "Unsafe.Coerce"
    networkGrpcHighLevelGeneratedM   = Module "Network.GRPC.HighLevel.Generated"
    networkGrpcHighLevelServerM      = Module "Network.GRPC.HighLevel.Server"
    networkGrpcHighLevelClientM      = Module "Network.GRPC.HighLevel.Client"
    networkGrpcHighLevelServerUnregM = Module "Network.GRPC.HighLevel.Server.Unregistered"

#ifdef DHALL
    proto3SuiteDhallPBM       = Module "Proto3.Suite.DhallPB"
#endif

    grpcNS                    = Module "HsGRPC"
    jsonpbNS                  = Module "HsJSONPB"
    protobufNS                = Module "HsProtobuf"
    proxyNS                   = Module "Proxy"

    importSym = HsIAbs . HsIdent

haskellNS :: Module
haskellNS = Module "Hs"

defaultMessageDeriving :: [HsQName]
defaultMessageDeriving = map haskellName [ "Show", "Eq", "Ord", "Generic", "NFData" ]

defaultEnumDeriving :: [HsQName]
defaultEnumDeriving = map haskellName [ "Show", "Bounded", "Eq", "Ord", "Generic", "NFData" ]

defaultServiceDeriving :: [HsQName]
defaultServiceDeriving = map haskellName [ "Generic" ]

--------------------------------------------------------------------------------
--
-- * Wrappers around haskell-src-exts constructors
--

apply :: HsExp -> [HsExp] -> HsExp
apply f = HsParen . foldl HsApp f

applicativeApply :: HsExp -> [HsExp] -> HsExp
applicativeApply f = foldl snoc nil
  where
    nil = HsApp pureE f

    snoc g x = HsInfixApp g apOp x

tyApp :: HsType -> [HsType] -> HsType
tyApp = foldl HsTyApp

module_ :: Module -> Maybe [HsExportSpec] -> [HsImportDecl] -> [HsDecl] -> HsModule
module_ = HsModule l

importDecl_ :: Module -> Bool -> Maybe Module -> Maybe (Bool, [HsImportSpec]) -> HsImportDecl
importDecl_ = HsImportDecl l

dataDecl_ :: String -> [HsConDecl] -> [HsQName] -> HsDecl
dataDecl_ messageName = HsDataDecl l [] (HsIdent messageName) []

recDecl_ :: HsName -> [([HsName], HsBangType)] -> HsConDecl
recDecl_ = HsRecDecl l

conDecl_ :: HsName -> [HsBangType] -> HsConDecl
conDecl_ = HsConDecl l

instDecl_ :: HsQName -> [HsType] -> [HsDecl] -> HsDecl
instDecl_ = HsInstDecl l []

match_ :: HsName -> [HsPat] -> HsRhs -> [HsDecl] -> HsMatch
match_ = HsMatch l

unqual_ :: String -> HsQName
unqual_ = UnQual . HsIdent

protobufType_, primType_ :: String -> HsType
protobufType_ = HsTyCon . protobufName
primType_ = HsTyCon . haskellName

type_ :: String -> HsType
type_ = HsTyCon . unqual_

patVar :: String -> HsPat
patVar =  HsPVar . HsIdent

alt_ :: HsPat -> HsGuardedAlts -> [HsDecl] -> HsAlt
alt_ = HsAlt l

-- | For some reason, haskell-src-exts needs this 'SrcLoc' parameter
--   for some data constructors. Its value does not affect
--   pretty-printed output
l :: SrcLoc
l = SrcLoc "<generated>" 0 0

__nowarn_unused :: a
__nowarn_unused = subfieldType `undefined` subfieldOptions `undefined` oneofType
