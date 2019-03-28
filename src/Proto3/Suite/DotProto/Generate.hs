{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}

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

  -- * Utilities
  , isPackable

  -- * Exposed for unit-testing
  , fieldLikeName
  , prefixedEnumFieldName
  , typeLikeName
  ) where

import           Control.Applicative
import           Control.Arrow                  ((&&&))
import           Control.Monad.Except
import           Control.Lens                   (ix, over)
import           Data.Bifunctor                 (first)
import           Data.Char
import           Data.Coerce
import           Data.Either                    (partitionEithers)
import           Data.List                      (find, intercalate, nub, sortBy,
                                                 stripPrefix)
import qualified Data.Map                       as M
import           Data.Maybe                     (catMaybes, fromMaybe)
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
import           Proto3.Suite.DotProto.Internal
import           Proto3.Wire.Types              (FieldNumber (..))
import           System.IO                      (writeFile, readFile)
import           Text.Parsec                    (ParseError)
import           Turtle                         (FilePath)
import qualified Turtle
import           Turtle.Format                  ((%))
import qualified Turtle.Format                  as F

-- * Public interface

data CompileError
  = CircularImport          FilePath
  | CompileParseError       ParseError
  | InternalEmptyModulePath
  | InternalError           String
  | InvalidMethodName       DotProtoIdentifier
  | InvalidTypeName         String
  | NoPackageDeclaration
  | NoSuchType              DotProtoIdentifier
  | Unimplemented           String
    deriving (Show, Eq)

#if !(MIN_VERSION_mtl(2,2,2))
liftEither :: MonadError e m => Either e a -> m a
liftEither x =
    case x of
        Left  e -> throwError e
        Right a -> return a
#endif

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
compileDotProtoFile
  extraInstanceFiles
  outputDirectory
  searchPaths
  dotProtoPath = runExceptT $ do
    (dotProto, importTypeContext) <- do
      ExceptT (readDotProtoWithContext searchPaths dotProtoPath)

    let DotProto     { protoMeta      } = dotProto
    let DotProtoMeta { metaModulePath } = protoMeta
    let Path         { components     } = metaModulePath

    when (null components) (throwError InternalEmptyModulePath)

    typeLikeComponents <- traverse typeLikeName components

    let relativePath = FP.concat (map fromString typeLikeComponents) <.> "hs"
    let modulePath   = outputDirectory </> relativePath

    Turtle.mktree (Turtle.directory modulePath)

    listOfExtraInstances <- traverse getExtraInstances extraInstanceFiles

    let extraInstances = mconcat listOfExtraInstances

    haskellModule <- do
      renderHsModuleForDotProto extraInstances dotProto importTypeContext

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
compileDotProtoFileOrDie
  extraInstanceFiles
  outputDirectory
  searchPaths
  dotProtoPath = do
  compileResult <- do
    compileDotProtoFile
      extraInstanceFiles
      outputDirectory
      searchPaths
      dotProtoPath

  case compileResult of
    Left e -> do
      -- TODO: pretty print the error messages
      let errText          = Turtle.format Turtle.w  e
      let dotProtoPathText = Turtle.format Turtle.fp dotProtoPath
      dieLines [Neat.text|
        Error: failed to compile "${dotProtoPathText}":

        ${errText}
      |]
    _ -> pure ()

getExtraInstances
    :: (MonadIO m, MonadError CompileError m)
    => FilePath -> m ([HsImportDecl], [HsDecl])
getExtraInstances extraInstanceFile = do
  let extraInstanceFileString = FP.encodeString extraInstanceFile

  parseRes <- parseModule <$> liftIO (readFile extraInstanceFileString)

  case parseRes of
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
    _
    DotProto { protoMeta = DotProtoMeta { metaModulePath = Path [] } }
    _ =
    throwError InternalEmptyModulePath
hsModuleForDotProto
  (extraImports, extraInstances)
  dotProto@DotProto
    { protoPackage     = DotProtoPackageSpec packageIdentifier
    , protoMeta        = DotProtoMeta { metaModulePath = modulePath }
    , protoDefinitions
    }
  importTypeContext = do
    moduleName <- modulePathModName modulePath

    typeContextImports <- ctxtImports importTypeContext

    let importDeclarations =
          concat [ defaultImports hasService, extraImports, typeContextImports ]

    typeContext <- dotProtoTypeContext dotProto

    let toDotProtoDeclaration =
            dotProtoDefinitionD packageIdentifier (typeContext <> importTypeContext)

    let instances = instancesForModule moduleName extraInstances

    listOfDeclarations <- traverse toDotProtoDeclaration protoDefinitions

    let overridenDeclarations =
          replaceHsInstDecls instances (mconcat listOfDeclarations)

    return (module_ moduleName Nothing importDeclarations overridenDeclarations)
  where
    hasService = not (null [ () | DotProtoService {} <- protoDefinitions ])

hsModuleForDotProto _ _ _ =
  throwError NoPackageDeclaration

-- This very specific function will only work for the qualification on the very first type
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
  where -- instances defined separately from data type definition:
        mbReplace hid@(HsInstDecl _ _ qn tys _) =
            (: []) . fromMaybe hid $ search qn tys

        -- instances listed in "deriving" clause of data type definition:
        mbReplace (HsDataDecl loc ctx tyn names def insts) =
            let (filtered,customized) = partitionEithers (map (deriv tyn) insts)
            in HsDataDecl loc ctx tyn names def filtered : customized

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
        tc <- mconcat <$> mapM importIt (protoImports dp)
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
  | otherwise =
      do import_ <- liftEither . first CompileParseError =<< importProto searchPaths toplevelFP path
         case protoPackage import_ of
           DotProtoPackageSpec importPkg ->
             do importTypeContext <- dotProtoTypeContext import_
                let importTypeContext' = flip fmap importTypeContext $ \tyInfo ->
                      tyInfo { dotProtoTypeInfoPackage    = DotProtoPackageSpec importPkg
                             , dotProtoTypeInfoModulePath = metaModulePath . protoMeta $ import_
                             }
                    qualifiedTypeContext = M.fromList <$>
                        mapM (\(nm, tyInfo) -> (,tyInfo) <$> concatDotProtoIdentifier importPkg nm)
                            (M.assocs importTypeContext')

                importTypeContext'' <- (importTypeContext' <>) <$> qualifiedTypeContext
                (importTypeContext'' <>) . mconcat <$> sequence
                    [ readImportTypeContext searchPaths toplevelFP (S.insert path alreadyRead) importImport
                    | importImport@(DotProtoImport DotProtoImportPublic _) <- protoImports import_
                    ]
           _ -> throwError NoPackageDeclaration

-- * Type-tracking data structures

-- | Whether a definition is an enumeration or a message
data DotProtoKind = DotProtoKindEnum
                  | DotProtoKindMessage
                  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Information about messages and enumerations
data DotProtoTypeInfo = DotProtoTypeInfo
  { dotProtoTypeInfoPackage    :: DotProtoPackageSpec
     -- ^ The package this type is defined in
  , dotProtoTypeInfoParent     :: DotProtoIdentifier
    -- ^ The message this type is nested under, or 'Anonymous' if it's top-level
  , dotProtoTypeChildContext   :: TypeContext
    -- ^ The context that should be used for declarations within the
    --   scope of this type
  , dotProtoTypeInfoKind       :: DotProtoKind
    -- ^ Whether this type is an enumeration or message
  , dotProtoTypeInfoModulePath :: Path
    -- ^ The include-relative module path used when importing this module
  } deriving Show

-- | A mapping from .proto type identifiers to their type information
type TypeContext = M.Map DotProtoIdentifier DotProtoTypeInfo

-- ** Generating type contexts from ASTs

dotProtoTypeContext :: MonadError CompileError m => DotProto -> m TypeContext
dotProtoTypeContext DotProto { protoDefinitions
                             , protoMeta = DotProtoMeta modulePath
                             }
  = mconcat <$> mapM (definitionTypeContext modulePath) protoDefinitions

definitionTypeContext
    :: MonadError CompileError m => Path -> DotProtoDefinition -> m TypeContext
definitionTypeContext modulePath (DotProtoMessage msgIdent parts) =
    do childTyContext <-
          mapM updateDotProtoTypeInfoParent =<<
          (mconcat <$> sequenceA
               [ definitionTypeContext modulePath def
               | DotProtoMessageDefinition def <- parts ])

       qualifiedChildTyContext <- M.fromList <$>
          mapM (\(nm, tyInfo) -> (,tyInfo) <$>
                                 concatDotProtoIdentifier msgIdent nm)
               (M.assocs childTyContext)

       pure (M.singleton msgIdent
                 (DotProtoTypeInfo DotProtoNoPackage Anonymous
                      childTyContext DotProtoKindMessage modulePath) <>
               qualifiedChildTyContext)
  where updateDotProtoTypeInfoParent tyInfo =
            do dotProtoTypeInfoParent <-
                     concatDotProtoIdentifier msgIdent (dotProtoTypeInfoParent tyInfo)
               pure tyInfo { dotProtoTypeInfoParent }
definitionTypeContext modulePath (DotProtoEnum enumIdent _) =
  pure (M.singleton enumIdent
            (DotProtoTypeInfo DotProtoNoPackage Anonymous mempty DotProtoKindEnum modulePath))
definitionTypeContext _ _ = pure mempty

concatDotProtoIdentifier :: MonadError CompileError m
                         => DotProtoIdentifier -> DotProtoIdentifier -> m DotProtoIdentifier

concatDotProtoIdentifier Qualified{} _ = internalError "concatDotProtoIdentifier: Qualified"
concatDotProtoIdentifier _ Qualified{} = internalError "concatDotProtoIdentifier Qualified"

concatDotProtoIdentifier Anonymous Anonymous = pure Anonymous
concatDotProtoIdentifier Anonymous b = pure b
concatDotProtoIdentifier a Anonymous = pure a

concatDotProtoIdentifier (Single a) b = concatDotProtoIdentifier (Dots (Path [a])) b
concatDotProtoIdentifier a (Single b) = concatDotProtoIdentifier a (Dots (Path [b]))

concatDotProtoIdentifier (Dots (Path a)) (Dots (Path b)) = pure . Dots . Path $ a ++ b

-- | Given a type context, generates the import statements necessary
--   to import all the required types.
ctxtImports :: MonadError CompileError m => TypeContext -> m [HsImportDecl]
ctxtImports tyCtxt =
  do imports <- nub <$> sequence
                          [ modulePathModName modulePath
                          | DotProtoTypeInfo { dotProtoTypeInfoModulePath = modulePath }
                             <- M.elems tyCtxt
                          ]
     pure [ importDecl_ modName True Nothing Nothing | modName <- imports ]

-- * Functions to convert 'DotProtoType' into Haskell types

-- | Produce the Haskell type for the given 'DotProtoType' in the
--   given 'TypeContext'
hsTypeFromDotProto :: MonadError CompileError m => TypeContext -> DotProtoType -> m HsType
hsTypeFromDotProto ctxt = \case
  Prim (Named msgName)
     | Just DotProtoKindMessage <- dotProtoTypeInfoKind <$> M.lookup msgName ctxt
     -> HsTyApp (primType_ "Maybe") <$> hsTypeFromDotProtoPrim ctxt (Named msgName)
  Prim pType           -> hsTypeFromDotProtoPrim ctxt pType
  Optional (Named nm)  -> hsTypeFromDotProto ctxt (Prim (Named nm))
  Optional pType       -> HsTyApp (primType_ "Maybe") <$> hsTypeFromDotProtoPrim ctxt pType
  Repeated pType       -> HsTyApp (primType_ "Vector") <$> hsTypeFromDotProtoPrim ctxt pType
  NestedRepeated pType -> HsTyApp (primType_ "Vector") <$> hsTypeFromDotProtoPrim ctxt pType
  Map k v              -> HsTyApp . HsTyApp (primType_ "Map")
                          <$> hsTypeFromDotProtoPrim ctxt k
                          <*> hsTypeFromDotProto ctxt (Prim v) -- need to 'Nest' message types

hsTypeFromDotProtoPrim :: MonadError CompileError m => TypeContext -> DotProtoPrimType -> m HsType
hsTypeFromDotProtoPrim _    Int32   = pure $ primType_ "Int32"
hsTypeFromDotProtoPrim _    Int64   = pure $ primType_ "Int64"
hsTypeFromDotProtoPrim _    SInt32  = pure $ primType_ "Int32"
hsTypeFromDotProtoPrim _    SInt64  = pure $ primType_ "Int64"
hsTypeFromDotProtoPrim _    UInt32  = pure $ primType_ "Word32"
hsTypeFromDotProtoPrim _    UInt64  = pure $ primType_ "Word64"
hsTypeFromDotProtoPrim _    Fixed32 = pure $ HsTyApp (protobufType_ "Fixed") (primType_ "Word32")
hsTypeFromDotProtoPrim _    Fixed64 = pure $ HsTyApp (protobufType_ "Fixed") (primType_ "Word64")
hsTypeFromDotProtoPrim _    SFixed32= pure $ HsTyApp (protobufType_ "Fixed") (primType_ "Int32")
hsTypeFromDotProtoPrim _    SFixed64= pure $ HsTyApp (protobufType_ "Fixed") (primType_ "Int64")
hsTypeFromDotProtoPrim _    String  = pure $ primType_ "Text"
hsTypeFromDotProtoPrim _    Bytes   = pure $ primType_ "ByteString"
hsTypeFromDotProtoPrim _    Bool    = pure $ primType_ "Bool"
hsTypeFromDotProtoPrim _    Float   = pure $ primType_ "Float"
hsTypeFromDotProtoPrim _    Double  = pure $ primType_ "Double"
hsTypeFromDotProtoPrim ctxt (Named msgName) =
    case M.lookup msgName ctxt of
      Just ty@(DotProtoTypeInfo { dotProtoTypeInfoKind = DotProtoKindEnum }) ->
          HsTyApp (protobufType_ "Enumerated") <$> msgTypeFromDpTypeInfo ty msgName
      Just ty -> msgTypeFromDpTypeInfo ty msgName
      Nothing -> noSuchTypeError msgName

-- | Generate the Haskell type name for a 'DotProtoTypeInfo' for a message /
--   enumeration being compiled. NB: We ignore the 'dotProtoTypeInfoPackage'
--   field of the 'DotProtoTypeInfo' parameter, instead demanding that we have
--   been provided with a valid module path in its 'dotProtoTypeInfoModulePath'
--   field. The latter describes the name of the Haskell module being generated.
msgTypeFromDpTypeInfo
    :: MonadError CompileError m
    => DotProtoTypeInfo -> DotProtoIdentifier -> m HsType
msgTypeFromDpTypeInfo
  DotProtoTypeInfo{ dotProtoTypeInfoModulePath = Path [] }
  _ident
  = throwError InternalEmptyModulePath
msgTypeFromDpTypeInfo
  DotProtoTypeInfo { dotProtoTypeInfoParent     = p
                   , dotProtoTypeInfoModulePath = modulePath
                   }
  ident
  = HsTyCon <$> do Qual <$> modulePathModName modulePath
                        <*> do HsIdent <$> do
                                 nestedTypeName p =<< dpIdentUnqualName ident

-- | Given a 'DotProtoIdentifier' for the parent type and the unqualified name of this type, generate the corresponding Haskell name
nestedTypeName :: MonadError CompileError m => DotProtoIdentifier -> String -> m String
nestedTypeName Anonymous             nm = typeLikeName nm
nestedTypeName (Single parent)       nm = intercalate "_" <$> sequenceA [ typeLikeName parent , typeLikeName nm ]
nestedTypeName (Dots (Path parents)) nm = intercalate "_" . (<>[nm]) <$> mapM typeLikeName parents
nestedTypeName (Qualified {})        _  = internalError "nestedTypeName: Qualified"

haskellName, jsonpbName, grpcName, protobufName, proxyName
    :: String -> HsQName
haskellName  name = Qual (Module "Hs")         (HsIdent name)
jsonpbName   name = Qual (Module "HsJSONPB")   (HsIdent name)
grpcName     name = Qual (Module "HsGRPC")     (HsIdent name)
protobufName name = Qual (Module "HsProtobuf") (HsIdent name)
proxyName    name = Qual (Module "Proxy")      (HsIdent name)

#ifdef DHALL
hsDhallPB :: String
hsDhallPB = "HsDhallPb"

dhallPBName :: String -> HsQName
dhallPBName name = Qual (Module hsDhallPB) (HsIdent name)
#endif

camelCased :: String -> String
camelCased s = do
  (prev, cur) <- zip (Nothing:map Just s) (map Just s ++ [Nothing])
  case (prev, cur) of
    (Just '_', Just x) | isAlpha x -> pure (toUpper x)
    (Just '_', Nothing) -> pure '_'
    (Just '_', Just '_') -> pure '_'
    (_, Just '_') -> empty
    (_, Just x) -> pure x
    (_, _) -> empty

typeLikeName :: MonadError CompileError m => String -> m String
typeLikeName ident@(firstChar:remainingChars)
  | isUpper firstChar = pure (camelCased ident)
  | isLower firstChar = pure (camelCased (toUpper firstChar:remainingChars))
  | firstChar == '_'  = pure (camelCased ('X':ident))
typeLikeName ident = invalidTypeNameError ident

fieldLikeName :: String -> String
fieldLikeName ident@(firstChar:_)
  | isUpper firstChar = let (prefix, suffix) = span isUpper ident
                        in map toLower prefix ++ suffix
fieldLikeName ident = ident

prefixedEnumFieldName :: String -> String -> String
prefixedEnumFieldName enumName fieldName = enumName <> fieldName

prefixedConName :: MonadError CompileError m => String -> String -> m String
prefixedConName msgName conName =
  (msgName ++) <$> typeLikeName conName

-- TODO: This should be ~:: MessageName -> FieldName -> ...; same elsewhere, the
-- String types are a bit of a hassle.
prefixedFieldName :: MonadError CompileError m => String -> String -> m String
prefixedFieldName msgName fieldName =
  (fieldLikeName msgName ++) <$> typeLikeName fieldName

dpIdentUnqualName :: MonadError CompileError m => DotProtoIdentifier -> m String
dpIdentUnqualName (Single name)       = pure name
dpIdentUnqualName (Dots (Path names)) = pure (last names)
dpIdentUnqualName (Qualified _ next)  = dpIdentUnqualName next
dpIdentUnqualName Anonymous           = internalError "dpIdentUnqualName: Anonymous"

dpIdentQualName :: MonadError CompileError m => DotProtoIdentifier -> m String
dpIdentQualName (Single name)       = pure name
dpIdentQualName (Dots (Path names)) = pure (intercalate "." names)
dpIdentQualName (Qualified _ _)     = internalError "dpIdentQualName: Qualified"
dpIdentQualName Anonymous           = internalError "dpIdentQualName: Anonymous"

modulePathModName :: MonadError CompileError m => Path -> m Module
modulePathModName (Path [])    = throwError InternalEmptyModulePath
modulePathModName (Path comps) = Module <$> (intercalate "." <$> mapM typeLikeName comps)

_pkgIdentModName :: MonadError CompileError m => DotProtoIdentifier -> m Module
_pkgIdentModName (Single s)          = Module <$> typeLikeName s
_pkgIdentModName (Dots (Path paths)) = Module <$> (intercalate "." <$> mapM typeLikeName paths)
_pkgIdentModName _                   = internalError "pkgIdentModName: Malformed package name"

-- * Generate instances for a 'DotProto' package

dotProtoDefinitionD
    :: MonadError CompileError m
    => DotProtoIdentifier -> TypeContext -> DotProtoDefinition -> m [HsDecl]
dotProtoDefinitionD _ ctxt (DotProtoMessage messageName dotProtoMessage) =
    dotProtoMessageD ctxt Anonymous messageName dotProtoMessage
dotProtoDefinitionD _ _ (DotProtoEnum messageName dotProtoEnum) =
    dotProtoEnumD Anonymous messageName dotProtoEnum
dotProtoDefinitionD pkgIdent ctxt (DotProtoService serviceName dotProtoService) =
    dotProtoServiceD pkgIdent ctxt serviceName dotProtoService

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
    :: MonadError CompileError m
    => TypeContext
    -> DotProtoIdentifier
    -> DotProtoIdentifier
    -> [DotProtoMessagePart]
    -> m [HsDecl]
dotProtoMessageD ctxt parentIdent messageIdent message = do
       messageName <- nestedTypeName parentIdent =<< dpIdentUnqualName messageIdent

       let ctxt' = maybe mempty dotProtoTypeChildContext (M.lookup messageIdent ctxt) <> ctxt

           messagePartFieldD (DotProtoMessageField (DotProtoField _ ty fieldName _ _)) = do
               fullName <- prefixedFieldName messageName =<< dpIdentUnqualName fieldName
               fullTy <- hsTypeFromDotProto ctxt' ty
               pure [ ([HsIdent fullName], HsUnBangedTy fullTy ) ]

           messagePartFieldD (DotProtoMessageOneOf fieldName _) = do
               fullName <- prefixedFieldName messageName =<< dpIdentUnqualName fieldName
               qualTyName <- prefixedConName messageName =<< dpIdentUnqualName fieldName
               let fullTy = HsTyApp (HsTyCon (haskellName "Maybe")) . type_ $ qualTyName
               pure [ ([HsIdent fullName], HsUnBangedTy fullTy) ]

           messagePartFieldD _ = pure []

           nestedDecls :: MonadError CompileError m => DotProtoDefinition -> m [HsDecl]
           nestedDecls (DotProtoMessage subMsgName subMessageDef) = do
               parentIdent' <- concatDotProtoIdentifier parentIdent messageIdent
               dotProtoMessageD ctxt' parentIdent' subMsgName subMessageDef

           nestedDecls (DotProtoEnum subEnumName subEnumDef) = do
               parentIdent' <- concatDotProtoIdentifier parentIdent messageIdent
               dotProtoEnumD parentIdent' subEnumName subEnumDef

           nestedDecls _ = pure []

           nestedOneOfDecls :: MonadError CompileError m
                            => DotProtoIdentifier -> [DotProtoField] -> m [HsDecl]
           nestedOneOfDecls identifier fields = do
               fullName <- prefixedConName messageName =<< dpIdentUnqualName identifier
               let oneOfCons (DotProtoField _ ty fieldName _ _) = do
                       consTy <- case ty of
                            Prim msg@(Named msgName)
                              | Just DotProtoKindMessage <- dotProtoTypeInfoKind <$> M.lookup msgName ctxt'
                                -> -- Do not wrap message summands with Maybe.
                                   hsTypeFromDotProtoPrim ctxt' msg

                            _   -> hsTypeFromDotProto ctxt' ty

                       consName <- prefixedConName fullName =<< dpIdentUnqualName fieldName
                       let ident = HsIdent consName
                       pure (conDecl_ ident [HsUnBangedTy consTy], ident)

                   oneOfCons DotProtoEmptyField = internalError "field type : empty field"

               (cons, idents) <- fmap unzip (mapM oneOfCons fields)

               fieldNames <- mapM (dpIdentUnqualName . dotProtoFieldName) fields

               toSchemaInstance <- toSchemaInstanceDeclaration fullName fieldNames (Just idents)

               pure [ dataDecl_ fullName cons defaultMessageDeriving
                    , namedInstD fullName
                    , toSchemaInstance

#ifdef DHALL
                    , dhallInterpretInstDecl fullName
                    , dhallInjectInstDecl fullName
#endif
                    ]

       conDecl <- recDecl_ (HsIdent messageName) . mconcat <$>
                  mapM messagePartFieldD message

       nestedDecls_ <- mconcat <$>
           sequence [ nestedDecls def | DotProtoMessageDefinition def <- message]

       nestedOneofs_ <- mconcat <$>
           sequence [ nestedOneOfDecls ident fields | DotProtoMessageOneOf ident fields <- message ]

       messageInst <- messageInstD ctxt' parentIdent messageIdent message

       toJSONPBInst   <- toJSONPBMessageInstD   ctxt' parentIdent messageIdent message
       fromJSONPBInst <- fromJSONPBMessageInstD ctxt' parentIdent messageIdent message

       fieldNames <- sequence $ do
           messagePart <- message
           dotProtoIdentifier <- case messagePart of
                 DotProtoMessageField dotProtoField ->
                   return (dotProtoFieldName dotProtoField)
                 DotProtoMessageOneOf dotProtoIdentifier _ ->
                   return dotProtoIdentifier
                 _ -> empty
           return (dpIdentUnqualName dotProtoIdentifier)

       toSchemaInstance <- toSchemaInstanceDeclaration messageName fieldNames Nothing

       pure $ [ dataDecl_ messageName [ conDecl ] defaultMessageDeriving
              , namedInstD messageName
              , messageInst
              , toJSONPBInst
              , fromJSONPBInst
                -- Generate Aeson instances in terms of JSONPB instances
              , toJSONInstDecl messageName
              , fromJSONInstDecl messageName
              -- And the Swagger ToSchema instance corresponding to JSONPB encodings
              , toSchemaInstance

#ifdef DHALL
              -- Generate Dhall instances
              , dhallInterpretInstDecl messageName
              , dhallInjectInstDecl messageName
#endif
              ]
              <> nestedOneofs_
              <> nestedDecls_

-- *** Generate Protobuf 'Message' instances

messageInstD
    :: MonadError CompileError m
    => TypeContext
    -> DotProtoIdentifier -> DotProtoIdentifier
    -> [DotProtoMessagePart]
    -> m HsDecl
messageInstD ctxt parentIdent msgIdent messageParts = do
     msgName <- nestedTypeName parentIdent =<< dpIdentUnqualName msgIdent
     qualifiedFields <- getQualifiedFields msgName messageParts

     let encodeMessagePart1 QualifiedField{recordFieldName, fieldInfo} =
             let recordFieldName' = HsVar (unqual_ (coerce recordFieldName)) in
             case fieldInfo of
                 FieldNormal _fieldName fieldNum dpType options ->
                     let fieldE = wrapE ctxt dpType options recordFieldName'
                     in apply encodeMessageFieldE [ fieldNumberE fieldNum, fieldE ]

                 FieldOneOf OneofField{subfields} ->
                      -- Create all pattern match & expr for each constructor:
                      --    Constructor y -> encodeMessageField num (Nested (Just y)) -- for embedded messages
                      --    Constructor y -> encodeMessageField num (ForceEmit y)     -- for everything else
                      let mkAlt (OneofSubfield fieldNum conName _ dpType options) =
                            let wrapMaybe
                                   | Prim (Named tyName) <- dpType
                                   , Just DotProtoKindMessage <- dotProtoTypeInfoKind <$> M.lookup tyName ctxt
                                   = HsParen . HsApp (HsVar (haskellName "Just"))
                                   | otherwise
                                   = forceEmitE

                                xE = wrapE ctxt dpType options
                                   . wrapMaybe
                                   $ HsVar (unqual_ "y")

                            in
                              alt_ (HsPApp (unqual_ conName) [patVar "y"])
                                   (HsUnGuardedAlt (apply encodeMessageFieldE [fieldNumberE fieldNum, xE]))
                                   []

                      in HsCase recordFieldName'
                             [ alt_ (HsPApp (haskellName "Nothing") [])
                                    (HsUnGuardedAlt memptyE)
                                    []
                             , alt_ (HsPApp (haskellName "Just") [patVar "x"])
                                    (HsUnGuardedAlt (HsCase (HsVar (unqual_ "x")) (map mkAlt subfields)))
                                    []
                             ]

     let decodeMessagePart1 QualifiedField{fieldInfo} =
             case fieldInfo of
                 FieldNormal _fieldName fieldNum dpType options ->
                     unwrapE ctxt dpType options $ apply atE [ decodeMessageFieldE, fieldNumberE fieldNum ]

                 FieldOneOf OneofField{subfields} ->
                     -- create a list of (fieldNumber, Cons <$> parser)
                     let subfieldParserE (OneofSubfield fieldNumber consName _ dpType options) =
                           let fE = case dpType of
                                      Prim (Named tyName)
                                        | Just DotProtoKindMessage <- dotProtoTypeInfoKind <$> M.lookup tyName ctxt
                                          -> HsParen (HsApp fmapE (HsVar (unqual_ consName)))
                                      _ -> HsParen (HsInfixApp (HsVar (haskellName "Just"))
                                                               composeOp
                                                               (HsVar (unqual_ consName)))
                           in HsTuple
                                [ fieldNumberE fieldNumber
                                , HsInfixApp (apply pureE [ fE ])
                                             apOp
                                             (unwrapE ctxt dpType options decodeMessageFieldE)
                                ]

                     in apply oneofE [ HsVar (haskellName "Nothing")
                                     , HsList (map subfieldParserE subfields)
                                     ]

     let dotProtoE = HsList
                       [ apply dotProtoFieldC
                            [ fieldNumberE fieldNum
                            , dpTypeE dpType
                            , dpIdentE fieldIdent
                            , HsList (map optionE options)
                            , maybeE (HsLit . HsString) comments
                            ]
                       | DotProtoMessageField (DotProtoField fieldNum dpType fieldIdent options comments)
                          <- messageParts
                       ]

     let punnedFieldsP =
             [ HsPFieldPat (unqual_ fieldName) (HsPVar (HsIdent fieldName))
             | QualifiedField (coerce -> fieldName) _ <- qualifiedFields
             ]

     let encodeMessageE = apply mconcatE [ HsList (map encodeMessagePart1 qualifiedFields) ]
     let decodeMessageE = foldl (\f -> HsInfixApp f apOp)
                                (apply pureE [ HsVar (unqual_ msgName) ])
                                (map decodeMessagePart1 qualifiedFields)

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


-- *** Generate ToJSONPB/FromJSONPB instances

toJSONPBMessageInstD
    :: MonadError CompileError m
    => TypeContext
    -> DotProtoIdentifier
    -> DotProtoIdentifier
    -> [DotProtoMessagePart]
    -> m HsDecl
toJSONPBMessageInstD _ctxt parentIdent msgIdent messageParts = do
  msgName    <- nestedTypeName parentIdent =<< dpIdentUnqualName msgIdent
  qualFields <- getQualifiedFields msgName messageParts

  -- E.g.
  -- "another" .= f2 -- always succeeds (produces default value on missing field)
  let defPairE fldName fldNum =
        HsInfixApp (HsLit (HsString (coerce fldName)))
                   toJSONPBOp
                   (HsVar (unqual_ (fieldBinder fldNum)))
  -- E.g.
  -- HsJSONPB.pair "name" f4 -- fails on missing field
  let pairE fldNm varNm =
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
  let oneofCaseE retJsonCtor (OneofField typeName subfields) =
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
                                      $ HsPApp (unqual_ conName) [patVar patVarNm]
                                    ]
                            )
                            (HsUnGuardedAlt (pairE pbFldNm patVarNm))
                            []
              fallthroughE =
                alt_ (HsPApp (haskellName "Nothing") [])
                     (HsUnGuardedAlt memptyE)
                     []

  let patBinder = onQF (const fieldBinder) (oneofSubDisjunctBinder . subfields)

  let applyE nm oneofNm =
        apply (HsVar (jsonpbName nm))
              [ HsList (onQF defPairE (oneofCaseE oneofNm) <$> qualFields) ]

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


fromJSONPBMessageInstD
    :: MonadError CompileError m
    => TypeContext
    -> DotProtoIdentifier
    -> DotProtoIdentifier
    -> [DotProtoMessagePart]
    -> m HsDecl
fromJSONPBMessageInstD _ctxt parentIdent msgIdent messageParts = do
  msgName    <- nestedTypeName parentIdent =<< dpIdentUnqualName msgIdent
  qualFields <- getQualifiedFields msgName messageParts

  let lambdaPVar = patVar "obj"
  let lambdaVar  = HsVar (unqual_ "obj")

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
  let oneofParserE (OneofField oneofType fields) =
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
  let normalParserE fldNm _ =
        HsInfixApp lambdaVar
                   parseJSONPBOp
                   (HsLit (HsString (coerce fldNm)))

  let parseJSONPBE =
        apply (HsVar (jsonpbName "withObject"))
              [ HsLit (HsString msgName)
              , HsParen (HsLambda l [lambdaPVar] fieldAps)
              ]
        where
          fieldAps = foldl (\f -> HsInfixApp f apOp)
                           (apply pureE [ HsVar (unqual_ msgName) ])
                           (onQF normalParserE oneofParserE <$> qualFields)

  let parseJSONPBDecl =
        match_ (HsIdent "parseJSONPB") [] (HsUnGuardedRhs parseJSONPBE) []

  pure (instDecl_ (jsonpbName "FromJSONPB")
                 [ type_ msgName ]
                 [ HsFunBind [ parseJSONPBDecl ] ])

#ifdef DHALL
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


-- ** `ToSchema` instance code-generation

toSchemaInstanceDeclaration
    :: MonadError CompileError m
    => String
    -- ^ Name of the message type to create an instance for
    -> [String]
    -- ^ Field names
    -> Maybe [HsName]
    -- ^ Oneof constructors
    -> m HsDecl
toSchemaInstanceDeclaration messageName fieldNames maybeConstructors = do
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
        HsDo (bindingStatements ++ inferenceStatements ++ [ returnStatement ])
        where
          bindingStatements = do
            (fieldName, qualifiedFieldName) <- zip fieldNames qualifiedFieldNames

            let declareIdentifier = HsIdent (toDeclareName fieldName)

            let rightHandSide0 =
                    HsUnGuardedRhs (HsVar (jsonpbName "declareSchemaRef"))

            let match = HsMatch l declareIdentifier [] rightHandSide0 []

            let statement0 = HsLetStmt [ HsFunBind [ match ] ]

            let declareVariable = HsVar (UnQual declareIdentifier)

            let proxy = HsCon (proxyName "Proxy")

            let rightHandSide1 = HsApp declareVariable proxy

            let pattern = HsPVar (HsIdent qualifiedFieldName)

            let statement1 = HsGenerator l pattern rightHandSide1

            [ statement0, statement1 ]


          inferenceStatements =
              if null fieldNames then [] else [ HsLetStmt [ patternBind ] ]
            where
              arguments = map toArgument fieldNames

              rightHandSide =
                HsUnGuardedRhs (applicativeApply messageConstructor arguments)

              patternBind = HsPatBind l HsPWildCard rightHandSide []

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
            (fieldName, qualifiedFieldName, constructor) <- zip3 fieldNames qualifiedFieldNames constructors

            let declareIdentifier = HsIdent (toDeclareName fieldName)

            let rightHandSide0 =
                    HsUnGuardedRhs (HsVar (jsonpbName "declareSchemaRef"))

            let match = HsMatch l declareIdentifier [] rightHandSide0 []

            let statement0 = HsLetStmt [ HsFunBind [ match ] ]

            let declareVariable = HsVar (UnQual declareIdentifier)

            let proxy = HsCon (proxyName "Proxy")

            let rightHandSide1 = HsApp declareVariable proxy

            let pattern = HsPVar (HsIdent qualifiedFieldName)

            let statement1 = HsGenerator l pattern rightHandSide1

            let inferenceStatements =
                  if null fieldNames then [] else [ HsLetStmt [ patternBind ] ]
                  where
                    arguments = [ toArgument fieldName ]

                    rightHandSide =
                      HsUnGuardedRhs (applicativeApply (HsCon (UnQual constructor)) arguments)

                    patternBind = HsPatBind l HsPWildCard rightHandSide []

            [ statement0, statement1 ] ++ inferenceStatements

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

-- ** Codegen bookkeeping helpers

-- | Bookeeping for qualified fields
data QualifiedField = QualifiedField
  { recordFieldName :: FieldName
  , fieldInfo       :: FieldInfo
  } deriving Show

-- | Bookkeeping for fields
data FieldInfo
  = FieldOneOf OneofField
  | FieldNormal FieldName FieldNumber DotProtoType [DotProtoOption]
  deriving Show

-- | Bookkeeping for oneof fields
data OneofField = OneofField
  { oneofType :: String
  , subfields :: [OneofSubfield]
  } deriving Show

-- | Bookkeeping for oneof subfields
data OneofSubfield = OneofSubfield
  { subfieldNumber   :: FieldNumber
  , subfieldConsName :: String
  , subfieldName     :: FieldName
  , subfieldType     :: DotProtoType
  , subfieldOptions  :: [DotProtoOption]
  } deriving Show

getQualifiedFields
    :: MonadError CompileError m
    => String -> [DotProtoMessagePart] -> m [QualifiedField]
getQualifiedFields msgName msgParts = fmap catMaybes . forM msgParts $ \case
  DotProtoMessageField (DotProtoField fieldNum dpType fieldIdent options _) -> do
    fieldName <- dpIdentUnqualName fieldIdent
    qualName  <- prefixedFieldName msgName fieldName
    pure $ Just $
      QualifiedField (coerce qualName) (FieldNormal (coerce fieldName) fieldNum dpType options)

  DotProtoMessageOneOf _ [] ->
    throwError (InternalError "getQualifiedFields: encountered oneof with no oneof fields")

  DotProtoMessageOneOf oneofIdent fields -> do
    ident <- dpIdentUnqualName oneofIdent
    oneofName <- prefixedFieldName msgName ident
    oneofTypeName <- prefixedConName msgName ident
    fieldElems <- sequence
                    [ do s <- dpIdentUnqualName subFieldName
                         c <- prefixedConName oneofTypeName s
                         pure (OneofSubfield fieldNum c (coerce s) dpType options)
                    | DotProtoField fieldNum dpType subFieldName options _ <- fields
                    ]
    pure $ Just $ QualifiedField (coerce oneofName) (FieldOneOf (OneofField ident fieldElems))

  _ ->
    pure Nothing

-- | Project qualified fields, given a projection function per field type.
onQF :: (FieldName -> FieldNumber -> a) -- ^ projection for normal fields
     -> (OneofField -> a)               -- ^ projection for oneof fields
     -> QualifiedField
     -> a
onQF f _ (QualifiedField _ (FieldNormal fldName fldNum _ _)) = f fldName fldNum
onQF _ g (QualifiedField _ (FieldOneOf fld))                 = g fld

fieldBinder :: FieldNumber -> String
fieldBinder = ("f" ++) . show

oneofSubBinder :: OneofSubfield -> String
oneofSubBinder = fieldBinder . subfieldNumber

oneofSubDisjunctBinder :: [OneofSubfield] -> String
oneofSubDisjunctBinder = intercalate "_or_" . fmap oneofSubBinder

-- ** Helpers to wrap/unwrap types for protobuf (de-)serialization

wrapE :: TypeContext -> DotProtoType -> [DotProtoOption] -> HsExp -> HsExp
wrapE ctxt dpt opts e = HsParen $ maybe id (HsApp . HsParen) (mkWrapE ctxt dpt opts) e

-- the unwrapping function has to be fmapped over the parser.
unwrapE :: TypeContext -> DotProtoType -> [DotProtoOption] -> HsExp -> HsExp
unwrapE ctxt dpt opts e = HsParen $ maybe id (\f -> HsInfixApp f fmapOp) (mkUnwrapE ctxt dpt opts) e

maybeCompose :: Maybe HsExp -> Maybe HsExp -> Maybe HsExp
maybeCompose Nothing e = e
maybeCompose e Nothing = e
maybeCompose (Just e1) (Just e2) = Just $ HsInfixApp e1 composeOp e2

-- | Make a transformer to apply to a haskell type based on the dot proto type.
mkWrapE :: TypeContext
       -> DotProtoType
       -> [DotProtoOption]
       -> Maybe HsExp
mkWrapE ctxt dpt opts = case dpt of
    Prim ty
      -> wrapPrimE ctxt ty
    Optional ty
      -> wrapPrimE ctxt ty
    Repeated (Named tyName)
      | Just DotProtoKindMessage <- dotProtoTypeInfoKind <$> M.lookup tyName ctxt
      -> Just (HsVar (protobufName "NestedVec"))
    Repeated ty
      | isUnpacked opts                     -> wrapVE "UnpackedVec" ty
      | isPacked opts || isPackable ctxt ty -> wrapVE "PackedVec"   ty
      | otherwise                           -> wrapVE "UnpackedVec" ty
    Map k v -> maybeCompose
                (HsApp (HsVar $ haskellName "mapKeysMonotonic") <$> wrapPrimE ctxt k)
                (HsApp fmapE <$> wrapPrimE ctxt v)
    _ -> Nothing
  where
    wrapVE nm ty = maybeCompose (Just (HsVar (protobufName nm))) (wrapPrimVecE ty)

-- | The inverse of wrapE.
mkUnwrapE :: TypeContext
          -> DotProtoType
          -> [DotProtoOption]
          -> Maybe HsExp
mkUnwrapE ctxt dpt opts = case dpt of
  Prim ty
    -> unwrapPrimE ctxt ty
  Optional ty
    -> unwrapPrimE ctxt ty
  Repeated (Named tyName)
    | Just DotProtoKindMessage <- dotProtoTypeInfoKind <$> M.lookup tyName ctxt
    -> Just (HsVar (protobufName "nestedvec"))
  Repeated ty
    | isUnpacked opts                     -> unwrapVE ty "unpackedvec"
    | isPacked opts || isPackable ctxt ty -> unwrapVE ty "packedvec"
    | otherwise                           -> unwrapVE ty "unpackedvec"
  Map k v -> maybeCompose
              (HsApp (HsVar $ haskellName "mapKeysMonotonic") <$> unwrapPrimE ctxt k)
              (HsApp fmapE <$> unwrapPrimE ctxt v)
  _ -> Nothing
 where
   unwrapVE ty nm = maybeCompose (unwrapPrimVecE ty) (Just (HsVar $ protobufName nm))

wrapPrimVecE :: DotProtoPrimType -> Maybe HsExp
wrapPrimVecE ty | isSignedPrim ty = Just $ apply fmapE [ HsVar (protobufName "Signed") ]
wrapPrimVecE _ = Nothing

unwrapPrimVecE :: DotProtoPrimType -> Maybe HsExp
unwrapPrimVecE ty | isSignedPrim ty = Just $ apply fmapE [ HsVar (protobufName "signed") ]
unwrapPrimVecE _ = Nothing

wrapPrimE :: TypeContext -> DotProtoPrimType -> Maybe HsExp
wrapPrimE ctxt (Named tyName)
    | Just DotProtoKindMessage <- dotProtoTypeInfoKind <$> M.lookup tyName ctxt
    = Just . HsVar . protobufName $ "Nested"
wrapPrimE _ ty | isSignedPrim ty = Just . HsVar . protobufName $ "Signed"
wrapPrimE _ _ = Nothing

unwrapPrimE :: TypeContext -> DotProtoPrimType -> Maybe HsExp
unwrapPrimE ctxt (Named tyName)
    | Just DotProtoKindMessage <- dotProtoTypeInfoKind <$> M.lookup tyName ctxt
    = Just . HsVar . protobufName $ "nested"
unwrapPrimE _ ty | isSignedPrim ty = Just . HsVar . protobufName $ "signed"
unwrapPrimE _ _ = Nothing

isPacked, isUnpacked :: [DotProtoOption] -> Bool
isPacked opts =
    case find (\(DotProtoOption name _) -> name == Single "packed") opts of
        Just (DotProtoOption _ (BoolLit x)) -> x
        _ -> False

isUnpacked opts =
    case find (\(DotProtoOption name _) -> name == Single "packed") opts of
        Just (DotProtoOption _ (BoolLit x)) -> not x
        _ -> False

isSignedPrim :: DotProtoPrimType -> Bool
isSignedPrim ty = ty `elem` [ SFixed32, SFixed64, SInt32, SInt64 ]

-- | Returns 'True' if the given primitive type is packable. The 'TypeContext'
-- is used to distinguish Named enums and messages, only the former of which are
-- packable.
isPackable :: TypeContext -> DotProtoPrimType -> Bool
isPackable _ Bytes    = False
isPackable _ String   = False
isPackable _ Int32    = True
isPackable _ Int64    = True
isPackable _ SInt32   = True
isPackable _ SInt64   = True
isPackable _ UInt32   = True
isPackable _ UInt64   = True
isPackable _ Fixed32  = True
isPackable _ Fixed64  = True
isPackable _ SFixed32 = True
isPackable _ SFixed64 = True
isPackable _ Bool     = True
isPackable _ Float    = True
isPackable _ Double   = True
isPackable ctxt (Named tyName) =
  Just DotProtoKindEnum == (dotProtoTypeInfoKind <$> M.lookup tyName ctxt)

internalError :: MonadError CompileError m => String -> m a
internalError = throwError . InternalError

invalidTypeNameError :: MonadError CompileError m => String -> m a
invalidTypeNameError = throwError . InvalidTypeName

_unimplementedError :: MonadError CompileError m => String -> m a
_unimplementedError = throwError . Unimplemented

invalidMethodNameError :: MonadError CompileError m => DotProtoIdentifier -> m a
invalidMethodNameError = throwError . InvalidMethodName

noSuchTypeError :: MonadError CompileError m => DotProtoIdentifier -> m a
noSuchTypeError = throwError. NoSuchType

-- ** Generate types and instances for .proto enums

dotProtoEnumD
    :: MonadError CompileError m
    => DotProtoIdentifier
    -> DotProtoIdentifier
    -> [DotProtoEnumPart]
    -> m [HsDecl]
dotProtoEnumD parentIdent enumIdent enumParts =
  do enumName <- nestedTypeName parentIdent =<<
                 dpIdentUnqualName enumIdent

     enumCons <- sortBy (comparing fst) <$>
                 sequence [ (i,) . prefixedEnumFieldName enumName <$> dpIdentUnqualName conIdent
                          | DotProtoEnumField conIdent i _options <- enumParts ]

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
         parseJSONPBDecls =
           [ let pat nm =
                   HsPApp (jsonpbName "String")
                     [ HsPLit (HsString (fromMaybe <*> stripPrefix enumName $ nm)) ]
             in
             match_ (HsIdent "parseJSONPB") [pat conName]
                    (HsUnGuardedRhs
                       (HsApp pureE (HsVar (unqual_ conName))))
                    []
           | conName <- enumConNames
           ]
           <> [ match_ (HsIdent "parseJSONPB") [patVar "v"]
                       (HsUnGuardedRhs
                          (apply (HsVar (jsonpbName "typeMismatch"))
                                 [ HsLit (HsString enumName)
                                 , HsVar (unqual_ "v")
                                 ]))
                       []
              ]

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

         serviceFieldD (DotProtoServiceRPC
                            rpcName
                            (request, requestStreaming)
                            (response, responseStreaming)
                            _
                       ) = do
           fullName <- prefixedFieldName serviceName =<<
                       dpIdentUnqualName rpcName

           methodName <- case rpcName of
                           Single nm -> pure nm
                           _ -> invalidMethodNameError rpcName

           requestTy <- hsTypeFromDotProtoPrim ctxt  (Named request)
           responseTy <- hsTypeFromDotProtoPrim ctxt (Named response)

           let streamingType =
                 case (requestStreaming, responseStreaming) of
                   (Streaming, Streaming)       -> biDiStreamingC
                   (Streaming, NonStreaming)    -> clientStreamingC
                   (NonStreaming, Streaming)    -> serverStreamingC
                   (NonStreaming, NonStreaming) -> normalC

           pure [ ( endpointPrefix ++ methodName
                  , fullName, requestStreaming, responseStreaming
                  , HsUnBangedTy $
                    HsTyFun (tyApp (HsTyVar (HsIdent "request")) [streamingType, requestTy, responseTy])
                            (tyApp ioT [tyApp (HsTyVar (HsIdent "response")) [streamingType, responseTy]])
                  )
                ]

         serviceFieldD _ = pure []

     fieldsD <- mconcat <$> mapM serviceFieldD service

     serverFuncName <- prefixedFieldName serviceName "server"
     clientFuncName <- prefixedFieldName serviceName "client"

     let conDecl = recDecl_ (HsIdent serviceName)
                            [ ([HsIdent hsName], ty) | (_, hsName, _, _, ty) <- fieldsD ]

         serverT = tyApp (HsTyCon (unqual_ serviceName))
                         [ serverRequestT, serverResponseT ]

         serviceServerTypeD = HsTypeSig l [ HsIdent serverFuncName ]
                                        (HsQualType [] (HsTyFun serverT (HsTyFun serviceOptionsC ioActionT)))

         serviceServerD =
             let serverFuncD =
                   match_ (HsIdent serverFuncName)
                          [ HsPRec (unqual_ serviceName)
                                   [ HsPFieldPat (unqual_ methodName)
                                                 (HsPVar (HsIdent methodName))
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
                     [ HsFieldUpdate (grpcName "optNormalHandlers")
                           (HsList [ handlerE unaryHandlerC convertServerHandlerE endpointName hsName
                                   | (endpointName, hsName, NonStreaming, NonStreaming, _) <- fieldsD
                                   ]
                           )

                     , HsFieldUpdate (grpcName "optClientStreamHandlers")
                           (HsList [ handlerE clientStreamHandlerC convertServerReaderHandlerE endpointName hsName
                                   | (endpointName, hsName, Streaming, NonStreaming, _) <- fieldsD
                                   ]
                           )

                     , HsFieldUpdate (grpcName "optServerStreamHandlers")
                           (HsList [ handlerE serverStreamHandlerC convertServerWriterHandlerE endpointName hsName
                                   | (endpointName, hsName, NonStreaming, Streaming, _) <- fieldsD
                                   ]
                           )

                     , HsFieldUpdate (grpcName "optBiDiStreamHandlers")
                           (HsList [ handlerE biDiStreamHandlerC convertServerRWHandlerE endpointName hsName
                                   | (endpointName, hsName, Streaming, Streaming, _) <- fieldsD
                                   ]
                           )
                     , update "optServerHost" "serverHost"
                     , update "optServerPort" "serverPort"
                     , update "optUseCompression" "useCompression"
                     , update "optUserAgentPrefix" "userAgentPrefix"
                     , update "optUserAgentSuffix" "userAgentSuffix"
                     , update "optInitialMetadata" "initialMetadata"
                     , update "optSSLConfig" "sslConfig"
                     , update "optLogger" "logger"
                     ]
             in
                HsFunBind [serverFuncD]

         clientT = tyApp (HsTyCon (unqual_ serviceName)) [ clientRequestT, clientResultT ]

         serviceClientTypeD =
             HsTypeSig l [ HsIdent clientFuncName ]
                       (HsQualType [] (HsTyFun grpcClientT (HsTyApp ioT clientT)))

         serviceClientD =
             let clientFuncD = match_ (HsIdent clientFuncName)
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
             in
                HsFunBind [ clientFuncD ]

     pure [ HsDataDecl l  [] (HsIdent serviceName)
                [ HsIdent "request", HsIdent "response" ]
                [ conDecl ] defaultServiceDeriving

          , serviceServerTypeD
          , serviceServerD

          , serviceClientTypeD
          , serviceClientD
          ]

-- * Common Haskell expressions, constructors, and operators

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
                                , HsIAbs (HsSymbol ".:") ]))
    , importDecl_ proto3WireM               True  (Just protobufNS) Nothing
    , importDecl_ controlApplicativeM       False Nothing
                  (Just (False, [ HsIAbs (HsSymbol "<*>")
                                , HsIAbs (HsSymbol "<|>")
                                , HsIAbs (HsSymbol "<$>")
                                ]
                        )
                  )
    , importDecl_ controlApplicativeM       True  (Just haskellNS) Nothing
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
                  (Just (False, [ importSym "Int16", importSym "Int32"
                                , importSym "Int64" ]))
    , importDecl_ dataWordM                 True  (Just haskellNS)
                  (Just (False, [ importSym "Word16", importSym "Word32"
                                , importSym "Word64" ]))
    , importDecl_ dataProxy                 True (Just proxyNS)   Nothing
    , importDecl_ ghcGenericsM              True (Just haskellNS) Nothing
    , importDecl_ ghcEnumM                  True (Just haskellNS) Nothing
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
defaultMessageDeriving = map haskellName [ "Show", "Eq", "Ord" , "Generic" ]

defaultEnumDeriving :: [HsQName]
defaultEnumDeriving = map haskellName [ "Show", "Bounded", "Eq",   "Ord" , "Generic" ]

defaultServiceDeriving :: [HsQName]
defaultServiceDeriving = map haskellName [ "Generic" ]

-- * Wrappers around haskell-src-exts constructors

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
