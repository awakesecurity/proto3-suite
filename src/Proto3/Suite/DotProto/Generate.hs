-- | This module provides functions to generate Haskell declarations for proto buf messages

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

module Proto3.Suite.DotProto.Generate
  ( CompileResult
  , CompileError(..)
  , TypeContext

  , compileDotProtoFile
  , compileDotProtoFileOrDie
  , hsModuleForDotProto
  , renderHsModuleForDotProto
  , readDotProtoWithContext

  -- * Exposed for unit-testing
  , typeLikeName
  , fieldLikeName
  ) where

import           Control.Applicative
import           Control.Lens                   ((%~), _1, _2, view)
import           Control.Monad.Except
import           Data.Char
import           Data.List                      (find, intercalate, nub, sortBy,
                                                 stripPrefix)
import qualified Data.Map                       as M
import           Data.Maybe                     (catMaybes)
import           Data.Monoid
import           Data.Ord                       (comparing)
import qualified Data.Set                       as S
import           Data.String                    (fromString)
import qualified Data.Text                      as T
import           Debug.Trace                    (trace)
import           Filesystem.Path.CurrentOS      ((</>))
import qualified Filesystem.Path.CurrentOS      as FP
import           Language.Haskell.Pretty
import           Language.Haskell.Syntax
import qualified NeatInterpolation              as Neat
import           Prelude                        hiding (FilePath)
import           Proto3.Suite.DotProto
import           Proto3.Suite.DotProto.Internal
import           Proto3.Wire.Types              (FieldNumber (..))
import           System.IO                      (writeFile)
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

-- | Result of a compilation. 'Left err' on error, where 'err' is a
--   'String' describing the error. Otherwise, the result of the
--   compilation.
type CompileResult = Either CompileError

-- | @compileDotProtoFile out includeDir dotProtoPath@ compiles the .proto file
-- at @dotProtoPath@ into a new Haskell module in @out/@, using the ordered
-- @paths@ list to determine which paths to be searched for the .proto file and
-- its transitive includes. 'compileDotProtoFileOrDie' provides a wrapper around
-- this function which terminates the program with an error message.
compileDotProtoFile :: FilePath -> [FilePath] -> FilePath -> IO (CompileResult ())
compileDotProtoFile out paths dotProtoPath = runExceptT $ do
  (dp, tc) <- ExceptT $ readDotProtoWithContext paths dotProtoPath
  let DotProtoMeta (Path mp) = protoMeta dp
      mkHsModPath            = (out </>) . (FP.<.> "hs") . FP.concat . (fromString <$>)
  when (null mp) $ throwError InternalEmptyModulePath
  mp' <- mkHsModPath <$> mapM (ExceptT . pure . typeLikeName) mp
  hs  <- ExceptT . pure $ renderHsModuleForDotProto dp tc
  Turtle.mktree (FP.directory mp')
  liftIO $ writeFile (FP.encodeString mp') hs

-- | As 'compileDotProtoFile', except terminates the program with an error
-- message on failure.
compileDotProtoFileOrDie :: FilePath -> [FilePath] -> FilePath -> IO ()
compileDotProtoFileOrDie out paths dotProtoPath =
  compileDotProtoFile out paths dotProtoPath >>= \case
    Left e -> do
      let errText          = T.pack (show e) -- TODO: pretty print the error messages
          dotProtoPathText = Turtle.format F.fp dotProtoPath
      dieLines [Neat.text|
        Error: failed to compile "${dotProtoPathText}":

        ${errText}
      |]
    _ -> pure ()

-- | Compile a 'DotProto' AST into a 'String' representing the Haskell
--   source of a module implementing types and instances for the .proto
--   messages and enums.
renderHsModuleForDotProto :: DotProto -> TypeContext -> CompileResult String
renderHsModuleForDotProto dp importCtxt =
    fmap (("{-# LANGUAGE DeriveGeneric #-}\n" ++) .
          ("{-# LANGUAGE DataKinds #-}\n" ++) .
          ("{-# LANGUAGE GADTs #-}\n" ++) .
          ("{-# LANGUAGE OverloadedStrings #-}\n" ++) .
          ("{-# OPTIONS_GHC -fno-warn-unused-imports #-}\n" ++) .
          ("{-# OPTIONS_GHC -fno-warn-name-shadowing #-}\n" ++) .
          ("{-# OPTIONS_GHC -fno-warn-unused-matches #-}\n" ++) .

          ("-- | Generated by Haskell protocol buffer compiler. " ++) .
          ("DO NOT EDIT!\n" ++) .
          prettyPrint) $
    hsModuleForDotProto dp importCtxt

-- | Compile a Haskell module AST given a 'DotProto' package AST.
hsModuleForDotProto :: DotProto -> TypeContext -> CompileResult HsModule
hsModuleForDotProto (DotProto{ protoMeta = DotProtoMeta (Path []) }) _importCtxt
  = Left InternalEmptyModulePath
hsModuleForDotProto
  dp@DotProto{ protoPackage     = DotProtoPackageSpec pkgIdent
             , protoMeta        = DotProtoMeta modulePath
             , protoDefinitions = defs
             }
  importCtxt
  = module_
    <$> modulePathModName modulePath
    <*> pure Nothing
    <*> do mappend (defaultImports hasService) <$> ctxtImports importCtxt
    <*> do tc <- dotProtoTypeContext dp
           mconcat <$> mapM (dotProtoDefinitionD pkgIdent (tc <> importCtxt)) defs

  where hasService = not (null [ () | DotProtoService {} <- defs ])
hsModuleForDotProto _ _
  = Left NoPackageDeclaration

-- | Parses the file at the given path and produces an AST along with a
-- 'TypeContext' representing all types from imported @.proto@ files, using the
-- first parameter as a list of paths to search for imported files. Terminates
-- with exit code 1 when an included file cannot be found in the search path.
readDotProtoWithContext :: [FilePath] -> FilePath -> IO (CompileResult (DotProto, TypeContext))

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

readImportTypeContext :: [FilePath] -> FilePath -> S.Set FilePath -> DotProtoImport
                      -> ExceptT CompileError IO TypeContext
readImportTypeContext searchPaths toplevelFP alreadyRead (DotProtoImport _ path)
  | path `S.member` alreadyRead = throwError (CircularImport path)
  | otherwise =
      do import_ <- wrapError CompileParseError =<< importProto searchPaths toplevelFP path
         case protoPackage import_ of
           DotProtoPackageSpec importPkg ->
             do importTypeContext <- wrapError id (dotProtoTypeContext import_)
                let importTypeContext' = flip fmap importTypeContext $ \tyInfo ->
                      tyInfo { dotProtoTypeInfoPackage    = DotProtoPackageSpec importPkg
                             , dotProtoTypeInfoModulePath = metaModulePath . protoMeta $ import_
                             }
                    qualifiedTypeContext = M.fromList <$>
                        mapM (\(nm, tyInfo) -> (,tyInfo) <$> concatDotProtoIdentifier importPkg nm)
                            (M.assocs importTypeContext')

                importTypeContext'' <- wrapError id ((importTypeContext' <>) <$> qualifiedTypeContext)
                (importTypeContext'' <>) . mconcat <$> sequence
                    [ readImportTypeContext searchPaths toplevelFP (S.insert path alreadyRead) importImport
                    | importImport@(DotProtoImport DotProtoImportPublic _) <- protoImports import_]
           _ -> throwError NoPackageDeclaration
  where
    wrapError :: (err' -> err) -> Either err' a -> ExceptT err IO a
    wrapError f = either (throwError . f) pure

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

dotProtoTypeContext :: DotProto -> CompileResult TypeContext
dotProtoTypeContext DotProto { protoDefinitions
                             , protoMeta = DotProtoMeta modulePath
                             }
  = mconcat <$> mapM (definitionTypeContext modulePath) protoDefinitions

definitionTypeContext :: Path -> DotProtoDefinition -> CompileResult TypeContext
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

concatDotProtoIdentifier :: DotProtoIdentifier -> DotProtoIdentifier
                         -> CompileResult DotProtoIdentifier
concatDotProtoIdentifier Qualified {} _ =
    internalError "concatDotProtoIdentifier: Qualified"
concatDotProtoIdentifier _ Qualified {} =
    internalError "concatDotProtoIdentifier Qualified"
concatDotProtoIdentifier Anonymous Anonymous = pure Anonymous
concatDotProtoIdentifier Anonymous b = pure b
concatDotProtoIdentifier a Anonymous = pure a
concatDotProtoIdentifier (Single a) b = concatDotProtoIdentifier (Dots (Path [a])) b
concatDotProtoIdentifier a (Single b) = concatDotProtoIdentifier a (Dots (Path [b]))
concatDotProtoIdentifier (Dots (Path a)) (Dots (Path b)) = pure . Dots . Path $ a ++ b

-- | Given a type context, generates the import statements necessary
--   to import all the required types.
ctxtImports :: TypeContext -> CompileResult [HsImportDecl]
ctxtImports tyCtxt =
  do imports <- nub <$> sequence
                          [ modulePathModName modulePath
                          | DotProtoTypeInfo
                            { dotProtoTypeInfoModulePath = modulePath
                            } <- M.elems tyCtxt
                          ]
     pure [ importDecl_ modName True Nothing Nothing | modName <- imports ]

-- * Functions to convert 'DotProtoType' into Haskell types

-- | Produce the Haskell type for the given 'DotProtoType' in the
--   given 'TypeContext'
hsTypeFromDotProto :: TypeContext -> DotProtoType -> CompileResult HsType
hsTypeFromDotProto ctxt (Prim (Named msgName))
    | Just DotProtoKindMessage <-
          dotProtoTypeInfoKind <$> M.lookup msgName ctxt =
        HsTyApp (primType_ "Maybe") <$>
            hsTypeFromDotProtoPrim ctxt (Named msgName)
hsTypeFromDotProto ctxt (Prim pType) =
    hsTypeFromDotProtoPrim ctxt pType
hsTypeFromDotProto ctxt (Optional (Named nm)) =
    hsTypeFromDotProto ctxt (Prim (Named nm))
hsTypeFromDotProto ctxt (Optional pType) =
    HsTyApp (primType_ "Maybe") <$> hsTypeFromDotProtoPrim ctxt pType
hsTypeFromDotProto ctxt (Repeated pType) =
    HsTyApp (primType_ "Vector") <$> hsTypeFromDotProtoPrim ctxt pType
hsTypeFromDotProto ctxt (NestedRepeated pType) =
    HsTyApp (primType_ "Vector") <$> hsTypeFromDotProtoPrim ctxt pType
hsTypeFromDotProto _    (Map _ _) =
    internalError "No support for protobuf mappings"

hsTypeFromDotProtoPrim :: TypeContext -> DotProtoPrimType -> CompileResult HsType
hsTypeFromDotProtoPrim _    Int32           = pure $ primType_ "Int32"
hsTypeFromDotProtoPrim _    Int64           = pure $ primType_ "Int64"
hsTypeFromDotProtoPrim _    SInt32          = pure $ primType_ "Int32"
hsTypeFromDotProtoPrim _    SInt64          = pure $ primType_ "Int64"
hsTypeFromDotProtoPrim _    UInt32          = pure $ primType_ "Word32"
hsTypeFromDotProtoPrim _    UInt64          = pure $ primType_ "Word64"
hsTypeFromDotProtoPrim _    Fixed32         = pure $ HsTyApp (protobufType_ "Fixed")
                                                             (primType_ "Word32")
hsTypeFromDotProtoPrim _    Fixed64         = pure $ HsTyApp (protobufType_ "Fixed")
                                                             (primType_ "Word64")
hsTypeFromDotProtoPrim _    SFixed32        = pure $ HsTyApp (protobufType_ "Fixed")
                                                             (primType_ "Int32")
hsTypeFromDotProtoPrim _    SFixed64        = pure $ HsTyApp (protobufType_ "Fixed")
                                                             (primType_ "Int64")
hsTypeFromDotProtoPrim _    String          = pure $ primType_ "Text"
hsTypeFromDotProtoPrim _    Bytes           = pure $ primType_ "ByteString"
hsTypeFromDotProtoPrim _    Bool            = pure $ primType_ "Bool"
hsTypeFromDotProtoPrim _    Float           = pure $ primType_ "Float"
hsTypeFromDotProtoPrim _    Double          = pure $ primType_ "Double"
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
msgTypeFromDpTypeInfo :: DotProtoTypeInfo -> DotProtoIdentifier
                      -> CompileResult HsType
msgTypeFromDpTypeInfo
  DotProtoTypeInfo{ dotProtoTypeInfoModulePath = Path [] }
  _ident
  = Left InternalEmptyModulePath
msgTypeFromDpTypeInfo
  DotProtoTypeInfo { dotProtoTypeInfoParent     = p
                   , dotProtoTypeInfoModulePath = modulePath
                   }
  ident
  = HsTyCon <$> do Qual <$> modulePathModName modulePath
                        <*> do HsIdent <$> do
                                 nestedTypeName p =<< dpIdentUnqualName ident

-- | Given a 'DotProtoIdentifier' for the parent type and the unqualified name of this type, generate the corresponding Haskell name
nestedTypeName :: DotProtoIdentifier -> String -> CompileResult String
nestedTypeName Anonymous       nm = typeLikeName nm
nestedTypeName (Single parent) nm =
    intercalate "_" <$> sequenceA [ typeLikeName parent
                                  , typeLikeName nm ]
nestedTypeName (Dots (Path parents)) nm =
    (<> ("_" <> nm)) <$> (intercalate "_" <$> mapM typeLikeName parents)
nestedTypeName (Qualified {})  _  = internalError "nestedTypeName: Qualified"

haskellName, jsonpbName, grpcName, protobufName :: String -> HsQName
haskellName  name = Qual (Module "Hs") (HsIdent name)
jsonpbName   name = Qual (Module "HsJSONPB") (HsIdent name)
grpcName     name = Qual (Module "HsGRPC") (HsIdent name)
protobufName name = Qual (Module "HsProtobuf") (HsIdent name)

camelCased :: String -> String
camelCased s = do (prev, cur) <- zip (Nothing:map Just s) (map Just s ++ [Nothing])
                  case (prev, cur) of
                    (Just '_', Just x) | isAlpha x -> pure (toUpper x)
                    (Just '_', Nothing) -> pure '_'
                    (Just '_', Just '_') -> pure '_'
                    (_, Just '_') -> empty
                    (_, Just x) -> pure x
                    (_, _) -> empty

typeLikeName :: String -> CompileResult String
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

-- E.g. "SomethingNameOrId" => "SomethingNameOrId_NOT_SET"
oneofNotSetName :: String -> CompileResult String
oneofNotSetName = typeLikeName . (<> "__NOT__SET")

prefixedConName :: String -> String -> CompileResult String
prefixedConName msgName conName =
  (msgName ++) <$> typeLikeName conName

prefixedFieldName :: String -> String -> CompileResult String
prefixedFieldName msgName fieldName =
  (fieldLikeName msgName ++) <$> typeLikeName fieldName

dpIdentUnqualName :: DotProtoIdentifier -> CompileResult String
dpIdentUnqualName (Single name)       = pure name
dpIdentUnqualName (Dots (Path names)) = pure (last names)
dpIdentUnqualName (Qualified _ next)  = dpIdentUnqualName next
dpIdentUnqualName Anonymous           = internalError "dpIdentUnqualName: Anonymous"

dpIdentQualName :: DotProtoIdentifier -> CompileResult String
dpIdentQualName (Single name)       = pure name
dpIdentQualName (Dots (Path names)) = pure (intercalate "." names)
dpIdentQualName (Qualified _ _)     = internalError "dpIdentQualName: Qualified"
dpIdentQualName Anonymous           = internalError "dpIdentQualName: Anonymous"

modulePathModName :: Path -> CompileResult Module
modulePathModName (Path [])    = Left InternalEmptyModulePath
modulePathModName (Path comps) = Module <$> (intercalate "." <$> mapM typeLikeName comps)

_pkgIdentModName :: DotProtoIdentifier -> CompileResult Module
_pkgIdentModName (Single s)          = Module <$> typeLikeName s
_pkgIdentModName (Dots (Path paths)) = Module <$> (intercalate "." <$> mapM typeLikeName paths)
_pkgIdentModName _                   = internalError "pkgIdentModName: Malformed package name"

-- * Generate instances for a 'DotProto' package

dotProtoDefinitionD :: DotProtoIdentifier -> TypeContext -> DotProtoDefinition
                    -> CompileResult [HsDecl]
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
dotProtoMessageD :: TypeContext -> DotProtoIdentifier -> DotProtoIdentifier
                 -> [DotProtoMessagePart] -> CompileResult [HsDecl]
dotProtoMessageD ctxt parentIdent messageIdent message =
    do messageName <- nestedTypeName parentIdent =<<
                      dpIdentUnqualName messageIdent

       let ctxt' = maybe mempty dotProtoTypeChildContext (M.lookup messageIdent ctxt) <>
                   ctxt

           messagePartFieldD (DotProtoMessageField (DotProtoField _ ty fieldName _ _)) =
               do fullName <- prefixedFieldName messageName =<<
                              dpIdentUnqualName fieldName
                  fullTy <- hsTypeFromDotProto ctxt' ty
                  pure [ ([HsIdent fullName], HsUnBangedTy fullTy ) ]
           messagePartFieldD (DotProtoMessageOneOf fieldName _) =
               do fullName <- prefixedFieldName messageName =<<
                              dpIdentUnqualName fieldName
                  fullTy <- prefixedConName messageName =<<
                            dpIdentUnqualName fieldName
                  pure [ ([HsIdent fullName], HsUnBangedTy (type_ fullTy) ) ]
           messagePartFieldD _ = pure []

           nestedDecls :: DotProtoDefinition -> CompileResult [HsDecl]
           nestedDecls (DotProtoMessage subMsgName subMessageDef) =
               do parentIdent' <- concatDotProtoIdentifier parentIdent messageIdent
                  dotProtoMessageD ctxt' parentIdent' subMsgName subMessageDef
           nestedDecls (DotProtoEnum subEnumName subEnumDef) =
               do parentIdent' <- concatDotProtoIdentifier parentIdent messageIdent
                  dotProtoEnumD parentIdent' subEnumName subEnumDef
           nestedDecls _ = pure []

           nestedOneOfDecls :: DotProtoIdentifier -> [DotProtoField] -> CompileResult [HsDecl]
           nestedOneOfDecls identifier fields =
               do fullName   <- prefixedConName messageName =<<
                                dpIdentUnqualName identifier
                  notSetName <- oneofNotSetName fullName
                  let oneOfCons (DotProtoField _ ty fieldName _ _) =
                        do consTy <- hsTypeFromDotProto ctxt' ty
                           consName <- prefixedConName fullName =<< dpIdentUnqualName fieldName
                           pure $ conDecl_ (HsIdent consName) [HsUnBangedTy consTy]
                      oneOfCons DotProtoEmptyField =
                          internalError "field type : empty field"

                  let notSetCons = conDecl_ (HsIdent notSetName) []
                  cons <- (notSetCons:) <$> mapM oneOfCons fields

                  pure [dataDecl_ fullName cons defaultMessageDeriving ]

       conDecl <- recDecl_ (HsIdent messageName) . mconcat <$>
                  mapM messagePartFieldD message

       nestedDecls_ <- mconcat <$>
           sequence [ nestedDecls def | DotProtoMessageDefinition def <- message]
       nestedOneofs_ <- mconcat <$>
           sequence [ nestedOneOfDecls ident fields
                    | DotProtoMessageOneOf ident fields <- message ]

       messageInst <- messageInstD ctxt' parentIdent messageIdent message

       toJSONPBInst   <- toJSONPBMessageInstD   ctxt' parentIdent messageIdent message
       fromJSONPBInst <- fromJSONPBMessageInstD ctxt' parentIdent messageIdent message

       pure $ [ dataDecl_ messageName [ conDecl ] defaultMessageDeriving
              , namedInstD messageName
              , messageInst
              , toJSONPBInst
              , fromJSONPBInst
              ]
              <> nestedOneofs_
              <> nestedDecls_

messageInstD :: TypeContext -> DotProtoIdentifier -> DotProtoIdentifier
             -> [DotProtoMessagePart] -> CompileResult HsDecl
messageInstD ctxt parentIdent msgIdent messageParts =
  do msgName <- nestedTypeName parentIdent =<<
                dpIdentUnqualName msgIdent
     qualifiedFields <- getQualifiedFields msgName messageParts
     encodeMessagePartEs <- forM qualifiedFields $ \(fieldName, field) ->
        case field of
            FieldNormal fieldNum dpType options ->
                do fieldE <- wrapE ctxt dpType options (HsVar (unqual_ fieldName))
                   pure (apply encodeMessageFieldE [ fieldNumberE fieldNum, fieldE ])
            FieldOneOf elems notSetName ->
                do -- Create all pattern match & expr for each constructor:
                   --    Constructor x -> encodeField fieldNumber x
                   --    ...
                   --    ..._NOT_SET   -> mempty
                   let alts =
                         [ alt_ (HsPApp (unqual_ conName) [patVar "x"])
                                (HsUnGuardedAlt (apply encodeMessageFieldE [ fieldNumberE fieldNum, HsVar (unqual_ "x") ]))
                                []
                         | OneofSubfield fieldNum conName _ <- elems
                         ]
                         <>
                         [ alt_ (HsPApp (unqual_ notSetName) [])
                                (HsUnGuardedAlt memptyE)
                                []
                         ]
                   pure $ HsCase (HsVar (unqual_ fieldName)) alts

     decodeMessagePartEs <- sequence
        [ case fieldType of
            FieldNormal fieldNum dpType options ->
                unwrapE ctxt dpType options $ apply atE
                        [ decodeMessageFieldE, fieldNumberE fieldNum ]
            FieldOneOf elems _notSetName ->
                -- create a list of (fieldNumber, Cons <$> parser)
                do let toListParser (OneofSubfield fieldNumber consName _) = HsTuple
                        [ fieldNumberE fieldNumber
                        , HsInfixApp (apply pureE [ HsVar (unqual_ consName) ])
                                     apOp
                                     decodeMessageFieldE ]
                   pure $ apply oneofE
                          [ HsList $ map toListParser elems ]
        | (_, fieldType) <- qualifiedFields ]

     dotProtoE <- HsList <$> sequence
         [ dpTypeE dpType >>= \typeE ->
             pure (apply dotProtoFieldC [ fieldNumberE fieldNum, typeE
                                        , dpIdentE fieldIdent
                                        , HsList (map optionE options)
                                        , maybeE (HsLit . HsString) comments ])
         | DotProtoMessageField (DotProtoField fieldNum dpType fieldIdent options comments)
             <- messageParts ]

     let encodeMessageDecl = match_ (HsIdent "encodeMessage")
                                    [HsPWildCard, HsPRec (unqual_ msgName) punnedFieldsP]
                                    (HsUnGuardedRhs encodeMessageE) []
         decodeMessageDecl = match_ (HsIdent "decodeMessage") [ HsPWildCard ]
                                    (HsUnGuardedRhs decodeMessageE) []
         dotProtoDecl = match_ (HsIdent "dotProto") [HsPWildCard]
                               (HsUnGuardedRhs dotProtoE) []

         punnedFieldsP =
             [ HsPFieldPat (unqual_ fieldName) (HsPVar (HsIdent fieldName))
             | (fieldName, _) <- qualifiedFields ]

         encodeMessageE = apply mconcatE [ HsList encodeMessagePartEs ]
         decodeMessageE = foldl (\f -> HsInfixApp f apOp)
                                (apply pureE [ HsVar (unqual_ msgName) ])
                                decodeMessagePartEs

     pure (instDecl_ (protobufName "Message")
                     [ type_ msgName ]
                     (map (HsFunBind . (: []))
                      [ encodeMessageDecl
                      , decodeMessageDecl
                      , dotProtoDecl ]))

toJSONPBMessageInstD :: TypeContext -> DotProtoIdentifier -> DotProtoIdentifier
                     -> [DotProtoMessagePart] -> CompileResult HsDecl
toJSONPBMessageInstD _ctxt parentIdent msgIdent messageParts =
  do
  -- TODO: rename to getNormalFields or somesuch, pass in from caller?
  kvps        <- sequence
                   [ (,"f" ++ show n) <$> dpIdentUnqualName fldIdent
                   | DotProtoMessageField
                       (DotProtoField (FieldNumber n) _ fldIdent _ _)
                       <- messageParts
                   ]
  msgName     <- nestedTypeName parentIdent =<< dpIdentUnqualName msgIdent
  -- TODO: could probably pass this and kvps in from the parent
  oneofFields <- getOneofFields msgName messageParts

  -- E.g.
  -- "another" .= f2
  let dpair fldNm varNm =
        HsInfixApp (HsLit (HsString fldNm)) toJSONPBOp (HsVar (unqual_ varNm))

  -- E.g.
  -- HsJSONPB.pair "name" f4
  let pair fldNm varNm =
        apply (HsVar (jsonpbName "pair"))
              [ HsLit (HsString fldNm)
              , HsVar (unqual_ varNm)
              ]

  -- E.g.
  -- case f4_or_f9 of
  --   SomethingNameOrIdName f4
  --     -> HsJSONPB.pair "name" f4
  --   SomethingNameOrIdSomeid f9
  --     -> HsJSONPB.pair "someid" f9
  --   SomethingNameOrId_NOT_SET
  --     -> mempty
  let oneofCase (OneofField{notSetName,subfields}) =
        HsCase (HsVar (unqual_ fldVarNm)) (alts <> [notSet])
        where
          alts =
            [ let patVarNm = oneofSubBinder sub
              in
              alt_ (HsPApp (unqual_ conName) [patVar patVarNm])
                   (HsUnGuardedAlt (pair pbFldName patVarNm))
                   []
            | sub@(OneofSubfield _ conName pbFldName) <- subfields
            ]
          fldVarNm = oneofSubBinderDisjunct subfields
          notSet =
            alt_ (HsPApp (unqual_ notSetName) [])
                 (HsUnGuardedAlt memptyE)
                 []

  let toEncodingPBE = fieldsPB (normal <> oneof)
        where
          normal        = uncurry dpair <$> kvps
          oneof         = oneofCase     <$> oneofFields
          fieldsPB flds = apply (HsVar (jsonpbName "fieldsPB")) [ HsList flds ]

  let toEncodingPBDecl =
        match_ (HsIdent "toEncodingPB")
               [ HsPApp (unqual_ msgName) $
                   [ patVar varNm | (_, varNm) <- kvps ]
                   <>
                   [ patVar (oneofSubBinderDisjunct subs) | OneofField subs _ <- oneofFields ]
               ]
               (HsUnGuardedRhs toEncodingPBE) []

  pure (instDecl_ (jsonpbName "ToJSONPB")
                 [ type_ msgName ]
                 [ HsFunBind [ toEncodingPBDecl ] ])

fromJSONPBMessageInstD :: TypeContext -> DotProtoIdentifier -> DotProtoIdentifier
                       -> [DotProtoMessagePart] -> CompileResult HsDecl
fromJSONPBMessageInstD _ctxt parentIdent msgIdent messageParts =
  do
  -- TODO: rename to getNormalFields or somesuch, pass in from caller?
  kvps        <- sequence
                   [ (,"f" ++ show n) <$> dpIdentUnqualName fldIdent
                   | DotProtoMessageField
                       (DotProtoField (FieldNumber n) _ fldIdent _ _)
                       <- messageParts
                   ]
  msgName     <- nestedTypeName parentIdent =<< dpIdentUnqualName msgIdent
  -- TODO: could probably pass this and kvps in from the parent
  oneofFields <- getOneofFields msgName messageParts

  let normals =
        [ HsInfixApp (HsVar (unqual_ "obj")) parseJSONPBOp (HsLit (HsString fldNm))
        | ( fldNm, _) <- kvps
        ]

  -- E.g., for message Something{ oneof name_or_id { string name = _; int32 someid = _; } }:
  -- [ Hs.msum
  --     [SomethingNameOrIdName <$> (HsJSONPB.parseField obj "name"),
  --      SomethingNameOrIdSomeid <$> (HsJSONPB.parseField obj "someid"),
  --      Hs.pure SomethingNameOrId_NOT_SET]
  -- , ...
  let oneofs =
        HsApp msumE . HsList . (subParsers <> notSetParser) <$> oneofFields
        where
          notSetParser OneofField{notSetName} = [ HsApp pureE (HsVar (unqual_ notSetName)) ]
          subParsers OneofField{subfields}    = subParser <$> subfields
            where
              -- E.g.
              -- SomethingNameOrIdName <$> (HsJSONPB.parseField obj "name")
              subParser OneofSubfield{subfieldConsName, subfieldName} =
                HsInfixApp (HsVar (unqual_ subfieldConsName))
                           fmapOp
                           (apply (HsVar (jsonpbName "parseField"))
                                  [ HsVar (unqual_ "obj")
                                  , HsLit (HsString subfieldName)
                                  ])

  let parseJSONPBE =
        apply (HsVar (jsonpbName "withObject"))
              [ HsLit (HsString msgName)
              , HsParen (HsLambda l [patVar "obj"] fieldAps)
              ]
        where
          fieldAps = foldl (\f -> HsInfixApp f apOp)
                           (apply pureE [ HsVar (unqual_ msgName) ])
                           (normals <> oneofs)

  let parseJSONPBDecl =
          match_ (HsIdent "parseJSONPB") [] (HsUnGuardedRhs parseJSONPBE) []

  pure (instDecl_ (jsonpbName "FromJSONPB")
                 [ type_ msgName ]
                 [ HsFunBind [ parseJSONPBDecl ] ])

-- ** Codegen bookkeeping helpers

-- | Bookkeeping for fields
data FieldValue
  = FieldOneOf [OneofSubfield] String {- notSetName -}
  | FieldNormal FieldNumber DotProtoType [DotProtoOption]
  deriving Show

-- | Bookkeeping for oneof fields
data OneofField = OneofField
  { subfields  :: [OneofSubfield]
  , notSetName :: String
  } deriving Show

-- | Bookkeeping for oneof subfields
data OneofSubfield = OneofSubfield
  { subfieldNumber   :: FieldNumber
  , subfieldConsName :: String
  , subfieldName     :: String
  } deriving Show

getOneofFields :: String
               -> [DotProtoMessagePart]
               -> CompileResult [OneofField]
getOneofFields msgName msgParts = do
  fieldValues <- fmap snd <$> getQualifiedFields msgName msgParts
  pure [ OneofField subflds notSetName | FieldOneOf subflds notSetName <- fieldValues ]

getQualifiedFields :: String
                   -> [DotProtoMessagePart]
                   -> CompileResult [(String, FieldValue)]
getQualifiedFields msgName msgParts = fmap catMaybes . forM msgParts $ \case
  DotProtoMessageField (DotProtoField fieldNum dpType fieldIdent options _) -> do
    qualName <- prefixedFieldName msgName =<< dpIdentUnqualName fieldIdent
    pure $ Just $ (qualName, FieldNormal fieldNum dpType options)
  DotProtoMessageOneOf _ [] ->
    Left (InternalError "getQualifiedFields: encountered oneof with no oneof fields")
  DotProtoMessageOneOf fieldIdent fields -> do
    fieldName  <- dpIdentUnqualName fieldIdent >>= prefixedFieldName msgName
    consName   <- dpIdentUnqualName fieldIdent >>= prefixedConName msgName
    fieldElems <- sequence
                    [ do s <- dpIdentUnqualName subFieldName
                         c <- prefixedConName consName s
                         pure (OneofSubfield fieldNum c s)
                    | DotProtoField fieldNum _ subFieldName _ _ <- fields
                    ]
    notSetName <- oneofNotSetName fieldName
    pure $ Just $ (fieldName, FieldOneOf fieldElems notSetName)
  _ ->
    pure Nothing

oneofSubBinder :: OneofSubfield -> String
oneofSubBinder = ("f" ++) . show . subfieldNumber

oneofSubBinderDisjunct :: [OneofSubfield] -> String
oneofSubBinderDisjunct = intercalate "_or_" . fmap oneofSubBinder

-- ** Helpers to wrap/unwrap types for protobuf (de-)serialization

wrapE, unwrapE :: TypeContext -> DotProtoType -> [DotProtoOption]
               -> HsExp -> CompileResult HsExp
wrapE ctxt (Prim ty)     _ = pure . wrapPrimE ctxt ty
wrapE ctxt (Optional ty) _ = pure . wrapPrimE ctxt ty
wrapE ctxt (Repeated (Named tyName)) _
    | Just DotProtoKindMessage <- dotProtoTypeInfoKind <$> M.lookup tyName ctxt
        = pure . wrapWithFuncE "NestedVec"
wrapE _ (Repeated ty)        opts
    | isUnpacked opts = pure . wrapWithFuncE "UnpackedVec" . wrapPrimVecE ty
    | isPacked opts || isPackableType ty = pure . wrapWithFuncE "PackedVec" .
                                           wrapPrimVecE ty
    | otherwise = pure . wrapWithFuncE "UnpackedVec" . wrapPrimVecE ty
wrapE _ _ _ = const (internalError "wrapE: unimplemented")

unwrapE ctxt (Prim ty)     _ = pure . unwrapPrimE ctxt  ty
unwrapE ctxt (Optional ty) _ = pure . unwrapPrimE ctxt ty
unwrapE ctxt (Repeated (Named tyName)) _
    | Just DotProtoKindMessage <- dotProtoTypeInfoKind <$> M.lookup tyName ctxt
        = pure . unwrapWithFuncE "nestedvec"
unwrapE _ (Repeated ty) opts
    | isUnpacked opts = pure . unwrapPrimVecE ty . unwrapWithFuncE "unpackedvec"
    | isPacked opts || isPackableType ty =  pure . unwrapPrimVecE ty .
                                            unwrapWithFuncE "packedvec"
    | otherwise = pure . unwrapPrimVecE ty . unwrapWithFuncE "unpackedvec"
unwrapE _ _ _ = const (internalError "unwrapE: unimplemented")

wrapPrimVecE, unwrapPrimVecE :: DotProtoPrimType -> HsExp -> HsExp
wrapPrimVecE SFixed32 = apply fmapE . (HsVar (protobufName "Signed"):) . (:[])
wrapPrimVecE SFixed64 = apply fmapE . (HsVar (protobufName "Signed"):) . (:[])
wrapPrimVecE _ = id

unwrapPrimVecE SFixed32 = HsParen .
    HsInfixApp (apply pureE [ apply fmapE [ HsVar (protobufName "signed") ] ]) apOp
unwrapPrimVecE SFixed64 = HsParen .
    HsInfixApp (apply pureE [ apply fmapE [ HsVar (protobufName "signed") ] ]) apOp
unwrapPrimVecE _ = id

wrapPrimE, unwrapPrimE :: TypeContext -> DotProtoPrimType -> HsExp -> HsExp
wrapPrimE ctxt (Named tyName)
    | Just DotProtoKindMessage <- dotProtoTypeInfoKind <$> M.lookup tyName ctxt
        = wrapWithFuncE "Nested"
wrapPrimE _ SInt32   = wrapWithFuncE "Signed"
wrapPrimE _ SInt64   = wrapWithFuncE "Signed"
wrapPrimE _ SFixed32 = wrapWithFuncE "Signed"
wrapPrimE _ SFixed64 = wrapWithFuncE "Signed"
wrapPrimE _ _        = id

unwrapPrimE ctxt (Named tyName)
    | Just DotProtoKindMessage <- dotProtoTypeInfoKind <$> M.lookup tyName ctxt
        = unwrapWithFuncE "nested"
unwrapPrimE _ SInt32   = unwrapWithFuncE "signed"
unwrapPrimE _ SInt64   = unwrapWithFuncE "signed"
unwrapPrimE _ SFixed32 = unwrapWithFuncE "signed"
unwrapPrimE _ SFixed64 = unwrapWithFuncE "signed"
unwrapPrimE _ _        = id

wrapWithFuncE, unwrapWithFuncE :: String -> HsExp -> HsExp
wrapWithFuncE wrappingFunc = HsParen . HsApp (HsVar (protobufName wrappingFunc))
unwrapWithFuncE unwrappingFunc = HsParen . HsInfixApp funcE apOp
  where funcE = HsParen (HsApp pureE (HsVar (protobufName unwrappingFunc)))

isPacked, isUnpacked :: [DotProtoOption] -> Bool
isPacked opts =
    case find (\(DotProtoOption name _) -> name == Single "packed") opts of
        Just (DotProtoOption _ (BoolLit x)) -> x
        _ -> False
isUnpacked opts =
    case find (\(DotProtoOption name _) -> name == Single "packed") opts of
        Just (DotProtoOption _ (BoolLit x)) -> not x
        _ -> False

internalError, invalidTypeNameError, _unimplementedError
    :: String -> CompileResult a
internalError = Left . InternalError
invalidTypeNameError = Left . InvalidTypeName
_unimplementedError = Left . Unimplemented

invalidMethodNameError, noSuchTypeError :: DotProtoIdentifier -> CompileResult a
noSuchTypeError = Left . NoSuchType
invalidMethodNameError = Left . InvalidMethodName

-- ** Generate types and instances for .proto enums

dotProtoEnumD :: DotProtoIdentifier -> DotProtoIdentifier -> [DotProtoEnumPart]
              -> CompileResult [HsDecl]
dotProtoEnumD parentIdent enumIdent enumParts =
  do enumName <- nestedTypeName parentIdent =<<
                 dpIdentUnqualName enumIdent

     enumCons <- sortBy (comparing fst) <$>
                 sequence [ (i,) <$> (prefixedConName enumName =<<
                                      dpIdentUnqualName conIdent)
                          | DotProtoEnumField conIdent i <- enumParts ]

     let enumNameE = HsLit (HsString enumName)
         enumConNames = map snd enumCons

         -- TODO assert that there is more than one enumeration constructor
         minEnumVal = fst (head enumCons)
         maxEnumVal = fst (last enumCons)
         boundsE = HsTuple
                     [ HsExpTypeSig l (intE minEnumVal) (HsQualType [] (HsTyCon (haskellName "Int")))
                     , intE maxEnumVal
                     ]

         toEnumD = toEnumDPatterns <> [ toEnumFailure ]
         fromEnumD =
             [ match_ (HsIdent "fromEnum") [ HsPApp (unqual_ conName) [] ]
                      (HsUnGuardedRhs (intE conIdx)) []
             | (conIdx, conName) <- enumCons ]
         succD = zipWith succDPattern enumConNames (tail enumConNames) <> [ succFailure]
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
                                  (HsUnGuardedRhs (apply toEnumErrorE
                                                          [enumNameE
                                                          , HsVar (unqual_ "i")
                                                          , boundsE])) []
         succFailure     = match_ (HsIdent "succ") [ HsPWildCard ]
                                  (HsUnGuardedRhs (HsApp succErrorE enumNameE)) []
         predFailure     = match_ (HsIdent "pred") [ HsPWildCard ]
                                  (HsUnGuardedRhs (HsApp predErrorE enumNameE)) []

         parseJSONPBDecls :: [HsMatch]
         parseJSONPBDecls =
           [ let pat nm =
                   HsPApp (jsonpbName "String")
                     [ HsPLit (HsString (case stripPrefix enumName nm of
                                           Just s  -> s
                                           Nothing -> nm))
                     ]
             in
             match_ (HsIdent "parseJSONPB") [pat conName]
                    (HsUnGuardedRhs
                       (HsApp pureE (HsVar (unqual_ conName))))
                    []
           | (_, conName) <- enumCons
           ]
           <> [ match_ (HsIdent "parseJSONPB") [patVar "v"]
                       (HsUnGuardedRhs
                          (apply (HsVar (jsonpbName "typeMismatch"))
                                 [ HsLit (HsString enumName)
                                 , HsVar (unqual_ "v")
                                 ]))
                       []
              ]

         toEncodingPBDecl =
           match_ (HsIdent "toEncodingPB") [ patVar "x", HsPWildCard ]
             (HsUnGuardedRhs
                (HsApp (HsVar (jsonpbName "namedEncoding"))
                       (HsVar (unqual_ "x"))))
             []

     pure [ dataDecl_ enumName [ conDecl_ (HsIdent con) []
                               | (_, con) <- enumCons] defaultEnumDeriving
          , namedInstD enumName
          , instDecl_ (haskellName "Enum") [ type_ enumName ]
                      [ HsFunBind toEnumD, HsFunBind fromEnumD
                      , HsFunBind succD, HsFunBind predD ]
          , instDecl_ (jsonpbName "ToJSONPB") [ type_ enumName ]
                      [ HsFunBind [toEncodingPBDecl] ]
          , instDecl_ (jsonpbName "FromJSONPB") [ type_ enumName ]
                      [ HsFunBind parseJSONPBDecls ]
          ]

-- ** Generate code for dot proto services

dotProtoServiceD :: DotProtoIdentifier -> TypeContext -> DotProtoIdentifier
                 -> [DotProtoServicePart] -> CompileResult [HsDecl]
dotProtoServiceD pkgIdent ctxt serviceIdent service =
  do serviceNameUnqual <- dpIdentUnqualName serviceIdent
     packageName <- dpIdentQualName pkgIdent

     serviceName <- typeLikeName serviceNameUnqual

     let endpointPrefix = "/" ++ packageName ++ "." ++ serviceName ++ "/"

         serviceFieldD (DotProtoServiceRPC rpcName (request, requestStreaming)
                            (response, responseStreaming) _) =
           do fullName <- prefixedFieldName serviceName =<<
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
                               (tyApp ioT [tyApp (HsTyVar (HsIdent "response")) [streamingType, responseTy]]))]
         serviceFieldD _ = pure []

     fieldsD <- mconcat <$> mapM serviceFieldD service

     serverFuncName <- prefixedFieldName serviceName "server"
     clientFuncName <- prefixedFieldName serviceName "client"

     let conDecl = recDecl_ (HsIdent serviceName)
                            [ ([HsIdent hsName], ty)
                            | (_, hsName, _, _, ty) <- fieldsD ]

         serverT = tyApp (HsTyCon (unqual_ serviceName))
                         [ serverRequestT, serverResponseT ]
         serviceServerTypeD = HsTypeSig l [ HsIdent serverFuncName ]
             (HsQualType [] (HsTyFun serverT (HsTyFun serviceOptionsC ioActionT)))

         serviceServerD =
             let serverFuncD = match_ (HsIdent serverFuncName)
                                      [ HsPRec (unqual_ serviceName)
                                            [ HsPFieldPat (unqual_ methodName)
                                                  (HsPVar (HsIdent methodName))
                                            | (_, methodName, _, _, _)
                                                  <- fieldsD ]
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
                                      (HsUnGuardedRhs
                                           (apply serverLoopE [ serverOptsE ])) []

                 handlerE handlerC adapterE methodName hsName =
                     apply handlerC [ apply methodNameC [ HsLit (HsString methodName) ]
                                    , apply adapterE [ HsVar (unqual_ hsName) ] ]

                 update u v = HsFieldUpdate (unqual_ u) (HsVar (unqual_ v))

                 serverOptsE = HsRecUpdate defaultOptionsE
                     [ HsFieldUpdate (grpcName "optNormalHandlers")
                           (HsList [ handlerE unaryHandlerC convertServerHandlerE
                                              endpointName hsName
                                   | (endpointName, hsName, NonStreaming
                                     , NonStreaming, _) <- fieldsD ])

                     , HsFieldUpdate (grpcName "optClientStreamHandlers")
                           (HsList [ handlerE clientStreamHandlerC
                                              convertServerReaderHandlerE
                                              endpointName hsName
                                   | (endpointName, hsName, Streaming
                                     , NonStreaming, _) <- fieldsD ])

                     , HsFieldUpdate (grpcName "optServerStreamHandlers")
                           (HsList [ handlerE serverStreamHandlerC
                                              convertServerWriterHandlerE
                                              endpointName hsName
                                   | (endpointName, hsName, NonStreaming
                                     , Streaming, _) <- fieldsD ])

                     , HsFieldUpdate (grpcName "optBiDiStreamHandlers")
                           (HsList [ handlerE biDiStreamHandlerC
                                              convertServerRWHandlerE
                                              endpointName hsName
                                   | (endpointName, hsName, Streaming
                                     , Streaming, _) <- fieldsD ])
                     , update "optServerHost" "serverHost"
                     , update "optServerPort" "serverPort"
                     , update "optUseCompression" "useCompression"
                     , update "optUserAgentPrefix" "userAgentPrefix"
                     , update "optUserAgentSuffix" "userAgentSuffix"
                     , update "optInitialMetadata" "initialMetadata"
                     , update "optSSLConfig" "sslConfig"
                     , update "optLogger" "logger"
                     ]
             in HsFunBind [serverFuncD]

         clientT = tyApp (HsTyCon (unqual_ serviceName))
                         [ clientRequestT, clientResultT ]
         serviceClientTypeD = HsTypeSig l [ HsIdent clientFuncName ]
             (HsQualType [] (HsTyFun grpcClientT (HsTyApp ioT clientT)))

         serviceClientD =
             let clientFuncD = match_ (HsIdent clientFuncName)
                                      [ HsPVar (HsIdent "client") ]
                                      ( HsUnGuardedRhs clientRecE ) []
                 clientRecE = foldl (\f -> HsInfixApp f apOp)
                                    (apply pureE [ HsVar (unqual_ serviceName) ])
                                    [ HsParen $ HsInfixApp clientRequestE' apOp
                                        (registerClientMethodE endpointName)
                                    | (endpointName, _, _, _, _) <- fieldsD ]
                 clientRequestE' = apply pureE [ apply clientRequestE [ HsVar (unqual_ "client") ] ]
                 registerClientMethodE endpoint =
                   apply clientRegisterMethodE [ HsVar (unqual_ "client")
                                               , apply methodNameC
                                                   [ HsLit (HsString endpoint) ] ]
             in HsFunBind [ clientFuncD ]

     pure [ HsDataDecl l  [] (HsIdent serviceName)
                [ HsIdent "request", HsIdent "response" ]
                [ conDecl ] defaultServiceDeriving

          , serviceServerTypeD
          , serviceServerD

          , serviceClientTypeD
          , serviceClientD ]

-- * Common Haskell expressions, constructors, and operators

dotProtoFieldC, primC, optionalC, repeatedC, nestedRepeatedC, namedC,
  fieldNumberC, singleC, dotsC, pathC, nestedC, anonymousC, dotProtoOptionC,
  identifierC, stringLitC, intLitC, floatLitC, boolLitC, trueC, falseC,
  unaryHandlerC, clientStreamHandlerC, serverStreamHandlerC, biDiStreamHandlerC,
  methodNameC, nothingC, justC, mconcatE, encodeMessageFieldE, fromStringE,
  decodeMessageFieldE, pureE, memptyE, msumE, atE, oneofE, succErrorE, predErrorE,
  toEnumErrorE, fmapE, defaultOptionsE, serverLoopE, convertServerHandlerE,
  convertServerReaderHandlerE, convertServerWriterHandlerE,
  convertServerRWHandlerE, clientRegisterMethodE, clientRequestE :: HsExp
dotProtoFieldC        = HsVar (protobufName "DotProtoField")
primC                 = HsVar (protobufName "Prim")
optionalC             = HsVar (protobufName "Optional")
repeatedC             = HsVar (protobufName "Repeated")
nestedRepeatedC       = HsVar (protobufName "NestedRepeated")
namedC                = HsVar (protobufName "Named")
fieldNumberC          = HsVar (protobufName "FieldNumber")
singleC               = HsVar (protobufName "Single")
pathC                 = HsVar (protobufName "Path")
dotsC                 = HsVar (protobufName "Dots")
nestedC               = HsVar (protobufName "Nested")
anonymousC            = HsVar (protobufName "Anonymous")
dotProtoOptionC       = HsVar (protobufName "DotProtoOption")
identifierC           = HsVar (protobufName "Identifier")
stringLitC            = HsVar (protobufName "StringLit")
intLitC               = HsVar (protobufName "IntLit")
floatLitC             = HsVar (protobufName "FloatLit")
boolLitC              = HsVar (protobufName "BoolLit")
trueC                 = HsVar (haskellName "True")
falseC                = HsVar (haskellName "False")
unaryHandlerC         = HsVar (grpcName "UnaryHandler")
clientStreamHandlerC  = HsVar (grpcName "ClientStreamHandler")
serverStreamHandlerC  = HsVar (grpcName "ServerStreamHandler")
biDiStreamHandlerC    = HsVar (grpcName "BiDiStreamHandler")
methodNameC           = HsVar (grpcName "MethodName")
nothingC              = HsVar (haskellName "Nothing")
justC                 = HsVar (haskellName "Just")

encodeMessageFieldE   = HsVar (protobufName "encodeMessageField")
decodeMessageFieldE   = HsVar (protobufName "decodeMessageField")
atE                   = HsVar (protobufName "at")
oneofE                = HsVar (protobufName "oneof")
mconcatE              = HsVar (haskellName "mconcat")
fromStringE           = HsVar (haskellName "fromString")
pureE                 = HsVar (haskellName "pure")
memptyE               = HsVar (haskellName "mempty")
msumE                 = HsVar (haskellName "msum")
succErrorE            = HsVar (haskellName "succError")
predErrorE            = HsVar (haskellName "predError")
toEnumErrorE          = HsVar (haskellName "toEnumError")
fmapE                 = HsVar (haskellName "fmap")
defaultOptionsE       = HsVar (grpcName "defaultOptions")
serverLoopE           = HsVar (grpcName "serverLoop")
convertServerHandlerE = HsVar (grpcName "convertGeneratedServerHandler")
convertServerReaderHandlerE = HsVar (grpcName "convertGeneratedServerReaderHandler")
convertServerWriterHandlerE = HsVar (grpcName "convertGeneratedServerWriterHandler")
convertServerRWHandlerE     = HsVar (grpcName "convertGeneratedServerRWHandler")
clientRegisterMethodE = HsVar (grpcName "clientRegisterMethod")
clientRequestE        = HsVar (grpcName "clientRequest")

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
ioActionT        = tyApp ioT [ HsTyTuple [] ]
ioT              = HsTyCon (haskellName "IO")
grpcClientT      = HsTyCon (grpcName "Client")

apOp :: HsQOp
apOp  = HsQVarOp (UnQual (HsSymbol "<*>"))

fmapOp :: HsQOp
fmapOp  = HsQVarOp (UnQual (HsSymbol "<$>"))

toJSONPBOp :: HsQOp
toJSONPBOp = HsQVarOp (UnQual (HsSymbol ".="))

parseJSONPBOp :: HsQOp
parseJSONPBOp = HsQVarOp (UnQual (HsSymbol ".:"))

intE :: Integral a => a -> HsExp
intE x = (if x < 0 then HsParen else id) . HsLit . HsInt . fromIntegral $ x

intP :: Integral a => a -> HsPat
intP x = (if x < 0 then HsPParen else id) . HsPLit . HsInt . fromIntegral $ x

-- ** Expressions for protobuf-wire types

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
optionE (DotProtoOption name value) = apply dotProtoOptionC [ dpIdentE name
                                                            , dpValueE value ]

dpTypeE :: DotProtoType -> CompileResult HsExp
dpTypeE (Prim p)           = pure (apply primC           [ dpPrimTypeE p ])
dpTypeE (Optional p)       = pure (apply optionalC       [ dpPrimTypeE p ])
dpTypeE (Repeated p)       = pure (apply repeatedC       [ dpPrimTypeE p ])
dpTypeE (NestedRepeated p) = pure (apply nestedRepeatedC [ dpPrimTypeE p ])
dpTypeE (Map _ _)          = internalError "dpTypeE: Map"

dpPrimTypeE :: DotProtoPrimType -> HsExp
dpPrimTypeE (Named named) = apply namedC [ dpIdentE named ]
dpPrimTypeE ty            =
    HsVar . protobufName $
    case ty of
        Int32    -> "Int32"
        Int64    -> "Int64"
        SInt32   -> "SInt32"
        SInt64   -> "SInt64"
        UInt32   -> "UInt32"
        UInt64   -> "UInt64"
        Fixed32  -> "Fixed32"
        Fixed64  -> "Fixed64"
        SFixed32 -> "SFixed32"
        SFixed64 -> "SFixed64"
        String   -> "String"
        Bytes    -> "Bytes"
        Bool     -> "Bool"
        Float    -> "Float"
        Double   -> "Double"

        -- 'error' okay because this is literally impossible
        Named _  -> error "dpPrimTypeE: impossible"

defaultImports :: Bool -> [HsImportDecl]
defaultImports usesGrpc =
  [ importDecl_ preludeM                  True  (Just haskellNS) Nothing
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
  , importDecl_ controlMonadM             True  (Just haskellNS) Nothing
  , importDecl_ dataTextM                 True
                (Just haskellNS) (Just (False, [ importSym "Text" ]))
  , importDecl_ dataByteStringM           True  (Just haskellNS) Nothing
  , importDecl_ dataStringM               True  (Just haskellNS)
                (Just (False, [ importSym "fromString" ]))
  , importDecl_ dataVectorM               True  (Just haskellNS)
                (Just (False, [ importSym "Vector" ]))
  , importDecl_ dataIntM                  True  (Just haskellNS)
                (Just (False, [ importSym "Int16", importSym "Int32"
                              , importSym "Int64" ]))
  , importDecl_ dataWordM                 True  (Just haskellNS)
                (Just (False, [ importSym "Word16", importSym "Word32"
                              , importSym "Word64" ]))
  , importDecl_ ghcGenericsM              True (Just haskellNS) Nothing
  , importDecl_ ghcEnumM                  True (Just haskellNS) Nothing
  ] <>
  if usesGrpc
    then [ importDecl_ networkGrpcHighLevelGeneratedM   False (Just grpcNS) Nothing
         , importDecl_ networkGrpcHighLevelClientM      False (Just grpcNS) Nothing
         , importDecl_ networkGrpcHighLevelServerM      False (Just grpcNS)
               (Just (True, [ importSym "serverLoop" ]))
         , importDecl_ networkGrpcHighLevelServerUnregM False (Just grpcNS)
               (Just (False, [ importSym "serverLoop" ]))
         , importDecl_ networkGrpcLowLevelCallM         False (Just grpcNS) Nothing  ]
    else []
  where preludeM                  = Module "Prelude"
        dataProtobufWireDotProtoM = Module "Proto3.Suite.DotProto"
        dataProtobufWireClassM    = Module "Proto3.Suite.Class"
        dataProtobufWireTypesM    = Module "Proto3.Suite.Types"
        proto3SuiteJSONPBM        = Module "Proto3.Suite.JSONPB"
        proto3WireM               = Module "Proto3.Wire"
        controlApplicativeM       = Module "Control.Applicative"
        controlMonadM             = Module "Control.Monad"
        dataTextM                 = Module "Data.Text.Lazy"
        dataByteStringM           = Module "Data.ByteString"
        dataStringM               = Module "Data.String"
        dataIntM                  = Module "Data.Int"
        dataVectorM               = Module "Data.Vector"
        dataWordM                 = Module "Data.Word"
        ghcGenericsM              = Module "GHC.Generics"
        ghcEnumM                  = Module "GHC.Enum"
        networkGrpcHighLevelGeneratedM   = Module "Network.GRPC.HighLevel.Generated"
        networkGrpcHighLevelServerM      = Module "Network.GRPC.HighLevel.Server"
        networkGrpcHighLevelClientM      = Module "Network.GRPC.HighLevel.Client"
        networkGrpcHighLevelServerUnregM = Module "Network.GRPC.HighLevel.Server.Unregistered"
        networkGrpcLowLevelCallM         = Module "Network.GRPC.LowLevel.Call"

        haskellNS                 = Module "Hs"
        grpcNS                    = Module "HsGRPC"
        jsonpbNS                  = Module "HsJSONPB"
        protobufNS                = Module "HsProtobuf"

        importSym = HsIAbs . HsIdent

defaultMessageDeriving, defaultEnumDeriving, defaultServiceDeriving :: [HsQName]
defaultMessageDeriving = map haskellName [ "Show"
                                         , "Eq",   "Ord"
                                         , "Generic" ]

defaultEnumDeriving = map haskellName [ "Show", "Bounded"
                                      , "Eq",   "Ord"
                                      , "Generic" ]

defaultServiceDeriving = map haskellName [ "Generic" ]

-- * Wrappers around haskell-src-exts constructors

apply :: HsExp -> [HsExp] -> HsExp
apply f = HsParen . foldl HsApp f

tyApp :: HsType -> [HsType] -> HsType
tyApp = foldl HsTyApp

module_ :: Module -> Maybe [HsExportSpec] -> [HsImportDecl] -> [HsDecl]
        -> HsModule
module_ = HsModule l

importDecl_ :: Module -> Bool -> Maybe Module
            -> Maybe (Bool, [HsImportSpec]) -> HsImportDecl
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
