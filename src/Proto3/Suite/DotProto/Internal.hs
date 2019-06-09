-- | This module provides misc internal helpers and utilities

{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}

module Proto3.Suite.DotProto.Internal where

import           Control.Applicative
import qualified Control.Foldl             as FL
import           Control.Lens              (Lens', lens, over)
import           Control.Lens.Cons         (_head)
import           Control.Monad.Except
import           Data.Bifunctor            (first)
import           Data.Char
import           Data.Coerce
import           Data.Either
import           Data.Foldable
import           Data.Functor.Compose
import           Data.List                 (find, intercalate)
import qualified Data.Map                  as M
import           Data.Maybe                (fromMaybe)
import           Data.Monoid
import qualified Data.Text                 as T
import           Data.Tuple                (swap)
import           Filesystem.Path.CurrentOS ((</>))
import qualified Filesystem.Path.CurrentOS as FP
import qualified NeatInterpolation         as Neat
import           Prelude                   hiding (FilePath)
import           Proto3.Suite.DotProto
import           Proto3.Wire.Types         (FieldNumber (..))
import           System.FilePath           (isPathSeparator)
import           Text.Parsec               (ParseError)
import qualified Turtle
import           Turtle                    (ExitCode (..), FilePath, MonadIO,
                                            Text)
import           Turtle.Format             ((%))
import qualified Turtle.Format             as F

-------------------------------------------------------------------------------
--
-- * Utilities
--

#if !(MIN_VERSION_mtl(2,2,2))
liftEither :: MonadError e m => Either e a -> m a
liftEither = either throwError pure
#endif

foldMapM :: (Foldable t, Monad m, Monoid b) => (a -> m b) -> t a -> m b
foldMapM f = foldM (\b a -> (b <>) <$> f a) mempty

type GettingM r s a = forall m. Applicative m => (a -> Compose m (Const r) a) -> s -> Compose m (Const r) s

foldMapOfM :: Monad m => GettingM r s a -> (a -> m r) -> s -> m r
foldMapOfM l f = fmap getConst . getCompose . l (Compose . fmap Const . f)

mapKeysM :: (Monad m, Ord k2) => (k1 -> m k2) -> M.Map k1 a -> m (M.Map k2 a)
mapKeysM f = fmap M.fromList . traverse (fmap swap . traverse f . swap) . M.assocs

-- $setup
-- >>> :set -XOverloadedStrings

dieLines :: MonadIO m => Text -> m a
dieLines (Turtle.textToLines -> msg) = do
  mapM_ Turtle.err msg
  Turtle.exit (ExitFailure 1)

--------------------------------------------------------------------------------
--
-- * Reading files
--

-- | toModulePath takes an include-relative path to a .proto file and produces a
-- "module path" which is used during code generation.
--
-- Note that, with the exception of the '.proto' portion of the input filepath,
-- this function interprets '.' in the filename components as if they were
-- additional slashes (assuming that the '.' is not the first character, which
-- is merely ignored). So e.g. "google/protobuf/timestamp.proto" and
-- "google.protobuf.timestamp.proto" map to the same module path.
--
-- >>> toModulePath "/absolute/path/fails.proto"
-- Left "expected include-relative path"
--
-- >>> toModulePath "relative/path/to/file_without_proto_suffix_fails"
-- Left "expected .proto suffix"
--
-- >>> toModulePath "relative/path/to/file_without_proto_suffix_fails.txt"
-- Left "expected .proto suffix"
--
-- >>> toModulePath "../foo.proto"
-- Left "expected include-relative path, but the path started with ../"
--
-- >>> toModulePath "foo..proto"
-- Left "path contained unexpected .. after canonicalization, please use form x.y.z.proto"
--
-- >>> toModulePath "foo/bar/baz..proto"
-- Left "path contained unexpected .. after canonicalization, please use form x.y.z.proto"
--
-- >>> toModulePath "foo.bar../baz.proto"
-- Left "path contained unexpected .. after canonicalization, please use form x.y.z.proto"
--
-- >>> toModulePath "google/protobuf/timestamp.proto"
-- Right (Path {components = ["Google","Protobuf","Timestamp"]})
--
-- >>> toModulePath "a/b/c/google.protobuf.timestamp.proto"
-- Right (Path {components = ["A","B","C","Google","Protobuf","Timestamp"]})
--
-- >>> toModulePath "foo/FiLeName_underscore.and.then.some.dots.proto"
-- Right (Path {components = ["Foo","FiLeName_underscore","And","Then","Some","Dots"]})
--
-- >>> toModulePath "foo/bar/././baz/../boggle.proto"
-- Right (Path {components = ["Foo","Bar","Boggle"]})
--
-- >>> toModulePath "./foo.proto"
-- Right (Path {components = ["Foo"]})
--
-- NB: We ignore preceding single '.' characters
-- >>> toModulePath ".foo.proto"
-- Right (Path {components = ["Foo"]})
toModulePath :: FilePath -> Either String Path
toModulePath fp0@(fromMaybe fp0 . FP.stripPrefix "./" -> fp)
  | Turtle.absolute fp
    = Left "expected include-relative path"
  | Turtle.extension fp /= Just "proto"
    = Left "expected .proto suffix"
  | otherwise
    = case FP.stripPrefix "../" fp of
        Just{}  -> Left "expected include-relative path, but the path started with ../"
        Nothing
          | T.isInfixOf ".." (Turtle.format F.fp . FP.collapse $ fp)
            -> Left "path contained unexpected .. after canonicalization, please use form x.y.z.proto"
          | otherwise
            -> Right
             . Path
             . dropWhile null -- Remove a potential preceding empty component which
                              -- arose from a preceding '.' in the input path, which we
                              -- want to ignore. E.g. ".foo.proto" => ["","Foo"].
             . fmap (T.unpack . over _head toUpper)
             . concatMap (T.splitOn ".")
             . T.split isPathSeparator
             . Turtle.format F.fp
             . FP.collapse
             . Turtle.dropExtension
             $ fp

fatalBadModulePath :: MonadIO m => FilePath -> String -> m a
fatalBadModulePath (Turtle.format F.fp -> fp) (T.pack -> rsn) =
  dieLines [Neat.text|
    Error: failed when computing the "module path" for "${fp}": ${rsn}

    Please ensure that the provided path to a .proto file is specified as
    relative to some --includeDir path and that it has the .proto suffix.
  |]

-- | @importProto searchPaths toplevel inc@ attempts to import include-relative
-- @inc@ after locating it somewhere in the @searchPaths@; @toplevel@ is simply
-- the path of toplevel .proto being processed so we can report it in an error
-- message. This function terminates the program if it cannot find the file to
-- import or if it cannot construct a valid module path from it.
importProto :: (MonadIO m, MonadError CompileError m)
            => [FilePath] -> FilePath -> FilePath -> m DotProto
importProto paths (Turtle.format F.fp -> toplevelProtoText) protoFP =
  findProto paths protoFP >>= \case
    Found mp fp     -> liftEither . first CompileParseError =<< parseProtoFile mp fp
    BadModulePath e -> fatalBadModulePath protoFP e
    NotFound        -> dieLines [Neat.text|
      Error: while processing include statements in "${toplevelProtoText}", failed
      to find the imported file "${protoFPText}", after looking in the following
      locations (controlled via the --includeDir switch(es)):

      $pathsText
    |]
  where
    pathsText   = T.unlines (Turtle.format ("  "%F.fp) . (</> protoFP) <$> paths)
    protoFPText = Turtle.format F.fp protoFP

data FindProtoResult
  = Found Path FilePath
  | NotFound
  | BadModulePath String
  deriving (Eq, Show)

-- | Attempts to locate the first (if any) filename that exists on the given
-- search paths, and constructs the "module path" from the given
-- include-relative filename (2nd parameter). Terminates the program with an
-- error if the given pathname is not relative.
findProto :: MonadIO m => [FilePath] -> FilePath -> m FindProtoResult
findProto searchPaths protoFP
  | Turtle.absolute protoFP = dieLines [Neat.text|
      Error: Absolute paths to .proto files, whether on the command line or
      in include directives, are not currently permitted; rather, all .proto
      filenames must be relative to the current directory, or relative to some
      search path specified via --includeDir.

      This is because we currently use the include-relative name to decide
      the structure of the Haskell module tree that we emit during code
      generation.
      |]
  | otherwise = case toModulePath protoFP of
      Left e -> pure (BadModulePath e)
      Right mp -> do
        mfp <- flip Turtle.fold FL.head $ do
          sp <- Turtle.select searchPaths
          let fp = sp </> protoFP
          True <- Turtle.testfile fp
          pure fp
        case mfp of
          Nothing -> pure NotFound
          Just fp -> pure (Found mp fp)

--------------------------------------------------------------------------------
--
-- * Type context
--

-- | A mapping from .proto type identifiers to their type information
type TypeContext = M.Map DotProtoIdentifier DotProtoTypeInfo

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

tiParent :: Lens' DotProtoTypeInfo DotProtoIdentifier
tiParent = lens dotProtoTypeInfoParent (\d p -> d{ dotProtoTypeInfoParent = p })

-- | Whether a definition is an enumeration or a message
data DotProtoKind = DotProtoKindEnum
                  | DotProtoKindMessage
                  deriving (Show, Eq, Ord, Enum, Bounded)

-- ** Generating type contexts from ASTs

dotProtoTypeContext :: MonadError CompileError m => DotProto -> m TypeContext
dotProtoTypeContext DotProto{..} =
  foldMapM (definitionTypeContext (metaModulePath protoMeta)) protoDefinitions

definitionTypeContext :: MonadError CompileError m
                      => Path -> DotProtoDefinition -> m TypeContext
definitionTypeContext modulePath (DotProtoMessage msgIdent parts) = do
  let updateParent = tiParent (concatDotProtoIdentifier msgIdent)

  childTyContext <- foldMapOfM (traverse . _messageDefinition)
                               (definitionTypeContext modulePath >=> traverse updateParent)
                               parts

  qualifiedChildTyContext <- mapKeysM (concatDotProtoIdentifier msgIdent) childTyContext

  let tyInfo = DotProtoTypeInfo { dotProtoTypeInfoPackage = DotProtoNoPackage
                                , dotProtoTypeInfoParent =  Anonymous
                                , dotProtoTypeChildContext = childTyContext
                                , dotProtoTypeInfoKind = DotProtoKindMessage
                                , dotProtoTypeInfoModulePath = modulePath
                                }

  pure $ M.singleton msgIdent tyInfo <> qualifiedChildTyContext

definitionTypeContext modulePath (DotProtoEnum enumIdent _) = do
  let tyInfo = DotProtoTypeInfo { dotProtoTypeInfoPackage = DotProtoNoPackage
                                , dotProtoTypeInfoParent =  Anonymous
                                , dotProtoTypeChildContext = mempty
                                , dotProtoTypeInfoKind = DotProtoKindEnum
                                , dotProtoTypeInfoModulePath = modulePath
                                }
  pure (M.singleton enumIdent tyInfo)

definitionTypeContext _ _ = pure mempty


isMessage :: TypeContext -> DotProtoIdentifier -> Bool
isMessage ctxt n = Just DotProtoKindMessage == (dotProtoTypeInfoKind <$> M.lookup n ctxt)

isPacked :: [DotProtoOption] -> Bool
isPacked opts =
    case find (\(DotProtoOption name _) -> name == Single "packed") opts of
        Just (DotProtoOption _ (BoolLit x)) -> x
        _ -> False

isUnpacked :: [DotProtoOption] -> Bool
isUnpacked opts =
    case find (\(DotProtoOption name _) -> name == Single "packed") opts of
        Just (DotProtoOption _ (BoolLit x)) -> not x
        _ -> False

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

isMap :: DotProtoType -> Bool
isMap Map{} = True
isMap _ = False

--------------------------------------------------------------------------------
--
-- * Name resolution
--

concatDotProtoIdentifier :: MonadError CompileError m
                         => DotProtoIdentifier -> DotProtoIdentifier -> m DotProtoIdentifier
concatDotProtoIdentifier i1 i2 = case (i1, i2) of
  (Qualified{}  ,  _           ) -> internalError "concatDotProtoIdentifier: Qualified"
  (_            , Qualified{}  ) -> internalError "concatDotProtoIdentifier Qualified"
  (Anonymous    , Anonymous    ) -> pure Anonymous
  (Anonymous    , b            ) -> pure b
  (a            , Anonymous    ) -> pure a
  (Single a     , b            ) -> concatDotProtoIdentifier (Dots (Path [a])) b
  (a            , Single b     ) -> concatDotProtoIdentifier a (Dots (Path [b]))
  (Dots (Path a), Dots (Path b)) -> pure (Dots (Path (a ++ b)))

camelCased :: String -> String
camelCased s = do
  (prev, cur) <- zip (Nothing:map Just s) (map Just s ++ [Nothing])
  case (prev, cur) of
    (Just '_', Just x)
      | isAlpha x        -> pure (toUpper x)
    (Just '_', Nothing)  -> pure '_'
    (Just '_', Just '_') -> pure '_'
    (_, Just '_')        -> empty
    (_, Just x)          -> pure x
    (_, _)               -> empty

typeLikeName :: MonadError CompileError m => String -> m String
typeLikeName ident@(c:cs)
  | isUpper c = pure (camelCased ident)
  | isLower c = pure (camelCased (toUpper c : cs))
  | '_'  == c = pure (camelCased ('X':ident))
typeLikeName ident = invalidTypeNameError ident

fieldLikeName :: String -> String
fieldLikeName ident@(c:_)
  | isUpper c = let (prefix, suffix) = span isUpper ident
                in map toLower prefix ++ suffix
fieldLikeName ident = ident

prefixedEnumFieldName :: String -> String -> String
prefixedEnumFieldName enumName fieldName = enumName <> fieldName

prefixedConName :: MonadError CompileError m => String -> String -> m String
prefixedConName msgName conName = (msgName ++) <$> typeLikeName conName

-- TODO: This should be ~:: MessageName -> FieldName -> ...; same elsewhere, the
-- String types are a bit of a hassle.
prefixedFieldName :: MonadError CompileError m => String -> String -> m String
prefixedFieldName msgName fieldName = (fieldLikeName msgName ++) <$> typeLikeName fieldName

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

-- | Given a 'DotProtoIdentifier' for the parent type and the unqualified name
-- of this type, generate the corresponding Haskell name
nestedTypeName :: MonadError CompileError m => DotProtoIdentifier -> String -> m String
nestedTypeName Anonymous             nm = typeLikeName nm
nestedTypeName (Single parent)       nm = intercalate "_" <$> traverse typeLikeName [parent, nm]
nestedTypeName (Dots (Path parents)) nm = intercalate "_" . (<> [nm]) <$> traverse typeLikeName parents
nestedTypeName (Qualified {})        _  = internalError "nestedTypeName: Qualified"

qualifiedMessageName :: MonadError CompileError m => DotProtoIdentifier -> DotProtoIdentifier -> m String
qualifiedMessageName parentIdent msgIdent = nestedTypeName parentIdent =<< dpIdentUnqualName msgIdent

--------------------------------------------------------------------------------
--
-- ** Codegen bookkeeping helpers
--

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

getQualifiedFields :: MonadError CompileError m
                   => String -> [DotProtoMessagePart] -> m [QualifiedField]
getQualifiedFields msgName msgParts = flip foldMapM msgParts $ \case
  DotProtoMessageField DotProtoField{..} -> do
    fieldName <- dpIdentUnqualName dotProtoFieldName
    qualName  <- prefixedFieldName msgName fieldName
    pure . (:[]) $ QualifiedField { recordFieldName = coerce qualName
                                  , fieldInfo = FieldNormal (coerce fieldName)
                                                            dotProtoFieldNumber
                                                            dotProtoFieldType
                                                            dotProtoFieldOptions
                                  }

  DotProtoMessageOneOf _ [] ->
    throwError (InternalError "getQualifiedFields: encountered oneof with no oneof fields")

  DotProtoMessageOneOf oneofIdent fields -> do
    ident <- dpIdentUnqualName oneofIdent
    oneofName <- prefixedFieldName msgName ident
    oneofTypeName <- prefixedConName msgName ident

    let mkSubfield DotProtoField{..} = do
            s <- dpIdentUnqualName dotProtoFieldName
            c <- prefixedConName oneofTypeName s
            pure [OneofSubfield dotProtoFieldNumber c (coerce s) dotProtoFieldType dotProtoFieldOptions]
        mkSubfield DotProtoEmptyField = pure []

    fieldElems <- foldMapM mkSubfield fields

    pure . (:[]) $ QualifiedField { recordFieldName = coerce oneofName
                                  , fieldInfo = FieldOneOf (OneofField ident fieldElems)
                                  }
  _ -> pure []

-- | Project qualified fields, given a projection function per field type.
foldQF :: (FieldName -> FieldNumber -> a) -- ^ projection for normal fields
       -> (OneofField -> a)               -- ^ projection for oneof fields
       -> QualifiedField
       -> a
foldQF f _ (QualifiedField _ (FieldNormal fldName fldNum _ _)) = f fldName fldNum
foldQF _ g (QualifiedField _ (FieldOneOf fld))                 = g fld

fieldBinder :: FieldNumber -> String
fieldBinder = ("f" ++) . show

oneofSubBinder :: OneofSubfield -> String
oneofSubBinder = fieldBinder . subfieldNumber

oneofSubDisjunctBinder :: [OneofSubfield] -> String
oneofSubDisjunctBinder = intercalate "_or_" . fmap oneofSubBinder

--------------------------------------------------------------------------------
--
-- * Errors
--

data CompileError
  = CircularImport          FilePath
  | CompileParseError       ParseError
  | InternalEmptyModulePath
  | InternalError           String
  | InvalidPackageName      DotProtoIdentifier
  | InvalidMethodName       DotProtoIdentifier
  | InvalidTypeName         String
  | InvalidMapKeyType       String
  | NoPackageDeclaration
  | NoSuchType              DotProtoIdentifier
  | Unimplemented           String
  deriving (Show, Eq)


internalError :: MonadError CompileError m => String -> m a
internalError = throwError . InternalError

invalidTypeNameError :: MonadError CompileError m => String -> m a
invalidTypeNameError = throwError . InvalidTypeName

_unimplementedError :: MonadError CompileError m => String -> m a
_unimplementedError = throwError . Unimplemented

invalidMethodNameError :: MonadError CompileError m => DotProtoIdentifier -> m a
invalidMethodNameError = throwError . InvalidMethodName

noSuchTypeError :: MonadError CompileError m => DotProtoIdentifier -> m a
noSuchTypeError = throwError . NoSuchType

protoPackageName :: MonadError CompileError m => DotProtoPackageSpec -> m DotProtoIdentifier
protoPackageName (DotProtoPackageSpec name) = pure name
protoPackageName DotProtoNoPackage = throwError NoPackageDeclaration
