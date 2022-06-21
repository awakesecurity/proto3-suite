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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}

module Proto3.Suite.DotProto.Internal where

import           Control.Applicative
import qualified Control.Foldl             as FL
import           Control.Lens              (Lens', lens, over)
import           Control.Lens.Cons         (_head)
import           Control.Monad
import           Control.Monad.Except
import           Data.Bifunctor            (first)
import           Data.Char
import           Data.Coerce
import           Data.Either
import           Data.Foldable
import           Data.Functor.Compose
import           Data.Int                  (Int32)
import           Data.List                 (intercalate)
import qualified Data.List.NonEmpty        as NE
import qualified Data.Map                  as M
import           Data.Maybe                (fromMaybe)
import qualified Data.Text                 as T
import           Data.Tuple                (swap)
import qualified NeatInterpolation         as Neat
import           Prelude                   hiding (FilePath)
import           Proto3.Suite.DotProto.AST
import           Proto3.Suite.DotProto.AST.Lens
import           Proto3.Suite.DotProto.Parsing
import           Proto3.Wire.Types         (FieldNumber (..))
import           System.FilePath           (isPathSeparator)
import           Text.Parsec               (ParseError)
import qualified Turtle
import           Turtle                    (ExitCode (..), FilePath, Text,
                                            (</>))
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

-- | Like 'foldMap', but with an effectful projection.
foldMapM ::
  (Foldable t, Monad m, Monoid b, Semigroup b) => (a -> m b) -> t a -> m b
foldMapM f = foldM (\b a -> (b <>) <$> f a) mempty

-- | Like 'Control.Lens.Getter.Getting', but allows for retrieving the 'r'
-- element in some Applicative context 'm'.
type GettingM r s a = forall m. Applicative m => (a -> Compose m (Const r) a) -> s -> Compose m (Const r) s

-- | Given an effectful projection from 'a' into a monoid 'r', retrieve the sum
-- of all 'a' values in an 's' structure as targetted by the 'GettingM' optic.
-- Note that the Monoid constraint on 'r' is implicit via 'Const', but we
-- note it in the type for clarity.
foldMapOfM :: (Applicative m, Monoid r) => GettingM r s a -> (a -> m r) -> s -> m r
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
-- Right (Path {components = "Google" :| ["Protobuf","Timestamp"]})
--
-- >>> toModulePath "a/b/c/google.protobuf.timestamp.proto"
-- Right (Path {components = "A" :| ["B","C","Google","Protobuf","Timestamp"]})
--
-- >>> toModulePath "foo/FiLeName_underscore.and.then.some.dots.proto"
-- Right (Path {components = "Foo" :| ["FiLeName_underscore","And","Then","Some","Dots"]})
--
#if MIN_VERSION_turtle(1,6,0)
-- >>> toModulePath "foo/bar/././baz/../boggle.proto"
-- Left "path contained unexpected .. after canonicalization, please use form x.y.z.proto"
#else
-- >>> toModulePath "foo/bar/././baz/../boggle.proto"
-- Right (Path {components = "Foo" :| ["Bar","Boggle"]})
#endif
--
-- >>> toModulePath "./foo.proto"
-- Right (Path {components = "Foo" :| []})
--
-- NB: We ignore preceding single '.' characters
-- >>> toModulePath ".foo.proto"
-- Right (Path {components = "Foo" :| []})
toModulePath :: FilePath -> Either String Path
toModulePath fp0@(fromMaybe fp0 . Turtle.stripPrefix "./" -> fp)
  | Turtle.absolute fp
    = Left "expected include-relative path"
  | Turtle.extension fp /= Just "proto"
    = Left "expected .proto suffix"
  | otherwise
    = case Turtle.stripPrefix "../" fp of
        Just{}  -> Left "expected include-relative path, but the path started with ../"
        Nothing
          | T.isInfixOf ".." . Turtle.format F.fp . Turtle.collapse $ fp
            -> Left "path contained unexpected .. after canonicalization, please use form x.y.z.proto"
          | otherwise
            -> maybe (Left "empty path after canonicalization") (Right . Path)
             . NE.nonEmpty
             . dropWhile null -- Remove a potential preceding empty component which
                              -- arose from a preceding '.' in the input path, which we
                              -- want to ignore. E.g. ".foo.proto" => ["","Foo"].
             . fmap (T.unpack . over _head toUpper)
             . concatMap (T.splitOn ".")
             . T.split isPathSeparator
             . Turtle.format F.fp
             . Turtle.collapse
             . Turtle.dropExtension
             $ fp

-- | @importProto searchPaths toplevel inc@ attempts to import include-relative
-- @inc@ after locating it somewhere in the @searchPaths@; @toplevel@ is simply
-- the path of toplevel .proto being processed so we can report it in an error
-- message. This function terminates the program if it cannot find the file to
-- import or if it cannot construct a valid module path from it.
importProto :: (MonadIO m, MonadError CompileError m)
            => [FilePath] -> FilePath -> FilePath -> m DotProto
importProto paths toplevelProto protoFP =
  findProto paths protoFP >>= \case
    Left e
      -> dieLines (badModulePathErrorMsg protoFP e)
    Right Nothing
      | toplevelProto == protoFP
        -> dieLines (toplevelNotFoundErrorMsg paths toplevelProto)
      | otherwise
        -> dieLines (importNotFoundErrorMsg paths toplevelProto protoFP)
    Right (Just (mp, fp))
      -> liftEither . first CompileParseError =<< parseProtoFile mp fp

type FindProtoResult = Either String (Maybe (Path, FilePath))

-- | Attempts to locate the first (if any) filename that exists on the given
-- search paths, and constructs the "module path" from the given
-- include-relative filename (2nd parameter). Terminates the program with an
-- error if the given pathname is not relative.
findProto :: MonadIO m => [FilePath] -> FilePath -> m FindProtoResult
findProto searchPaths protoFP
  | Turtle.absolute protoFP = dieLines absolutePathErrorMsg
  | otherwise = forM (toModulePath protoFP) $ \mp ->
                  flip Turtle.fold FL.head $ do
                     sp <- Turtle.select searchPaths
                     let fp = sp </> protoFP
                     True <- Turtle.testfile fp
                     pure (mp, fp)

-- * Pretty Error Messages

badModulePathErrorMsg :: FilePath -> String -> T.Text
badModulePathErrorMsg (Turtle.format F.fp -> fp) (T.pack -> rsn) =
  [Neat.text|
    Error: failed when computing the "module path" for "${fp}": ${rsn}

    Please ensure that the provided path to a .proto file is specified as
    relative to some --includeDir path and that it has the .proto suffix.
  |]

importNotFoundErrorMsg :: [FilePath] -> FilePath -> FilePath -> T.Text
importNotFoundErrorMsg paths toplevelProto protoFP =
    [Neat.text|
      Error: while processing include statements in "${toplevelProtoText}", failed
      to find the imported file "${protoFPText}", after looking in the following
      locations (controlled via the --includeDir switch(es)):

      $pathsText
    |]
  where
    pathsText = T.unlines (Turtle.format ("  "%F.fp) . (</> protoFP) <$> paths)
    toplevelProtoText = Turtle.format F.fp toplevelProto
    protoFPText = Turtle.format F.fp protoFP

toplevelNotFoundErrorMsg :: [FilePath] -> FilePath -> T.Text
toplevelNotFoundErrorMsg searchPaths toplevelProto =
    [Neat.text|
      Error: failed to find file "${toplevelProtoText}", after looking in
      the following locations (controlled via the --includeDir switch(es)):

      $searchPathsText
    |]
  where
    searchPathsText   = T.unlines (Turtle.format ("  "%F.fp) . (</> toplevelProto) <$> searchPaths)
    toplevelProtoText = Turtle.format F.fp toplevelProto

absolutePathErrorMsg :: T.Text
absolutePathErrorMsg =
    [Neat.text|
     Error: Absolute paths to .proto files, whether on the command line or
     in include directives, are not currently permitted; rather, all .proto
     filenames must be relative to the current directory, or relative to some
     search path specified via --includeDir.

     This is because we currently use the include-relative name to decide
     the structure of the Haskell module tree that we emit during code
     generation.
    |]

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
definitionTypeContext modulePath (DotProtoMessage _ msgIdent parts) = do
  let updateParent = tiParent (concatDotProtoIdentifier msgIdent)

  childTyContext <- foldMapOfM (traverse . _DotProtoMessageDefinition)
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

definitionTypeContext modulePath (DotProtoEnum _ enumIdent _) = do
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

boolOption :: String -> [DotProtoOption] -> Maybe Bool
boolOption desired opts =
    case find (\(DotProtoOption name _) -> name == Single desired) opts of
        Just (DotProtoOption _ (BoolLit x)) -> Just x
        _ -> Nothing

isPacked :: [DotProtoOption] -> Bool
isPacked = fromMaybe False . boolOption "packed"

isUnpacked :: [DotProtoOption] -> Bool
isUnpacked = maybe False not . boolOption "packed"

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

concatDotProtoIdentifier ::
  MonadError CompileError m =>
  DotProtoIdentifier ->
  DotProtoIdentifier ->
  m DotProtoIdentifier
concatDotProtoIdentifier i1 i2 = case (i1, i2) of
  (Qualified{}  ,  _           ) -> internalError "concatDotProtoIdentifier: Qualified"
  (_            , Qualified{}  ) -> internalError "concatDotProtoIdentifier Qualified"
  (Anonymous    , Anonymous    ) -> pure Anonymous
  (Anonymous    , b            ) -> pure b
  (a            , Anonymous    ) -> pure a
  (Single a     , b            ) -> concatDotProtoIdentifier (Dots (Path (pure a))) b
  (a            , Single b     ) -> concatDotProtoIdentifier a (Dots (Path (pure b)))
  (Dots (Path a), Dots (Path b)) -> pure (Dots (Path (a <> b)))

-- | @'toPascalCase' xs'@ sends a snake-case string @xs@ to a pascal-cased string. Trailing underscores are not dropped
-- from the input string and exactly double underscores are replaced by a single underscore.
toPascalCase :: String -> String
toPascalCase xs = foldMap go (segmentBy (== '_') xs)
  where
    go (Left seg) = toUpperFirst seg
    go (Right seg)
      | seg == "__" = "_"
      | otherwise = ""

-- | @'toCamelCase' xs@ sends a snake-case string @xs@ to a camel-cased string.
toCamelCase :: String -> String
toCamelCase xs =
  case toPascalCase xs of
    "" -> ""
    x : xs' -> toLower x : xs'

-- | Uppercases the first character of a string.
--
-- ==== __Examples__
--
-- >>> toUpperFirst "abc"
-- "Abc"
--
-- >>> toUpperFirst ""
-- ""
toUpperFirst :: String -> String
toUpperFirst "" = ""
toUpperFirst (x : xs) = toUpper x : xs

-- | @'segmentBy' p xs@  partitions @xs@ into segments of @'Either' [a] [a]@
-- with:
--
-- * 'Right' sublists containing elements satisfying @p@, otherwise;
--
-- * 'Left' sublists containing elements that do not satisfy @p@
--
-- ==== __Examples__
--
-- >>> segmentBy (\c -> c == '_') "abc_123_xyz"
-- [Left "abc",Right "_",Left "123",Right "_",Left "xyz"]
segmentBy :: (a -> Bool) -> [a] -> [Either [a] [a]]
segmentBy p xs = case span p xs of
  ([], []) -> []
  (ys, []) -> [Right ys]
  ([], ys) -> Left seg : segmentBy p ys'
    where
      (seg, ys') = break p ys
  (xs', ys) -> Right xs' : Left seg : segmentBy p ys'
    where
      (seg, ys') = break p ys

-- | @'suffixBy' p xs@ yields @'Right' (xs', suf)@ if @suf@ is the longest suffix satisfying @p@ and @xs'@ is the rest
-- of the rest, otherwise the string is given back as @'Left' xs@ signifying @xs@ had no suffix satisfying @p@.
suffixBy :: forall a. (a -> Bool) -> [a] -> Either [a] ([a], [a])
suffixBy p xs' = do
  (pref, suf) <- foldr go (Left []) xs'
  if null suf
    then Left pref
    else return (pref, suf)
  where
    go :: a -> Either [a] ([a], [a]) -> Either [a] ([a], [a])
    go x (Right (xs, suf)) = Right (x : xs, suf)
    go x (Left xs)
      | p x = Left (x : xs)
      | otherwise = Right ([x], xs)

-- | @'typeLikeName' xs@ produces either the pascal-cased version of the string @xs@ if it begins with an alphabetical
-- character or underscore - which is replaced with 'X'. A 'CompileError' is emitted if the starting character is
-- non-alphabetic or if @xs == ""@.
typeLikeName :: MonadError CompileError m => String -> m String
typeLikeName "" = invalidTypeNameError "<empty name>"
typeLikeName s@(x : xs)
  | isAlpha x = pure $ case suffixBy (== '_') s of
      Left xs' -> invalidToCamelCase $ toPascalCase xs'
      Right (xs', suf) -> invalidToCamelCase $ toPascalCase xs' <> suf
  | x == '_' = pure $ case suffixBy (== '_') xs of
      Left xs' -> invalidToCamelCase $ 'X' : toPascalCase xs'
      Right (xs', suf) -> invalidToCamelCase $ 'X' : (toPascalCase xs' <> suf)
  | otherwise = invalidTypeNameError s
  where
    -- Transforms special characters that are not valid as a part of a Haskell name to CamelCase.
    -- For instance “foo-bar---baz” will become “FooBarBaz”.
    -- This function presumes that the first character of the initial value satisfies "isAlpha".
    -- This must be checked outside of this function.
    invalidToCamelCase a =
      case span isValidNameChar a of
        ("", "") -> ""
        ("", cs) -> invalidToCamelCase . dropWhile (not . isValidNameChar) $ cs
        (b : bs, cs) -> toUpper b : bs <> invalidToCamelCase cs

    -- Only valid as a secondary character.
    -- First character of a Haskell name can only be "isAlpha".
    isValidNameChar ch = isAlphaNum ch || ch == '_'

-- | @'fieldLikeName' field@ is the casing transformation used to produce record selectors from message fields. If
-- @field@ is prefixed by a span of uppercase characters then that prefix will be lowercased while the remaining string
-- is left unchanged.
fieldLikeName :: String -> String
fieldLikeName "" = ""
fieldLikeName (x : xs)
  | isUpper x = map toLower prefix ++ suffix
  | otherwise = x : xs
  where (prefix, suffix) = span isUpper (x : xs)

prefixedEnumFieldName :: String -> String -> String
prefixedEnumFieldName enumName enumItem = enumName ++ enumItem

prefixedConName :: MonadError CompileError m => String -> String -> m String
prefixedConName msgName conName = do
  constructor <- typeLikeName conName
  return (msgName ++ constructor)

-- | @'prefixedMethodName' service method@ produces a Haskell record selector name for the service method @method@ by
-- joining the names @service@, @method@ under concatenation on a camel-casing transformation.
prefixedMethodName :: MonadError CompileError m => String -> String -> m String
prefixedMethodName _ "" = invalidTypeNameError "<empty name>"
prefixedMethodName serviceName (x : xs)
  | isLower x = return (fieldLikeName serviceName ++ fieldLikeName (x : xs))
  | otherwise = do
      method <- typeLikeName (x : xs)
      return (fieldLikeName serviceName ++ method)

-- | @'prefixedFieldName' prefix field@ constructs a Haskell record selector name by prepending @prefix@ in camel-case
-- to the message field/service method name @field@.
prefixedFieldName :: MonadError CompileError m => String -> String -> m String
prefixedFieldName msgName fieldName = do
  field <- typeLikeName fieldName
  return (fieldLikeName msgName ++ field)

dpIdentUnqualName :: MonadError CompileError m => DotProtoIdentifier -> m String
dpIdentUnqualName (Single name)       = pure name
dpIdentUnqualName (Dots (Path names)) = pure (NE.last names)
dpIdentUnqualName (Qualified _ next)  = dpIdentUnqualName next
dpIdentUnqualName Anonymous           = internalError "dpIdentUnqualName: Anonymous"

dpIdentQualName :: MonadError CompileError m => DotProtoIdentifier -> m String
dpIdentQualName (Single name)       = pure name
dpIdentQualName (Dots (Path names)) = pure (intercalate "." (NE.toList names))
dpIdentQualName (Qualified _ _)     = internalError "dpIdentQualName: Qualified"
dpIdentQualName Anonymous           = internalError "dpIdentQualName: Anonymous"

-- | Given a 'DotProtoIdentifier' for the parent type and the unqualified name
-- of this type, generate the corresponding Haskell name
nestedTypeName :: MonadError CompileError m => DotProtoIdentifier -> String -> m String
nestedTypeName Anonymous             nm = typeLikeName nm
nestedTypeName (Single parent)       nm = intercalate "_" <$> traverse typeLikeName [parent, nm]
nestedTypeName (Dots (Path parents)) nm = intercalate "_" . (<> [nm]) <$> traverse typeLikeName (NE.toList parents)
nestedTypeName (Qualified {})        _  = internalError "nestedTypeName: Qualified"

qualifiedMessageName :: MonadError CompileError m => DotProtoIdentifier -> DotProtoIdentifier -> m String
qualifiedMessageName parentIdent msgIdent = nestedTypeName parentIdent =<< dpIdentUnqualName msgIdent

qualifiedMessageTypeName :: MonadError CompileError m =>
                            TypeContext ->
                            DotProtoIdentifier ->
                            DotProtoIdentifier ->
                            m String
qualifiedMessageTypeName ctxt parentIdent msgIdent = do
  xs <- parents parentIdent []
  case xs of
    [] -> nestedTypeName parentIdent =<< dpIdentUnqualName msgIdent
    x : xs' -> nestedTypeName (Dots . Path $ x NE.:| xs') =<< dpIdentUnqualName msgIdent
  where
    parents par@(Single x) xs =
      case M.lookup par ctxt of
        Just (DotProtoTypeInfo { dotProtoTypeInfoParent = parentIdent' }) ->
          parents parentIdent' $ x : xs
        Nothing ->
          pure $ x : xs
    parents Anonymous xs =
      pure xs
    parents par _ =
      internalError $ "qualifiedMessageTypeName: wrong parent " <> show par

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
    qualName <- prefixedFieldName msgName fieldName
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
  = CircularImport FilePath
  | CompileParseError ParseError
  | InternalError String
  | InvalidPackageName DotProtoIdentifier
  | InvalidMethodName DotProtoIdentifier
  | InvalidModuleName String
  | InvalidTypeName String
  | InvalidMapKeyType String
  | NoPackageDeclaration
  | NoSuchType DotProtoIdentifier
  | NonzeroFirstEnumeration String DotProtoIdentifier Int32
  | EmptyEnumeration String
  | Unimplemented String
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
