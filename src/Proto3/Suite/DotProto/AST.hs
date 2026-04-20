-- | Fairly straightforward AST encoding of the .proto grammar

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RecordWildCards            #-}

module Proto3.Suite.DotProto.AST
  ( -- * Types
      MessageName(..)
    , FieldName(..)
    , PackageName(..)
    , DotProtoIdentifier(..)
    , DotProtoImport(..)
    , DotProtoImportQualifier(..)
    , DotProtoPackageSpec(..)
    , DotProtoOption(..)
    , DotProtoDefinition(..)
    , DotProtoMeta(..)
    , DotProto(..)
    , DotProtoValue(..)
    , DotProtoPrimType(..)
    , Packing(..)
    , Path(..), fakePath
    , DotProtoType(..)
    , DotProtoEnumValue
    , DotProtoEnumPart(..)
    , Streaming(..)
    , DotProtoServicePart(..)
    , RPCMethod(..)
    , DotProtoMessagePart(..)
    , DotProtoField(..)
    , DotProtoReservedField(..)
  ) where

import           Control.Applicative
import           Data.Char                 (toLower)
import           Control.Monad
import           Data.Data                 (Data)
import           Data.Int                  (Int32)
import qualified Data.List.NonEmpty        as NE
import           Data.String               (IsString(..))
import           GHC.Generics              (Generic)
import           Numeric.Natural
import           Prelude                   hiding (FilePath)
import           Proto3.Wire.Types         (FieldNumber (..))
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           Text.PrettyPrint          ((<+>))
import qualified Text.PrettyPrint          as PP
import           Text.PrettyPrint.HughesPJClass (Pretty(..))
import           Turtle                    (FilePath)
import           Turtle.Compat             (encodeString)
import "template-haskell" Language.Haskell.TH.Syntax (Lift (..))

-- | The name of a message
newtype MessageName = MessageName
  { getMessageName :: String }
  deriving (Data, Eq, Generic, IsString, Ord)

instance Show MessageName where
  show = show . getMessageName

-- | The name of some field
newtype FieldName = FieldName
  { getFieldName :: String }
  deriving (Data, Eq, Generic, IsString, Lift, Ord)

instance Show FieldName where
  show = show . getFieldName

-- | The name of the package
newtype PackageName = PackageName
  { getPackageName :: String }
  deriving (Data, Eq, Generic, IsString, Lift, Ord)

instance Show PackageName where
  show = show . getPackageName

newtype Path = Path
  { components :: NE.NonEmpty String }
  deriving (Data, Eq, Generic, Lift, Ord, Show)

-- Used for testing
fakePath :: Path
fakePath = Path ("fakePath" NE.:| [])

data DotProtoIdentifier
  = Single String
  | Dots   Path
  | Qualified DotProtoIdentifier DotProtoIdentifier
  | Anonymous -- [recheck] is there a better way to represent unnamed things
  deriving (Data, Eq, Generic, Lift, Ord, Show)

instance Pretty DotProtoIdentifier where
  pPrint (Single name)                    = PP.text name
  pPrint (Dots (Path names))              = PP.hcat . PP.punctuate (PP.text ".") $ PP.text <$> NE.toList names
  pPrint (Qualified qualifier identifier) = PP.parens (pPrint qualifier) <> PP.text "." <> pPrint identifier
  pPrint Anonymous                        = PP.empty

-- | Top-level import declaration
data DotProtoImport = DotProtoImport
  { dotProtoImportQualifier :: DotProtoImportQualifier
  , dotProtoImportPath      :: FilePath
  }
  deriving (Data, Eq, Generic, Lift, Ord, Show)

instance Pretty DotProtoImport where
  pPrint (DotProtoImport q i) =
    PP.text "import" <+> pPrint q <+> strLit fp PP.<> PP.text ";"
    where
      fp = encodeString i

instance Arbitrary DotProtoImport where
  arbitrary = do
    dotProtoImportQualifier <- arbitrary
    dotProtoImportPath <- fmap fromString arbitrary
    return (DotProtoImport {..})

data DotProtoImportQualifier
  = DotProtoImportPublic
  | DotProtoImportWeak
  | DotProtoImportDefault
  deriving (Bounded, Data, Enum, Eq, Generic, Lift, Ord, Show)

instance Pretty DotProtoImportQualifier where
  pPrint DotProtoImportDefault = PP.empty
  pPrint DotProtoImportPublic  = PP.text "public"
  pPrint DotProtoImportWeak    = PP.text "weak"

instance Arbitrary DotProtoImportQualifier where
  arbitrary = elements
    [ DotProtoImportDefault
    , DotProtoImportWeak
    , DotProtoImportPublic
    ]

-- | The namespace declaration
data DotProtoPackageSpec
  = DotProtoPackageSpec DotProtoIdentifier
  | DotProtoNoPackage
  deriving (Data, Eq, Generic, Lift, Ord, Show)

instance Pretty DotProtoPackageSpec where
  pPrint (DotProtoPackageSpec p) = PP.text "package" <+> pPrint p PP.<> PP.text ";"
  pPrint (DotProtoNoPackage)     = PP.empty

instance Arbitrary DotProtoPackageSpec where
  arbitrary = oneof
    [ return DotProtoNoPackage
    , fmap DotProtoPackageSpec arbitrarySingleIdentifier
    , fmap DotProtoPackageSpec arbitraryPathIdentifier
    ]

-- | An option id/value pair, can be attached to many types of statements
data DotProtoOption = DotProtoOption
  { dotProtoOptionIdentifier :: DotProtoIdentifier
  , dotProtoOptionValue      :: DotProtoValue
  } deriving (Data, Eq, Generic, Lift, Ord, Show)

instance Pretty DotProtoOption where
  pPrint (DotProtoOption key value) = pPrint key <+> PP.text "=" <+> pPrint value

instance Arbitrary DotProtoOption where
    arbitrary = do
      dotProtoOptionIdentifier <- oneof
        [ arbitraryPathIdentifier
        , arbitraryNestedIdentifier
        ]
      dotProtoOptionValue <- arbitrary
      return (DotProtoOption {..})

-- | Top-level protocol definitions
data DotProtoDefinition
  = DotProtoMessage String DotProtoIdentifier [DotProtoMessagePart]
  | DotProtoEnum    String DotProtoIdentifier [DotProtoEnumPart]
  | DotProtoService String DotProtoIdentifier [DotProtoServicePart]
  deriving (Data, Eq, Generic, Lift, Ord, Show)

instance Arbitrary DotProtoDefinition where
  arbitrary = oneof [arbitraryMessage, arbitraryEnum]
    where
      arbitraryMessage = do
        comment    <- pure mempty  -- until parser supports comments
        identifier <- arbitrarySingleIdentifier
        parts      <- smallListOf arbitrary
        return (DotProtoMessage comment identifier parts)

      arbitraryEnum = do
        comment    <- pure mempty  -- until parser supports comments
        identifier <- arbitrarySingleIdentifier
        parts      <- smallListOf arbitrary
        return (DotProtoEnum comment identifier parts)

-- | Tracks misc metadata about the AST
data DotProtoMeta = DotProtoMeta
  { metaModulePath :: Path
    -- ^ The "module path" associated with the .proto file from which this AST
    -- was parsed. The "module path" is derived from the `--includeDir`-relative
    -- .proto filename passed to `Proto3.Suite.DotProto.Parsing.parseProtoFile`. See
    -- 'Proto3.Suite.DotProto.Internal.toModulePath' for details on how module
    -- path values are constructed. See
    -- 'Proto3.Suite.DotProto.Generate.modulePathModName' to see how it is used
    -- during code generation.
  } deriving (Data, Eq, Generic, Lift, Ord, Show)

instance Arbitrary DotProtoMeta where
  arbitrary = pure (DotProtoMeta fakePath)

-- | This data structure represents a .proto file
--   The actual source order of protobuf statements isn't meaningful so
--   statements are sorted by type during parsing.
--   A .proto file with more than one package declaration is considered invalid.
data DotProto = DotProto
  { protoImports     :: [DotProtoImport]
  , protoOptions     :: [DotProtoOption]
  , protoPackage     :: DotProtoPackageSpec
  , protoDefinitions :: [DotProtoDefinition]
  , protoMeta        :: DotProtoMeta
  } 
  deriving (Data, Eq, Generic, Lift, Ord, Show)

instance Arbitrary DotProto where
  arbitrary = do
    protoImports     <- smallListOf arbitrary
    protoOptions     <- smallListOf arbitrary
    protoPackage     <- arbitrary
    protoDefinitions <- smallListOf arbitrary
    protoMeta        <- arbitrary
    return (DotProto {..})

-- | Matches the definition of @constant@ in the proto3 language spec
--   These are only used as rvalues
data DotProtoValue
  = Identifier DotProtoIdentifier
  | StringLit  String
  | IntLit     Int
  | FloatLit   Double
  | BoolLit    Bool
  deriving (Data, Eq, Generic, Lift, Ord, Show)

instance Pretty DotProtoValue where
  pPrint (Identifier value) = pPrint value
  pPrint (StringLit  value) = strLit value
  pPrint (IntLit     value) = PP.text $ show value
  pPrint (FloatLit   value) = PP.text $ show value
  pPrint (BoolLit    value) = PP.text $ toLower <$> show value

instance Arbitrary DotProtoValue where
  arbitrary = oneof
    [ fmap Identifier  arbitrarySingleIdentifier
    , fmap StringLit  (return "")
    , fmap IntLit      arbitrary
    , fmap FloatLit    arbitrary
    , fmap BoolLit     arbitrary
    ]

data DotProtoPrimType
  = Int32
  | Int64
  | SInt32
  | SInt64
  | UInt32
  | UInt64
  | Fixed32
  | Fixed64
  | SFixed32
  | SFixed64
  | String
  | Bytes
  | Bool
  | Float
  | Double
  | Named DotProtoIdentifier
  -- ^ A named type, referring to another message or enum defined in the same file
  deriving (Data, Eq, Generic, Lift, Ord, Show)

instance Pretty DotProtoPrimType where
  pPrint (Named i)  = pPrint i
  pPrint Int32      = PP.text "int32"
  pPrint Int64      = PP.text "int64"
  pPrint SInt32     = PP.text "sint32"
  pPrint SInt64     = PP.text "sint64"
  pPrint UInt32     = PP.text "uint32"
  pPrint UInt64     = PP.text "uint64"
  pPrint Fixed32    = PP.text "fixed32"
  pPrint Fixed64    = PP.text "fixed64"
  pPrint SFixed32   = PP.text "sfixed32"
  pPrint SFixed64   = PP.text "sfixed64"
  pPrint String     = PP.text "string"
  pPrint Bytes      = PP.text "bytes"
  pPrint Bool       = PP.text "bool"
  pPrint Float      = PP.text "float"
  pPrint Double     = PP.text "double"

instance Arbitrary DotProtoPrimType where
  arbitrary = oneof
    [ elements
      [ Int32
      , Int64
      , SInt32
      , SInt64
      , UInt32
      , UInt64
      , Fixed32
      , Fixed64
      , SFixed32
      , SFixed64
      , String
      , Bytes
      , Bool
      , Float
      , Double
      ]
    , fmap Named arbitrarySingleIdentifier
    ]

data Packing
  = PackedField
  | UnpackedField
  deriving (Bounded, Data, Enum, Eq, Generic, Lift, Ord, Show)

instance Arbitrary Packing where
  arbitrary = elements [PackedField, UnpackedField]

-- | This type is an almagamation of the modifiers used in types.
--   It corresponds to a syntax role but not a semantic role, not all modifiers
--   are meaningful in every type context.
data DotProtoType
  = Prim           DotProtoPrimType
  | Optional       DotProtoPrimType
  | Repeated       DotProtoPrimType
  | NestedRepeated DotProtoPrimType
  | Map            DotProtoPrimType DotProtoPrimType
  deriving (Data, Eq, Generic, Lift, Ord, Show)

instance Pretty DotProtoType where
  pPrint (Prim           ty) = pPrint ty
  pPrint (Optional       ty) = PP.text "optional" <+> pPrint ty
  pPrint (Repeated       ty) = PP.text "repeated" <+> pPrint ty
  pPrint (NestedRepeated ty) = PP.text "repeated" <+> pPrint ty
  pPrint (Map keyty valuety) = PP.text "map<" <> pPrint keyty <> PP.text ", " <> pPrint valuety <> PP.text ">"

instance Arbitrary DotProtoType where
  arbitrary = oneof [fmap Prim arbitrary]

type DotProtoEnumValue = Int32

data DotProtoEnumPart
  = DotProtoEnumField DotProtoIdentifier DotProtoEnumValue [DotProtoOption]
  | DotProtoEnumOption DotProtoOption
  | DotProtoEnumReserved   [DotProtoReservedField]
  deriving (Data, Eq, Generic, Lift, Ord, Show)

instance Arbitrary DotProtoEnumPart where
  arbitrary = oneof [arbitraryField, arbitraryOption]
    where
      arbitraryField = do
        identifier <- arbitraryIdentifier
        enumValue  <- arbitrary
        opts       <- arbitrary
        return (DotProtoEnumField identifier enumValue opts)

      arbitraryOption = do
        option <- arbitrary
        return (DotProtoEnumOption option)

data Streaming
  = Streaming
  | NonStreaming
  deriving (Bounded, Data, Enum, Eq, Generic, Lift, Ord, Show)

instance Pretty Streaming where
  pPrint Streaming    = PP.text "stream"
  pPrint NonStreaming = PP.empty

instance Arbitrary Streaming where
  arbitrary = elements [Streaming, NonStreaming]

data DotProtoServicePart
  = DotProtoServiceRPCMethod RPCMethod
  | DotProtoServiceOption DotProtoOption
  deriving (Data, Eq, Generic, Lift, Ord, Show)

instance Pretty DotProtoServicePart where
  pPrint (DotProtoServiceRPCMethod RPCMethod{..})
    =   PP.text "rpc"
    <+> pPrint rpcMethodName
    <+> PP.parens (pPrint rpcMethodRequestStreaming <+> pPrint rpcMethodRequestType)
    <+> PP.text "returns"
    <+> PP.parens (pPrint rpcMethodResponseStreaming <+> pPrint rpcMethodResponseType)
    <+> case rpcMethodOptions of
          [] -> PP.text ";"
          _  -> PP.braces . PP.vcat $ serviceOption <$> rpcMethodOptions
  pPrint (DotProtoServiceOption option) = serviceOption option

instance Arbitrary DotProtoServicePart where
  arbitrary = oneof
    [ DotProtoServiceRPCMethod <$> arbitrary
    , DotProtoServiceOption <$> arbitrary
    ]

data RPCMethod = RPCMethod
  { rpcMethodName :: DotProtoIdentifier
  , rpcMethodRequestType :: DotProtoIdentifier
  , rpcMethodRequestStreaming :: Streaming
  , rpcMethodResponseType :: DotProtoIdentifier
  , rpcMethodResponseStreaming :: Streaming
  , rpcMethodOptions :: [DotProtoOption]
  }
  deriving (Data, Eq, Generic, Lift, Ord, Show)

instance Arbitrary RPCMethod where
  arbitrary = do
    rpcMethodName <- arbitrarySingleIdentifier
    rpcMethodRequestType <- arbitraryIdentifier
    rpcMethodRequestStreaming  <- arbitrary
    rpcMethodResponseType <- arbitraryIdentifier
    rpcMethodResponseStreaming  <- arbitrary
    rpcMethodOptions <- smallListOf arbitrary
    return RPCMethod{..}

data DotProtoMessagePart
  = DotProtoMessageField DotProtoField
  | DotProtoMessageOneOf DotProtoIdentifier [DotProtoField]
  | DotProtoMessageDefinition DotProtoDefinition
  | DotProtoMessageReserved   [DotProtoReservedField]
  | DotProtoMessageOption DotProtoOption
  deriving (Data, Eq, Generic, Lift, Ord, Show)

instance Arbitrary DotProtoMessagePart where
  arbitrary = oneof
    [ arbitraryField
    , arbitraryOneOf
    , arbitraryDefinition
    , arbitraryReserved
    ]
    where
      arbitraryField = do
        field <- arbitrary
        return (DotProtoMessageField field)

      arbitraryOneOf = do
        name   <- arbitrarySingleIdentifier
        fields <- smallListOf arbitrary
        return (DotProtoMessageOneOf name fields)

      arbitraryDefinition = do
        definition <- arbitrary
        return (DotProtoMessageDefinition definition)

      arbitraryReserved = do
        fields <- oneof [smallListOf1 arbitrary, arbitraryReservedLabels]
        return (DotProtoMessageReserved fields)

      arbitraryReservedLabels :: Gen [DotProtoReservedField]
      arbitraryReservedLabels =
          smallListOf1 (ReservedIdentifier <$> arbitraryIdentifierName)

data DotProtoField = DotProtoField
  { dotProtoFieldNumber  :: FieldNumber
  , dotProtoFieldType    :: DotProtoType
  , dotProtoFieldName    :: DotProtoIdentifier
  , dotProtoFieldOptions :: [DotProtoOption]
  , dotProtoFieldComment :: String
  }
  deriving (Data, Eq, Generic, Lift, Ord, Show)

instance Arbitrary DotProtoField where
  arbitrary = do
    dotProtoFieldNumber  <- arbitrary
    dotProtoFieldType    <- arbitrary
    dotProtoFieldName    <- arbitraryIdentifier
    dotProtoFieldOptions <- smallListOf arbitrary
    -- TODO: Generate random comments once the parser supports comments
    dotProtoFieldComment <- pure mempty
    return (DotProtoField {..})

data DotProtoReservedField
  = SingleField Int
  | FieldRange  Int Int
  | ReservedIdentifier String
  deriving (Data, Eq, Generic, Lift, Ord, Show)

instance Pretty DotProtoReservedField where
  pPrint (SingleField num)      = PP.text $ show num
  pPrint (FieldRange start end) = (PP.text $ show start) <+> PP.text "to" <+> (PP.text $ show end)
  pPrint (ReservedIdentifier i) = PP.text $ show i

instance Arbitrary DotProtoReservedField where
  arbitrary =
    oneof [arbitrarySingleField, arbitraryFieldRange]
      where
        arbitraryFieldNumber = do
          natural <- arbitrary
          return (fromIntegral (natural :: Natural))

        arbitrarySingleField = do
          fieldNumber <- arbitraryFieldNumber
          return (SingleField fieldNumber)

        arbitraryFieldRange = do
          begin <- arbitraryFieldNumber
          end   <- arbitraryFieldNumber
          return (FieldRange begin end)

--------------------------------------------------------------------------------
-- | QC Arbitrary instance for generating random protobuf

_arbitraryService :: Gen DotProtoDefinition
_arbitraryService = do
  comment    <- pure mempty  -- until parser supports comments
  identifier <- arbitrarySingleIdentifier
  parts      <- smallListOf arbitrary
  return (DotProtoService comment identifier parts)

arbitraryIdentifierName :: Gen String
arbitraryIdentifierName = do
  c  <- elements (['a'..'z'] ++ ['A'..'Z'])
  cs <- smallListOf (elements (['a'..'z'] ++ ['A'..'Z'] ++ ['_']))
  return (c:cs)

arbitrarySingleIdentifier :: Gen DotProtoIdentifier
arbitrarySingleIdentifier = fmap Single arbitraryIdentifierName

arbitraryPathIdentifier :: Gen DotProtoIdentifier
arbitraryPathIdentifier = do
  name  <- arbitraryIdentifierName
  names <- smallListOf1 arbitraryIdentifierName
  pure . Dots . Path $ name NE.:| names

arbitraryNestedIdentifier :: Gen DotProtoIdentifier
arbitraryNestedIdentifier = do
  identifier0 <- arbitraryIdentifier
  identifier1 <- arbitrarySingleIdentifier
  return (Qualified identifier0 identifier1)

-- these two kinds of identifiers are usually interchangeable, the others are not
arbitraryIdentifier :: Gen DotProtoIdentifier
arbitraryIdentifier = oneof [arbitrarySingleIdentifier, arbitraryPathIdentifier]

-- [note] quickcheck's default scaling generates *extremely* large asts past 20 iterations
--        the parser is not particularly slow but it does have noticeable delay on megabyte-large .proto files
smallListOf :: Gen a -> Gen [a]
smallListOf x = choose (0, 5) >>= \n -> vectorOf n x

smallListOf1 :: Gen a -> Gen [a]
smallListOf1 x = choose (1, 5) >>= \n -> vectorOf n x

strLit :: String -> PP.Doc
strLit string = PP.text "\"" <> foldMap escape string <> PP.text "\""
  where
    escape '\n' = PP.text "\\n"
    escape '\\' = PP.text "\\\\"
    escape '\0' = PP.text "\\x00"
    escape '"'  = PP.text "\\\""
    escape  c   = PP.text [ c ]

serviceOption :: DotProtoOption -> PP.Doc
serviceOption o = PP.text "option" <+> pPrint o PP.<> PP.text ";"
