-- | Fairly straightforward AST encoding of the .proto grammar

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
    , DotProto(..)
    , DotProtoValue(..)
    , DotProtoPrimType(..)
    , Packing(..)
    , DotProtoType(..)
    , DotProtoEnumValue
    , DotProtoEnumPart(..)
    , Streaming(..)
    , DotProtoServicePart(..)
    , DotProtoMessagePart(..)
    , DotProtoField(..)
    , DotProtoReservedField(..)

    -- * Utilities
    , isPackableType
  ) where

import           Data.String        (IsString)
import           Numeric.Natural
import           Proto3.Wire.Types  (FieldNumber (..))
import           Test.QuickCheck

-- | The name of a message
newtype MessageName = MessageName
  { getMessageName :: String
  } deriving (Eq, Ord, IsString)

instance Show MessageName where
  show = show . getMessageName

-- | The name of a field in an enum
newtype FieldName = FieldName
  { getFieldName :: String
  } deriving (Eq, Ord, IsString)

instance Show FieldName where
  show = show . getFieldName

-- | The name of the package
newtype PackageName = PackageName
  { getPackageName :: String
  } deriving (Eq, Ord, IsString)

instance Show PackageName where
  show = show . getPackageName

data DotProtoIdentifier
  = Single String
  | Path   [String]
  | Qualified DotProtoIdentifier DotProtoIdentifier
  | Anonymous -- [recheck] is there a better way to represent unnamed things
  deriving (Show, Eq, Ord)

-- | Top-level import declaration
data DotProtoImport = DotProtoImport
  { dotProtoImportQualifier :: DotProtoImportQualifier
  , dotProtoImportPath      :: String
  } deriving (Show, Eq)

instance Arbitrary DotProtoImport where
    arbitrary = do
      dotProtoImportQualifier <- arbitrary
      let dotProtoImportPath = ""
      return (DotProtoImport {..})

data DotProtoImportQualifier
  = DotProtoImportPublic
  | DotProtoImportWeak
  | DotProtoImportDefault
  deriving (Show, Eq)

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
  deriving (Show, Eq)

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
  } deriving (Show, Eq)

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
  = DotProtoMessage DotProtoIdentifier [DotProtoMessagePart]
  | DotProtoEnum    DotProtoIdentifier [DotProtoEnumPart]
  | DotProtoService DotProtoIdentifier [DotProtoServicePart]
  | DotProtoNullDef
  deriving (Show, Eq)

instance Arbitrary DotProtoDefinition where
  arbitrary = oneof [arbitraryMessage, arbitraryEnum]
    where
      arbitraryMessage = do
        identifier <- arbitrarySingleIdentifier
        parts      <- smallListOf arbitrary
        return (DotProtoMessage identifier parts)

      arbitraryEnum = do
        identifier <- arbitrarySingleIdentifier
        parts      <- smallListOf arbitrary
        return (DotProtoEnum identifier parts)

-- | This data structure represents a .proto file
--   The actual source order of protobuf statements isn't meaningful so statements are sorted by type during parsing
--   A .proto file with more than one package declaration is considered invalid
data DotProto = DotProto
  { protoImports     :: [DotProtoImport]
  , protoOptions     :: [DotProtoOption]
  , protoPackage     :: DotProtoPackageSpec
  , protoDefinitions :: [DotProtoDefinition]
  } deriving (Show, Eq)

instance Arbitrary DotProto where
  arbitrary = do
    protoImports     <- smallListOf arbitrary
    protoOptions     <- smallListOf arbitrary
    protoPackage     <- arbitrary
    protoDefinitions <- smallListOf arbitrary
    return (DotProto {..})

-- | Matches the definition of `constant` in the proto3 language spec
--   These are only used as rvalues
data DotProtoValue
  = Identifier DotProtoIdentifier
  | StringLit  String
  | IntLit     Int
  | FloatLit   Double
  | BoolLit    Bool
  deriving (Show, Eq)

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
  | Named DotProtoIdentifier -- ^ A named type, referring to another message or enum defined in the same file
  deriving (Show, Eq)

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
  deriving (Show, Eq)

instance Arbitrary Packing where
  arbitrary = elements [PackedField, UnpackedField]

-- | This type is an almagamation of the modifiers used in types
--   It corresponds to a syntax role but not a semantic role, not all modifiers are meaningful in every type context
data DotProtoType
  = Prim           DotProtoPrimType
  | Optional       DotProtoPrimType
  | Repeated       DotProtoPrimType
  | NestedRepeated DotProtoPrimType
  | Map            DotProtoPrimType DotProtoPrimType
  deriving (Show, Eq)

instance Arbitrary DotProtoType where
  arbitrary = oneof [fmap Prim arbitrary]

type DotProtoEnumValue = Int

data DotProtoEnumPart
  = DotProtoEnumField DotProtoIdentifier DotProtoEnumValue -- [DotProtoOption]
  | DotProtoEnumOption DotProtoOption
  | DotProtoEnumEmpty
  deriving (Show, Eq)

instance Arbitrary DotProtoEnumPart where
  arbitrary = oneof [arbitraryField, arbitraryOption]
    where
      arbitraryField = do
        identifier <- arbitraryIdentifier
        enumValue  <- arbitrary
        return (DotProtoEnumField identifier enumValue)

      arbitraryOption = do
        option <- arbitrary
        return (DotProtoEnumOption option)

data Streaming
  = Streaming
  | NonStreaming
  deriving (Show, Eq)

instance Arbitrary Streaming where
  arbitrary = elements [Streaming, NonStreaming]

-- [refactor] add named accessors to ServiceRPC
--            break this into two types
data DotProtoServicePart
  = DotProtoServiceRPC    DotProtoIdentifier (DotProtoIdentifier, Streaming) (DotProtoIdentifier, Streaming) [DotProtoOption]
  | DotProtoServiceOption DotProtoOption
  | DotProtoServiceEmpty
  deriving (Show, Eq)

instance Arbitrary DotProtoServicePart where
  arbitrary = oneof
    [ arbitraryServiceRPC
    , arbitraryServiceOption
    ]
    where
      arbitraryServiceRPC = do
        identifier <- arbitrarySingleIdentifier
        rpcClause0 <- arbitraryRPCClause
        rpcClause1 <- arbitraryRPCClause
        options    <- smallListOf arbitrary
        return (DotProtoServiceRPC identifier rpcClause0 rpcClause1 options)
        where
          arbitraryRPCClause = do
            identifier <- arbitraryIdentifier
            streaming  <- arbitrary
            return (identifier, streaming)

      arbitraryServiceOption = do
        option <- arbitrary
        return (DotProtoServiceOption option)

data DotProtoMessagePart
  = DotProtoMessageField DotProtoField
  | DotProtoMessageOneOf
  { dotProtoOneOfName   :: DotProtoIdentifier
  , dotProtoOneOfFields :: [DotProtoField]
  }
  | DotProtoMessageDefinition DotProtoDefinition
  | DotProtoMessageReserved   [DotProtoReservedField]
  deriving (Show, Eq)

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
        dotProtoOneOfName   <- arbitrarySingleIdentifier
        dotProtoOneOfFields <- smallListOf arbitrary
        return (DotProtoMessageOneOf {..})

      arbitraryDefinition = do
        definition <- arbitrary
        return (DotProtoMessageDefinition definition)

      arbitraryReserved = do
        fields <- oneof [smallListOf1 arbitrary, arbitraryReservedLabels]
        return (DotProtoMessageReserved fields)

      arbitraryReservedLabels :: Gen [DotProtoReservedField]
      arbitraryReservedLabels = smallListOf1 (ReservedIdentifier <$> return "")

data DotProtoField = DotProtoField
  { dotProtoFieldNumber  :: FieldNumber
  , dotProtoFieldType    :: DotProtoType
  , dotProtoFieldName    :: DotProtoIdentifier
  , dotProtoFieldOptions :: [DotProtoOption]
  , dotProtoFieldComment :: Maybe String
  }
  | DotProtoEmptyField
  deriving (Show, Eq)

instance Arbitrary DotProtoField where
  arbitrary = do
    dotProtoFieldNumber  <- arbitrary
    dotProtoFieldType    <- arbitrary
    dotProtoFieldName    <- arbitraryIdentifier
    dotProtoFieldOptions <- smallListOf arbitrary
    -- TODO: Generate random comments once the parser supports comments
    dotProtoFieldComment <- pure Nothing
    return (DotProtoField {..})

data DotProtoReservedField
  = SingleField Int
  | FieldRange  Int Int
  | ReservedIdentifier String
  deriving (Show, Eq)

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

-- | Returns 'True' if the given primitive type is packable
isPackableType :: DotProtoPrimType -> Bool
isPackableType (Named _) = False
isPackableType Bytes = False
isPackableType String = False
isPackableType Int32 = True
isPackableType Int64 = True
isPackableType SInt32 = True
isPackableType SInt64 = True
isPackableType UInt32 = True
isPackableType UInt64 = True
isPackableType Fixed32 = True
isPackableType Fixed64 = True
isPackableType SFixed32 = True
isPackableType SFixed64 = True
isPackableType Bool = True
isPackableType Float = True
isPackableType Double = True

--------------------------------------------------------------------------------
-- | QC Arbitrary instance for generating random protobuf

arbitraryService :: Gen DotProtoDefinition
arbitraryService = do
  identifier <- arbitrarySingleIdentifier
  parts      <- smallListOf arbitrary
  return (DotProtoService identifier parts)

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
  return (Path (name:names))

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
