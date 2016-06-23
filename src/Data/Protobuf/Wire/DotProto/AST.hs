-- | Fairly straightforward AST encoding of the .proto grammar

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Protobuf.Wire.DotProto.AST where

import           Data.Protobuf.Wire.Types (FieldNumber (..))
import           Data.String              (IsString)
import           Control.Monad
import           Numeric.Natural
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
  deriving (Show, Eq)

-- | Top-level import declaration
data DotProtoImport = DotProtoImport
  { dotProtoImportQualifier :: DotProtoImportQualifier
  , dotProtoImportPath      :: String
  } deriving (Show, Eq)

data DotProtoImportQualifier
  = DotProtoImportPublic
  | DotProtoImportWeak
  | DotProtoImportDefault
  deriving (Show, Eq)

-- | The namespace declaration
data DotProtoPackageSpec
  = DotProtoPackageSpec DotProtoIdentifier
  | DotProtoNoPackage
  deriving (Show, Eq)

-- | An option id/value pair, can be attached to many types of statements
data DotProtoOption = DotProtoOption DotProtoIdentifier DotProtoValue deriving (Show, Eq)

-- | Top-level protocol definitions
data DotProtoDefinition
  = DotProtoMessage DotProtoIdentifier [DotProtoMessagePart]
  | DotProtoEnum    DotProtoIdentifier [DotProtoEnumPart]
  | DotProtoService DotProtoIdentifier [DotProtoServicePart]
  | DotProtoNullDef
  deriving (Show, Eq)

-- | This data structure represents a .proto file
--   The actual source order of protobuf statements isn't meaningful so statements are sorted by type during parsing
--   A .proto file with more than one package declaration is considered invalid
data DotProto = DotProto
  { protoImports     :: [DotProtoImport]
  , protoOptions     :: [DotProtoOption]
  , protoPackage     :: DotProtoPackageSpec
  , protoDefinitions :: [DotProtoDefinition]
  } deriving (Show, Eq)

-- | Matches the definition of `constant` in the proto3 language spec
--   These are only used as rvalues
data DotProtoValue
  = Identifier DotProtoIdentifier
  | StringLit  String
  | IntLit     Int
  | FloatLit   Double
  | BoolLit    Bool
  deriving (Show, Eq)

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

data Packing
  = PackedField
  | UnpackedField
  deriving (Show, Eq)

-- | This type is an almagamation of the modifiers used in types
--   It corresponds to a syntax role but not a semantic role, not all modifiers are meaningful in every type context
data DotProtoType
  = Prim           DotProtoPrimType
  | Optional       DotProtoPrimType
  | Repeated       DotProtoPrimType
  | NestedRepeated DotProtoPrimType
  | Map            DotProtoPrimType DotProtoPrimType
  deriving (Show, Eq)

type DotProtoEnumValue = Int

data DotProtoEnumPart
  = DotProtoEnumField DotProtoIdentifier DotProtoEnumValue -- [DotProtoOption]
  | DotProtoEnumOption DotProtoOption
  | DotProtoEnumEmpty
  deriving (Show, Eq)

data Streaming
  = Streaming
  | NonStreaming
  deriving (Show, Eq)

-- [refactor] add named accessors to ServiceRPC
--            break this into two types
data DotProtoServicePart
  = DotProtoServiceRPC    DotProtoIdentifier (DotProtoIdentifier, Streaming) (DotProtoIdentifier, Streaming) [DotProtoOption]
  | DotProtoServiceOption DotProtoOption
  | DotProtoServiceEmpty
  deriving (Show, Eq)

data DotProtoMessagePart
  = DotProtoMessageField DotProtoField
  | DotProtoMessageOneOf
  { dotProtoOneOfName   :: DotProtoIdentifier
  , dotProtoOneOfFields :: [DotProtoField]
  }
  | DotProtoMessageDefinition DotProtoDefinition
  | DotProtoMessageReserved   [DotProtoReservedField]
  deriving (Show, Eq)

data DotProtoField = DotProtoField
  { dotProtoFieldNumber  :: FieldNumber
  , dotProtoFieldType    :: DotProtoType
  , dotProtoFieldName    :: DotProtoIdentifier
  , dotProtoFieldOptions :: [DotProtoOption]
  }
  | DotProtoEmptyField
  deriving (Show, Eq)

data DotProtoReservedField
  = SingleField Int
  | FieldRange  Int Int
  | ReservedIdentifier String
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- | QC Arbitrary instance for generating random protobuf

instance Arbitrary DotProto where
  arbitrary = DotProto <$> smallListOf arbitraryImport <*> smallListOf arbitraryOption <*> arbitraryPackageSpec <*> smallListOf arbitraryDefinition

arbitraryImport :: Gen DotProtoImport
arbitraryImport = DotProtoImport <$> elements [DotProtoImportDefault, DotProtoImportWeak, DotProtoImportPublic]
                                 <*> return "" -- arbitrary

arbitraryOption :: Gen DotProtoOption
arbitraryOption = DotProtoOption <$> oneof [arbitraryPathIdentifier, arbitraryNestedIdentifier] <*> arbitraryValue

arbitraryPackageSpec :: Gen DotProtoPackageSpec
arbitraryPackageSpec = oneof [ return DotProtoNoPackage
                             , DotProtoPackageSpec <$> arbitrarySingleIdentifier
                             , DotProtoPackageSpec <$> arbitraryPathIdentifier
                             ]

arbitraryDefinition :: Gen DotProtoDefinition
arbitraryDefinition = oneof [ arbitraryMessage
                            , arbitraryEnum
                            ]

arbitraryMessage :: Gen DotProtoDefinition
arbitraryMessage = DotProtoMessage <$> arbitrarySingleIdentifier <*> smallListOf arbitraryMessagePart

arbitraryMessagePart :: Gen DotProtoMessagePart
arbitraryMessagePart = oneof [ DotProtoMessageField      <$> arbitraryField
                             , DotProtoMessageOneOf      <$> arbitrarySingleIdentifier <*> smallListOf arbitraryField
                             , DotProtoMessageDefinition <$> arbitraryDefinition
                             , DotProtoMessageReserved   <$> oneof [arbitraryReservedNumbers, arbitraryReservedLabels]
                             ]

arbitraryReservedNumbers :: Gen [DotProtoReservedField]
arbitraryReservedNumbers = smallListOf1 $ oneof [SingleField <$> arbitraryFieldNumber, FieldRange <$> arbitraryFieldNumber <*> arbitraryFieldNumber]

arbitraryReservedLabels :: Gen [DotProtoReservedField]
arbitraryReservedLabels = smallListOf1 $ ReservedIdentifier <$> return "" -- arbitrary

arbitraryEnum :: Gen DotProtoDefinition
arbitraryEnum = DotProtoEnum <$> arbitrarySingleIdentifier <*> smallListOf arbitraryEnumPart

arbitraryEnumPart :: Gen DotProtoEnumPart
arbitraryEnumPart = oneof [ DotProtoEnumField <$> arbitraryIdentifier <*> arbitrary
                          , DotProtoEnumOption <$> arbitraryOption
                          ]

arbitraryService :: Gen DotProtoDefinition
arbitraryService = DotProtoService <$> arbitrarySingleIdentifier <*> smallListOf arbitraryServicePart

arbitraryServicePart :: Gen DotProtoServicePart
arbitraryServicePart = oneof [ DotProtoServiceRPC <$> arbitrarySingleIdentifier <*> arbitraryRPCClause <*> arbitraryRPCClause <*> smallListOf arbitraryOption
                             , DotProtoServiceOption <$> arbitraryOption
                             ]

arbitraryRPCClause :: Gen (DotProtoIdentifier, Streaming)
arbitraryRPCClause = (,) <$> arbitraryIdentifier <*> elements [Streaming, NonStreaming]

arbitraryField :: Gen DotProtoField
arbitraryField = DotProtoField <$> arbitrary <*> arbitraryType <*> arbitraryIdentifier <*> smallListOf arbitraryOption

arbitraryType :: Gen DotProtoType
arbitraryType = oneof [ Prim <$> arbitraryPrimType ]

arbitraryPrimType :: Gen DotProtoPrimType
arbitraryPrimType = oneof [ elements [ Int32
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
                          , Named <$> arbitrarySingleIdentifier
                          ]

arbitraryIdentifierName :: Gen String
arbitraryIdentifierName = liftM2 (:) (elements $ ['a'..'z'] ++ ['A'..'Z'])
                                     (smallListOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['_'])

arbitrarySingleIdentifier :: Gen DotProtoIdentifier
arbitrarySingleIdentifier = Single <$> arbitraryIdentifierName

arbitraryPathIdentifier :: Gen DotProtoIdentifier
arbitraryPathIdentifier = Path <$> liftM2 (:) arbitraryIdentifierName (smallListOf1 arbitraryIdentifierName)

arbitraryNestedIdentifier :: Gen DotProtoIdentifier
arbitraryNestedIdentifier = Qualified <$> arbitraryIdentifier <*> arbitrarySingleIdentifier

-- these two kinds of identifiers are usually interchangeable, the others are not
arbitraryIdentifier :: Gen DotProtoIdentifier
arbitraryIdentifier = oneof [arbitrarySingleIdentifier, arbitraryPathIdentifier]

arbitraryValue :: Gen DotProtoValue
arbitraryValue = oneof [ Identifier <$> arbitrarySingleIdentifier
                       , StringLit  <$> return "" -- arbitrary
                       , IntLit     <$> arbitrary
                       , FloatLit   <$> arbitrary
                       , BoolLit    <$> arbitrary
                       ]

arbitraryFieldNumber :: Gen Int
arbitraryFieldNumber = fromIntegral <$> (arbitrary :: Gen Natural)

-- [note] quickcheck's default scaling generates *extremely* large asts past 20 iterations
--        the parser is not particularly slow but it does have noticeable delay on megabyte-large .proto files
smallListOf :: Gen a -> Gen [a]
smallListOf x = choose (0, 5) >>= \n -> vectorOf n x

smallListOf1 :: Gen a -> Gen [a]
smallListOf1 x = choose (1, 5) >>= \n -> vectorOf n x
