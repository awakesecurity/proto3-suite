{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}

-- | Fairly straightforward AST encoding of the .proto grammar
module Proto3.Suite.DotProto.AST
  ( -- * Basic Operations 
    toProtoFile
  , toProtoFileDef 
  , pPrintFieldNumber 
    -- * RenderingOptions
  , RenderingOptions (..)
    -- ** Basic Operations
  , defRenderingOptions
  , defSelectorName
  , defEnumMemberName 
    -- * DotProto
  , DotProto (..)
    -- ** Basic Operations
  , pPrintDotProto
    -- * Types
  , MessageName (..)
  , FieldName (..)
  , PackageName (..)
  , DotProtoIdentifier (..)
  , DotProtoImport (..)
  , DotProtoImportQualifier (..)
  , DotProtoPackageSpec (DotProtoPackageSpec, DotProtoNoPackage, ..)
  , DotProtoOption (..)
  , DotProtoDefinition (..)
  , DotProtoMeta (..)
  , DotProtoValue (..)
  , DotProtoPrimType (..)
  , Packing (PackedField, UnpackedField, ..)
  , Path (..)
  , fakePath
  , DotProtoType (..)
  , DotProtoEnumValue
  , DotProtoEnumPart (..)
  , Streaming (Streaming, NonStreaming, ..)
  , DotProtoServicePart (..)
  , RPCMethod (..)
    -- * DotProtoMessagePart
  , DotProtoMessagePart (..)
    -- ** Basic Operations
  , pPrintMessagePart
  , DotProtoField (..)
  , DotProtoReservedField (..)
  ) where

import Control.Applicative

import Control.Monad
import Control.Monad.Reader (MonadReader (..), runReader)

import Data.Char qualified as Char
import Data.Data (Data)
import Data.Int (Int32)
import Data.List.NonEmpty qualified as NE
import Data.String (IsString (..))

import GHC.Generics (Generic)

import Numeric.Natural (Natural)

import Prelude hiding (FilePath)

import Proto3.Wire.Types (FieldNumber (..))

import Test.QuickCheck (Arbitrary (..), Gen)
import Test.QuickCheck qualified as QuickCheck
import Test.QuickCheck.Instances ()

import Turtle (FilePath)
import Turtle.Compat qualified as Turtle

import Text.PrettyPrint (($$), (<+>))
import Text.PrettyPrint qualified as PP
import Text.PrettyPrint.HughesPJClass (Pretty (..))

--------------------------------------------------------------------------------

-- | Render protobufs metadata as a .proto file stringy
toProtoFile :: RenderingOptions -> DotProto -> String
toProtoFile opts = PP.render . pPrintDotProto opts

-- | Render protobufs metadata as a .proto file string,
-- using the default rendering options.

toProtoFileDef :: DotProto -> String
toProtoFileDef = toProtoFile defRenderingOptions

pPrintFieldNumber :: FieldNumber -> PP.Doc 
pPrintFieldNumber = PP.text . show . getFieldNumber

-- instance Pretty FieldNumber where
--   pPrint = PP.text . show . getFieldNumber

strLit :: String -> PP.Doc
strLit string = PP.text "\"" <> foldMap escape string <> PP.text "\""
  where
    escape '\n' = PP.text "\\n"
    escape '\\' = PP.text "\\\\"
    escape '\0' = PP.text "\\x00"
    escape '"'  = PP.text "\\\""
    escape  c   = PP.text [ c ]

renderComment :: String -> PP.Doc
renderComment = PP.vcat . map ((PP.text "//" <+>) . PP.text) . lines

-- Put the final closing brace on the next line.
-- This is important, since the final field might have a comment, and
-- the brace cannot be part of the comment.
-- We could use block comments instead, once the parser/lexer supports them.
vbraces :: PP.Doc -> PP.Doc -> PP.Doc
vbraces header body = header <+> PP.char '{' $$ PP.nest 2 body $$ PP.char '}'

optionAnnotation :: [DotProtoOption] -> PP.Doc
optionAnnotation [] = PP.empty
optionAnnotation os = PP.brackets . PP.hcat . PP.punctuate (PP.text ", ") $ pPrint <$> os

prettyPrintProtoDefinition :: RenderingOptions -> DotProtoDefinition -> PP.Doc
prettyPrintProtoDefinition opts = flip runReader opts . pPrintDefinitionPart

-- RenderingOptions ------------------------------------------------------------

-- | Options for rendering a @.proto@ file.
data RenderingOptions = RenderingOptions
  { roSelectorName :: 
      Maybe DotProtoIdentifier -> 
      DotProtoIdentifier -> 
      FieldNumber -> 
      PP.Doc
  -- ^ This function will be applied to each record selector name to turn it 
  -- into a protobuf field name (default: uses the selector name, unchanged).
  , roEnumMemberName :: 
      Maybe DotProtoIdentifier -> 
      DotProtoIdentifier -> 
      PP.Doc
  -- ^ This function will be applied to each enum member name to turn it into a 
  -- protobuf field name (default: uses the field name, unchanged).
  }

-- RenderingOptions - Basic Operations -----------------------------------------

-- | The canonical default value for 'RenderingOptions'.
defRenderingOptions :: RenderingOptions
defRenderingOptions = RenderingOptions defSelectorName defEnumMemberName

-- | The default choice of field name for a selector.
defSelectorName :: Maybe DotProtoIdentifier -> DotProtoIdentifier -> FieldNumber -> PP.Doc
defSelectorName _ fieldName _ = pPrint fieldName

defEnumMemberName :: Maybe DotProtoIdentifier -> DotProtoIdentifier -> PP.Doc
defEnumMemberName _ = pPrint

-- MessageName -----------------------------------------------------------------

-- | The name of a message
newtype MessageName = MessageName
  { getMessageName :: String }
  deriving newtype (Eq, IsString, Ord, Show)
  deriving stock (Data, Generic)

-- FieldName -------------------------------------------------------------------

-- | The name of some field
newtype FieldName = FieldName
  { getFieldName :: String }
  deriving newtype (Eq, IsString, Ord, Show)
  deriving stock (Data, Generic)

-- PackageName -----------------------------------------------------------------

-- | The name of the package
newtype PackageName = PackageName
  { getPackageName :: String }
  deriving newtype (Eq, IsString, Ord, Show)
  deriving stock (Data, Generic)

-- Path ------------------------------------------------------------------------

newtype Path = Path
  { components :: NE.NonEmpty String }
  deriving (Data, Eq, Generic, Ord, Show)

-- Used for testing
fakePath :: Path
fakePath = Path ("fakePath" NE.:| [])

-- DotProtoIdentifier ----------------------------------------------------------

data DotProtoIdentifier 
  = Single String 
  | Dots Path 
  | Qualified DotProtoIdentifier DotProtoIdentifier
  | Anonymous
  deriving (Data, Eq, Generic, Ord, Show)

instance Pretty DotProtoIdentifier where
  pPrint (Single name) = PP.text name
  pPrint (Dots names) = mconcat . PP.punctuate (PP.text ".") $ PP.text <$> NE.toList (components names)
  pPrint (Qualified qualifier identifier) = PP.parens (pPrint qualifier) <> PP.text "." <> pPrint identifier
  pPrint Anonymous = PP.empty

-- DotProtoImport --------------------------------------------------------------

-- | Top-level import declaration
data DotProtoImport = DotProtoImport
  { dotProtoImportQualifier :: DotProtoImportQualifier
  , dotProtoImportPath      :: FilePath
  }
  deriving (Data, Eq, Generic, Ord, Show)

instance Arbitrary DotProtoImport where
  arbitrary = DotProtoImport <$> arbitrary <*> fmap fromString arbitrary

-- | @since 1.0.0
instance Pretty DotProtoImport where
  pPrint (DotProtoImport q i) = 
    PP.text "import" 
      <+> pPrint q
      <+> strLit (Turtle.encodeString i) 
      PP.<> PP.text ";"

-- DotProtoImportQualifier -----------------------------------------------------

data DotProtoImportQualifier
  = DotProtoImportPublic
  | DotProtoImportWeak
  | DotProtoImportDefault
  deriving stock (Bounded, Data, Enum, Eq, Generic, Ord, Show)

instance Arbitrary DotProtoImportQualifier where
  arbitrary = 
    QuickCheck.elements
      [ DotProtoImportDefault
      , DotProtoImportWeak
      , DotProtoImportPublic
      ]

-- | @since 1.0.0
instance Pretty DotProtoImportQualifier where
  pPrint DotProtoImportDefault = PP.empty
  pPrint DotProtoImportPublic = PP.text "public"
  pPrint DotProtoImportWeak = PP.text "weak"

-- DotProtoPackageSpec --------------------------------------------------------------

-- | The namespace declaration
newtype DotProtoPackageSpec = MkDotProtoPackageSpec
  { getDotProtoPackageSpec :: Maybe DotProtoIdentifier }
  deriving stock (Data, Generic, Show)
  deriving newtype (Eq, Ord)

pattern DotProtoPackageSpec :: DotProtoIdentifier -> DotProtoPackageSpec
pattern DotProtoPackageSpec x = MkDotProtoPackageSpec (Just x)

pattern DotProtoNoPackage :: DotProtoPackageSpec
pattern DotProtoNoPackage = MkDotProtoPackageSpec Nothing

{-# COMPLETE DotProtoPackageSpec, DotProtoNoPackage #-}

instance Arbitrary DotProtoPackageSpec where
  arbitrary = 
    QuickCheck.oneof 
      [ pure DotProtoNoPackage 
      , DotProtoPackageSpec <$> arbitraryIdentifier
      ]

-- @since 1.0.0
instance Pretty DotProtoPackageSpec where
  pPrint (DotProtoPackageSpec p) = PP.text "package" <+> pPrint p PP.<> PP.text ";"
  pPrint DotProtoNoPackage = PP.empty
  
-- DotProtoOption --------------------------------------------------------------

-- | An option id/value pair, can be attached to many types of statements
data DotProtoOption = DotProtoOption
  { dotProtoOptionIdentifier :: DotProtoIdentifier
  , dotProtoOptionValue      :: DotProtoValue
  } deriving (Data, Eq, Generic, Ord, Show)

instance Arbitrary DotProtoOption where
  arbitrary = DotProtoOption <$> arbitraryIdentifier <*> arbitrary

-- | @since 1.0.0
instance Pretty DotProtoOption where
  pPrint (DotProtoOption key value) = pPrint key <+> PP.text "=" <+> pPrint value

-- DotProtoDefinition ----------------------------------------------------------

-- | Top-level protocol definitions
data DotProtoDefinition
  = DotProtoMessage String DotProtoIdentifier [DotProtoMessagePart]
  | DotProtoEnum    String DotProtoIdentifier [DotProtoEnumPart]
  | DotProtoService String DotProtoIdentifier [DotProtoServicePart]
  deriving (Data, Eq, Generic, Ord, Show)

instance Arbitrary DotProtoDefinition where
  arbitrary = 
    QuickCheck.oneof 
      [ DotProtoMessage mempty 
          <$> arbitrarySingleIdentifier 
          <*> smallListOf arbitrary
      , DotProtoEnum mempty 
          <$> arbitrarySingleIdentifier
          <*> smallListOf arbitrary
      ]

-- | @since 1.0.0
instance Pretty DotProtoDefinition where
  pPrint = flip runReader defRenderingOptions . pPrintDefinitionPart

-- DotProtoDefinition - Basic Operations ---------------------------------------

-- | Render a 'DotProtoDefinition' as a 'PP.Doc' 
pPrintDefinitionPart :: 
  MonadReader RenderingOptions m => 
  DotProtoDefinition -> 
  m PP.Doc
pPrintDefinitionPart (DotProtoMessage comment name parts) = do 
  docParts <- traverse (pPrintMessagePart (Just name)) parts
  pure (renderComment comment $$ vbraces (PP.text "message" <+> pPrint name) (PP.vcat docParts))
pPrintDefinitionPart (DotProtoEnum comment name parts) = do 
  docParts <- traverse (pPrintEnumPart (Just name)) parts
  pure (renderComment comment $$ vbraces (PP.text "enum" <+> pPrint name) (PP.vcat docParts))
pPrintDefinitionPart (DotProtoService comment name parts) = 
  pure (renderComment comment $$ vbraces (PP.text "service" <+> pPrint name) (PP.vcat $ pPrint <$> parts))

-- DotProtoMeta ----------------------------------------------------------------

-- | Tracks misc metadata about the AST
newtype DotProtoMeta = DotProtoMeta
  { metaModulePath :: Path
    -- ^ The "module path" associated with the .proto file from which this AST
    -- was parsed. The "module path" is derived from the `--includeDir`-relative
    -- .proto filename passed to `Proto3.Suite.DotProto.Parsing.parseProtoFile`. See
    -- 'Proto3.Suite.DotProto.Internal.toModulePath' for details on how module
    -- path values are constructed. See
    -- 'Proto3.Suite.DotProto.Generate.modulePathModName' to see how it is used
    -- during code generation.
  } 
  deriving newtype (Eq, Ord, Show)
  deriving stock (Data, Generic)

instance Arbitrary DotProtoMeta where
  arbitrary = pure (DotProtoMeta fakePath)

-- DotProto --------------------------------------------------------------------

-- | This data structure represents a .proto file
--   The actual source order of protobuf statements isn't meaningful so
--   statements are sorted by type during parsing.
--   A .proto file with more than one package declaration is considered invalid.
data DotProto = DotProto
  { protoImports :: [DotProtoImport]
  , protoOptions :: [DotProtoOption]
  , protoPackage :: DotProtoPackageSpec
  , protoDefinitions :: [DotProtoDefinition]
  , protoMeta :: DotProtoMeta
  } 
  deriving (Data, Eq, Generic, Ord, Show)

instance Arbitrary DotProto where
  arbitrary =
    DotProto
      <$> smallListOf arbitrary
      <*> smallListOf arbitrary
      <*> arbitrary
      <*> smallListOf arbitrary
      <*> arbitrary

-- | @since 1.0.0
instance Pretty DotProto where 
  pPrint = pPrintDotProto defRenderingOptions

-- DotProtoMeta - Basic Operations ---------------------------------------------

-- | Traverses a DotProto AST and generates a .proto file from it
pPrintDotProto :: RenderingOptions -> DotProto -> PP.Doc
pPrintDotProto opts DotProto{..} = 
  PP.text "syntax = \"proto3\";"
    $$ pPrint protoPackage
    $$ (PP.vcat $ pPrint <$> protoImports)
    $$ (PP.vcat $ topOption <$> protoOptions)
    $$ (PP.vcat $ prettyPrintProtoDefinition opts <$> protoDefinitions)

-- DotProtoValue ---------------------------------------------------------------

-- | Matches the definition of @constant@ in the proto3 language spec
-- These are only used as rvalues
data DotProtoValue
  = Identifier DotProtoIdentifier
  | StringLit String
  | IntLit {-# UNPACK #-} !Int
  | FloatLit {-# UNPACK #-} !Double
  | BoolLit Bool
  deriving stock (Data, Eq, Generic, Ord, Show)

instance Arbitrary DotProtoValue where
  arbitrary = 
    QuickCheck.oneof
      [ fmap Identifier  arbitrarySingleIdentifier
      , fmap StringLit  (return "")
      , fmap IntLit      arbitrary
      , fmap FloatLit    arbitrary
      , fmap BoolLit     arbitrary
      ]

-- | @since 1.0.0
instance Pretty DotProtoValue where
  pPrint (Identifier value) = pPrint value
  pPrint (StringLit value) = strLit value
  pPrint (IntLit value) = PP.text $ show value
  pPrint (FloatLit value) = PP.text $ show value
  pPrint (BoolLit value) = PP.text $ Char.toLower <$> show value

-- DotProtoPrimType ------------------------------------------------------------

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
  deriving (Data, Eq, Generic, Ord, Show)

instance Arbitrary DotProtoPrimType where
  arbitrary = QuickCheck.oneof
    [ QuickCheck.elements
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

-- Packing ---------------------------------------------------------------------

newtype Packing = Packing 
  { getPacking :: Bool }
  deriving stock (Data, Generic)
  deriving newtype (Bounded, Enum, Eq, Ord)

pattern PackedField :: Packing
pattern PackedField = Packing True

pattern UnpackedField :: Packing
pattern UnpackedField = Packing False

{-# COMPLETE PackedField, UnpackedField #-}

instance Arbitrary Packing where
  arbitrary = QuickCheck.elements [PackedField, UnpackedField]

instance Show Packing where
  show PackedField = "PackedField"
  show UnpackedField = "UnpackedField"

-- DotProtoType ----------------------------------------------------------------

-- | This type is an almagamation of the modifiers used in types.
-- It corresponds to a syntax role but not a semantic role, not all modifiers
-- are meaningful in every type context.
data DotProtoType
  = Prim           DotProtoPrimType
  | Repeated       DotProtoPrimType
  | NestedRepeated DotProtoPrimType
  | Map            DotProtoPrimType DotProtoPrimType
  deriving (Data, Eq, Generic, Ord, Show)

instance Arbitrary DotProtoType where
  arbitrary = QuickCheck.oneof [fmap Prim arbitrary]

-- @since 1.0.0
instance Pretty DotProtoType where
  pPrint (Prim           ty) = pPrint ty
  pPrint (Repeated       ty) = PP.text "repeated" <+> pPrint ty
  pPrint (NestedRepeated ty) = PP.text "repeated" <+> pPrint ty
  pPrint (Map keyty valuety) = PP.text "map<" <> pPrint keyty <> PP.text ", " <> pPrint valuety <> PP.text ">"

-- DotProtoEnumPart ------------------------------------------------------------

type DotProtoEnumValue = Int32

data DotProtoEnumPart
  = DotProtoEnumField DotProtoIdentifier DotProtoEnumValue [DotProtoOption]
  | DotProtoEnumOption DotProtoOption
  | DotProtoEnumReserved   [DotProtoReservedField]
  deriving (Data, Eq, Generic, Ord, Show)

instance Arbitrary DotProtoEnumPart where
  arbitrary = 
    QuickCheck.oneof 
      [ DotProtoEnumField 
          <$> arbitraryIdentifier
          <*> arbitrary
          <*> arbitrary
      , DotProtoEnumOption <$> arbitrary
      ]

instance Pretty DotProtoEnumPart where 
  pPrint = flip runReader defRenderingOptions . pPrintEnumPart Nothing 

-- DotProtoEnumPart - Basic Operations -----------------------------------------

pPrintEnumPart :: 
  MonadReader RenderingOptions m => 
  Maybe DotProtoIdentifier -> 
  DotProtoEnumPart -> 
  m PP.Doc
pPrintEnumPart msgName = \case 
  DotProtoEnumField name value options -> 
    reader \opts ->
      roEnumMemberName opts msgName name
        <+> PP.text "="
        <+> pPrint (fromIntegral value :: Int)
        <+> optionAnnotation options
        PP.<> PP.text ";"
  DotProtoEnumReserved reservedFields -> 
    pure (PP.text "reserved" <+> (PP.hcat . PP.punctuate (PP.text ", ") $ pPrint <$> reservedFields))
  DotProtoEnumOption opt ->
    pure (PP.text "option" <+> pPrint opt PP.<> PP.text ";")


-- Streaming -------------------------------------------------------------------

newtype Streaming = MkStreaming 
  { getStreaming :: Bool }
  deriving stock (Data, Generic)
  deriving newtype (Bounded, Enum, Eq, Ord, Show)

pattern Streaming :: Streaming
pattern Streaming = MkStreaming True

pattern NonStreaming :: Streaming
pattern NonStreaming = MkStreaming False

{-# COMPLETE Streaming, NonStreaming #-}

instance Arbitrary Streaming where
  arbitrary = QuickCheck.elements [Streaming, NonStreaming]

-- | @since 1.0.0
instance Pretty Streaming where
  pPrint Streaming    = PP.text "stream"
  pPrint NonStreaming = PP.empty

-- DotProtoServicePart ---------------------------------------------------------

data DotProtoServicePart
  = DotProtoServiceRPCMethod RPCMethod
  | DotProtoServiceOption DotProtoOption
  deriving (Data, Eq, Generic, Ord, Show)

instance Arbitrary DotProtoServicePart where
  arbitrary = QuickCheck.oneof
    [ DotProtoServiceRPCMethod <$> arbitrary
    , DotProtoServiceOption <$> arbitrary
    ]

instance Pretty DotProtoServicePart where
  pPrint (DotProtoServiceRPCMethod RPCMethod{..}) =
    PP.text "rpc"
      <+> pPrint rpcMethodName
      <+> PP.parens (pPrint rpcMethodRequestStreaming <+> pPrint rpcMethodRequestType)
      <+> PP.text "returns"
      <+> PP.parens (pPrint rpcMethodResponseStreaming <+> pPrint rpcMethodResponseType)
      <+> case rpcMethodOptions of
          [] -> PP.text ";"
          _  -> PP.braces . PP.vcat $ topOption <$> rpcMethodOptions
  pPrint (DotProtoServiceOption option) = topOption option

topOption :: DotProtoOption -> PP.Doc
topOption o = PP.text "option" <+> pPrint o PP.<> PP.text ";"

-- RPCMethod -------------------------------------------------------------------

data RPCMethod = RPCMethod
  { rpcMethodName :: DotProtoIdentifier
  , rpcMethodRequestType :: DotProtoIdentifier
  , rpcMethodRequestStreaming :: Streaming
  , rpcMethodResponseType :: DotProtoIdentifier
  , rpcMethodResponseStreaming :: Streaming
  , rpcMethodOptions :: [DotProtoOption]
  }
  deriving (Data, Eq, Generic, Ord, Show)

instance Arbitrary RPCMethod where
  arbitrary = 
    RPCMethod
      <$> arbitrarySingleIdentifier
      <*> arbitraryIdentifier
      <*> arbitrary
      <*> arbitraryIdentifier
      <*> arbitrary
      <*> smallListOf arbitrary

-- DotProtoMessagePart ---------------------------------------------------------

data DotProtoMessagePart
  = DotProtoMessageField DotProtoField
  | DotProtoMessageOneOf DotProtoIdentifier [DotProtoField]
  | DotProtoMessageDefinition DotProtoDefinition
  | DotProtoMessageReserved   [DotProtoReservedField]
  | DotProtoMessageOption DotProtoOption
  deriving (Data, Eq, Generic, Ord, Show)

instance Arbitrary DotProtoMessagePart where
  arbitrary = QuickCheck.oneof
    [ DotProtoMessageField <$> arbitrary
    , DotProtoMessageOneOf 
        <$> arbitrarySingleIdentifier
        <*> smallListOf arbitrary
    , DotProtoMessageDefinition <$> arbitrary
    , DotProtoMessageReserved 
        <$> QuickCheck.oneof
              [ smallListOf1 arbitrary
              , smallListOf1 (ReservedIdentifier <$> arbitraryIdentifierName)
              ]
    ]

-- DotProtoMessagePart -- Basic Operations -------------------------------------

pPrintMessagePart :: 
  MonadReader RenderingOptions m => 
  Maybe DotProtoIdentifier -> 
  DotProtoMessagePart -> 
  m PP.Doc
pPrintMessagePart msgName = \case 
  DotProtoMessageField f -> 
    pPrintFieldPart msgName f
  DotProtoMessageDefinition definition ->
    pPrintDefinitionPart definition
  DotProtoMessageReserved reservations ->
    let docRanges :: PP.Doc 
        docRanges = PP.hcat . PP.punctuate (PP.text ", ") $ pPrint <$> reservations
     in pure (PP.text "reserved" <+> docRanges PP.<> PP.text ";")
  DotProtoMessageOneOf name fields -> do 
    docParts <- traverse (pPrintFieldPart msgName) fields
    pure (vbraces (PP.text "oneof" <+> pPrint name) (PP.vcat docParts))
  DotProtoMessageOption opt ->
    pure (PP.text "option" <+> pPrint opt PP.<> PP.text ";")

-- DotProtoField ---------------------------------------------------------------

data DotProtoField = DotProtoField
  { dotProtoFieldNumber  :: FieldNumber
  , dotProtoFieldType    :: DotProtoType
  , dotProtoFieldName    :: DotProtoIdentifier
  , dotProtoFieldOptions :: [DotProtoOption]
  , dotProtoFieldComment :: String
  }
  deriving (Data, Eq, Generic, Ord, Show)

instance Arbitrary DotProtoField where
  arbitrary = 
    DotProtoField
      <$> arbitrary
      <*> arbitrary
      <*> arbitraryIdentifier
      <*> smallListOf arbitrary
      -- TODO: Generate random comments once the parser supports comments
      <*> pure mempty

-- | @since 1.0.0
instance Pretty DotProtoField where 
  pPrint = flip runReader defRenderingOptions . pPrintFieldPart Nothing 
    
-- DotProtoField - Basic Operations --------------------------------------------

pPrintFieldPart :: 
  MonadReader RenderingOptions m => 
  Maybe DotProtoIdentifier ->
  DotProtoField -> 
  m PP.Doc 
pPrintFieldPart msgName (DotProtoField number mtype name options comments) = 
  reader \opts ->
    pPrint mtype
      <+> roSelectorName opts msgName name number
      <+> PP.text "="
      <+> pPrintFieldNumber number
      <+> optionAnnotation options
      PP.<> PP.text ";"
      $$  PP.nest 2 (renderComment comments)

-- DotProtoReservedField -------------------------------------------------------

data DotProtoReservedField
  = SingleField Int
  | FieldRange  Int Int
  | ReservedIdentifier String
  deriving (Data, Eq, Generic, Ord, Show)

instance Arbitrary DotProtoReservedField where
  arbitrary =
    QuickCheck.oneof 
      [ SingleField <$> arbitraryFieldNumber
      , FieldRange <$> arbitraryFieldNumber <*> arbitraryFieldNumber
      ]
    where
      arbitraryFieldNumber = do
        natural <- arbitrary
        return (fromIntegral (natural :: Natural))

-- | @since 1.0.0
instance Pretty DotProtoReservedField where
  pPrint (SingleField num)      = PP.text $ show num
  pPrint (FieldRange start end) = (PP.text $ show start) <+> PP.text "to" <+> (PP.text $ show end)
  pPrint (ReservedIdentifier i) = PP.text $ show i

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
  c  <- QuickCheck.elements (['a'..'z'] ++ ['A'..'Z'])
  cs <- smallListOf (QuickCheck.elements (['a'..'z'] ++ ['A'..'Z'] ++ ['_']))
  return (c:cs)

arbitrarySingleIdentifier :: Gen DotProtoIdentifier
arbitrarySingleIdentifier = fmap Single arbitraryIdentifierName

arbitraryPathIdentifier :: Gen DotProtoIdentifier
arbitraryPathIdentifier = do
  name  <- arbitraryIdentifierName
  names <- smallListOf1 arbitraryIdentifierName
  pure . Dots . Path $ name NE.:| names



-- these two kinds of identifiers are usually interchangeable, the others are not
arbitraryIdentifier :: Gen DotProtoIdentifier
arbitraryIdentifier = QuickCheck.oneof [arbitrarySingleIdentifier, arbitraryPathIdentifier]

-- [note] quickcheck's default scaling generates *extremely* large asts past 20 iterations
--        the parser is not particularly slow but it does have noticeable delay on megabyte-large .proto files
smallListOf :: Gen a -> Gen [a]
smallListOf x = QuickCheck.choose (0, 5) >>= \n -> QuickCheck.vectorOf n x

smallListOf1 :: Gen a -> Gen [a]
smallListOf1 x = QuickCheck.choose (1, 5) >>= \n -> QuickCheck.vectorOf n x
