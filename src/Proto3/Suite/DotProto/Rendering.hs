-- | This module provides types and functions to generate .proto files.

{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

module Proto3.Suite.DotProto.Rendering
  ( renderDotProto
  , defRenderingOptions
  , defSelectorName
  , defEnumMemberName
  , javaPackageOption
  , goPackageOption
  , packageFromDefs
  , packageFromDefsWithOptions
  , toProtoFile
  , toProtoFileDef
  , RenderingOptions(..)
  ) where

import           Data.Char
import qualified Data.Text                 as T
import           Filesystem.Path.CurrentOS (toText)
import           Proto3.Suite.DotProto.AST
import           Proto3.Wire.Types         (FieldNumber (..))
import           Text.PrettyPrint.Leijen   ((<$$>), (<+>), (<>), Pretty(..))
import qualified Text.PrettyPrint.Leijen   as PP

-- | Options for rendering a @.proto@ file.
data RenderingOptions = RenderingOptions
  { roSelectorName   :: DotProtoIdentifier -> DotProtoIdentifier -> FieldNumber -> PP.Doc
  -- ^ This function will be applied to each
  -- record selector name to turn it into a protobuf
  -- field name (default: uses the selector name, unchanged).
  , roEnumMemberName :: DotProtoIdentifier -> DotProtoIdentifier -> PP.Doc
  -- ^ This function will be applied to each
  -- enum member name to turn it into a protobuf
  -- field name (default: uses the field name, unchanged).
  }

-- | Default rendering options.
defRenderingOptions :: RenderingOptions
defRenderingOptions =
    RenderingOptions { roSelectorName   = defSelectorName
                     , roEnumMemberName = defEnumMemberName
                     }

-- | The default choice of field name for a selector.
defSelectorName :: DotProtoIdentifier -> DotProtoIdentifier -> FieldNumber -> PP.Doc
defSelectorName _ fieldName _ = pretty fieldName

-- | The default choice of enum member name for an enum
defEnumMemberName :: DotProtoIdentifier -> DotProtoIdentifier -> PP.Doc
defEnumMemberName = const pretty

-- | Traverses a DotProto AST and generates a .proto file from it
renderDotProto :: RenderingOptions -> DotProto -> PP.Doc
renderDotProto opts DotProto{..}
  = PP.text "syntax = \"proto3\";" <> PP.linebreak
 <$$> pretty protoPackage <> PP.linebreak
 <>   pList pretty protoImports
 <>   pList topOption protoOptions
 <$$> (PP.vcat . PP.punctuate PP.linebreak $ (prettyPrintProtoDefinition opts) <$> protoDefinitions)
 <$$> PP.linebreak
 where pList _ [] = PP.empty
       pList f xs =  PP.linebreak
                  <> PP.vcat (f <$> xs)
                  <> PP.linebreak

instance Pretty DotProtoPackageSpec where
  pretty (DotProtoPackageSpec p) = PP.text "package" <+> pretty p <> PP.semi
  pretty (DotProtoNoPackage)     = PP.empty

instance Pretty DotProtoImport where
  pretty (DotProtoImport q i) = pretty q <+> PP.dquotes (PP.text fp) <> PP.semi
    where
      fp = case T.unpack . either id id . toText $ i of
             [] -> show ("" :: String)
             x  -> x

instance Pretty DotProtoImportQualifier where
  pretty DotProtoImportDefault = PP.text "import"
  pretty DotProtoImportPublic  = PP.text "import public"
  pretty DotProtoImportWeak    = PP.text "import weak"

optionAnnotation :: [DotProtoOption] -> PP.Doc
optionAnnotation [] = PP.empty
optionAnnotation os = (<> PP.space)
                    . PP.brackets
                    . PP.hcat
                    . PP.punctuate (PP.text ", ")
                    $ pretty <$> os

topOption :: DotProtoOption -> PP.Doc
topOption o = PP.text "option" <+> pretty o <> PP.semi

instance Pretty DotProtoOption where
  pretty (DotProtoOption key value) = pretty key <+> PP.text "=" <+> pretty value

prettyPrintProtoDefinition :: RenderingOptions -> DotProtoDefinition -> PP.Doc
prettyPrintProtoDefinition opts = defn where
  defn :: DotProtoDefinition -> PP.Doc
  defn (DotProtoMessage name parts) = PP.text "message" <+> pretty name <+> vcat (msgPart name <$> parts)
  defn (DotProtoEnum    name parts) = PP.text "enum"    <+> pretty name <+> vcat (enumPart name <$> parts)
  defn (DotProtoService name parts) = PP.text "service" <+> pretty name <+> vcat (pretty <$> parts)

  -- Vertical concat with braces that appropriately handles empty list (prints
  -- `{ }` on one line) and handles nesting and trailing newlines. Also puts the
  -- final closing brace on the next line since the final field might have a
  -- comment, and the brace cannot be part of the comment. We could use block
  -- comments instead, once the parser/lexer supports them.
  vcat :: [PP.Doc] -> PP.Doc
  vcat [] = PP.braces (PP.char ' ')
  vcat xs =    PP.char '{'
          <$$> PP.indent 2 (PP.vcat xs)
          <$$> PP.char '}'

  msgPart :: DotProtoIdentifier -> DotProtoMessagePart -> PP.Doc
  msgPart msgName (DotProtoMessageField f)               = field msgName f
  msgPart _       (DotProtoMessageDefinition definition) = defn definition
  msgPart _       (DotProtoMessageReserved reservations)
    =   PP.text "reserved"
    <+> (PP.hcat . PP.punctuate (PP.text ", ") $ pretty <$> reservations)
    <>  PP.semi
  msgPart msgName (DotProtoMessageOneOf name fields)     = PP.text "oneof" <+> pretty name <+> vcat (field msgName <$> fields)

  field :: DotProtoIdentifier -> DotProtoField -> PP.Doc
  field msgName (DotProtoField number mtype name options comments)
    =   pretty mtype
    <+> roSelectorName opts msgName name number
    <+> PP.text "="
    <+> pretty number
    <>  optionAnnotation options
    <>  PP.semi
    <>  maybe PP.empty (PP.text . (" // " ++)) comments
  field _ DotProtoEmptyField = PP.empty

  enumPart :: DotProtoIdentifier -> DotProtoEnumPart -> PP.Doc
  enumPart msgName (DotProtoEnumField name value options)
    = roEnumMemberName opts msgName name
    <+> PP.text "="
    <+> pretty value
    <> optionAnnotation options
    <> PP.semi
  enumPart _       (DotProtoEnumOption opt)
    = PP.text "option" <+> pretty opt <> PP.semi
  enumPart _       DotProtoEnumEmpty
    = PP.empty

instance Pretty DotProtoServicePart where
  pretty (DotProtoServiceRPC name (callname, callstrm) (retname, retstrm) options)
    =   PP.text "rpc"
    <+> pretty name
    <+> PP.parens (pretty callstrm <+> pretty callname)
    <+> PP.text "returns"
    <+> PP.parens (pretty retstrm <+> pretty retname)
    <>  case options of
          [] -> PP.semi
          _  -> PP.space <> (PP.braces . PP.vcat $ topOption <$> options)
  pretty (DotProtoServiceOption option) = topOption option
  pretty DotProtoServiceEmpty           = PP.empty

instance Pretty Streaming where
  pretty Streaming    = PP.text "stream"
  pretty NonStreaming = PP.empty

instance Pretty DotProtoIdentifier where
  pretty (Single name)                    = PP.text name
  pretty (Dots (Path names))              = PP.hcat . PP.punctuate PP.dot $ PP.text <$> names
  pretty (Qualified qualifier identifier) = PP.parens (pretty qualifier) <> PP.dot <> pretty identifier
  pretty Anonymous                        = PP.empty

instance Pretty DotProtoValue where
  pretty (Identifier value) = pretty value
  pretty (StringLit  value) = PP.text $ show value
  pretty (IntLit     value) = PP.text $ show value
  pretty (FloatLit   value) = PP.text $ show value
  pretty (BoolLit    value) = PP.text $ toLower <$> show value

instance Pretty DotProtoType where
  pretty (Prim           ty) = pretty ty
  pretty (Optional       ty) = pretty ty
  pretty (Repeated       ty) = PP.text "repeated" <+> pretty ty
  pretty (NestedRepeated ty) = PP.text "repeated" <+> pretty ty
  pretty (Map keyty valuety) = PP.text "map<" <> pretty keyty <> PP.text ", " <> pretty valuety <> PP.text ">"

instance Pretty DotProtoPrimType where
  pretty (Named i)  = pretty i
  pretty Int32      = PP.text "int32"
  pretty Int64      = PP.text "int64"
  pretty SInt32     = PP.text "sint32"
  pretty SInt64     = PP.text "sint64"
  pretty UInt32     = PP.text "uint32"
  pretty UInt64     = PP.text "uint64"
  pretty Fixed32    = PP.text "fixed32"
  pretty Fixed64    = PP.text "fixed64"
  pretty SFixed32   = PP.text "sfixed32"
  pretty SFixed64   = PP.text "sfixed64"
  pretty String     = PP.text "string"
  pretty Bytes      = PP.text "bytes"
  pretty Bool       = PP.text "bool"
  pretty Float      = PP.text "float"
  pretty Double     = PP.text "double"

instance Pretty FieldNumber where
  pretty = PP.text . show . getFieldNumber

instance Pretty DotProtoReservedField where
  pretty (SingleField num)      = PP.text $ show num
  pretty (FieldRange start end) = (PP.text $ show start) <+> PP.text "to" <+> (PP.text $ show end)
  pretty (ReservedIdentifier i) = PP.text $ show i

-- | Render protobufs metadata as a .proto file stringy
toProtoFile :: RenderingOptions -> DotProto -> String
toProtoFile opts d = PP.displayS ((PP.renderPretty 0.4 100) (renderDotProto opts d)) ""

-- | Render protobufs metadata as a .proto file string,
-- using the default rendering options.

toProtoFileDef :: DotProto -> String
toProtoFileDef = toProtoFile defRenderingOptions

packageFromDefs :: String -> [DotProtoDefinition] -> DotProto
packageFromDefs package = packageFromDefsWithOptions package mempty []

packageFromDefsWithOptions :: String -> [DotProtoImport] -> [DotProtoOption] -> [DotProtoDefinition] -> DotProto
packageFromDefsWithOptions package imports opts defs =
  DotProto imports opts (DotProtoPackageSpec $ Single package) defs (DotProtoMeta $ Path [])

javaPackageOption :: String -> DotProtoOption
javaPackageOption p = DotProtoOption (Single "java_package") (StringLit p)

goPackageOption :: String -> DotProtoOption
goPackageOption p = DotProtoOption (Single "go_package") (StringLit p)
