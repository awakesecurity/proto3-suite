-- | This module provides types and functions to generate .proto files.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

module Data.Protobuf.Wire.DotProto.Rendering
  ( renderDotProto
  , defRenderingOptions
  , defSelectorName
  , packageFromDefs
  , toProtoFile
  , toProtoFileDef
  , RenderingOptions(..)
  ) where

import           Data.Char
import           Text.PrettyPrint                (($$), (<+>), (<>))
import qualified Text.PrettyPrint                as PP
import           Text.PrettyPrint.HughesPJClass  (Pretty(..))

import           Data.Protobuf.Wire.DotProto.AST
import           Data.Protobuf.Wire.Types        (FieldNumber (..))

-- | Options for rendering a @.proto@ file.
data RenderingOptions = RenderingOptions
  { roSelectorName   :: MessageName -> Maybe FieldName -> FieldNumber -> String
  -- ^ This function will be applied to each
  -- record selector name to turn it into a protobuf
  -- field name (default: uses the selector name, unchanged).
  , roEnumMemberName :: MessageName -> FieldName -> String
  -- ^ This function will be applied to each
  -- enum member name to turn it into a protobuf
  -- field name (default: uses the field name, unchanged).
  }

-- | Default rendering options.
defRenderingOptions :: RenderingOptions
defRenderingOptions =
    RenderingOptions { roSelectorName = defSelectorName
                     , roEnumMemberName = const getFieldName
                     }

-- | The default choice of field name for a selector.
defSelectorName :: MessageName -> Maybe FieldName -> FieldNumber -> String
defSelectorName msgName fieldName fieldNum =
  maybe (getMessageName msgName ++ "_" ++ show (getFieldNumber fieldNum))
        getFieldName
        fieldName

-- | Traverses a DotProto AST and generates a .proto file from it
renderDotProto :: RenderingOptions -> DotProto -> PP.Doc
renderDotProto RenderingOptions{..} DotProto{..}
  = PP.text "syntax = \"proto3\";"
 $$ pPrint protoPackage
 $$ (PP.vcat $ pPrint    <$> protoImports)
 $$ (PP.vcat $ topOption <$> protoOptions)
 $$ (PP.vcat $ pPrint    <$> protoDefinitions)

instance Pretty DotProtoPackageSpec where
  pPrint (DotProtoPackageSpec p) = PP.text "package" <+> pPrint p <> PP.text ";"
  pPrint (DotProtoNoPackage)     = PP.empty

instance Pretty DotProtoImport where
  pPrint (DotProtoImport q i) = PP.text "import" <+> pPrint q <+> pPrint i <> PP.text ";"

instance Pretty DotProtoImportQualifier where
  pPrint DotProtoImportDefault = PP.empty
  pPrint DotProtoImportPublic  = PP.text "public"
  pPrint DotProtoImportWeak    = PP.text "weak"

optionAnnotation :: [DotProtoOption] -> PP.Doc
optionAnnotation [] = PP.empty
optionAnnotation os = PP.brackets
                    . PP.hcat
                    . PP.punctuate (PP.text ", ")
                    $ pPrint <$> os

topOption :: DotProtoOption -> PP.Doc
topOption o = PP.text "option" <+> pPrint o <> PP.text ";"

instance Pretty DotProtoOption where
  pPrint (DotProtoOption key value) = pPrint key <+> PP.text "=" <+> pPrint value

-- [issue] there's a pPrint instance for Maybe and it's sneaking in here
instance Pretty DotProtoDefinition where
  pPrint (DotProtoMessage name parts) = PP.text "message" <+> pPrint name <+> (PP.braces $ PP.vcat $ pPrint <$> parts)
  pPrint (DotProtoEnum    name parts) = PP.text "enum"    <+> pPrint name <+> (PP.braces $ PP.vcat $ pPrint <$> parts)
  pPrint (DotProtoService name parts) = PP.text "service" <+> pPrint name <+> (PP.braces $ PP.vcat $ pPrint <$> parts)
  pPrint DotProtoNullDef              = PP.empty

instance Pretty DotProtoMessagePart where
  pPrint (DotProtoMessageField field)           = pPrint field
  pPrint (DotProtoMessageDefinition definition) = pPrint definition
  pPrint (DotProtoMessageReserved reservations)
    =   PP.text "reserved"
    <+> (PP.hcat . PP.punctuate (PP.text ", ") $ pPrint <$> reservations)
    <>  PP.text ";"
  pPrint (DotProtoMessageOneOf name fields)     = PP.text "oneof" <+> pPrint name <+> (PP.braces $ PP.vcat $ pPrint <$> fields)

instance Pretty DotProtoField where
  pPrint (DotProtoField number mtype name options)
    =   pPrint mtype
    <+> pPrint name
    <+> PP.text "="
    <+> pPrint number
    <+> optionAnnotation options
    <>  PP.text ";"
  pPrint DotProtoEmptyField                        = PP.empty

instance Pretty DotProtoEnumPart where
  pPrint (DotProtoEnumField name value) = pPrint name <+> PP.text "=" <+> pPrint value <> PP.text ";"
  pPrint (DotProtoEnumOption opt)       = PP.text "option" <+> pPrint opt <> PP.text ";"
  pPrint DotProtoEnumEmpty              = PP.empty

instance Pretty DotProtoServicePart where
  pPrint (DotProtoServiceRPC name (callname, callstrm) (retname, retstrm) options)
    =   PP.text "rpc"
    <+> pPrint name
    <+> PP.parens (pPrint callstrm <+> pPrint callname)
    <+> PP.text "returns"
    <+> PP.parens (pPrint retstrm <+> pPrint retname)
    <+> case options of
          [] -> PP.text ";"
          _  -> PP.braces . PP.vcat $ topOption <$> options
  pPrint (DotProtoServiceOption option) = topOption option
  pPrint DotProtoServiceEmpty           = PP.empty

instance Pretty Streaming where
  pPrint Streaming    = PP.text "stream"
  pPrint NonStreaming = PP.empty

instance Pretty DotProtoIdentifier where
  pPrint (Single name)                    = PP.text name
  pPrint (Path names)                     = PP.hcat . PP.punctuate (PP.text ".") $ PP.text <$> names
  pPrint (Qualified qualifier identifier) = PP.parens (pPrint qualifier) <> PP.text "." <> pPrint identifier
  pPrint Anonymous                        = PP.empty

instance Pretty DotProtoValue where
  pPrint (Identifier value) = pPrint value
  pPrint (StringLit  value) = PP.text $ show value
  pPrint (IntLit     value) = PP.text $ show value
  pPrint (FloatLit   value) = PP.text $ show value
  pPrint (BoolLit    value) = PP.text $ toLower <$> show value

instance Pretty DotProtoType where
  pPrint (Prim           ty) = pPrint ty
  pPrint (Optional       ty) = pPrint ty
  pPrint (Repeated       ty) = PP.text "repeated" <+> pPrint ty
  pPrint (NestedRepeated ty) = pPrint ty
  pPrint (Map keyty valuety) = PP.text "<" <> pPrint keyty <> PP.text ", " <> pPrint valuety <> PP.text ">"

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

instance Pretty FieldNumber where
  pPrint = PP.text . show . getFieldNumber

instance Pretty DotProtoReservedField where
  pPrint (SingleField num)      = PP.text $ show num
  pPrint (FieldRange start end) = (PP.text $ show start) <+> PP.text "to" <+> (PP.text $ show end)
  pPrint (ReservedIdentifier i) = PP.text $ show i

-- | Render protobufs metadata as a .proto file stringy
toProtoFile :: RenderingOptions -> DotProto -> String
toProtoFile opts = PP.render . renderDotProto opts

-- | Render protobufs metadata as a .proto file string,
-- using the default rendering options.
toProtoFileDef :: DotProto -> String
toProtoFileDef = toProtoFile defRenderingOptions

packageFromDefs :: String -> [DotProtoDefinition] -> DotProto
packageFromDefs package defs = DotProto [] [] (DotProtoPackageSpec $ Single package) defs
