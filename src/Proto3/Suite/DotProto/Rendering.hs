-- | This module provides types and functions to generate .proto files.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Proto3.Suite.DotProto.Rendering
  ( renderDotProto
  , defRenderingOptions
  , defSelectorName
  , defEnumMemberName
  , packageFromDefs
  , toProtoFile
  , toProtoFileDef
  , RenderingOptions(..)
  , Pretty(..)
  ) where

import           Proto3.Suite.DotProto.AST
import           Proto3.Wire.Types               (FieldNumber (..))
import           Text.PrettyPrint                (($$), (<+>))
import qualified Text.PrettyPrint                as PP
import           Text.PrettyPrint.HughesPJClass  (Pretty(..))

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
defSelectorName _ fieldName _ = pPrint fieldName

-- | The default choice of enum member name for an enum
defEnumMemberName :: DotProtoIdentifier -> DotProtoIdentifier -> PP.Doc
defEnumMemberName = const pPrint

-- | Traverses a DotProto AST and generates a .proto file from it
renderDotProto :: RenderingOptions -> DotProto -> PP.Doc
renderDotProto opts DotProto{..}
  = PP.text "syntax = \"proto3\";"
 $$ pPrint protoPackage
 $$ (PP.vcat $ pPrint    <$> protoImports)
 $$ (PP.vcat $ topOption <$> protoOptions)
 $$ (PP.vcat $ prettyPrintProtoDefinition opts <$> protoDefinitions)

optionAnnotation :: [DotProtoOption] -> PP.Doc
optionAnnotation [] = PP.empty
optionAnnotation os = PP.brackets
                    . PP.hcat
                    . PP.punctuate (PP.text ", ")
                    $ pPrint <$> os

topOption :: DotProtoOption -> PP.Doc
topOption o = PP.text "option" <+> pPrint o PP.<> PP.text ";"

renderComment :: String -> PP.Doc
renderComment = PP.vcat . map ((PP.text "//" <+>) . textIfNonempty) . lines
  where
    textIfNonempty [] = PP.empty
    textIfNonempty text = PP.text text

-- Put the final closing brace on the next line.
-- This is important, since the final field might have a comment, and
-- the brace cannot be part of the comment.
-- We could use block comments instead, once the parser/lexer supports them.
vbraces :: PP.Doc -> PP.Doc -> PP.Doc
vbraces header body = header <+> PP.char '{' $$ PP.nest 2 body $$ PP.char '}'

prettyPrintProtoDefinition :: RenderingOptions -> DotProtoDefinition -> PP.Doc
prettyPrintProtoDefinition opts = defn where
  defn :: DotProtoDefinition -> PP.Doc
  defn (DotProtoMessage comment name parts) = renderComment comment $$
    vbraces (PP.text "message" <+> pPrint name) (PP.vcat $ msgPart name <$> parts)
  defn (DotProtoEnum    comment name parts) = renderComment comment $$
    vbraces (PP.text "enum"    <+> pPrint name) (PP.vcat $ enumPart name <$> parts)
  defn (DotProtoService comment name parts) = renderComment comment $$
    vbraces (PP.text "service" <+> pPrint name) (PP.vcat $ pPrint <$> parts)

  msgPart :: DotProtoIdentifier -> DotProtoMessagePart -> PP.Doc
  msgPart msgName (DotProtoMessageField f)           = field msgName f
  msgPart _       (DotProtoMessageDefinition definition) = defn definition
  msgPart _       (DotProtoMessageReserved reservations)
    =   PP.text "reserved"
    <+> (PP.hcat . PP.punctuate (PP.text ", ") $ pPrint <$> reservations)
    PP.<> PP.text ";"
  msgPart msgName (DotProtoMessageOneOf name fields)     = vbraces (PP.text "oneof" <+> pPrint name) (PP.vcat $ field msgName <$> fields)
  msgPart _       (DotProtoMessageOption opt)
    = PP.text "option" <+> pPrint opt PP.<> PP.text ";"

  field :: DotProtoIdentifier -> DotProtoField -> PP.Doc
  field msgName (DotProtoField number mtype name options comments)
    =   pPrint mtype
    <+> roSelectorName opts msgName name number
    <+> PP.text "="
    <+> pPrintFieldNumber number
    <+> optionAnnotation options
    PP.<> PP.text ";"
    $$  PP.nest 2 (renderComment comments)

  enumPart :: DotProtoIdentifier -> DotProtoEnumPart -> PP.Doc
  enumPart msgName (DotProtoEnumField name value options)
    = roEnumMemberName opts msgName name
    <+> PP.text "="
    <+> pPrint (fromIntegral value :: Int)
    <+> optionAnnotation options
    PP.<> PP.text ";"
  enumPart _       (DotProtoEnumReserved reservedFields)
    = PP.text "reserved" <+> (PP.hcat . PP.punctuate (PP.text ", ") $ pPrint <$> reservedFields)
  enumPart _       (DotProtoEnumOption opt)
    = PP.text "option" <+> pPrint opt PP.<> PP.text ";"

pPrintFieldNumber :: FieldNumber -> PP.Doc
pPrintFieldNumber = PP.text . show . getFieldNumber

-- | Render protobufs metadata as a .proto file stringy
toProtoFile :: RenderingOptions -> DotProto -> String
toProtoFile opts = PP.render . renderDotProto opts

-- | Render protobufs metadata as a .proto file string,
-- using the default rendering options.

toProtoFileDef :: DotProto -> String
toProtoFileDef = toProtoFile defRenderingOptions

packageFromDefs :: String -> [DotProtoDefinition] -> DotProto
packageFromDefs package defs =
  DotProto [] [] (DotProtoPackageSpec $ Single package) defs (DotProtoMeta fakePath)
