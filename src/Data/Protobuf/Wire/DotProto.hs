-- | This module provides types and functions to generate @.proto@ files.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Protobuf.Wire.DotProto
  (
  -- * Data Structures
    DotProto(..)
  , DotProtoPart(..)
  , DotProtoMessage(..)
  , DotProtoMessagePart(..)
  , DotProtoEnum(..)
  , MessageName(..)
  , FieldName(..)
  , PackageName(..)
  , DotProtoPrimType(..)
  , DotProtoType(..)
  , Packing(..)

  -- * Rendering
  , RenderingOptions(..)
  , defRenderingOptions
  , defSelectorName
  , toProtoFile
  , toProtoFileDef
  , renderDotProto
  ) where

import           Data.Monoid ((<>))
import           Data.Protobuf.Wire.Shared (FieldNumber(..))
import           Data.String (IsString)
import qualified Text.PrettyPrint as PP
import           Text.PrettyPrint (($+$))

-- | The name of a message in a @.proto@ file
newtype MessageName = MessageName
  { getMessageName :: String
  } deriving (Eq, Ord, IsString)

instance Show MessageName where
  show = show . getMessageName

-- | The name of a field in an enum in a @.proto@ file
newtype FieldName = FieldName
  { getFieldName :: String
  } deriving (Eq, Ord, IsString)

instance Show FieldName where
  show = show . getFieldName

-- | The name of a package appearing in a @.proto@ file.
newtype PackageName = PackageName
  { getPackageName :: String
  } deriving (Eq, Ord, IsString)

instance Show PackageName where
  show = show . getPackageName

-- | A part of a @.proto@ file: either a message or an enum definition.
data DotProtoPart = DotProtoPart MessageName (Either DotProtoMessage DotProtoEnum) deriving Show

-- | This data structure represents a protocol buffers message enum types,
-- during the translation from Haskell data types to a @.proto@ file.
-- The types in this file are not meant to represent the spec exactly,
-- but the translation of Haskell types to the spec.
newtype DotProto = DotProto { runDotProto :: [DotProtoPart] }
  deriving (Show, Monoid)

-- | The primitive types from the protocol buffers specification.
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
  | Named MessageName
  -- ^ A named type, referring to another message or enum defined in the same file
  deriving Show

-- | Whether or not fields should be packed while encoding.
data Packing
  = Packed
  | Unpacked
  deriving Show

-- | The types of members of protocol buffers messages.
data DotProtoType
  = Prim DotProtoPrimType
  | Optional DotProtoPrimType
  | Repeated DotProtoPrimType Packing
  | NestedRepeated DotProtoPrimType
  deriving Show

-- | A message field appearing in a message definition in a @.proto@ file.
data DotProtoMessagePart = DotProtoMessagePart
  { dotProtoMessagePartFieldNumber :: FieldNumber
  -- ^ The field number for this field
  , dotProtoMessagePartFieldName :: Maybe FieldName
  -- ^ The field name for this field. This uses 'Maybe' because it is extracted
  -- from the Haskell data type, and a Haskell data type may or may not specify
  -- selector names. If this is 'Nothing', we will generate a name.
  , dotProtoMessagePartFieldType :: DotProtoType
  -- ^ The type of this field
  } deriving Show

-- | A message definition appearing in a @.proto@ file.
newtype DotProtoMessage = DotProtoMessage
  { runDotProtoMessage :: [DotProtoMessagePart]
  } deriving (Show, Monoid)

-- | An enum definition appearing in a @.proto@ file.
newtype DotProtoEnum = DotProtoEnum
  { runDotProtoEnum :: [(Int, FieldName)]
  } deriving (Show, Monoid)

-- | Options for rendering a @.proto@ file.
data RenderingOptions = RenderingOptions
  { roSelectorName :: MessageName -> Maybe FieldName -> FieldNumber -> String
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
  maybe (getMessageName msgName <> "_" <> show (getFieldNumber fieldNum))
        getFieldName
        fieldName

-- | Render a 'DotProto' structure as a @.proto@ file
renderDotProto :: RenderingOptions -> PackageName -> DotProto -> PP.Doc
renderDotProto RenderingOptions{..} pn =
    PP.vcat
    . prependPackageInfo
    . map renderPart
    . runDotProto
  where
    prependPackageInfo :: [PP.Doc] -> [PP.Doc]
    prependPackageInfo xs = PP.text "syntax = \"proto3\";"
                            : PP.hcat [ PP.text "package "
                                      , PP.text (getPackageName pn)
                                      , PP.text ";"
                                      ]
                            : xs
    renderPart :: DotProtoPart -> PP.Doc
    renderPart (DotProtoPart name e) = either (renderMessage name) (renderEnum name) e

    renderMessage :: MessageName -> DotProtoMessage -> PP.Doc
    renderMessage msgName = wrap . PP.vcat . map renderField . runDotProtoMessage
      where
        renderField :: DotProtoMessagePart -> PP.Doc
        renderField (DotProtoMessagePart fieldNum fieldName ty) = PP.hcat
          [ renderType ty
          , PP.text " "
          , PP.text (roSelectorName msgName fieldName fieldNum)
          , PP.text " = "
          , PP.int (fromIntegral (getFieldNumber fieldNum))
          , renderPackedOption ty
          , renderLineEnding ty
          ]
        renderPackedOption (Repeated _ Packed) = PP.text " [packed=true]"
        renderPackedOption (Repeated _ Unpacked) = PP.text " [packed=false]"
        renderPackedOption (Optional _) = PP.text " [packed=false]"
        renderPackedOption (NestedRepeated _) = PP.text " [packed=false]"
        renderPackedOption _ = mempty

        renderLineEnding (Optional _) = PP.text "; // 0..1"
        renderLineEnding _ = PP.text ";"

        wrap :: PP.Doc -> PP.Doc
        wrap = ((PP.text "message " <> PP.text (getMessageName msgName) <> PP.text " {") $+$) . ($+$ (PP.text "}")) . PP.nest 2

        renderPrimType :: DotProtoPrimType -> PP.Doc
        renderPrimType Int32          = PP.text "int32"
        renderPrimType Int64          = PP.text "int64"
        renderPrimType SInt32         = PP.text "sint32"
        renderPrimType SInt64         = PP.text "sint64"
        renderPrimType UInt32         = PP.text "uint32"
        renderPrimType UInt64         = PP.text "uint64"
        renderPrimType Fixed32        = PP.text "fixed32"
        renderPrimType Fixed64        = PP.text "fixed64"
        renderPrimType SFixed32       = PP.text "sfixed32"
        renderPrimType SFixed64       = PP.text "sfixed64"
        renderPrimType String         = PP.text "string"
        renderPrimType Bytes          = PP.text "bytes"
        renderPrimType Bool           = PP.text "bool"
        renderPrimType Float          = PP.text "float"
        renderPrimType Double         = PP.text "double"
        renderPrimType (Named name)   = PP.text (getMessageName name)

        renderType :: DotProtoType -> PP.Doc
        renderType (Prim ty)      = renderPrimType ty
        renderType (Optional ty)  = PP.text "repeated " <> renderPrimType ty
        renderType (Repeated ty _)  = PP.text "repeated " <> renderPrimType ty
        renderType (NestedRepeated ty) = PP.text "repeated " <> renderPrimType ty

    renderEnum :: MessageName -> DotProtoEnum -> PP.Doc
    renderEnum msgName = wrap . PP.vcat . map renderField . runDotProtoEnum
      where
        renderField :: (Int, FieldName) -> PP.Doc
        renderField (index, memberName) = PP.hcat
          [ PP.text (roEnumMemberName msgName memberName)
          , PP.text " = "
          , PP.int index
          , PP.text ";"
          ]

        wrap :: PP.Doc -> PP.Doc
        wrap = ((PP.text "enum " <> PP.text (getMessageName msgName) <> PP.text " {") $+$) . ($+$ (PP.text "}")) . PP.nest 2

-- | Render protobufs metadata as a @.proto@ file string
toProtoFile :: RenderingOptions -> PackageName -> DotProto -> String
toProtoFile opts pn = PP.render . renderDotProto opts pn

-- | Render protobufs metadata as a @.proto@ file string,
-- using the default rendering options.
toProtoFileDef :: PackageName -> DotProto -> String
toProtoFileDef = toProtoFile defRenderingOptions
