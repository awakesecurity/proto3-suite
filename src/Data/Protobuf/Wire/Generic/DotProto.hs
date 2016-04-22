-- | This module provides a way to generate .proto files from Haskell types
-- using 'Generic' deriving.
--
-- See the 'Data.Protobuf.Wire.Example' module for a worked example, but in brief,
-- derive an instance of the 'HasMessage' class for any message types you wish to
-- include, and an instance of the 'HasEnum' class for an enum types, and then use the
-- 'toProtoFile' function to turn those types into a .proto file.
--
-- * Strings
--
-- Use 'TL.Text' instead of 'String' for string types inside messages.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Protobuf.Wire.Generic.DotProto
  (
  -- * Data Structures
    DotProto
  , message
  , Data.Protobuf.Wire.Generic.DotProto.enum

  -- * Rendering
  , toProtoFile
  , renderDotProto

  -- * Supporting Classes
  , HasMessage(..)
  , GenericHasMessage(..)
  , MessageName(..)
  , FieldName(..)
  , PackageName(..)
  , HasMessageName(..)
  , HasEnum(..)
  , HasPrimType(..)
  , HasType(..)
  ) where

import           Control.Monad (guard)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.Int (Int32, Int64)
import           Data.Functor (($>))
import           Data.Monoid ((<>))
import           Data.Protobuf.Wire.Shared(FieldNumber(..), fieldNumber,
                                           Fixed(..), Signed(..),
                                           Enumerated(..),
                                           UnpackedVec(..),
                                           PackedVec(..))
import           Data.Protobuf.Wire.Generic
import           Data.Proxy (Proxy(..))
import           Data.String (IsString)
import           Data.Word (Word32, Word64)
import qualified Data.Text.Lazy as TL
import           GHC.Generics
import           GHC.TypeLits
import qualified Text.PrettyPrint as PP
import           Text.PrettyPrint (($+$))

-- | The name of a message in a .proto file
newtype MessageName = MessageName
  { getMessageName :: String
  } deriving (Eq, Ord, IsString)

instance Show MessageName where
  show = show . getMessageName

-- | The name of a field in an enum in a .proto file
newtype FieldName = FieldName
  { getFieldName :: String
  } deriving (Eq, Ord, IsString)

instance Show FieldName where
  show = show . getFieldName

newtype PackageName = PackageName
  { getPackageName :: String
  } deriving (Eq, Ord, IsString)

instance Show PackageName where
  show = show . getPackageName

data DotProtoPart = DotProtoPart MessageName (Either DotProtoMessage DotProtoEnum) deriving Show

-- | This data structure represents a .proto file
newtype DotProto = DotProto { runDotProto :: [DotProtoPart] }
  deriving (Show, Monoid)

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
  | Named MessageName -- ^ A named type, referring to another message or enum defined in the same file
  deriving Show

data Packing
  = Packed
    | Unpacked
    deriving Show

data DotProtoType
  = Prim DotProtoPrimType
  | Optional DotProtoPrimType
  | Repeated DotProtoPrimType Packing
  deriving Show

data DotProtoMessagePart = DotProtoMessagePart
  { dotProtoMessagePartFieldNumber :: FieldNumber
  , dotProtoMessagePartFieldName :: Maybe FieldName
  , _dotProtoMessagePartFieldType :: DotProtoType
  } deriving Show

newtype DotProtoMessage = DotProtoMessage
  { runDotProtoMessage :: [DotProtoMessagePart]
  } deriving (Show, Monoid)

newtype DotProtoEnum = DotProtoEnum
  { runDotProtoEnum :: [FieldName]
  } deriving (Show, Monoid)

-- | Generate metadata for a message type.
message :: (HasMessage a, HasMessageName a) => Proxy a -> DotProto
message pr =
  DotProto
  . (: [])
  . DotProtoPart (messageName pr)
  . Left
  . dotProto
  $ pr

-- | Generate metadata for an enum type.
enum :: forall e. (HasEnum e, HasMessageName e) => Proxy e -> DotProto
enum pr =
  DotProto
  . (: [])
  . DotProtoPart (messageName pr)
  . Right
  $ enumFields (Proxy :: Proxy e)

-- | Render a 'DotProto' structure as a .proto file
renderDotProto :: PackageName -> DotProto -> PP.Doc
renderDotProto pn = PP.vcat
                    . prependPackageInfo pn
                    . map renderPart
                    . runDotProto
  where
    prependPackageInfo :: PackageName -> [PP.Doc] -> [PP.Doc]
    prependPackageInfo pn xs = PP.text "syntax = \"proto3\";"
                               :(PP.text "package "
                                 <> PP.text (getPackageName pn) <> PP.text ";")
                               : xs
    renderPart :: DotProtoPart -> PP.Doc
    renderPart (DotProtoPart name e) = either (renderMessage name) (renderEnum name) e

    renderMessage :: MessageName -> DotProtoMessage -> PP.Doc
    renderMessage msgName = wrap . PP.vcat . map renderField . runDotProtoMessage
      where
        renderField :: DotProtoMessagePart -> PP.Doc
        renderField (DotProtoMessagePart (FieldNumber i) fieldName ty) = PP.hcat
          [ renderType ty
          , PP.text " "
          , maybe (PP.text (getMessageName msgName <> "_") <> PP.int (fromIntegral i)) (PP.text . getFieldName) fieldName
          , PP.text " = "
          , PP.int (fromIntegral i)
          , renderPackedOption ty
          , renderLineEnding ty
          ]
        renderPackedOption (Repeated _ Packed) = PP.text " [packed=true]"
        renderPackedOption (Repeated _ Unpacked) = PP.text " [packed=false]"
        renderPackedOption (Optional _) = PP.text " [packed=false]"
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

    renderEnum :: MessageName -> DotProtoEnum -> PP.Doc
    renderEnum msgName = wrap . PP.vcat . zipWith renderField [0..] . runDotProtoEnum
      where
        renderField :: Int -> FieldName -> PP.Doc
        renderField i memberName = PP.hcat
          [ PP.text (getFieldName memberName)
          , PP.text " = "
          , PP.int i
          , PP.text ";"
          ]

        wrap :: PP.Doc -> PP.Doc
        wrap = ((PP.text "enum " <> PP.text (getMessageName msgName) <> PP.text " {") $+$) . ($+$ (PP.text "}")) . PP.nest 2

-- | Render protobufs metadata as a .proto file string
toProtoFile :: PackageName -> DotProto -> String
toProtoFile pn = PP.render . renderDotProto pn

-- | This class captures those types which correspond to .proto messages.
--
-- It has a default implementation for any data type which is an instance of the
-- 'Generic' class, which will extract the name of the type constructor.
class HasMessageName a where
  messageName :: Proxy a -> MessageName
  default messageName :: (Generic a, GenericHasMessageName (Rep a)) => Proxy a -> MessageName
  messageName _ = genericMessageName (Proxy :: Proxy (Rep a))

class GenericHasMessageName (f :: * -> *) where
  genericMessageName :: Proxy f -> MessageName

instance Datatype d => GenericHasMessageName (M1 D d f) where
  genericMessageName _ = MessageName (datatypeName (undefined :: M1 D d f ()))

-- | This class captures those types which correspond to .proto enums.
--
-- It has a default implementation for any data type which is an instance of the
-- 'Generic' class, which will extract the enum field names from the data constructors.
class HasEnum a where
  enumFields :: Proxy a -> DotProtoEnum
  default enumFields :: (Generic a, GenericHasEnum (Rep a)) => Proxy a -> DotProtoEnum
  enumFields _ = genericEnumFields (Proxy :: Proxy (Rep a))

class GenericHasEnum (f :: * -> *) where
  genericEnumFields :: Proxy f -> DotProtoEnum

instance ( GenericHasEnum f
         , GenericHasEnum g
         ) => GenericHasEnum (f :+: g) where
  genericEnumFields _ = genericEnumFields (Proxy :: Proxy f) <> genericEnumFields (Proxy :: Proxy g)

instance Constructor c => GenericHasEnum (M1 C c f) where
  genericEnumFields _ = DotProtoEnum [ FieldName (conName (undefined :: M1 C c f ())) ]

instance GenericHasEnum f => GenericHasEnum (M1 D t f) where
  genericEnumFields _ = genericEnumFields (Proxy :: Proxy f)

instance GenericHasEnum f => GenericHasEnum (M1 S t f) where
  genericEnumFields _ = genericEnumFields (Proxy :: Proxy f)

-- | This class captures those types which correspond to primitive message field types.
--
-- It has a default implementation for any data type which is an instance of the
-- 'Generic' class, which will extract the name of the type constructor.
class HasPrimType a where
  -- | Get the type which represents this type inside another message.
  primType :: Proxy a -> DotProtoPrimType
  default primType :: HasMessageName a => Proxy a -> DotProtoPrimType
  primType pr = Named (messageName pr)

instance HasPrimType Int32 where
  primType _ = Int32

instance HasPrimType Int64 where
  primType _ = Int64

instance HasPrimType Word32 where
  primType _ = UInt32

instance HasPrimType Word64 where
  primType _ = UInt64

instance HasPrimType (Signed Int32) where
  primType _ = SInt32

instance HasPrimType (Signed Int64) where
  primType _ = SInt64

instance HasPrimType (Fixed Word32) where
  primType _ = Fixed32

instance HasPrimType (Fixed Word64) where
  primType _ = Fixed64

instance HasPrimType (Signed (Fixed Int32)) where
  primType _ = SFixed32

instance HasPrimType (Signed (Fixed Int64)) where
  primType _ = SFixed64

instance HasPrimType Bool where
  primType _ = Bool

instance HasPrimType Float where
  primType _ = Float

instance HasPrimType Double where
  primType _ = Double

instance HasPrimType TL.Text where
  primType _ = String

instance HasPrimType B.ByteString where
  primType _ = Bytes

instance HasPrimType BL.ByteString where
  primType _ = Bytes

instance HasMessageName e => HasPrimType (Enumerated e) where
  primType _ = Named (messageName (Proxy :: Proxy e))

-- | This class captures those types which correspond to message field types.
--
-- It has a default implementation for any data type which is an instance of the
-- 'Generic' class, which will extract the message name from the first data constructor.
class HasType a where
  -- | Get the type which represents this type inside another message.
  protoType :: Proxy a -> DotProtoType
  default protoType :: HasPrimType a => Proxy a -> DotProtoType
  protoType = Prim . primType

instance HasType Int32
instance HasType Int64
instance HasType Word32
instance HasType Word64
instance HasType (Signed Int32)
instance HasType (Signed Int64)
instance HasType (Fixed Word32)
instance HasType (Fixed Word64)
instance HasType (Signed (Fixed Int32))
instance HasType (Signed (Fixed Int64))
instance HasType Bool
instance HasType Float
instance HasType Double
instance HasType TL.Text
instance HasType B.ByteString
instance HasType BL.ByteString
instance HasMessageName e => HasType (Enumerated e)

instance HasPrimType a => HasType (Maybe a) where
  protoType _ = Optional (primType (Proxy :: Proxy a))

instance HasPrimType a => HasType (UnpackedVec a) where
  protoType _ = Repeated (primType (Proxy :: Proxy a)) Unpacked

instance HasPrimType a => HasType (PackedVec a) where
  protoType _ = Repeated (primType (Proxy :: Proxy a)) Packed

-- | This class captures those types which can represent .proto messages and
-- be used to generate a message entry in a .proto file.
--
-- It has a default implementation for any data type which is an instance of the
-- 'Generic' class, which will extract the field names from the record selector
-- names.
class HasEncoding a => HasMessage a where
  -- | Generate a .proto message from the type information.
  dotProto :: Proxy a -> DotProtoMessage
  default dotProto :: (Generic a, GenericHasMessage (Rep a)) => Proxy a -> DotProtoMessage
  dotProto _ = genericDotProto (Proxy :: Proxy (Rep a))

primDotProto :: DotProtoType -> DotProtoMessage
primDotProto ty = DotProtoMessage [ DotProtoMessagePart (fieldNumber 1) Nothing ty ]

class GenericHasEncoding f => GenericHasMessage f where
  genericDotProto :: Proxy f -> DotProtoMessage

instance GenericHasMessage V1 where
  genericDotProto _ = mempty

instance GenericHasMessage U1 where
  genericDotProto _ = mempty

instance ( KnownNat (GenericFieldCount f)
         , GenericHasMessage f
         , GenericHasMessage g
         ) => GenericHasMessage (f :*: g) where
  genericDotProto _ = genericDotProto (Proxy :: Proxy f) <> adjust (genericDotProto (Proxy :: Proxy g))
    where
      offset = fromIntegral $ natVal (Proxy :: Proxy (GenericFieldCount f))
      adjust = DotProtoMessage . map adjustPart . runDotProtoMessage
      adjustPart part = part { dotProtoMessagePartFieldNumber = (FieldNumber . (offset +) . getFieldNumber . dotProtoMessagePartFieldNumber) part }

instance (HasEncoding c, HasType c) => GenericHasMessage (K1 i c) where
  genericDotProto _ = primDotProto (protoType (Proxy :: Proxy c))

instance (Selector s, GenericHasMessage f) => GenericHasMessage (M1 S s f) where
  genericDotProto _ = DotProtoMessage
                      . map applyName
                      . runDotProtoMessage
                      $ genericDotProto (Proxy :: Proxy f)
    where
      applyName :: DotProtoMessagePart -> DotProtoMessagePart
      applyName mp = mp { dotProtoMessagePartFieldName = newName }

      newName :: Maybe FieldName
      newName = guard (not (null name)) $> FieldName name
        where
          name = selName (undefined :: S1 s f ())

instance GenericHasMessage f => GenericHasMessage (M1 C t f) where
  genericDotProto _ = genericDotProto (Proxy :: Proxy f)

instance GenericHasMessage f => GenericHasMessage (M1 D t f) where
  genericDotProto _ = genericDotProto (Proxy :: Proxy f)
