-- | This module provides a safer alternative to the raw 'Data.Protobuf.Wire'
-- library based on 'GHC.Generics'.
--
-- Instead of generating Haskell code from a .proto file, we write our message
-- formats as Haskell types, and generate the serializer. We would also be able
-- to generate the .proto file, but that is not supported yet.
--
-- To use this library, simply derive a 'Generic' instance for your type(s), and
-- use the default `HasEncoding` instance.
--
-- * Field Numbers
--
-- Field numbers are automatically generated by the library, starting at 1.
-- Therefore, adding new fields is a compatible change only at the end of a
-- record. Renaming fields is also safe.
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

module Data.Protobuf.Wire.Generic
  ( HasEncoding(..)

  -- * Encoding
  , toLazyByteString

  -- * Supporting Classes
  , GenericHasEncoding(..)
  , MemberType(..)
  , Embedded(..)
  ) where

import           Control.DeepSeq (NFData)
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import           Data.Int (Int32, Int64)
import           Data.Monoid ((<>))
import           Data.Protobuf.Wire.Encode as Wire
import           Data.Protobuf.Wire.Shared as Wire
import           Data.Proxy (Proxy(..))
import           Data.Word (Word32, Word64)
import qualified Data.Text.Lazy as TL
import           GHC.Generics
import           GHC.TypeLits

-- | Members in a record can be primitive or non-primitive, and this affects
-- how they get encoded.
data MemberType
  = Primitive
  | NonPrimitive

-- | This class defines how the two member types get embedded inside larger messages.
class Embedded (mt :: MemberType) where
  embed :: Proxy mt -> (FieldNumber -> a -> BB.Builder) -> FieldNumber -> a -> BB.Builder

-- | Primitive members get embedded trivially, by just inserting their fields normally.
instance Embedded 'Primitive where
  embed _ = id

-- | Non-primitive members get serialized and embedded into a single field in the
-- larger message.
instance Embedded 'NonPrimitive where
  embed _ f num = embedded num . f (fieldNumber 1)

-- | This class captures those types which can be serialized as protobuf messages.
--
-- Instances are provides for supported primitive types, and instances can be derived
-- for other types.
class HasEncoding a where
  -- | A member is either 'Primitive' or 'NonPrimitive', and this affects the binary
  -- encoding. This is tracked by the 'GetMemberType' associated type.
  type GetMemberType a :: MemberType
  type GetMemberType a = 'NonPrimitive

  -- | The number of protobuf fields in a message.
  type FieldCount a :: Nat
  type FieldCount a = GenericFieldCount (Rep a)

  -- | Encode a message as a binary protobuf message, starting at the specified 'FieldNumber'.
  encode :: FieldNumber -> a -> BB.Builder
  default encode :: (Generic a, GenericHasEncoding (Rep a)) => FieldNumber -> a -> BB.Builder
  encode num = genericEncode num . from

-- | Serialize a message as a lazy 'BL.ByteString'.
toLazyByteString :: HasEncoding a => a -> BL.ByteString
toLazyByteString = BB.toLazyByteString . encode (fieldNumber 1)

instance HasEncoding Int32 where
  type GetMemberType Int32 = 'Primitive
  type FieldCount Int32 = 1
  encode = int32

instance HasEncoding Int64 where
  type GetMemberType Int64 = 'Primitive
  type FieldCount Int64 = 1
  encode = int64

instance HasEncoding Word32 where
  type GetMemberType Word32 = 'Primitive
  type FieldCount Word32 = 1
  encode = uint32

instance HasEncoding Word64 where
  type GetMemberType Word64 = 'Primitive
  type FieldCount Word64 = 1
  encode = uint64

instance HasEncoding (Signed Int32) where
  type GetMemberType (Signed Int32) = 'Primitive
  type FieldCount (Signed Int32) = 1
  encode num = sint32 num . signed

instance HasEncoding (Signed Int64) where
  type GetMemberType (Signed Int64) = 'Primitive
  type FieldCount (Signed Int64) = 1
  encode num = sint64 num . signed

instance HasEncoding (Fixed Word32) where
  type GetMemberType (Fixed Word32) = 'Primitive
  type FieldCount (Fixed Word32) = 1
  encode num = fixed32 num . fixed

instance HasEncoding (Fixed Word64) where
  type GetMemberType (Fixed Word64) = 'Primitive
  type FieldCount (Fixed Word64) = 1
  encode num = fixed64 num . fixed

instance HasEncoding (Signed (Fixed Int32)) where
  type GetMemberType (Signed (Fixed Int32)) = 'Primitive
  type FieldCount (Signed (Fixed Int32)) = 1
  encode num = sfixed32 num . fixed . signed

instance HasEncoding (Signed (Fixed Int64)) where
  type GetMemberType (Signed (Fixed Int64)) = 'Primitive
  type FieldCount (Signed (Fixed Int64)) = 1
  encode num = sfixed64 num . fixed . signed

instance HasEncoding Bool where
  type GetMemberType Bool = 'Primitive
  type FieldCount Bool = 1
  encode = Wire.enum

instance HasEncoding Float where
  type GetMemberType Float = 'Primitive
  type FieldCount Float = 1
  encode = float

instance HasEncoding Double where
  type GetMemberType Double = 'Primitive
  type FieldCount Double = 1
  encode = double

instance HasEncoding TL.Text where
  type GetMemberType TL.Text = 'Primitive
  type FieldCount TL.Text = 1
  encode = text

instance HasEncoding B.ByteString where
  type GetMemberType B.ByteString = 'Primitive
  type FieldCount B.ByteString = 1
  encode = bytes

instance HasEncoding BL.ByteString where
  type GetMemberType BL.ByteString = 'Primitive
  type FieldCount BL.ByteString = 1
  encode = bytes'

instance Enum e => HasEncoding (Enumerated e) where
  type GetMemberType (Enumerated e) = 'Primitive
  type FieldCount (Enumerated e) = 1
  encode num = Wire.enum num . enumerated

instance HasEncoding a => HasEncoding (Maybe a) where
  type GetMemberType (Maybe a) = GetMemberType a
  type FieldCount (Maybe a) = FieldCount a
  encode = foldMap . encode

instance HasEncoding a => HasEncoding [a] where
  type GetMemberType [a] = GetMemberType a
  type FieldCount [a] = FieldCount a
  encode = foldMap . encode

class GenericHasEncoding f where
  type GenericFieldCount f :: Nat

  genericEncode :: FieldNumber -> f a -> BB.Builder

instance GenericHasEncoding V1 where
  type GenericFieldCount V1 = 0
  genericEncode _ _ = error "genericEncode: empty type"

instance GenericHasEncoding U1 where
  type GenericFieldCount U1 = 0
  genericEncode _ _ = mempty

-- | Because of the lack of a type-level 'max' operation, we make the
-- somewhat artifical restriction that the first summand should have the most
-- fields.
instance ( GenericFieldCount g <= GenericFieldCount f
         , GenericHasEncoding f
         , GenericHasEncoding g
         ) => GenericHasEncoding (f :+: g) where
  type GenericFieldCount (f :+: g) = GenericFieldCount f
  genericEncode num (L1 x) = genericEncode num x
  genericEncode num (R1 x) = genericEncode num x

instance ( KnownNat (GenericFieldCount f)
         , GenericHasEncoding f
         , GenericHasEncoding g
         ) => GenericHasEncoding (f :*: g) where
  type GenericFieldCount (f :*: g) = GenericFieldCount f + GenericFieldCount g
  genericEncode num (x :*: y) = genericEncode num x <> genericEncode (FieldNumber (getFieldNumber num + offset)) y
    where
      offset = fromIntegral $ natVal (Proxy :: Proxy (GenericFieldCount f))

instance ( HasEncoding c
         , Embedded (GetMemberType c)
         ) => GenericHasEncoding (K1 i c) where
  type GenericFieldCount (K1 i c) = 1
  genericEncode num (K1 x) = embed (Proxy :: Proxy (GetMemberType c)) encode num x

instance GenericHasEncoding f => GenericHasEncoding (M1 i t f) where
  type GenericFieldCount (M1 i t f) = GenericFieldCount f
  genericEncode num (M1 x) = genericEncode num x
