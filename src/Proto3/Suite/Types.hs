{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExplicitNamespaces #-}

module Proto3.Suite.Types
  (
  -- * Integral Types
    Fixed(..)
  , Signed(..)

  -- * Enumerable Types
  , Enumerated(..)

  -- * String and Bytes Types
  , String(..)
  , Bytes(..)

  , ForceEmit(..)
  , Nested(..)
  , UnpackedVec(..)
  , PackedVec(..)
  , NestedVec(..)
  , Commented(..)
  , type (//)()
  ) where

import           Control.Applicative
import           Control.DeepSeq (NFData)
import           GHC.Exts (IsList(..))
import           GHC.Generics
import           Data.Int (Int32)
import qualified Data.Vector as V
import           GHC.TypeLits (Symbol)
import           Prelude hiding (String)
import           Proto3.Wire.Class (ProtoEnum(..))
import           Test.QuickCheck (Arbitrary(..))

-- | 'Fixed' provides a way to encode integers in the fixed-width wire formats.
newtype Fixed a = Fixed { fixed :: a }
  deriving (Show, Eq, Ord, Num, Generic, NFData, Arbitrary, Enum, Bounded
           , Functor, Foldable, Traversable)

-- | 'Signed' provides a way to encode integers in the signed wire formats.
newtype Signed a = Signed { signed :: a }
  deriving (Show, Eq, Ord, Num, Generic, NFData, Arbitrary, Bounded
           , Functor, Foldable, Traversable)

-- | 'Enumerated' lifts any type with an 'IsEnum' instance so that it can be encoded
-- with 'HasEncoding'.
newtype Enumerated a = Enumerated { enumerated :: Either Int32 a }
  deriving (Show, Eq, Ord, Generic, NFData
           , Functor, Foldable, Traversable)

instance ProtoEnum a => Arbitrary (Enumerated a) where
  arbitrary = do
    i <- arbitrary
    return . Enumerated $ maybe (Left i) Right (toProtoEnumMay i)

-- | 'String' provides a way to indicate that the given type expresses
-- a Protobuf string scalar.  @'String' a@ may have type class instances
-- that are more specific to Protobuf uses than those of @a@.
newtype String a = String { string :: a }
  deriving (Show, Eq, Ord, Generic, Monoid, NFData, Arbitrary,
            Functor, Foldable, Semigroup, Traversable)

-- | 'Bytes' provides a way to indicate that the given type expresses
-- a Protobuf bytes scalar.  @'Bytes' a@ may have type class instances
-- that are more specific to Protobuf uses than those of @a@.
newtype Bytes a = Bytes { bytes :: a }
  deriving (Show, Eq, Ord, Generic, Monoid, NFData, Arbitrary,
            Functor, Foldable, Semigroup, Traversable)

-- | 'PackedVec' provides a way to encode packed lists of basic protobuf types into
-- the wire format.
newtype PackedVec a = PackedVec { packedvec :: V.Vector a }
  deriving (Show, Eq, Functor, Foldable, Traversable, Ord, NFData, Applicative,
            Alternative, Monoid, Semigroup)

instance IsList (PackedVec a) where
  type Item (PackedVec a) = a
  fromList = PackedVec . V.fromList
  toList = V.toList . packedvec

instance Arbitrary a => Arbitrary (PackedVec a) where
  arbitrary = fmap (PackedVec . V.fromList) arbitrary

newtype UnpackedVec a = UnpackedVec {unpackedvec :: V.Vector a }
  deriving (Show, Eq, Functor, Foldable, Traversable, Ord, NFData, Applicative,
            Alternative, Monoid, Semigroup)

instance IsList (UnpackedVec a) where
  type Item (UnpackedVec a) = a
  fromList = UnpackedVec . V.fromList
  toList = V.toList . unpackedvec

instance Arbitrary a => Arbitrary (UnpackedVec a) where
  arbitrary = fmap (UnpackedVec . V.fromList) arbitrary

newtype NestedVec a =
  NestedVec { nestedvec :: V.Vector a }
  deriving (Show, Eq, Functor, Foldable, Traversable, Ord, NFData, Applicative,
            Alternative, Monoid, Semigroup)

instance IsList (NestedVec a) where
  type Item (NestedVec a) = a
  fromList = NestedVec . V.fromList
  toList = V.toList . nestedvec

instance Arbitrary a => Arbitrary (NestedVec a) where
  arbitrary = fmap (NestedVec . V.fromList) arbitrary

-- | 'Nested' provides a way to nest protobuf messages within protobuf messages.
newtype Nested a = Nested { nested :: Maybe a }
  deriving (Show, Eq, Ord, Generic, NFData, Monoid, Arbitrary, Functor, Foldable,
            Traversable, Applicative, Alternative, Monad, Semigroup)

-- | 'ForceEmit' provides a way to force emission of field values, even when
-- default-value semantics states otherwise. Used when serializing oneof
-- subfields.
newtype ForceEmit a = ForceEmit{ forceEmit :: a }
  deriving (Show, Eq, Ord, Generic, NFData, Monoid, Arbitrary, Functor, Foldable,
            Traversable, Semigroup)

-- | 'Commented' provides a way to add comments to generated @.proto@ files.
newtype Commented (comment :: Symbol) a = Commented { unCommented :: a }
  deriving (Show, Eq, Ord, Generic, NFData, Monoid, Arbitrary, Functor, Foldable, Traversable, Semigroup)

-- | A type operator synonym for 'Commented', so that we can write C-style
-- comments on fields.
type a // (comment :: Symbol) = Commented comment a
