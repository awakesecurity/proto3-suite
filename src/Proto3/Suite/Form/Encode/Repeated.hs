{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Proto3.Suite.Form.Encode.Repeated
  ( Forward(..)
  , Reverse(..)
  , ReverseN(ReverseN, ..)
  , FoldBuilders(..)
  , MapToRepeated(..)
  , ToRepeated(..)
  ) where

import Data.Coerce (coerce)
import Data.Foldable (fold)
import Data.Functor.Identity (Identity(..))
import Data.IntMap.Lazy qualified
import Data.IntSet qualified
import Data.Kind (Type)
import Data.List.NonEmpty qualified
import Data.Map.Lazy qualified
import Data.Monoid (Dual(..))
import Data.Sequence qualified
import Data.Set qualified
import Data.Vector qualified
import Data.Vector.Generic qualified
import Data.Vector.Storable qualified
import Data.Vector.Unboxed qualified
import Foreign (Storable)
import GHC.Exts (Constraint, TYPE)
import Proto3.Wire.Encode qualified as Encode

-- | A sequence to be emitted in order, expressed as a mapping on
-- the elements of a container equipped with a 'foldr'-like function.
--
-- By delaying the application of the often-recursive 'foldr'-like
-- function, we hope to fully enable whatever inlining it supports.
--
-- If your sequence can be computed in reverse order, it is often more
-- efficient to use 'Reverse' instead, in order to match the fact that
-- elements of a repeated protobuf field are encoded in reverse order.
data Forward a = forall c . Forward (forall b . (a -> b -> b) -> b -> c -> b) c

deriving stock instance Functor Forward

instance Foldable Forward
  where
    foldr op z (Forward f c) = f op z c

-- | Like 'Forward' but requests that the order be reversed upon usage.
--
-- Consider 'ReverseN' if you can predict the number of elements.
--
-- NOTE: This type is /not/ 'Foldable' because the natural order
-- of the fold would oppose the order in which the elements appear
-- when eventually decoded, and that seems too confusing.  But you
-- may examine the field of the constructor, which is 'Foldable'
-- and presents the elements in reverse order.
newtype Reverse a = Reverse (Forward a)
  deriving newtype (Functor)

-- | Like 'Reverse' but includes a prediction of the number
-- of elements in the sequence, which can improve efficiency.
--
-- The prediction must not underestimate because that could lead to
-- a crash.  An overestimate will overallocate output buffer space.
--
-- For fixed-width elements (booleans, fixed-width integers,
-- and floating-point numbers) the prediction can reduce the
-- number of checks of available space in the output buffer.
--
-- If the elements have variable width then the element count
-- prediction may go unused, though there are no guarantees.
data ReverseN a = UnsafeReverseN {-# UNPACK #-}!Int (Forward a)
  -- ^ The fields are the element count prediction
  -- and the element sequence /in reverse order/.
  --
  -- This constructor is unsafe because you must accurately
  -- predict the number of elements, or at least overapproximate
  -- without too much error.  See the comments for this type.
  deriving stock (Functor)

{-# COMPLETE ReverseN #-}

pattern ReverseN :: Int -> Forward a -> ReverseN a
pattern ReverseN count xs <- UnsafeReverseN count xs

-- | Inhabited by type constructors providing an efficient way to fold over contained
-- builders and an 'fmap' that is efficient when followed by that operation.
class Functor t =>
      FoldBuilders t
  where
    foldBuilders :: t Encode.MessageBuilder -> Encode.MessageBuilder

instance FoldBuilders Identity
  where
    foldBuilders = coerce
    {-# INLINE foldBuilders #-}

instance FoldBuilders Forward
  where
    -- | Concatenates the specified encodings in order.
    --
    -- For efficiency, consider using 'Reverse' or 'Vector' instead.
    foldBuilders xs = Encode.etaMessageBuilder (fold @Forward @Encode.MessageBuilder) xs
    {-# INLINE foldBuilders #-}

instance FoldBuilders Reverse
  where
    -- | Concatenates the specified encodings in the /reverse/ order specified by the argument.
    foldBuilders (Reverse xs) = Encode.etaMessageBuilder (getDual . foldMap Dual) xs
    {-# INLINE foldBuilders #-}

instance FoldBuilders ReverseN
  where
    -- | Concatenates the specified encodings in the /reverse/ order specified by the argument.
    --
    -- The predicted count of elements is /not/ used because it is only useful
    -- for specific element types whose encodings have a fixed width.  We must
    -- avoid delegating to this general-purpose method in such cases.
    foldBuilders (ReverseN _ xs) = foldBuilders (Reverse xs)
    {-# INLINE foldBuilders #-}

-- | Converts to a sequence suitable for protobuf serialization,
-- often taking advantage of special optimized iterations.
--
-- See also 'ToRepeated'.
type MapToRepeated :: forall {cr} . TYPE cr -> Constraint
class MapToRepeated (container :: TYPE cr)
  where
    -- | 'Identity', 'Forward', 'Reverse', 'ReverseN', or some
    -- other sequence type constructor that supports encoding
    -- its elements to a protobuf repeated field.
    --
    -- The choice of type constructor should reflect the
    -- best way to iterate over the container when encoding
    -- a repeated field from that container in reverse order.
    type RepeaterFor container :: Type -> Type

    -- | Returns @(->) a@, where @a@ is the possibly-unlifted
    -- type of item that is found within the container.
    -- See 'mapToRepeated' for how we use this type family.
    type MapElementTo container :: Type -> Type
    type MapElementTo container = (->) (RepeatedElement container)

    -- | Similar to 'fmap' but yields the associated
    -- emittable sequence type specified by 'RepeaterFor'.
    --
    -- See also 'toRepeated'.
    mapToRepeated :: forall b . MapElementTo container b -> container -> RepeaterFor container b

-- | A common special case of 'MapToRepeated'
-- for containers that have lifted elements.
type ToRepeated :: forall {cr} . TYPE cr -> Constraint
class ( MapToRepeated container
      , MapElementTo container ~ (->) (RepeatedElement container)
      ) =>
      ToRepeated container
  where
    -- | The type of element in the container.
    type RepeatedElement container :: Type

    -- | Similar to 'fmap' but yields the associated emittable sequence
    -- type specified by 'RepeaterFor'.  See also 'toRepeated'.
    toRepeated :: container -> RepeaterFor container (RepeatedElement container)

instance MapToRepeated (Identity a)
  where
    type RepeaterFor (Identity a) = Identity
    mapToRepeated = coerce
    {-# INLINE mapToRepeated #-}

instance ToRepeated (Identity a)
  where
    type RepeatedElement (Identity a) = a
    toRepeated = id
    {-# INLINE toRepeated #-}

instance MapToRepeated [a]
  where
    type RepeaterFor [a] = Forward
    mapToRepeated = mapFoldableToForward
    {-# INLINE mapToRepeated #-}

instance ToRepeated [a]
  where
    type RepeatedElement [a] = a
    toRepeated = foldableToForward
    {-# INLINE toRepeated #-}

instance MapToRepeated (Data.List.NonEmpty.NonEmpty a)
  where
    type RepeaterFor (Data.List.NonEmpty.NonEmpty a) = Forward
    mapToRepeated = mapFoldableToForward
    {-# INLINE mapToRepeated #-}

instance ToRepeated (Data.List.NonEmpty.NonEmpty a)
  where
    type RepeatedElement (Data.List.NonEmpty.NonEmpty a) = a
    toRepeated = foldableToForward
    {-# INLINE toRepeated #-}

instance MapToRepeated (Data.Vector.Vector a)
  where
    type RepeaterFor (Data.Vector.Vector a) = ReverseN
    mapToRepeated = mapVectorToReverseN
    {-# INLINE mapToRepeated #-}

instance ToRepeated (Data.Vector.Vector a)
  where
    type RepeatedElement (Data.Vector.Vector a) = a
    toRepeated = vectorToReverseN
    {-# INLINE toRepeated #-}

instance Storable a =>
         MapToRepeated (Data.Vector.Storable.Vector a)
  where
    type RepeaterFor (Data.Vector.Storable.Vector a) = ReverseN
    mapToRepeated = mapVectorToReverseN
    {-# INLINE mapToRepeated #-}

instance Storable a =>
         ToRepeated (Data.Vector.Storable.Vector a)
  where
    type RepeatedElement (Data.Vector.Storable.Vector a) = a
    toRepeated = vectorToReverseN
    {-# INLINE toRepeated #-}

instance Data.Vector.Unboxed.Unbox a =>
         MapToRepeated (Data.Vector.Unboxed.Vector a)
  where
    type RepeaterFor (Data.Vector.Unboxed.Vector a) = ReverseN
    mapToRepeated = mapVectorToReverseN
    {-# INLINE mapToRepeated #-}

instance Data.Vector.Unboxed.Unbox a =>
         ToRepeated (Data.Vector.Unboxed.Vector a)
  where
    type RepeatedElement (Data.Vector.Unboxed.Vector a) = a
    toRepeated = vectorToReverseN
    {-# INLINE toRepeated #-}

instance MapToRepeated (Data.Sequence.Seq a)
  where
    type RepeaterFor (Data.Sequence.Seq a) = ReverseN
    mapToRepeated = unsafeMapFoldableToReverseN Data.Sequence.length
    {-# INLINE mapToRepeated #-}

instance ToRepeated (Data.Sequence.Seq a)
  where
    type RepeatedElement (Data.Sequence.Seq a) = a
    toRepeated = unsafeFoldableToReverseN Data.Sequence.length
    {-# INLINE toRepeated #-}

instance MapToRepeated (Data.Set.Set a)
  where
    type RepeaterFor (Data.Set.Set a) = ReverseN
    mapToRepeated = unsafeMapFoldableToReverseN Data.Set.size
    {-# INLINE mapToRepeated #-}

instance ToRepeated (Data.Set.Set a)
  where
    type RepeatedElement (Data.Set.Set a) = a
    toRepeated = unsafeFoldableToReverseN Data.Set.size
    {-# INLINE toRepeated #-}

instance MapToRepeated Data.IntSet.IntSet
  where
    type RepeaterFor Data.IntSet.IntSet = ReverseN
    mapToRepeated m xs = fmap m (toRepeated xs)
    {-# INLINE mapToRepeated #-}

instance ToRepeated Data.IntSet.IntSet
  where
    type RepeatedElement Data.IntSet.IntSet = Int
    toRepeated xs = UnsafeReverseN (Data.IntSet.size xs) (Forward (Data.IntSet.foldl . flip) xs)
    {-# INLINE toRepeated #-}

instance MapToRepeated (Data.Map.Lazy.Map k a)
  where
    type RepeaterFor (Data.Map.Lazy.Map k a) = ReverseN
    mapToRepeated m xs = fmap m (toRepeated xs)
    {-# INLINE mapToRepeated #-}

instance ToRepeated (Data.Map.Lazy.Map k a)
  where
    type RepeatedElement (Data.Map.Lazy.Map k a) = (k, a)
    toRepeated xs = UnsafeReverseN (Data.Map.Lazy.size xs) (Forward f xs)
      where
        f op = Data.Map.Lazy.foldlWithKey (\a k v -> op (k, v) a)
        {-# INLINE f #-}
    {-# INLINE toRepeated #-}

instance MapToRepeated (Data.IntMap.Lazy.IntMap a)
  where
    type RepeaterFor (Data.IntMap.Lazy.IntMap a) = ReverseN
    mapToRepeated m xs = fmap m (toRepeated xs)
    {-# INLINE mapToRepeated #-}

instance ToRepeated (Data.IntMap.Lazy.IntMap a)
  where
    type RepeatedElement (Data.IntMap.Lazy.IntMap a) = (Int, a)
    toRepeated xs = UnsafeReverseN (Data.IntMap.Lazy.size xs) (Forward f xs)
      where
        f op = Data.IntMap.Lazy.foldlWithKey (\a k v -> op (k, v) a)
        {-# INLINE f #-}
    {-# INLINE toRepeated #-}

-- | Simply records the lazy 'foldr' along with the sequence.
foldableToForward :: Foldable t => t a -> Forward a
foldableToForward xs = Forward foldr xs
{-# INLINE foldableToForward #-}

mapFoldableToForward :: Foldable t => (e -> a) -> t e -> Forward a
mapFoldableToForward m xs = fmap m (foldableToForward xs)
{-# INLINE mapFoldableToForward #-}

-- | Like 'foldableToReverse' but accepts a length predictor.
-- Unsafe because the caller must ensure the accuracy of that predictor.
unsafeFoldableToReverseN :: Foldable t => (t a -> Int) -> t a -> ReverseN a
unsafeFoldableToReverseN count xs = UnsafeReverseN (count xs) (Forward (foldl . flip) xs)
{-# INLINE unsafeFoldableToReverseN #-}

unsafeMapFoldableToReverseN :: Foldable t => (t e -> Int) -> (e -> a) -> t e -> ReverseN a
unsafeMapFoldableToReverseN count m xs = fmap m (unsafeFoldableToReverseN count xs)
{-# INLINE unsafeMapFoldableToReverseN #-}

vectorToReverseN :: Data.Vector.Generic.Vector v a => v a -> ReverseN a
vectorToReverseN xs = UnsafeReverseN (Data.Vector.Generic.length xs) (Forward f xs)
  where
    f op z v = Data.Vector.Generic.foldr op z (Data.Vector.Generic.reverse v)
    {-# INLINE f #-}
      -- We expect vector fusion rewrite rules to avoid the need to actually
      -- create the reversed vector, and instead fold over a @Bundle@ that
      -- consumes elements of the original vector from right to left.
{-# INLINE vectorToReverseN #-}

mapVectorToReverseN :: Data.Vector.Generic.Vector v e => (e -> a) -> v e -> ReverseN a
mapVectorToReverseN m xs = fmap m (vectorToReverseN xs)
{-# INLINE mapVectorToReverseN #-}
