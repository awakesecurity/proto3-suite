{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Encodes to protobuf directly from application-specific source data without
-- an intermediate value of a type generated from protobuf message definitions.
--
-- /WARNING/: This module is experimental and breaking changes may occur much more
-- frequently than in the other modules of this package, perhaps even in patch releases.
module Proto3.Suite.Form.Encode
  ( Encoding(..)
  , toLazyByteString
  , Prefix
  , Distinct
  , FindField
  , messageFromFields
  , Repetition(..)
  , IsPacked
  , RepetitionOf
  , RepetitionOfField
  , LeafRepetition
  , LeafElement
  , Occupies
  , omitted
  , IsMessage
  , submessage
  , optionalSubmessage
  , submessagesF
  , submessagesR
  , submessagesV
  , submessageFromFields
  , wrappedField
  , wrapped
  , leaf
  , Leaf(..)
  , leavesF
  , leavesR
  , leavesV
  , Leaves(..)
  , PackedPrimitives
  , foldPrefixF
  , foldPrefixR
  , foldPrefixV
  , encodeField
  ) where

import Control.Category (Category(..))
import Data.ByteString.Lazy qualified as BL
import Data.Int (Int32, Int64)
import Data.Kind (Type)
import Data.Map qualified as M
import Data.Type.Bool (If)
import Data.Vector.Generic (Vector)
import qualified Data.Vector.Generic
import Data.Word (Word32, Word64)
import GHC.Exts (Constraint, Proxy#, proxy#)
import GHC.TypeLits (ErrorMessage(..), KnownNat, Nat, Symbol, TypeError, natVal')
import Google.Protobuf.Wrappers.Polymorphic (Wrapped(..))
import Prelude hiding ((.), id)
import Proto3.Suite.Class (HasDefault(..), MessageField(..), Primitive(..), zigZagEncode)
import Proto3.Suite.Form (FieldForm(..), FieldFormsOf)
import Proto3.Suite.Types
         (Enumerated(..), Fixed(..), Nested, NestedVec, PackedVec, Signed(..), UnpackedVec)
import Proto3.Wire.Class (ProtoEnum(..))
import Proto3.Wire.Encode qualified as Encode
import Proto3.Wire.Types (FieldNumber(..))
import Witch (From(..))

-- | Annotates 'Encode.MessageBuilder' with the type of protobuf message it encodes.
-- When it is a submessage, the field number, tag, and length are not written by the builder.
newtype Encoding (a :: Type) = UnsafeEncoding { untypedEncoding :: Encode.MessageBuilder }

type role Encoding nominal

-- | Serialize a message (or portion thereof) as a lazy 'BL.ByteString'.
toLazyByteString :: forall a . Encoding a -> BL.ByteString
toLazyByteString = Encode.toLazyByteString . untypedEncoding

-- | A 'Category' on builders that prefix zero or more fields to a message.
-- Use '.' to accumulate prefixes to create an 'Encoding' for a whole message.
--
-- The first type parameter specifies the type of message being built.
--
-- The second and third type parameters list the names of some subset
-- of the oneofs and non-repeatable non-oneof fields possible within
-- the type of message specified by the first type parameter.  The
-- third type parameter must be a suffix of the second, and limits
-- the oneofs and non-repeatable non-oneof fields that may occur in
-- any builder which follows the contained builder.  The first parameter
-- prefixes the names of the oneofs and non-repeatable non-oneof
-- fields that /might/ be written by the contained builder.
--
-- If a name ends up listed more than once, that will eventually
-- be detected as a compilation error; see type family 'Distinct'.
--
-- Note that this type system permits multiple blocks of the same
-- packed repeated field.  Though that would be less compact than
-- a single block, it is allowed by the protobuf standard.
newtype Prefix (message :: Type) (possible :: [Symbol]) (following :: [Symbol]) =
  UnsafePrefix { untypedPrefix :: Encode.MessageBuilder }

type role Prefix nominal nominal nominal

-- | The 'Category' on prefixes of a particular type of
-- message whose '.' is '<>' on the contained builders.
--
-- Note that '.' preserves the requirements
-- on the type parameters of 'Prefix'.
instance Category (Prefix message)
  where
    id = UnsafePrefix mempty
    f . g = UnsafePrefix (untypedPrefix f <> untypedPrefix g)

type family Omits (name :: k) (original :: [k]) :: Bool
  where
    Omits name '[] = 'True
    Omits name (name ': names) = 'False
    Omits name (other ': names) = Omits name names

type family Clashing (names :: [k]) :: [k]
  where
    Clashing '[] = '[]
    Clashing (name ': names) = If (Omits name names) (Clashing names) (name ': Clashing names)

type family DistinctCheck (message :: Type) (clashing :: [k]) :: Constraint
  where
    DistinctCheck _ '[] = ()
    DistinctCheck message clashing = TypeError
      ( 'ShowType message ':<>: 'Text " forbids repetition of these fields:"
        ':$$: 'ShowType clashing )

type Distinct (message :: Type) (names :: [k]) = DistinctCheck message (Clashing names)

type family FindFieldIn (name :: Symbol) (fields :: [(Symbol, FieldForm)])
            :: Maybe (Symbol, FieldForm)
  where
    FindFieldIn name ('(name, form) ': _) = 'Just '(name, form)
    FindFieldIn name ('(fnm, 'FieldForm num ty name) ': _) = 'Just '(fnm, 'FieldForm num ty name)
    FindFieldIn name (_ ': fields) = FindFieldIn name fields
    FindFieldIn name '[] = 'Nothing

type family FindFieldCheck (name :: Symbol) (message :: Type) (mf :: Maybe (Symbol, FieldForm))
              :: (Symbol, FieldForm)
  where
    FindFieldCheck _ _ ('Just form) = form
    FindFieldCheck name message 'Nothing = TypeError
      ( 'ShowType name ':<>: 'Text " not found among the fields and oneofs of message type "
        ':<>: 'ShowType message )

-- | Yields 'Just' of the format of the a field whose field
-- name or @oneof@ name matches the given name, unless there
-- is no such match, in which case the result is 'Nothing'.
--
-- Multiple matches are possible only when matching the name
-- of a @oneof@.  In that case the result is suitable only
-- for functions such as 'omitted' that work with any match.
type FindField (name :: Symbol) (message :: Type) =
  FindFieldCheck name message (FindFieldIn name (FieldFormsOf message))

-- | Relabels a prefix of fields of a message as an encoding for
-- the message as a whole (though without any tag or length that
-- would make it a submessage).
messageFromFields ::
  forall (message :: Type) (names :: [Symbol]) .
  Distinct message names =>
  Prefix message '[] names ->
  Encoding message
messageFromFields = UnsafeEncoding . untypedPrefix

data Repetition
  = Singular Symbol Bool
      -- ^ Forbid repetition of this name (the @oneof@ name if any, otherwise the field name).
      -- The 'Bool' field indicates whether default values should be explicitly encoded;
      -- it is 'True' for @oneof@ fields and 'False' otherwise.
  | Repeated Bool
      -- ^ Allow repetition, preferring packing if and only if the 'Bool' is 'True'.

type family IsPacked (repetition :: Repetition) :: Bool
  where
    IsPacked ('Singular name forceEmit) = 'False
    IsPacked ('Repeated packed) = packed

-- | Assumes that the field in question is NOT within a @oneof@.
type family RepetitionOfNonOneOf (name :: Symbol) (ty :: Type) :: (Repetition, Type)
  where
    RepetitionOfNonOneOf _ (PackedVec e) = '( 'Repeated 'True, e )
    RepetitionOfNonOneOf _ (UnpackedVec e) = '( 'Repeated 'False, e )
    RepetitionOfNonOneOf _ (NestedVec e) = '( 'Repeated 'False, e )
    RepetitionOfNonOneOf _ (M.Map k v) = '( 'Repeated 'False, (k, v) )
    RepetitionOfNonOneOf name (Nested m) = '( 'Singular name 'False, m )
    RepetitionOfNonOneOf name ty = '( 'Singular name 'False, ty )

-- | Determines whether and how a field may be repeated,
-- and the formatting of a single occurrence of that field.
type family RepetitionOf (name :: Symbol) (ty :: Type) (oneof :: Symbol) :: (Repetition, Type)
  where
    RepetitionOf name ty "" = RepetitionOfNonOneOf name ty
    RepetitionOf _ ty oneof = '( 'Singular oneof 'True, ty )

type family RepetitionOfField (name :: Symbol) (message :: Type) (namedForm :: (Symbol, FieldForm))
                              :: (Repetition, Type)
  where
    RepetitionOfField name message '(name, 'FieldForm _ ty oneof) = RepetitionOf name ty oneof
    RepetitionOfField name message _ = TypeError
      ( 'ShowType name ':<>: 'Text " is a oneof in message type " ':<>: 'ShowType message
        ':$$: 'Text "and therefore you must specify which of its fields to encode" )

type LeafRepetition (name :: Symbol) (message :: Type) =
  Fst (RepetitionOfField name message (FindField name message))

type LeafElement (name :: Symbol) (message :: Type) =
  Snd (RepetitionOfField name message (FindField name message))

type family Fst (p :: (j, k)) :: j
  where
    Fst '(x, _) = x

type family Snd (p :: (j, k)) :: k
  where
    Snd '(_, y) = y

class Occupies (repetition :: Repetition) (names :: [Symbol]) (moreNames :: [Symbol])
  | repetition names -> moreNames, repetition moreNames -> names

instance Occupies ('Singular name forceEmit) names (name ': names)

instance Occupies ('Repeated packed) names names

-- | The term expressing the same field number as the given type.
reflectFieldNumber :: forall (num :: Nat) . KnownNat num => FieldNumber
reflectFieldNumber = FieldNumber (fromInteger (natVal' (proxy# :: Proxy# num)))

-- | Uses an empty encoding for a message field, thereby
-- expressing its default value unless it is a repeated
-- field, in which case the effect is the same as 'id'.
--
-- To omit an entire @oneof@, omit it by its
-- own name or by the name of any of its fields.
--
-- This function is not needed for repeated fields,
-- and for other fields is needed only to match the
-- type of another code path in which the same field
-- has a nonempty encoding.
omitted ::
  forall (name :: Symbol) (message :: Type) (names :: [Symbol])
         (fieldName :: Symbol) (num :: Nat) (ty :: Type) (oneof :: Symbol)
         (repetition :: Repetition) (elem :: Type) (moreNames :: [Symbol]) .
  ( FindField name message ~ '(fieldName, 'FieldForm num ty oneof)
  , RepetitionOf name ty oneof ~ '(repetition, elem)
  , Occupies repetition names moreNames
  , KnownNat num
  ) =>
  Prefix message names moreNames
omitted = UnsafePrefix mempty

type family WHNFList (xs :: [k]) :: Constraint
  where
    WHNFList (h ': t) = ()
    WHNFList '[] = ()

-- | Message types are those for which 'FieldFormsOf' has been instantiated.
type IsMessage (ty :: Type) = WHNFList (FieldFormsOf ty)

-- | Prefixes the encoding of a submessage with its tag and length.
--
-- See Also: 'submessageFromFields', 'optionalSubmessage',
-- 'submessagesF', 'submessagesR', 'submessagesV'
submessage ::
  forall (name :: Symbol) (message :: Type) (names :: [Symbol])
         (num :: Nat) (ty :: Type) (oneof :: Symbol)
         (repetition :: Repetition) (elem :: Type) (moreNames :: [Symbol]) .
  ( FindField name message ~ '(name, 'FieldForm num ty oneof)
  , RepetitionOf name ty oneof ~ '(repetition, elem)
  , Occupies repetition names moreNames
  , KnownNat num
  , IsMessage elem
  ) =>
  Encoding elem ->
  Prefix message names moreNames
submessage = UnsafePrefix . Encode.embedded (reflectFieldNumber @num) . untypedEncoding
{-# INLINE submessage #-}

-- | Prefixes the encoding of a submessage with its tag and length,
-- but omits the entire encoding if the argument is 'Nothing'.
optionalSubmessage ::
  forall (name :: Symbol) (a :: Type) (message :: Type) (names :: [Symbol])
         (num :: Nat) (ty :: Type) (oneof :: Symbol)
         (occupied :: Symbol) (forceEmit :: Bool) (elem :: Type) .
  ( FindField name message ~ '(name, 'FieldForm num ty oneof)
  , RepetitionOf name ty oneof ~ '( 'Singular occupied forceEmit, elem )
  , KnownNat num
  , IsMessage elem
  ) =>
  (a -> Encoding elem) ->
  Maybe a ->
  Prefix message names (occupied ': names)
optionalSubmessage f = maybe (omitted @name) (submessage @name . f)
{-# INLINE optionalSubmessage #-}

-- | Like 'submessage' but for repeated submessage fields.
--
-- For greater efficiency, consider 'submessagesR' or  'submessagesV'.
submessagesF ::
  forall (name :: Symbol) (a :: Type) (t :: Type -> Type) (message :: Type) (names :: [Symbol])
         (num :: Nat) (ty :: Type) (oneof :: Symbol) (elem :: Type) .
  ( FindField name message ~ '(name, 'FieldForm num ty oneof)
  , RepetitionOf name ty oneof ~ '( 'Repeated 'False, elem )
  , KnownNat num
  , IsMessage elem
  , Foldable t
  ) =>
  (a -> Encoding elem) ->
  t a ->
  Prefix message names names
submessagesF f = foldPrefixF (submessage @name . f)
{-# INLINE submessagesF #-}

-- | Like 'submessagesF' but reverses the order of the submessagse.
submessagesR ::
  forall (name :: Symbol) (a :: Type) (t :: Type -> Type) (message :: Type) (names :: [Symbol])
         (num :: Nat) (ty :: Type) (oneof :: Symbol) (elem :: Type) .
  ( FindField name message ~ '(name, 'FieldForm num ty oneof)
  , RepetitionOf name ty oneof ~ '( 'Repeated 'False, elem )
  , KnownNat num
  , IsMessage elem
  , Foldable t
  ) =>
  (a -> Encoding elem) ->
  t a ->
  Prefix message names names
submessagesR f = foldPrefixR (submessage @name . f)
{-# INLINE submessagesR #-}

-- | Like 'submessagesF' but requires a 'Vector'.
submessagesV ::
  forall (name :: Symbol) (a :: Type) (v :: Type -> Type) (message :: Type) (names :: [Symbol])
         (num :: Nat) (ty :: Type) (oneof :: Symbol) (elem :: Type) .
  ( FindField name message ~ '(name, 'FieldForm num ty oneof)
  , RepetitionOf name ty oneof ~ '( 'Repeated 'False, elem )
  , KnownNat num
  , IsMessage elem
  , Vector v a
  ) =>
  (a -> Encoding elem) ->
  v a ->
  Prefix message names names
submessagesV f = foldPrefixV (submessage @name . f)
{-# INLINE submessagesV #-}

-- | A convenience function that applies 'submessage'
-- to the encoding yielded by 'messageFromFields'.
submessageFromFields ::
  forall (name :: Symbol) (outerMessage :: Type) (names :: [Symbol])
         (num :: Nat) (ty :: Type) (oneof :: Symbol)
         (repetition :: Repetition) (innerMessage :: Type) (moreNames :: [Symbol])
         (innerNames :: [Symbol]) .
  ( FindField name outerMessage ~ '(name, 'FieldForm num ty oneof)
  , RepetitionOf name ty oneof ~ '(repetition, innerMessage)
  , Occupies repetition names moreNames
  , IsPacked repetition ~ 'False
  , KnownNat num
  , IsMessage innerMessage
  , Distinct innerMessage innerNames
  ) =>
  Prefix innerMessage '[] innerNames ->
  Prefix outerMessage names moreNames
submessageFromFields = submessage @name . messageFromFields
{-# INLINE submessageFromFields #-}

-- | Encodes a submessage field whose message type is
-- a standard protobuf wrapper from an optional value.
-- The submessage is omitted in the 'Nothing' case.
wrappedField ::
  forall (name :: Symbol) (message :: Type) (names :: [Symbol])
         (num :: Nat) (ty :: Type) (oneof :: Symbol)
         (occupied :: Symbol) (forceEmit :: Bool) (elem :: Type) .
  ( FindField name message ~ '(name, 'FieldForm num ty oneof)
  , RepetitionOf name ty oneof ~ '( 'Singular occupied forceEmit, Wrapped elem )
  , RepetitionOfNonOneOf "value" elem ~ '( 'Singular "value" 'False, elem )
  , KnownNat num
  , HasDefault elem
  , Primitive elem
  ) =>
  Maybe elem ->
  Prefix message names (occupied ': names)
wrappedField = optionalSubmessage @name (messageFromFields . leafField @"value")
{-# INLINE wrappedField #-}

-- | Short for @'wrappedField' \@name . 'fmap' \@Maybe 'from'@.
wrapped ::
  forall (name :: Symbol) (a :: Type) (message :: Type) (names :: [Symbol])
         (num :: Nat) (ty :: Type) (oneof :: Symbol)
         (occupied :: Symbol) (forceEmit :: Bool) (elem :: Type) .
  ( FindField name message ~ '(name, 'FieldForm num ty oneof)
  , RepetitionOf name ty oneof ~ '( 'Singular occupied forceEmit, Wrapped elem )
  , RepetitionOfNonOneOf "value" elem ~ '( 'Singular "value" 'False, elem )
  , KnownNat num
  , HasDefault elem
  , Primitive elem
  , From a elem
  ) =>
  Maybe a ->
  Prefix message names (occupied ': names)
wrapped = wrappedField @name . fmap @Maybe from
{-# INLINE wrapped #-}

-- | Short for @'leafField' \@name . 'from'@.
leaf ::
  forall (name :: Symbol) (a :: Type) (message :: Type) (names :: [Symbol])
         (moreNames :: [Symbol]) .
  ( Leaf name message
  , From a (LeafElement name message)
  , Occupies (LeafRepetition name message) names moreNames
  ) =>
  a ->
  Prefix message names moreNames
leaf = leafField @name . from
{-# INLINE leaf #-}

-- | Inhabited by leaf fields (those that are not submessages)
-- whose values can be emitted individually.
--
-- Unpacked repeated fields may be encoded in this way and/or with
-- type class 'Leaves', but packed repeated fields require 'Leaves'.
class (IsPacked (LeafRepetition name message) ~ 'False) =>
      Leaf (name :: Symbol) (message :: Type)
  where
    -- | Encodes a field with the given value.
    --
    -- If the field is neither repeated nor part of a oneof, then its default
    -- value is represented implicitly--that is, it encodes to zero octets.
    leafField ::
      forall (names :: [Symbol]) (moreNames :: [Symbol]) .
      Occupies (LeafRepetition name message) names moreNames =>
      LeafElement name message ->
      Prefix message names moreNames

instance ( LeafImpl name message num ty oneof repetition elem
         , IsPacked repetition ~ 'False
         ) =>
         Leaf name message
  where
    leafField = leafImpl @name

class ( FindField name message ~ '(name, 'FieldForm num ty oneof)
      , RepetitionOf name ty oneof ~ '(repetition, elem)
      , IsPacked repetition ~ 'False
      ) =>
      LeafImpl (name :: Symbol) (message :: Type)
               (num :: Nat) (ty :: Type) (oneof :: Symbol)
               (repetition :: Repetition) (elem :: Type)
  where
    leafImpl ::
      forall (names :: [Symbol]) (moreNames :: [Symbol]) .
      Occupies repetition names moreNames =>
      elem ->
      Prefix message names moreNames

instance ( FindField name message ~ '(name, 'FieldForm num ty oneof)
         , RepetitionOf name ty oneof ~ '( 'Singular occupied 'False, elem )
         , KnownNat num
         , HasDefault elem
         , Primitive elem
         ) =>
         LeafImpl name message num ty oneof ('Singular occupied 'False) elem
  where
    leafImpl value
      | isDefault value = UnsafePrefix mempty
      | otherwise = UnsafePrefix (encodePrimitive (reflectFieldNumber @num) value)
    {-# INLINE leafImpl #-}

instance ( FindField name message ~ '(name, 'FieldForm num ty oneof)
         , RepetitionOf name ty oneof ~ '( 'Singular occupied 'True, elem )
         , KnownNat num
         , Primitive elem
         ) =>
         LeafImpl name message num ty oneof ('Singular occupied 'True) elem
  where
    leafImpl value = UnsafePrefix (encodePrimitive (reflectFieldNumber @num) value)
    {-# INLINE leafImpl #-}

instance ( FindField name message ~ '(name, 'FieldForm num ty oneof)
         , RepetitionOf name ty oneof ~ '( 'Repeated 'False, elem )
         , KnownNat num
         , Primitive elem
         ) =>
         LeafImpl name message num ty oneof ('Repeated 'False) elem
  where
    leafImpl value = UnsafePrefix (encodePrimitive (reflectFieldNumber @num) value)
    {-# INLINE leafImpl #-}

-- | Short for @'leavesFOn' \@name 'from'@.
leavesF ::
  forall (name :: Symbol) (a :: Type) (t :: Type -> Type) (message :: Type) (names :: [Symbol]) .
  ( Leaves name message
  , From a (LeafElement name message)
  , Foldable t
  ) =>
  t a ->
  Prefix message names names
leavesF = leavesFOn @name from
{-# INLINE leavesF #-}

-- | Short for @'leavesROn' \@name 'from'@.
leavesR ::
  forall (name :: Symbol) (a :: Type) (t :: Type -> Type) (message :: Type) (names :: [Symbol]) .
  ( Leaves name message
  , From a (LeafElement name message)
  , Foldable t
  ) =>
  t a ->
  Prefix message names names
leavesR = leavesROn @name from
{-# INLINE leavesR #-}

-- | Short for @'leavesVOn' \@name 'from'@.
leavesV ::
  forall (name :: Symbol) (a :: Type) (v :: Type -> Type) (message :: Type) (names :: [Symbol]) .
  ( Leaves name message
  , From a (LeafElement name message)
  , Vector v a
  ) =>
  v a ->
  Prefix message names names
leavesV = leavesVOn @name from
{-# INLINE leavesV #-}

-- | Inhabited by leaf fields (those that are not submessages)
-- that are repeated, whether or not they are packed.
--
-- Unpacked repeated fields may also be encoded using type class 'Leaf'.
-- Non-repeated fields are not supported; use 'Leaf' instead.
class Leaves (name :: Symbol) (message :: Type)
  where
    -- | Encodes a repeated field with the given sequence of values,
    -- but first applies the given function to each value.
    --
    -- If there are zero values to map and encode, then the sequence
    -- is represented implicitly--that is, it encodes to zero octets.
    --
    -- NOTE: 'leavesROn' and 'leavesVOn' are probably more efficient.
    leavesFOn ::
      forall (a :: Type) (t :: Type -> Type) (names :: [Symbol]) .
      Foldable t =>
      (a -> LeafElement name message) ->
      t a ->
      Prefix message names names

    -- | Like 'leavesFOn' but reverses the order of the values,
    -- which allows it to be more efficient in many cases,
    -- though 'leavesVOn' may be even faster in some cases.
    leavesROn ::
      forall (a :: Type) (t :: Type -> Type) (names :: [Symbol]) .
      Foldable t =>
      (a -> LeafElement name message) ->
      t a ->
      Prefix message names names

    -- | Like 'leavesFOn' but requires a 'Vector', which may be more
    -- efficient than even 'leavesROn' but only when starting from
    -- a 'Vector'.  (It is inefficient to create a temporary 'Vector'
    -- from some other 'Foldable' container just to use this method.)
    leavesVOn ::
      forall (a :: Type) (v :: Type -> Type) (names :: [Symbol]) .
      Vector v a =>
      (a -> LeafElement name message) ->
      v a ->
      Prefix message names names

instance LeavesImpl name message num ty oneof repetition elem packed =>
         Leaves name message
  where
    leavesFOn f xs
      | null xs = UnsafePrefix mempty
      | otherwise = leavesFOnImpl @name f xs

    leavesROn f xs
      | null xs = UnsafePrefix mempty
      | otherwise = leavesROnImpl @name f xs

    leavesVOn f xs
      | Data.Vector.Generic.null xs = UnsafePrefix mempty
      | otherwise = leavesVOnImpl @name f xs

class ( FindField name message ~ '(name, 'FieldForm num ty oneof)
      , RepetitionOf name ty oneof ~ '(repetition, elem)
      , LeafRepetition name message ~ 'Repeated packed
      ) =>
      LeavesImpl (name :: Symbol) (message :: Type)
                 (num :: Nat) (ty :: Type) (oneof :: Symbol)
                 (repetition :: Repetition) (elem :: Type) (packed :: Bool)
  where
    -- | Implements 'leavesFOn' except for the check for an empty sequence.
    leavesFOnImpl ::
      forall (a :: Type) (t :: Type -> Type) (names :: [Symbol]) .
      Foldable t =>
      (a -> elem) ->
      t a ->
      Prefix message names names

    -- | Implements 'leavesROn' except for the check for an empty sequence.
    leavesROnImpl ::
      forall (a :: Type) (t :: Type -> Type) (names :: [Symbol]) .
      Foldable t =>
      (a -> elem) ->
      t a ->
      Prefix message names names

    -- | Implements 'leavesFOn' except for the check for an empty vector.
    leavesVOnImpl ::
      forall (a :: Type) (v :: Type -> Type) (names :: [Symbol]) .
      Vector v a =>
      (a -> elem) ->
      v a ->
      Prefix message names names

instance ( FindField name message ~ '(name, 'FieldForm num ty oneof)
         , RepetitionOf name ty oneof ~ '( 'Repeated 'False, elem )
         , KnownNat num
         , Primitive elem
         ) =>
         LeavesImpl name message num ty oneof ('Repeated 'False) elem 'False
  where
    leavesFOnImpl f = foldPrefixF (leafField @name . f)
    {-# INLINE leavesFOnImpl #-}

    leavesROnImpl f = foldPrefixR (leafField @name . f)
    {-# INLINE leavesROnImpl #-}

    leavesVOnImpl f = foldPrefixV (leafField @name . f)
    {-# INLINE leavesVOnImpl #-}

instance ( FindField name message ~ '(name, 'FieldForm num ty oneof)
         , RepetitionOf name ty oneof ~ '(repetition, elem)
         , LeafRepetition name message ~ 'Repeated 'True
         , KnownNat num
         , PackedPrimitives elem
         ) =>
         LeavesImpl name message num ty oneof ('Repeated 'True) elem 'True
  where
    leavesFOnImpl f = UnsafePrefix . packedPrimitivesF f (reflectFieldNumber @num)
    {-# INLINE leavesFOnImpl #-}

    leavesROnImpl f = UnsafePrefix . packedPrimitivesR f (reflectFieldNumber @num)
    {-# INLINE leavesROnImpl #-}

    leavesVOnImpl f = UnsafePrefix . packedPrimitivesV f (reflectFieldNumber @num)
    {-# INLINE leavesVOnImpl #-}

class PackedPrimitives (a :: Type)
  where
    -- | Encode the elements of the given foldable container as a packed repeated field.
    --
    -- It is ASSUMED that the caller already excluded the empty-sequence case.
    --
    -- NOTE: 'packedPrimitivesR' is probably faster than 'packedPrimitivesF'
    -- because 'Encode.MessageBuilder' encodes in reverse.
    packedPrimitivesF :: Foldable f => (b -> a) -> FieldNumber -> f b -> Encode.MessageBuilder

    -- | Like 'packedPrimitivesF' but reverses the order of the elements.
    --
    -- NOTE: 'packedPrimitivesR' is probably faster than 'packedPrimitivesF'
    -- because 'Encode.MessageBuilder' encodes in reverse.
    packedPrimitivesR :: Foldable f => (b -> a) -> FieldNumber -> f b -> Encode.MessageBuilder

    -- | A faster but more specialized variant of 'packedPrimitivesF'.
    packedPrimitivesV :: Vector v b => (b -> a) -> FieldNumber -> v b -> Encode.MessageBuilder

instance PackedPrimitives Bool
  where
    packedPrimitivesF = Encode.packedBoolsF
    packedPrimitivesR = Encode.packedBoolsR
    packedPrimitivesV = Encode.packedBoolsV

instance PackedPrimitives Word64
  where
    packedPrimitivesF = Encode.packedVarintsF
    packedPrimitivesR = Encode.packedVarintsR
    packedPrimitivesV = Encode.packedVarintsV

instance PackedPrimitives Word32
  where
    packedPrimitivesF f = packedPrimitivesF (fromIntegral @Word32 @Word64 . f)
    packedPrimitivesR f = packedPrimitivesR (fromIntegral @Word32 @Word64 . f)
    packedPrimitivesV f = packedPrimitivesV (fromIntegral @Word32 @Word64 . f)

-- | NOTE: Converts to 'Word64' as before encoding, which is correct.
-- To quote <https://protobuf.dev/programming-guides/encoding/#signed-ints>,
-- "The intN types encode negative numbers as two’s complement,
-- which means that, as unsigned, 64-bit integers, they have their highest
-- bit set.  As a result, this means that all ten bytes must be used."
instance PackedPrimitives Int32
  where
    packedPrimitivesF f = packedPrimitivesF (fromIntegral @Int32 @Word64 . f)
    packedPrimitivesR f = packedPrimitivesR (fromIntegral @Int32 @Word64 . f)
    packedPrimitivesV f = packedPrimitivesV (fromIntegral @Int32 @Word64 . f)

instance PackedPrimitives Int64
  where
    packedPrimitivesF f = packedPrimitivesF (fromIntegral @Int64 @Word64 . f)
    packedPrimitivesR f = packedPrimitivesR (fromIntegral @Int64 @Word64 . f)
    packedPrimitivesV f = packedPrimitivesV (fromIntegral @Int64 @Word64 . f)

instance PackedPrimitives (Signed Int32)
  where
    packedPrimitivesF f = packedPrimitivesF (zigZagEncode . f)
    {-# INLINE packedPrimitivesF #-}

    packedPrimitivesR f = packedPrimitivesR (zigZagEncode . f)
    {-# INLINE packedPrimitivesR #-}

    packedPrimitivesV f = packedPrimitivesV (zigZagEncode . f)
    {-# INLINE packedPrimitivesV #-}

instance PackedPrimitives (Signed Int64)
  where
    packedPrimitivesF f = packedPrimitivesF (zigZagEncode . f)
    {-# INLINE packedPrimitivesF #-}

    packedPrimitivesR f = packedPrimitivesR (zigZagEncode . f)
    {-# INLINE packedPrimitivesR #-}

    packedPrimitivesV f = packedPrimitivesV (zigZagEncode . f)
    {-# INLINE packedPrimitivesV #-}

instance PackedPrimitives (Fixed Word32)
  where
    packedPrimitivesF f = Encode.packedFixed32F (fixed . f)
    packedPrimitivesR f = Encode.packedFixed32R (fixed . f)
    packedPrimitivesV f = Encode.packedFixed32V (fixed . f)

instance PackedPrimitives (Fixed Word64)
  where
    packedPrimitivesF f = Encode.packedFixed64F (fixed . f)
    packedPrimitivesR f = Encode.packedFixed64R (fixed . f)
    packedPrimitivesV f = Encode.packedFixed64V (fixed . f)

instance PackedPrimitives (Signed (Fixed Int32))
  where
    packedPrimitivesF f = packedPrimitivesF (fixed32ToUnsigned . f)
    {-# INLINE packedPrimitivesF #-}

    packedPrimitivesR f = packedPrimitivesR (fixed32ToUnsigned . f)
    {-# INLINE packedPrimitivesR #-}

    packedPrimitivesV f = packedPrimitivesV (fixed32ToUnsigned . f)
    {-# INLINE packedPrimitivesV #-}

fixed32ToUnsigned :: Signed (Fixed Int32) -> Fixed Word32
fixed32ToUnsigned = Fixed . fromIntegral @Int32 @Word32 . fixed . signed
{-# INLINE fixed32ToUnsigned #-}

instance PackedPrimitives (Signed (Fixed Int64))
  where
    packedPrimitivesF f = packedPrimitivesF (fixed64ToUnsigned . f)
    {-# INLINE packedPrimitivesF #-}

    packedPrimitivesR f = packedPrimitivesR (fixed64ToUnsigned . f)
    {-# INLINE packedPrimitivesR #-}

    packedPrimitivesV f = packedPrimitivesV (fixed64ToUnsigned . f)
    {-# INLINE packedPrimitivesV #-}

fixed64ToUnsigned :: Signed (Fixed Int64) -> Fixed Word64
fixed64ToUnsigned = Fixed . fromIntegral @Int64 @Word64 . fixed . signed
{-# INLINE fixed64ToUnsigned #-}

instance PackedPrimitives Float
  where
    packedPrimitivesF = Encode.packedFloatsF
    packedPrimitivesR = Encode.packedFloatsR
    packedPrimitivesV = Encode.packedFloatsV

instance PackedPrimitives Double
  where
    packedPrimitivesF = Encode.packedDoublesF
    packedPrimitivesR = Encode.packedDoublesR
    packedPrimitivesV = Encode.packedDoublesV

instance ProtoEnum e => PackedPrimitives (Enumerated e)
  where
    packedPrimitivesF f = packedPrimitivesF (codeFromEnumerated . f)
    {-# INLINE packedPrimitivesF #-}

    packedPrimitivesR f = packedPrimitivesR (codeFromEnumerated . f)
    {-# INLINE packedPrimitivesR #-}

    packedPrimitivesV f = packedPrimitivesV (codeFromEnumerated . f)
    {-# INLINE packedPrimitivesV #-}

-- | Pass through those values that are outside the enum range;
-- this is for forward compatibility as enumerations are extended.
codeFromEnumerated :: ProtoEnum e => Enumerated e -> Int32
codeFromEnumerated = either id fromProtoEnum . enumerated
{-# INLINE codeFromEnumerated #-}

-- | Applies a function that yields the encoding of repeatable field(s)
-- at every element of a collection and combines the results in order.
--
-- For efficiency, consider using 'foldPrefixR' or 'foldPrefixV' instead.
foldPrefixF ::
  forall t a message names .
  Foldable t =>
  (a -> Prefix message names names) ->
  t a ->
  Prefix message names names
foldPrefixF f =
  UnsafePrefix . Encode.etaMessageBuilder (foldr ((<>) . untypedPrefix . f) mempty)
{-# INLINE foldPrefixF #-}

-- | Applies a function that yields the encoding of repeatable field(s)
-- at every element of a collection and combines the results in /reverse/ order.
--
-- For vectors, consider 'foldPrefixV'.
foldPrefixR ::
  forall t a message names .
  Foldable t =>
  (a -> Prefix message names names) ->
  t a ->
  Prefix message names names
foldPrefixR f =
  UnsafePrefix . Encode.etaMessageBuilder (foldr (flip (<>) . untypedPrefix . f) mempty)
{-# INLINE foldPrefixR #-}

-- | Equivalent to 'foldPrefixF' specialized to vectors, but iterates right to left for efficiency.
foldPrefixV ::
  forall v a message names .
  Vector v a =>
  (a -> Prefix message names names) ->
  v a ->
  Prefix message names names
foldPrefixV f =
  UnsafePrefix . Encode.vectorMessageBuilder @v @a (untypedPrefix . f)
{-# INLINE foldPrefixV #-}

-- | Delegates field encoding to 'MessageField' at the cost
-- of requiring the relevant intermediate representation.
--
-- Repeated fields must be supplied as
-- the appropriately-typed collection.
encodeField ::
  forall (name :: Symbol) (message :: Type) (names :: [Symbol])
         (num :: Nat) (ty :: Type) (oneof :: Symbol)
         (repetition :: Repetition) (elem :: Type) (moreNames :: [Symbol]) .
  ( FindField name message ~ '(name, 'FieldForm num ty oneof)
  , RepetitionOf name ty oneof ~ '(repetition, elem)
  , Occupies repetition names moreNames
  , KnownNat num
  , MessageField ty
  ) =>
  ty ->
  Prefix message names moreNames
encodeField = UnsafePrefix . encodeMessageField (reflectFieldNumber @num)
{-# INLINE encodeField #-}
