{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Type-Level Format Information
--
-- The code generator instantiates these type families to provide type-level
-- information that may be used to define custom encoders and decoders.
--
-- /WARNING/: This module is experimental and breaking changes may occur much more
-- frequently than in the other modules of this package, perhaps even in patch releases.
module Proto3.Suite.Form
  ( NamesOf
  , NumberOf
  , ProtoTypeOf
  , OneOfOf
  , RepetitionOf
  , Presence(..)
  , Packing(..)
  , Repetition(..)
  , ProtoType(..)
  , Association
  , MappedPresence
  , Wrapper
  , Wrap(..)
  , RecoverRepetition
  , RecoverProtoType
  , MessageFieldType
  ) where

import Data.Int (Int32, Int64)
import Data.Kind (Type)
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)
import GHC.TypeLits (ErrorMessage(..), Nat, Symbol, TypeError)
import Prelude hiding (String)
import Proto3.Suite.Types (Bytes, Enumerated, Commented, Fixed, ForceEmit, Nested,
                           NestedVec, PackedVec, Signed, String, UnpackedVec)

import qualified Data.Map as M

-- | Returns the names of the fields within a given type of message.
--
-- The names appear in increasing order of field number, in case that helps
-- type-level search algorithms, though at present it is likely that there
-- is no benefit other than ensuring that declaration order is irrelevant.
--
-- Note that the names of fields and oneofs must be unique within their shared namespace
-- (see <https://protobuf.dev/reference/protobuf/proto3-spec/#message_definition>).
type family NamesOf (message :: Type) :: [Symbol]

-- | In the context of a message type, maps its field names to their numbers.
type family NumberOf (message :: Type) (name :: Symbol) :: Nat

-- | In the context of a message type, maps the name of every field to its protobuf type.
type family ProtoTypeOf (message :: Type) (name :: Symbol) :: ProtoType

-- | In the context of a message type, maps the name of every field to the name
-- of its containing @oneof@, or to @""@ if that field is not part of a oneof.
-- Also maps the name of every @oneof@ in the message to itself.
type family OneOfOf (message :: Type) (name :: Symbol) :: Symbol

-- | In the context of a message type, maps the name
-- of every field and @oneof@ to its 'Repetition'.
--
-- (Every @oneof@ in the message is mapped to 'OneOf',
-- as are those fields that are within @oneof@s.)
type family RepetitionOf (message :: Type) (name :: Symbol) :: Repetition

-- | The field presence of a field that is not @repeated@.
data Presence
  = Implicit
      -- ^ A scalar/enumerated field whose omission implies its default value.
  | Optional
      -- ^ A submessage field or an @optional@ scalar/enumerated field whose
      -- omission implies an "unset" value distinct from the default value.

-- | The packing of field that is @repeated@ or is a @map@.  Every
-- encoding appends and element, and omission means zero elements.
data Packing
  = Unpacked
      -- ^ Packing is not preferred or not supported (for example, submessages and maps).
  | Packed
      -- ^ Packing is supported and preferred (perhaps implicitly).

-- | Whether or a field is @repeated@, what it means when its encoding is omitted,
-- and if it is @repeated@, whether or not packing is preferred.
data Repetition
  = Singular Presence
      -- ^ The field is neither @repeated@, nor a @map, nor part of a @oneof@.
      -- It has the specified field presence.
  | Alternative
      -- ^ The field is part of a @oneof@.  The entire @oneof@ is either set or unset,
      -- and if it is set then a particular field of the @oneof@ has been selected, and
      -- must be encoded explicitly even if it has the default value for its type.
  | Repeated Packing
      -- ^ The field is @repeated@ or is a @map@,
      -- with the indicated packing preference.

-- | The type of a message field, but when the field is repeated, this is the element type.
--
-- Design Note: We do not use the same Haskell types recognized
-- by `Proto3.Suite.Class.MessageField` because there more than
-- one such type corresponds to protobuf type @string@.
-- But see 'RecoverProtoType'.
data ProtoType
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
  | Enumeration Type
      -- ^ The field is the type created by code generation from the protobuf enum.
  | Message Type
      -- ^ The field is the type created by code generation from the protobuf message.
  | Map ProtoType ProtoType
      -- ^ A map with the given key and value types.

-- | The type of the repeated key-value pair submessage implied by a map field.
--
-- We never need to construct values; instead we construct values of types
-- such as @`Proto3.Suite.Form.Encode.Encoding` ('Association' key value)@.
data Association (key :: ProtoType) (value :: ProtoType)

type instance NamesOf (Association key value) = '[ "key", "value" ]

type instance NumberOf (Association key value) "key" = 1
type instance NumberOf (Association key value) "value" = 2

type instance ProtoTypeOf (Association key value) "key" = key
type instance ProtoTypeOf (Association key value) "value" = value

type instance OneOfOf (Association key value) "key" = ""
type instance OneOfOf (Association key value) "value" = ""

type instance RepetitionOf (Association key value) "key" = 'Singular 'Implicit
type instance RepetitionOf (Association key value) "value" = 'Singular (MappedPresence value)

-- | Yields the field presence of a mapped value of the given protobuf type.
type family MappedPresence (protoType :: ProtoType) :: Presence
  where
    MappedPresence ('Message _) = 'Optional
    MappedPresence ('Map k v) = TypeError
      ( 'Text "Nested maps are disallowed, so this cannot be a mapped type:"
        ':$$: 'ShowType ('Map k v) )
    MappedPresence _ = 'Implicit

-- | Indicates the standard protobuf wrapper having
-- the field type given by the type argument.
--
-- We never need to construct values; instead we construct values of types
-- such as @`Proto3.Suite.Form.Encode.Encoding` ('Wrapper' protoType)@ or
-- @'Wrap' a@, where @a@ is a corresponding native representation.
--
-- Note that if Google ever adds wrappers for "sint..." or "...fixed..."
-- then this type constructor will naturally support such wrappers.
data Wrapper (protoType :: ProtoType)

type instance NamesOf (Wrapper protoType) = '[ "value" ]

type instance NumberOf (Wrapper protoType) "value" = 1

type instance ProtoTypeOf (Wrapper protoType) "value" = protoType

type instance OneOfOf (Wrapper protoType) "value" = ""

type instance RepetitionOf (Wrapper protoType) "value" = 'Singular 'Implicit

-- | Helps some type classes distinguish wrapped values from encodings of wrapper submessages.
--
-- See also 'Wrapper'.
newtype Wrap (a :: Type) = Wrap { unwrap :: a }
  deriving stock (Foldable, Functor, Generic, Traversable)
  deriving newtype (Bounded, Enum, Eq, Fractional, Integral, Ord, Num, Read, Real, Show)

-- | Given the Haskell type used by features such as `Proto3.Suite.Class.MessageField`
-- to indicate the encoding of a message field.
type family RecoverRepetition (haskellType :: Type) :: Repetition
  where
    RecoverRepetition (Commented _ haskellType) = RecoverRepetition haskellType
    RecoverRepetition (ForceEmit _) = 'Alternative
    RecoverRepetition (PackedVec _) = 'Repeated 'Packed
    RecoverRepetition (UnpackedVec _) = 'Repeated 'Unpacked
    RecoverRepetition (NestedVec _) = 'Repeated 'Unpacked
    RecoverRepetition (Nested _) = 'Singular 'Optional
    RecoverRepetition (Enumerated _) = 'Singular 'Implicit
    RecoverRepetition (M.Map _ _) = 'Repeated 'Unpacked
    RecoverRepetition Int32 = 'Singular 'Implicit
    RecoverRepetition Int64 = 'Singular 'Implicit
    RecoverRepetition (Signed Int32) = 'Singular 'Implicit
    RecoverRepetition (Signed Int64) = 'Singular 'Implicit
    RecoverRepetition Word32 = 'Singular 'Implicit
    RecoverRepetition Word64 = 'Singular 'Implicit
    RecoverRepetition (Fixed Word32) = 'Singular 'Implicit
    RecoverRepetition (Fixed Word64) = 'Singular 'Implicit
    RecoverRepetition (Signed (Fixed Int32)) = 'Singular 'Implicit
    RecoverRepetition (Signed (Fixed Int64)) = 'Singular 'Implicit
    RecoverRepetition (String _) = 'Singular 'Implicit
    RecoverRepetition (Bytes _) = 'Singular 'Implicit
    RecoverRepetition Bool = 'Singular 'Implicit
    RecoverRepetition Float = 'Singular 'Implicit
    RecoverRepetition Double = 'Singular 'Implicit
    RecoverRepetition _ = 'Alternative  -- Unnested message type implies @oneof@.

-- | Given the Haskell type used by features such as `Proto3.Suite.Class.MessageField`
-- to indicate the encoding of a message field, returns the corresponding type of kind
-- 'ProtoType', which for repeated types is the element type.  This type family is /not/
-- injective because multiple types are supported for fields of protobuf type @string@.
type family RecoverProtoType (haskellType :: Type) :: ProtoType
  where
    RecoverProtoType Int32 = 'Int32
    RecoverProtoType Int64 = 'Int64
    RecoverProtoType (Signed Int32) = 'SInt32
    RecoverProtoType (Signed Int64) = 'SInt64
    RecoverProtoType Word32 = 'UInt32
    RecoverProtoType Word64 = 'UInt64
    RecoverProtoType (Fixed Word32) = 'Fixed32
    RecoverProtoType (Fixed Word64) = 'Fixed64
    RecoverProtoType (Signed (Fixed Int32)) = 'SFixed32
    RecoverProtoType (Signed (Fixed Int64)) = 'SFixed64
    RecoverProtoType (String _) = 'String
    RecoverProtoType (Bytes _) = 'Bytes
    RecoverProtoType Bool = 'Bool
    RecoverProtoType Float = 'Float
    RecoverProtoType Double = 'Double
    RecoverProtoType (Commented _ haskellType) = RecoverProtoType haskellType
    RecoverProtoType (ForceEmit haskellType) = RecoverProtoType haskellType
    RecoverProtoType (PackedVec haskellType) = RecoverProtoType haskellType
    RecoverProtoType (UnpackedVec haskellType) = RecoverProtoType haskellType
    RecoverProtoType (Enumerated e) = 'Enumeration e
    RecoverProtoType (Nested m) = 'Message m
    RecoverProtoType (NestedVec m) = 'Message m
    RecoverProtoType (M.Map k v) = 'Map (RecoverProtoType k) (RecoverProtoType v)
    RecoverProtoType m = 'Message m

-- | Inhabited by Haskell types used by features such as `Proto3.Suite.Class.MessageField`
-- that correspond to particular protobuf types that are repeated in the specified way.
class ( RecoverRepetition haskellType ~ repetition
      , RecoverProtoType haskellType ~ protoType
      ) =>
      MessageFieldType (repetition :: Repetition) (protoType :: ProtoType) (haskellType :: Type)

instance MessageFieldType ('Singular 'Implicit) 'Int32 Int32
instance MessageFieldType ('Singular 'Implicit) 'Int64 Int64
instance MessageFieldType ('Singular 'Implicit) 'SInt32 (Signed Int32)
instance MessageFieldType ('Singular 'Implicit) 'SInt64 (Signed Int64)
instance MessageFieldType ('Singular 'Implicit) 'UInt32 (Word32)
instance MessageFieldType ('Singular 'Implicit) 'UInt64  (Word64)
instance MessageFieldType ('Singular 'Implicit) 'Fixed32 (Fixed Word32)
instance MessageFieldType ('Singular 'Implicit) 'Fixed64 (Fixed Word64)
instance MessageFieldType ('Singular 'Implicit) 'SFixed32 (Signed (Fixed Int32))
instance MessageFieldType ('Singular 'Implicit) 'SFixed64 (Signed (Fixed Int64))
instance MessageFieldType ('Singular 'Implicit) 'String (String a)
instance MessageFieldType ('Singular 'Implicit) 'Bytes (Bytes a)
instance MessageFieldType ('Singular 'Implicit) 'Bool Bool
instance MessageFieldType ('Singular 'Implicit) 'Float Float
instance MessageFieldType ('Singular 'Implicit) 'Double Double
instance MessageFieldType ('Singular 'Implicit) ('Enumeration e) (Enumerated e)
instance MessageFieldType ('Singular 'Optional) ('Message m) (Nested m)

instance MessageFieldType 'Alternative 'Int32 (ForceEmit Int32)
instance MessageFieldType 'Alternative 'Int64 (ForceEmit Int64)
instance MessageFieldType 'Alternative 'SInt32 (ForceEmit (Signed Int32))
instance MessageFieldType 'Alternative 'SInt64 (ForceEmit (Signed Int64))
instance MessageFieldType 'Alternative 'UInt32 (ForceEmit (Word32))
instance MessageFieldType 'Alternative 'UInt64 (ForceEmit (Word64))
instance MessageFieldType 'Alternative 'Fixed32 (ForceEmit (Fixed Word32))
instance MessageFieldType 'Alternative 'Fixed64 (ForceEmit (Fixed Word64))
instance MessageFieldType 'Alternative 'SFixed32 (ForceEmit (Signed (Fixed Int32)))
instance MessageFieldType 'Alternative 'SFixed64 (ForceEmit (Signed (Fixed Int64)))
instance MessageFieldType 'Alternative 'String (ForceEmit (String a))
instance MessageFieldType 'Alternative 'Bytes (ForceEmit (Bytes a))
instance MessageFieldType 'Alternative 'Bool (ForceEmit Bool)
instance MessageFieldType 'Alternative 'Float (ForceEmit Float)
instance MessageFieldType 'Alternative 'Double (ForceEmit Double)
instance MessageFieldType 'Alternative ('Enumeration e) (ForceEmit (Enumerated e))
instance ( RecoverRepetition m ~ 'Alternative
         , RecoverProtoType m ~ 'Message m
         ) =>
         MessageFieldType 'Alternative ('Message m) m

instance MessageFieldType ('Repeated 'Unpacked) 'Int32 (UnpackedVec Int32)
instance MessageFieldType ('Repeated 'Unpacked) 'Int64 (UnpackedVec Int64)
instance MessageFieldType ('Repeated 'Unpacked) 'SInt32 (UnpackedVec (Signed Int32))
instance MessageFieldType ('Repeated 'Unpacked) 'SInt64 (UnpackedVec (Signed Int64))
instance MessageFieldType ('Repeated 'Unpacked) 'UInt32 (UnpackedVec (Word32))
instance MessageFieldType ('Repeated 'Unpacked) 'UInt64 (UnpackedVec (Word64))
instance MessageFieldType ('Repeated 'Unpacked) 'Fixed32 (UnpackedVec (Fixed Word32))
instance MessageFieldType ('Repeated 'Unpacked) 'Fixed64 (UnpackedVec (Fixed Word64))
instance MessageFieldType ('Repeated 'Unpacked) 'SFixed32 (UnpackedVec (Signed (Fixed Int32)))
instance MessageFieldType ('Repeated 'Unpacked) 'SFixed64 (UnpackedVec (Signed (Fixed Int64)))
instance MessageFieldType ('Repeated 'Unpacked) 'String (UnpackedVec (String a))
instance MessageFieldType ('Repeated 'Unpacked) 'Bytes (UnpackedVec (Bytes a))
instance MessageFieldType ('Repeated 'Unpacked) 'Bool (UnpackedVec Bool)
instance MessageFieldType ('Repeated 'Unpacked) 'Float (UnpackedVec Float)
instance MessageFieldType ('Repeated 'Unpacked) 'Double (UnpackedVec Double)
instance MessageFieldType ('Repeated 'Unpacked) ('Enumeration e) (UnpackedVec (Enumerated e))
instance MessageFieldType ('Repeated 'Unpacked) ('Message m) (NestedVec m)
instance ( MessageFieldType ('Singular 'Implicit) k kh
         , MessageFieldType ('Singular (MappedPresence v)) v vh
         ) =>
         MessageFieldType ('Repeated 'Unpacked) ('Map k v) (M.Map kh vh)

instance MessageFieldType ('Repeated 'Packed) 'Int32 (PackedVec Int32)
instance MessageFieldType ('Repeated 'Packed) 'Int64 (PackedVec Int64)
instance MessageFieldType ('Repeated 'Packed) 'SInt32 (PackedVec (Signed Int32))
instance MessageFieldType ('Repeated 'Packed) 'SInt64 (PackedVec (Signed Int64))
instance MessageFieldType ('Repeated 'Packed) 'UInt32 (PackedVec (Word32))
instance MessageFieldType ('Repeated 'Packed) 'UInt64 (PackedVec (Word64))
instance MessageFieldType ('Repeated 'Packed) 'Fixed32 (PackedVec (Fixed Word32))
instance MessageFieldType ('Repeated 'Packed) 'Fixed64 (PackedVec (Fixed Word64))
instance MessageFieldType ('Repeated 'Packed) 'SFixed32 (PackedVec (Signed (Fixed Int32)))
instance MessageFieldType ('Repeated 'Packed) 'SFixed64 (PackedVec (Signed (Fixed Int64)))
instance MessageFieldType ('Repeated 'Packed) 'Bool (PackedVec Bool)
instance MessageFieldType ('Repeated 'Packed) 'Float (PackedVec Float)
instance MessageFieldType ('Repeated 'Packed) 'Double (PackedVec Double)
instance MessageFieldType ('Repeated 'Packed) ('Enumeration e) (PackedVec (Enumerated e))
