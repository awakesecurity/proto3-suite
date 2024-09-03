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
  , Repetition(..)
  , ProtoType(..)
  , Association
  , Optional
  , Wrapped(..)
  , RecoverRepetition
  , RecoverProtoType
  , MessageFieldType
  ) where

import Data.Int (Int32, Int64)
import Data.Kind (Type)
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)
import GHC.TypeLits (Nat, Symbol)
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

-- | Whether or a field is repeated, the meaning of its omission, and
-- if it is repeated, whether or not packing has been requested.
data Repetition
  = Singular
      -- ^ Non-repeatable field that is /not/ part of a @oneof@.
      -- Omission means that it takes on its default value.
  | OneOf
      -- ^ A @oneof@ or a field that is part of a @oneof@.
      -- Omission of all fields in the oneof means that the entire oneof is omitted;
      -- the default value of any single alternative must be encoded explicitly,
      -- in order to indicate which alternative was chosen.
  | Unpacked
      -- ^ Repeatable field for which packing is /not/ requested.
      -- Omission means zero elements.
  | Packed
      -- ^ Repeatable field for which packing /is/ requested.
      -- Omission means zero elements.

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

type instance RepetitionOf (Association key value) "key" = 'Singular
type instance RepetitionOf (Association key value) "value" = 'Singular

-- | Indicates the standard protobuf wrapper having
-- the field type given by the type argument.
--
-- We never need to construct values; instead we construct values of types
-- such as @`Proto3.Suite.Form.Encode.Encoding` ('Optional' protoType)@ or
-- @'Wrapped' a@, where @a@ is a corresponding native representation.
--
-- Note that if Google ever adds wrappers for "sint..." or "...fixed..."
-- then this type constructor will naturally support such wrappers.
data Optional (protoType :: ProtoType)

type instance NamesOf (Optional protoType) = '[ "value" ]

type instance NumberOf (Optional protoType) "value" = 1

type instance ProtoTypeOf (Optional protoType) "value" = protoType

type instance OneOfOf (Optional protoType) "value" = ""

type instance RepetitionOf (Optional protoType) "value" = 'Singular

-- | Helps some type classes distinguish wrapped values from encodings of wrapper submessages.
--
-- See also 'Optional'.
newtype Wrapped (a :: Type) = Wrapped { wrapped :: a }
  deriving stock (Foldable, Functor, Generic, Traversable)
  deriving newtype (Bounded, Enum, Eq, Fractional, Integral, Ord, Num, Read, Real, Show)

-- | Given the Haskell type used by features such as `Proto3.Suite.Class.MessageField`
-- to indicate the encoding of a message field.
type family RecoverRepetition (haskellType :: Type) :: Repetition
  where
    RecoverRepetition (Commented _ haskellType) = RecoverRepetition haskellType
    RecoverRepetition (ForceEmit _) = 'OneOf
    RecoverRepetition (PackedVec _) = 'Packed
    RecoverRepetition (UnpackedVec _) = 'Unpacked
    RecoverRepetition (NestedVec _) = 'Unpacked
    RecoverRepetition (Nested _) = 'Singular
    RecoverRepetition (Enumerated _) = 'Singular
    RecoverRepetition (M.Map _ _) = 'Unpacked
    RecoverRepetition Int32 = 'Singular
    RecoverRepetition Int64 = 'Singular
    RecoverRepetition (Signed Int32) = 'Singular
    RecoverRepetition (Signed Int64) = 'Singular
    RecoverRepetition Word32 = 'Singular
    RecoverRepetition Word64 = 'Singular
    RecoverRepetition (Fixed Word32) = 'Singular
    RecoverRepetition (Fixed Word64) = 'Singular
    RecoverRepetition (Signed (Fixed Int32)) = 'Singular
    RecoverRepetition (Signed (Fixed Int64)) = 'Singular
    RecoverRepetition (String _) = 'Singular
    RecoverRepetition (Bytes _) = 'Singular
    RecoverRepetition Bool = 'Singular
    RecoverRepetition Float = 'Singular
    RecoverRepetition Double = 'Singular
    RecoverRepetition _ = 'OneOf

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

instance MessageFieldType 'Singular 'Int32 Int32
instance MessageFieldType 'Singular 'Int64 Int64
instance MessageFieldType 'Singular 'SInt32 (Signed Int32)
instance MessageFieldType 'Singular 'SInt64 (Signed Int64)
instance MessageFieldType 'Singular 'UInt32 (Word32)
instance MessageFieldType 'Singular 'UInt64  (Word64)
instance MessageFieldType 'Singular 'Fixed32 (Fixed Word32)
instance MessageFieldType 'Singular 'Fixed64 (Fixed Word64)
instance MessageFieldType 'Singular 'SFixed32 (Signed (Fixed Int32))
instance MessageFieldType 'Singular 'SFixed64 (Signed (Fixed Int64))
instance MessageFieldType 'Singular 'String (String a)
instance MessageFieldType 'Singular 'Bytes (Bytes a)
instance MessageFieldType 'Singular 'Bool Bool
instance MessageFieldType 'Singular 'Float Float
instance MessageFieldType 'Singular 'Double Double
instance MessageFieldType 'Singular ('Enumeration e) (Enumerated e)
instance MessageFieldType 'Singular ('Message m) (Nested m)

instance MessageFieldType 'OneOf 'Int32 (ForceEmit Int32)
instance MessageFieldType 'OneOf 'Int64 (ForceEmit Int64)
instance MessageFieldType 'OneOf 'SInt32 (ForceEmit (Signed Int32))
instance MessageFieldType 'OneOf 'SInt64 (ForceEmit (Signed Int64))
instance MessageFieldType 'OneOf 'UInt32 (ForceEmit (Word32))
instance MessageFieldType 'OneOf 'UInt64 (ForceEmit (Word64))
instance MessageFieldType 'OneOf 'Fixed32 (ForceEmit (Fixed Word32))
instance MessageFieldType 'OneOf 'Fixed64 (ForceEmit (Fixed Word64))
instance MessageFieldType 'OneOf 'SFixed32 (ForceEmit (Signed (Fixed Int32)))
instance MessageFieldType 'OneOf 'SFixed64 (ForceEmit (Signed (Fixed Int64)))
instance MessageFieldType 'OneOf 'String (ForceEmit (String a))
instance MessageFieldType 'OneOf 'Bytes (ForceEmit (Bytes a))
instance MessageFieldType 'OneOf 'Bool (ForceEmit Bool)
instance MessageFieldType 'OneOf 'Float (ForceEmit Float)
instance MessageFieldType 'OneOf 'Double (ForceEmit Double)
instance MessageFieldType 'OneOf ('Enumeration e) (ForceEmit (Enumerated e))
instance ( RecoverRepetition m ~ 'OneOf
         , RecoverProtoType m ~ 'Message m
         ) =>
         MessageFieldType 'OneOf ('Message m) m

instance MessageFieldType 'Unpacked 'Int32 (UnpackedVec Int32)
instance MessageFieldType 'Unpacked 'Int64 (UnpackedVec Int64)
instance MessageFieldType 'Unpacked 'SInt32 (UnpackedVec (Signed Int32))
instance MessageFieldType 'Unpacked 'SInt64 (UnpackedVec (Signed Int64))
instance MessageFieldType 'Unpacked 'UInt32 (UnpackedVec (Word32))
instance MessageFieldType 'Unpacked 'UInt64 (UnpackedVec (Word64))
instance MessageFieldType 'Unpacked 'Fixed32 (UnpackedVec (Fixed Word32))
instance MessageFieldType 'Unpacked 'Fixed64 (UnpackedVec (Fixed Word64))
instance MessageFieldType 'Unpacked 'SFixed32 (UnpackedVec (Signed (Fixed Int32)))
instance MessageFieldType 'Unpacked 'SFixed64 (UnpackedVec (Signed (Fixed Int64)))
instance MessageFieldType 'Unpacked 'String (UnpackedVec (String a))
instance MessageFieldType 'Unpacked 'Bytes (UnpackedVec (Bytes a))
instance MessageFieldType 'Unpacked 'Bool (UnpackedVec Bool)
instance MessageFieldType 'Unpacked 'Float (UnpackedVec Float)
instance MessageFieldType 'Unpacked 'Double (UnpackedVec Double)
instance MessageFieldType 'Unpacked ('Enumeration e) (UnpackedVec (Enumerated e))
instance MessageFieldType 'Unpacked ('Message m) (NestedVec m)
instance ( MessageFieldType 'Singular k kh
         , MessageFieldType 'Singular v vh
         ) =>
         MessageFieldType 'Unpacked ('Map k v) (M.Map kh vh)

instance MessageFieldType 'Packed 'Int32 (PackedVec Int32)
instance MessageFieldType 'Packed 'Int64 (PackedVec Int64)
instance MessageFieldType 'Packed 'SInt32 (PackedVec (Signed Int32))
instance MessageFieldType 'Packed 'SInt64 (PackedVec (Signed Int64))
instance MessageFieldType 'Packed 'UInt32 (PackedVec (Word32))
instance MessageFieldType 'Packed 'UInt64 (PackedVec (Word64))
instance MessageFieldType 'Packed 'Fixed32 (PackedVec (Fixed Word32))
instance MessageFieldType 'Packed 'Fixed64 (PackedVec (Fixed Word64))
instance MessageFieldType 'Packed 'SFixed32 (PackedVec (Signed (Fixed Int32)))
instance MessageFieldType 'Packed 'SFixed64 (PackedVec (Signed (Fixed Int64)))
instance MessageFieldType 'Packed 'Bool (PackedVec Bool)
instance MessageFieldType 'Packed 'Float (PackedVec Float)
instance MessageFieldType 'Packed 'Double (PackedVec Double)
instance MessageFieldType 'Packed ('Enumeration e) (PackedVec (Enumerated e))
