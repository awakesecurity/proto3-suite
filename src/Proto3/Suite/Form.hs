{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Type-Level Format Information
--
-- Use @compile-proto-file --typeLevelFormat ...@ to instantiate these type families to
-- provide type-level information that may be used to define custom encoders and decoders.
--
-- /WARNING/: This module is experimental and breaking changes may occur much more
-- frequently than in the other modules of this package, perhaps even in patch releases.
module Proto3.Suite.Form
  ( NamesOf
  , NumberOf
  , ProtoTypeOf
  , OneOfOf
  , CardinalityOf
  , FieldNotFound
  , FieldOrOneOfNotFound
  , Packing(..)
  , Cardinality(..)
  , ProtoType(..)
  , Association
  , CardinalityOfMapped
  , Wrapper
  , RecoverCardinality
  , RecoverProtoType
  , MessageFieldType
  , OptionalMessageFieldType
  , RepeatedMessageFieldType
  ) where

import Data.Int (Int32, Int64)
import Data.Kind (Type)
import Data.Map qualified as M
import Data.Word (Word32, Word64)
import GHC.Exts (Constraint)
import GHC.TypeLits (ErrorMessage(..), Nat, Symbol, TypeError)
import Prelude hiding (String)
import Proto3.Suite.Types (Bytes, Enumerated, Commented, Fixed, ForceEmit, Nested,
                           NestedVec, PackedVec, Signed, String, UnpackedVec)

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
-- of every field and @oneof@ to its 'Cardinality'.
--
-- (Every @oneof@ in the message is mapped to 'OneOf',
-- as are those fields that are within @oneof@s.)
type family CardinalityOf (message :: Type) (name :: Symbol) :: Cardinality

-- | A compilation error message for when the given message
-- does not contain a field with the given name.
type FieldNotFound (message :: Type) (name :: Symbol) =
  'Text "Field " ':<>: 'ShowType name ':<>:
  'Text " not found in message:" ':$$: 'ShowType message

-- | A compilation error message for when the given message
-- does not contain a field or oneof with the given name.
type FieldOrOneOfNotFound (message :: Type) (name :: Symbol) =
  'Text "Field or oneof " ':<>: 'ShowType name ':<>:
  'Text " not found in message:" ':$$: 'ShowType message

-- | The packing of field that is @repeated@ or is a @map@.  Every
-- encoding appends and element, and omission means zero elements.
data Packing
  = Unpacked
      -- ^ Packing is not preferred or not supported (for example, submessages and maps).
  | Packed
      -- ^ Packing is supported and preferred (perhaps implicitly).

-- | Expresses how many values we expect a field to contain, what it means when its
-- encoding is omitted, and if it is @repeated@, whether or not packing is preferred.
--
-- Each data constructor is chosen in such a way that it implies how many
-- values are expected as arguments to `Proto3.Suite.Form.Encode.field`.
-- This choice simplifies instantiation of type classes.
data Cardinality
  = Implicit
      -- ^ We expect exactly one value for this field,
      -- though the default value is expressed by omission
      -- in the binary format.  Such a field is never "unset".
      --
      -- In the terminology of the proto3 specification, the field is
      -- "singular" and is neither @optional@ nor part of a @oneof@.
      --
      -- Put another way: the field is not declared @optional@ or
      -- @repeated@, it is not a @map@, it does not have a message type,
      -- and it is not part of a @oneof@.
      --
      -- `Proto3.Suite.Form.Encode.field` will expect exactly one field
      -- value as its argument.  Typically the argument type is not
      -- a container type.  But there may be application-specific reasons
      -- for supplying a container type, such as the field type being
      -- a message that has container semantics implemented by an
      -- application-specific instance of `Proto3.Suite.Form.Encode.FieldForm`.
  | Optional
      -- ^ The field is singular and has optional field presence.  That is,
      -- That is, it is a non-@repeated@ submessage field (which is optional
      -- even when not explicitly declared @optional@), or an @optional@
      -- scalar/enumerated field whose omission implies an "unset" value
      -- distinct from the default value, or any field within a @oneof@.
      --
      -- `Proto3.Suite.Form.Encode.field` will expect zero or one field
      -- values as its argument.  Typically the type involves 'Maybe',
      -- but when the 'Nothing' case can be excluded, feel free to
      -- use `Data.Functor.Identity.Identity` to illustrate that fact.
  | Repeated Packing
      -- ^ The field is @repeated@ or is a @map@, with the indicated
      -- packing preference (which for @map is always 'Unpacked').
      --
      -- `Proto3.Suite.Form.Encode.field` will expect zero or more
      -- field values, typically expressed by a container type having
      -- an instance of `Proto3.Wire.Encode.Repeated.ToRepeated`.

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
--
-- The key field is named @"key"@ and the value field is named @"value"@.
data Association (key :: ProtoType) (value :: ProtoType)

type instance NamesOf (Association _ _) = '[ "key", "value" ]

type instance NumberOf (Association _ _) "key" = 1
type instance NumberOf (Association _ _) "value" = 2

type instance ProtoTypeOf (Association key _) "key" = key
type instance ProtoTypeOf (Association _ value) "value" = value

type instance OneOfOf (Association _ _) "key" = ""
type instance OneOfOf (Association _ _) "value" = ""

type instance CardinalityOf (Association _ _) "key" = 'Implicit
type instance CardinalityOf (Association _ value) "value" = CardinalityOfMapped value

-- | Yields the field presence of a mapped value of the given protobuf type.
type family CardinalityOfMapped (protoType :: ProtoType) :: Cardinality
  where
    CardinalityOfMapped ('Message _) = 'Optional
    CardinalityOfMapped ('Map k v) = TypeError
      ( 'Text "Nested maps are disallowed, so this cannot be a mapped type:"
        ':$$: 'ShowType ('Map k v) )
    CardinalityOfMapped _ = 'Implicit

-- | Indicates the standard protobuf wrapper having
-- the field type given by the type argument.
--
-- We never need to construct values; instead we construct values of types
-- such as @`Proto3.Suite.Form.Encode.Encoding` ('Wrapper' protoType)@ or
-- @`Proto.Suite.Form.Encode.Wrap` a@, where @a@ is a corresponding native representation.
--
-- Note that if Google ever adds wrappers for "sint..." or "...fixed..."
-- then this type constructor will naturally support such wrappers.
data Wrapper (protoType :: ProtoType)

type instance NamesOf (Wrapper protoType) = '[ "value" ]

type instance NumberOf (Wrapper protoType) "value" = 1

type instance ProtoTypeOf (Wrapper protoType) "value" = protoType

type instance OneOfOf (Wrapper protoType) "value" = ""

type instance CardinalityOf (Wrapper protoType) "value" = 'Implicit

-- | Given the Haskell type used by features such as `Proto3.Suite.Class.MessageField`
-- to indicate the encoding of a message field.
type family RecoverCardinality (haskellType :: Type) :: Cardinality
  where
    RecoverCardinality (Commented _ haskellType) = RecoverCardinality haskellType
    RecoverCardinality (Maybe (ForceEmit _)) = 'Optional
    RecoverCardinality (PackedVec _) = 'Repeated 'Packed
    RecoverCardinality (UnpackedVec _) = 'Repeated 'Unpacked
    RecoverCardinality (NestedVec _) = 'Repeated 'Unpacked
    RecoverCardinality (Nested _) = 'Optional
    RecoverCardinality (Enumerated _) = 'Implicit
    RecoverCardinality (M.Map _ _) = 'Repeated 'Unpacked
    RecoverCardinality Int32 = 'Implicit
    RecoverCardinality Int64 = 'Implicit
    RecoverCardinality (Signed Int32) = 'Implicit
    RecoverCardinality (Signed Int64) = 'Implicit
    RecoverCardinality Word32 = 'Implicit
    RecoverCardinality Word64 = 'Implicit
    RecoverCardinality (Fixed Word32) = 'Implicit
    RecoverCardinality (Fixed Word64) = 'Implicit
    RecoverCardinality (Signed (Fixed Int32)) = 'Implicit
    RecoverCardinality (Signed (Fixed Int64)) = 'Implicit
    RecoverCardinality (String _) = 'Implicit
    RecoverCardinality (Bytes _) = 'Implicit
    RecoverCardinality Bool = 'Implicit
    RecoverCardinality Float = 'Implicit
    RecoverCardinality Double = 'Implicit

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
    RecoverProtoType (Maybe (ForceEmit haskellType)) = RecoverProtoType haskellType
    RecoverProtoType (PackedVec haskellType) = RecoverProtoType haskellType
    RecoverProtoType (UnpackedVec haskellType) = RecoverProtoType haskellType
    RecoverProtoType (Enumerated e) = 'Enumeration e
    RecoverProtoType (Nested m) = 'Message m
    RecoverProtoType (NestedVec m) = 'Message m
    RecoverProtoType (M.Map k v) = 'Map (RecoverProtoType k) (RecoverProtoType v)
    RecoverProtoType m = 'Message m

-- | Inhabited by Haskell types used by features such as `Proto3.Suite.Class.MessageField`
-- that correspond to particular protobuf types that are repeated in the specified way.
class ( RecoverCardinality haskellType ~ cardinality
      , RecoverProtoType haskellType ~ protoType
      ) =>
      MessageFieldType (cardinality :: Cardinality) (protoType :: ProtoType) (haskellType :: Type)

instance MessageFieldType 'Implicit 'Int32 Int32
instance MessageFieldType 'Implicit 'Int64 Int64
instance MessageFieldType 'Implicit 'SInt32 (Signed Int32)
instance MessageFieldType 'Implicit 'SInt64 (Signed Int64)
instance MessageFieldType 'Implicit 'UInt32 (Word32)
instance MessageFieldType 'Implicit 'UInt64  (Word64)
instance MessageFieldType 'Implicit 'Fixed32 (Fixed Word32)
instance MessageFieldType 'Implicit 'Fixed64 (Fixed Word64)
instance MessageFieldType 'Implicit 'SFixed32 (Signed (Fixed Int32))
instance MessageFieldType 'Implicit 'SFixed64 (Signed (Fixed Int64))
instance MessageFieldType 'Implicit 'String (String a)
instance MessageFieldType 'Implicit 'Bytes (Bytes a)
instance MessageFieldType 'Implicit 'Bool Bool
instance MessageFieldType 'Implicit 'Float Float
instance MessageFieldType 'Implicit 'Double Double
instance MessageFieldType 'Implicit ('Enumeration e) (Enumerated e)

instance MessageFieldType 'Optional 'Int32 (Maybe (ForceEmit Int32))
instance MessageFieldType 'Optional 'Int64 (Maybe (ForceEmit Int64))
instance MessageFieldType 'Optional 'SInt32 (Maybe (ForceEmit (Signed Int32)))
instance MessageFieldType 'Optional 'SInt64 (Maybe (ForceEmit (Signed Int64)))
instance MessageFieldType 'Optional 'UInt32 (Maybe (ForceEmit Word32))
instance MessageFieldType 'Optional 'UInt64  (Maybe (ForceEmit Word64))
instance MessageFieldType 'Optional 'Fixed32 (Maybe (ForceEmit (Fixed Word32)))
instance MessageFieldType 'Optional 'Fixed64 (Maybe (ForceEmit (Fixed Word64)))
instance MessageFieldType 'Optional 'SFixed32 (Maybe (ForceEmit (Signed (Fixed Int32))))
instance MessageFieldType 'Optional 'SFixed64 (Maybe (ForceEmit (Signed (Fixed Int64))))
instance MessageFieldType 'Optional 'String (Maybe (ForceEmit (String a)))
instance MessageFieldType 'Optional 'Bytes (Maybe (ForceEmit (Bytes a)))
instance MessageFieldType 'Optional 'Bool (Maybe (ForceEmit Bool))
instance MessageFieldType 'Optional 'Float (Maybe (ForceEmit Float))
instance MessageFieldType 'Optional 'Double (Maybe (ForceEmit Double))
instance MessageFieldType 'Optional ('Enumeration e) (Maybe (ForceEmit (Enumerated e)))

-- | Helps to diagnose the absence of an instance for 'MessageFieldType'
-- for optional submessages by requiring that the second type parameter
-- be 'Nested' of the first.  Please try to avoid using this type family
-- directly; it is exported only to help explain compilation errors.
type family OptionalMessageFieldType (m :: Type) (haskellType :: Type)
  where
    OptionalMessageFieldType m (Nested m) = (() :: Constraint)
    OptionalMessageFieldType m (Nested a) = TypeError
      ( 'Text "Expected reflected protobuf submessage type " ':<>: 'ShowType m ':$$:
        'Text "Actual type: " ':<>: 'ShowType a )
    OptionalMessageFieldType m haskellType = TypeError
      ( 'Text "When using a Haskell type to specify an optional protobuf submessage" ':$$:
        'Text "(as opposed to repeated one or a submessage within a oneof)" ':$$:
        'Text "you must wrap the Haskell reflection type in Proto3.Suite.Nested." ':$$:
        'Text "Expected reflected protobuf submessage type " ':<>: 'ShowType m ':$$:
        'Text "Haskell type provided: " ':<>: 'ShowType haskellType )

instance ( OptionalMessageFieldType m haskellType
         , RecoverCardinality haskellType ~ 'Optional
         , RecoverProtoType haskellType ~ 'Message m
         ) =>
         MessageFieldType 'Optional ('Message m) haskellType

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

-- | Helps to diagnose the absence of an instance for 'MessageFieldType'
-- for repeated submessages by requiring that the second type parameter
-- be 'NestedVec' of the first.  Please try to avoid using this type family
-- directly; it is exported only to help explain compilation errors.
type family RepeatedMessageFieldType (m :: Type) (haskellType :: Type)
  where
    RepeatedMessageFieldType m (NestedVec m) = (() :: Constraint)
    RepeatedMessageFieldType m (NestedVec a) = TypeError
      ( 'Text "Expected reflected protobuf submessage type " ':<>: 'ShowType m ':$$:
        'Text "Actual type: " ':<>: 'ShowType a )
    RepeatedMessageFieldType m haskellType = TypeError
      ( 'Text "When using a Haskell type to specify a repeated protobuf submessage" ':$$:
        'Text "(as opposed to an optional one or a submessage within a oneof)" ':$$:
        'Text "you must wrap the Haskell reflection type in Proto3.Suite.NestedVec." ':$$:
        'Text "Expected reflected protobuf submessage type " ':<>: 'ShowType m ':$$:
        'Text "Haskell type provided: " ':<>: 'ShowType haskellType )

instance ( RepeatedMessageFieldType m haskellType
         , RecoverCardinality haskellType ~ 'Repeated 'Unpacked
         , RecoverProtoType haskellType ~ 'Message m
         ) =>
         MessageFieldType ('Repeated 'Unpacked) ('Message m) haskellType

instance ( MessageFieldType 'Implicit k kh
         , MessageFieldType (CardinalityOfMapped v) v vh
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
