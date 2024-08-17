{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- | Type-Level Format Information
--
-- /WARNING/: This module is experimental and breaking changes may occur much more
-- frequently than in the other modules of this package, perhaps even in patch releases.
module Proto3.Suite.Form
  ( FieldForm(..)
  , FieldFormsOf
  ) where

import Data.Kind (Type)
import GHC.TypeLits (Nat, Symbol)
import Google.Protobuf.Wrappers.Polymorphic (Wrapped(..))

-- | Type-level information concerning the protobuf encoding of a field.
-- We intend the data constructors of this type to be promoted to types.
data FieldForm = FieldForm Nat Type Symbol
  -- ^ The number of a non-oneof protobuf field, the Haskell type specifying
  -- how its value should be encoded (such as @'Fixed' `Data.Word.Word32`@),
  -- and the name of any containing @oneof@, or @""@ if not in a @oneof@.
  --
  -- Submessage types are wrapped by 'Nested' (unless repeated) or
  -- 'NestedVec' (if repeated), and other special type constructors
  -- are used to indicate maps and repeated scalars.

-- | Returns the fields within a given type of message, each paired
-- with its name.  The order of names is strictly increasing.
--
-- Note that the names of fields and oneofs must be unique within their shared namespace
-- (see <https://protobuf.dev/reference/protobuf/proto3-spec/#message_definition>).
--
-- The code generator may instantiate this type family to provide type-level
-- information that may be used to define custom encoders and decoders.
type family FieldFormsOf (message :: Type) :: [(Symbol, FieldForm)]

-- | @'Wrapped' a@ is the special message type
-- for the standard protobuf wrapper for @a@.
--
-- For every specific field type, the single field within
-- the wrapper message has name "value" and field number 1.
-- No wrapper message type involves a @oneof@.
type instance FieldFormsOf (Wrapped a) =
  '[ '( "value", 'FieldForm 1 a "" ) ]

-- | The implicit key-value association message within a protobuf map.
type instance FieldFormsOf (k, v) =
  '[ '( "key", 'FieldForm 1 k "" )
   , '( "value", 'FieldForm 2 v "" )
   ]
