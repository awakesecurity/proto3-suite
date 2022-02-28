{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DerivingStrategies #-}

-- |
--
-- @since 1.0.0
module Proto3.Suite.DotProto.AST.Field
  ( -- * Field Names
    FieldName (FieldName),
    getFieldName,

    -- * Fields
    DotProtoField (DotProtoField, DotProtoEmptyField),
    dotProtoFieldNumber,
    dotProtoFieldType,
    dotProtoFieldName,
    dotProtoFieldOptions,
    dotProtoFieldComment,

    -- * Reserved Fields
    DotProtoReservedField (SingleField, FieldRange, ReservedIdentifier),

    -- * Field Packing
    Packing (PackedField, UnpackedField),
  )
where

import Data.String (IsString)
import Proto3.Wire.Types (FieldNumber)
import Text.PrettyPrint.HughesPJClass  (Pretty, pPrint)
import Text.PrettyPrint ((<+>))
import Text.PrettyPrint qualified as PP

import Proto3.Suite.DotProto.AST.Identifier (DotProtoIdentifier)
import Proto3.Suite.DotProto.AST.Option (DotProtoOption)
import Proto3.Suite.DotProto.AST.Type (DotProtoType)

--------------------------------------------------------------------------------

-- | Proto3 field names.
--
-- @since 1.0.0
newtype FieldName = FieldName
  {getFieldName :: String}
  deriving stock (Eq, Ord)
  deriving (IsString, Show) via String

--------------------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data DotProtoField
  = DotProtoField
      { dotProtoFieldNumber :: FieldNumber
      , dotProtoFieldType :: DotProtoType
      , dotProtoFieldName :: DotProtoIdentifier
      , dotProtoFieldOptions :: [DotProtoOption]
      , dotProtoFieldComment :: String
      }
  | DotProtoEmptyField
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data DotProtoReservedField
  = SingleField Int
  | FieldRange Int Int
  | ReservedIdentifier String
  deriving stock (Eq, Show)

-- | @since 1.0.0
instance Pretty DotProtoReservedField where
  pPrint (SingleField num)      = PP.text $ show num
  pPrint (FieldRange start end) = (PP.text $ show start) <+> PP.text "to" <+> (PP.text $ show end)
  pPrint (ReservedIdentifier i) = PP.text $ show i

--------------------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data Packing
  = PackedField
  | UnpackedField
  deriving stock (Enum, Eq, Ord, Show)
