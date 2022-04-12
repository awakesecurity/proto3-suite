{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

-- |
--
-- @since 1.0.0
module Proto3.Suite.DotProto.AST.Message
  ( -- * Names
    MessageName (MessageName),
    getMessageName,

    -- * Messages
    DotProtoMessagePart
      ( DotProtoMessageField,
        DotProtoMessageOneOf,
        DotProtoMessageDefinition,
        DotProtoMessageReserved,
        DotProtoMessageOption
      ),

    -- * Definitions
    DotProtoDefinition (DotProtoEnum, DotProtoMessage, DotProtoService),

    -- * Services
    DotProtoServicePart
      ( DotProtoServiceRPCMethod,
        DotProtoServiceOption,
        DotProtoServiceEmpty
      ),

    -- * RPC Methods
    RPCMethod (RPCMethod),
    rpcMethodName,
    rpcMethodRequestType,
    rpcMethodRequestStreaming,
    rpcMethodResponseType,
    rpcMethodResponseStreaming,
    rpcMethodOptions,

    -- * Streaming Types
    Streaming (Streaming, NonStreaming),
  )
where

import Control.Lens (makePrisms)
import Data.String (IsString)
import Text.PrettyPrint.HughesPJClass  (Pretty, pPrint)
import Text.PrettyPrint ((<+>))
import Text.PrettyPrint qualified as PP

import Proto3.Suite.DotProto.AST.Enumerate (DotProtoEnumPart)
import Proto3.Suite.DotProto.AST.Field (DotProtoField, DotProtoReservedField)
import Proto3.Suite.DotProto.AST.Identifier (DotProtoIdentifier)
import Proto3.Suite.DotProto.AST.Option (DotProtoOption)

--------------------------------------------------------------------------------

-- | Proto3 message names.
--
-- @since 1.0.0
newtype MessageName = MessageName
  {getMessageName :: String}
  deriving stock (Eq, Ord)
  deriving (IsString, Show) via String

--------------------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data DotProtoMessagePart
  = DotProtoMessageField DotProtoField
  | DotProtoMessageOneOf DotProtoIdentifier [DotProtoField]
  | DotProtoMessageDefinition DotProtoDefinition
  | DotProtoMessageReserved [DotProtoReservedField]
  | DotProtoMessageOption DotProtoOption
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------

-- | TODO: docs/Top-level protocol definitions
--
-- @since 1.0.0
data DotProtoDefinition
  = DotProtoMessage String DotProtoIdentifier [DotProtoMessagePart]
  | DotProtoEnum String DotProtoIdentifier [DotProtoEnumPart]
  | DotProtoService String DotProtoIdentifier [DotProtoServicePart]
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data DotProtoServicePart
  = DotProtoServiceRPCMethod RPCMethod
  | DotProtoServiceOption DotProtoOption
  | DotProtoServiceEmpty
  deriving stock (Eq, Show)

-- | @since 1.0.0
instance Pretty DotProtoServicePart where
  pPrint = \case
    DotProtoServiceRPCMethod RPCMethod{..} ->
      PP.text "rpc"
        <+> pPrint rpcMethodName
        <+> PP.parens (pPrint rpcMethodRequestStreaming <+> pPrint rpcMethodRequestType)
        <+> PP.text "returns"
        <+> PP.parens (pPrint rpcMethodResponseStreaming <+> pPrint rpcMethodResponseType)
        <+> case rpcMethodOptions of
              [] -> PP.text ";"
              _  -> PP.braces . PP.vcat $ topOption <$> rpcMethodOptions
    DotProtoServiceOption option -> topOption option
    DotProtoServiceEmpty          -> PP.empty
    where
      topOption o = (PP.text "option" <+> pPrint o) <> PP.text ";"

--------------------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data RPCMethod = RPCMethod
  { rpcMethodName :: DotProtoIdentifier
  , rpcMethodRequestType :: DotProtoIdentifier
  , rpcMethodRequestStreaming :: Streaming
  , rpcMethodResponseType :: DotProtoIdentifier
  , rpcMethodResponseStreaming :: Streaming
  , rpcMethodOptions :: [DotProtoOption]
  }
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data Streaming
  = Streaming
  | NonStreaming
  deriving stock (Eq, Show)

-- | @since 1.0.0
instance Pretty Streaming where
  pPrint Streaming    = PP.text "stream"
  pPrint NonStreaming = PP.empty
