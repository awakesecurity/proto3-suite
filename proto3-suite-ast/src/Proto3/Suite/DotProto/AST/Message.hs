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
    DotProtoDefinition (DotProtoMessage, DotProtoEnum, DotProtoService),

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

import Data.String (IsString)

import Proto3.Suite.DotProto.AST.Enumerate (DotProtoEnumPart)
import Proto3.Suite.DotProto.AST.Field (DotProtoField, DotProtoReservedField)
import Proto3.Suite.DotProto.AST.Option (DotProtoOption, DotProtoIdentifier)

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
