{-# LANGUAGE TemplateHaskell #-}

-- |
--
-- @since 1.0.0
module Proto3.Suite.DotProto.AST.Lens
  ( _DotProtoMessageField,
    _DotProtoMessageOneOf,
    _DotProtoMessageDefinition,
    _DotProtoMessageReserved,
    _DotProtoMessageOption,
    _DotProtoEnum,
    _DotProtoMessage,
    _DotProtoService,
  )
where

import Control.Lens (makePrisms)

import Proto3.Suite.DotProto.AST.Message

--------------------------------------------------------------------------------

makePrisms ''DotProtoMessagePart
makePrisms ''DotProtoDefinition
