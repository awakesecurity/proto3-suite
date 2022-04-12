{-# LANGUAGE TemplateHaskell #-}

-- |
module Proto3.Suite.DotProto.AST.Lens where

import Control.Lens.TH
import Proto3.Suite.DotProto.AST.Message

makePrisms ''DotProtoDefinition
makePrisms ''DotProtoMessagePart
