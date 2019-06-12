{-# LANGUAGE TemplateHaskell            #-}
module Proto3.Suite.DotProto.AST.Lens where

import Control.Lens.TH (makePrisms)
import Proto3.Suite.DotProto.AST

makePrisms ''DotProtoDefinition
