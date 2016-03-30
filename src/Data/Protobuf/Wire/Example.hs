{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Protobuf.Wire.Example where

import Data.Protobuf.Wire.Generic
import Data.Word (Word32)
import GHC.Generics

-- | Enumerated types can be used by deriving 'Enum'.
data Shape
  = Circle
  | Square
  | Triangle
  deriving (Eq, Ord, Enum)

-- | We can encode a value of type 'Foo' using 'toLazyByteString'.
--
-- For example:
--
-- >>> toLazyByteString (Foo 42 [Bar (Enumerated Circle)])
-- "\b*\DC2\STX\b\NUL"
data Foo
  = Foo Word32 [Foo]
  | Bar (Enumerated Shape)
  deriving Generic

instance HasEncoding Foo
