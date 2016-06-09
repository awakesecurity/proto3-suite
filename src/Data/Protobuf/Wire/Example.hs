{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Protobuf.Wire.Example where

import Data.Foldable (fold)
import Data.Protobuf.Wire.Generic
import Data.Protobuf.Wire.Shared
import Data.Protobuf.Wire.DotProto
import Data.Proxy
import Data.Word (Word32)
import GHC.Generics

-- | Enumerated types can be used by deriving 'Enum'.
data Shape
  = Circle
  | Square
  | Triangle
  deriving (Eq, Enum, Finite, Generic, Named, Ord)

-- | We can encode a value of type 'Foo' using 'toLazyByteString'.
--
-- For example:
--
-- >>> toLazyByteString (Foo 42 [Bar (Enumerated Circle) Nothing])
-- "\b*\DC2\EOT\b\NUL\DC2\NUL"
data Foo = Foo
  { fooID   :: Word32
  , fooBars :: NestedVec Bar
  } deriving (Eq, Generic)

instance Message Foo
instance Named Foo

data Bar = Bar
  { barShape :: Enumerated Shape
  , barFoo   :: Nested Foo
  , foos     :: NestedVec Foo
  }
  deriving (Eq, Generic)

instance Message Bar
instance Named Bar

-- | Generates the .proto file for the 'Foo' and 'Bar' data types.
--
-- >>> putStrLn protoFile
-- enum Shape {
--   Circle = 1;
--   Square = 2;
--   Triangle = 3;
-- }
-- message Foo {
--   uint32 fooID = 1;
--   repeated Bar fooBars = 2;
-- }
-- message Bar {
--   Shape barShape = 1;
--   /* optional */ Foo barFoo = 2;
-- }
protoFile :: String
protoFile = toProtoFileDef "examplePackageName" $ fold
  ([ enum    (Proxy :: Proxy Shape)
   , message (Proxy :: Proxy Foo)
   , message (Proxy :: Proxy Bar)
   ] :: [DotProto])
