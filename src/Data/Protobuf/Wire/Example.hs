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
import Data.Protobuf.Wire.Generic.DotProto
import Data.Protobuf.Wire.Shared
import Data.Proxy
import Data.Word (Word32)
import GHC.Generics

-- | Enumerated types can be used by deriving 'Enum'.
data Shape
  = Circle
  | Square
  | Triangle
  deriving (Eq, Ord, Enum, Generic, HasEnum, HasMessageName)

-- | We can encode a value of type 'Foo' using 'toLazyByteString'.
--
-- For example:
--
-- >>> toLazyByteString (Foo 42 [Bar (Enumerated Circle) Nothing])
-- "\b*\DC2\EOT\b\NUL\DC2\NUL"
data Foo = Foo
  { fooID   :: Word32
  , fooBars :: UnpackedVec Bar
  }
  deriving Generic

instance HasEncoding Foo
instance HasPrimType Foo
instance HasType Foo
instance HasMessage Foo
instance HasMessageName Foo

data Bar = Bar
  { barShape :: Enumerated Shape
  , barFoo   :: Maybe Foo
  }
  deriving Generic

instance HasEncoding Bar
instance HasPrimType Bar
instance HasType Bar
instance HasMessage Bar
instance HasMessageName Bar

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
protoFile = toProtoFile "examplePackageName" $ fold
  ([ enum    (Proxy :: Proxy Shape)
   , message (Proxy :: Proxy Foo)
   , message (Proxy :: Proxy Bar)
   ] :: [DotProto])
