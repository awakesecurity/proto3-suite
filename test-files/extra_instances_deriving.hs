{-# LANGUAGE StandaloneDeriving #-}

module ExtraInstances where

data Foo = Foo

helper :: Int
helper = 42

instance Show Foo where
  show _ = "Foo"

deriving instance Eq Foo
