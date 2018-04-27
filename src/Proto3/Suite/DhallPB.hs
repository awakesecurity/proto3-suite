{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Proto3.Suite.DhallPB where

import           Data.Int (Int16, Int32, Int64)
import           Proto3.Suite.Types
import           GHC.Float (double2Float)

import qualified Dhall

instance Dhall.Interpret a => Dhall.Interpret (Enumerated a)

instance Dhall.Interpret a => Dhall.Interpret (Either Int a)

-- The following instances do not interpret loss-lessly unless they
-- consume Dhall rendered from the same types. Loss-less
-- interpretation presents the possibility for sneaky bad-behavior if
-- transformation of the rendered Dhall occurs before an endpoint
-- consumes and interprets the message.
--
-- TODO: can we perform run-time bounds-checking to at least hint to
-- the user that something unexpected happened?
instance Dhall.Interpret Int where
  autoWith _ = fmap fromInteger Dhall.integer

instance Dhall.Interpret Int16 where
  autoWith _ = fmap fromInteger Dhall.integer

instance Dhall.Interpret Int32 where
  autoWith _ = fmap fromInteger Dhall.integer

instance Dhall.Interpret Int64 where
  autoWith _ = fmap fromInteger Dhall.integer

instance Dhall.Interpret Float where
  autoWith _ = fmap double2Float Dhall.double
