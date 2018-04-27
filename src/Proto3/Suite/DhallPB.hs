{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Proto3.Suite.DhallPB where

import           Data.Int                (Int16, Int32, Int64)
import           GHC.Float               (double2Float)
import           Proto3.Suite.Types      (Enumerated (..))

import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Data.Text.Lazy.Encoding
import qualified Dhall
import qualified Dhall.Core

class Render a where
  render :: a -> Dhall.Core.Expr s b

instance Dhall.Interpret a => Dhall.Interpret (Enumerated a)

instance Dhall.Interpret a => Dhall.Interpret (Either Int a)

-- Unsafe decoding from Lazy.Text to Lazy.ByteString
--
-- This instance should not throw an error if the Dhall code it
-- interprets was rendered from a ByteString (hence why we provide it)
-- but transformation of the rendered Dhall code without careful
-- checking of the encoding could introduce an unsafe value.
instance Dhall.Interpret Data.ByteString.Lazy.ByteString where
  autoWith _ = fmap Data.Text.Lazy.Encoding.decodeUtf8 Dhall.lazyText

-- Unsafe decoding from strict Text to strict ByteString
--
-- This instance should not throw an error if the Dhall code it
-- interprets was rendered from a ByteString (hence why we provide it)
-- but transformation of the rendered Dhall code without careful
-- checking of the encoding could introduce an unsafe value.
instance Dhall.Interpret Data.ByteString.ByteString where
  autoWith _ = fmap Data.Text.Encoding.decodeUtf8 Dhall.strictText

-- The following instances do not interpret loss-lessly unless they
-- consume Dhall rendered from the same types. Loss-less
-- interpretation presents the possibility for sneaky bad-behavior if
-- transformation of the rendered Dhall occurs before an endpoint
-- consumes and interprets the message.
--
-- TODO: we should perform run-time bounds-checking to at least hint
-- to the user that something unexpected happened.
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
