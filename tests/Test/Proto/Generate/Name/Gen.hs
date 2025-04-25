{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
module Test.Proto.Generate.Name.Gen
  ( GenName (GenName, nameOcc, nameRes),
    protofile,
  )
where

import Control.Applicative (liftA2)

import Data.Char qualified as Char

import Hedgehog (MonadGen, Range)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

--------------------------------------------------------------------------------

-- | 'GenName' is an association between generated names.
--
--   * 'nameOcc' is how the name occurs in the context of protocol buffers.
--
--   * 'nameRes' is a Haskell name that 'nameOcc' is expected to resolve as.
--
-- ==== __Example__
--
-- The *.proto filename "my_messages" should be resolved to the Haskell module
-- "MyMessages", so we would generate a 'GenName':
--
-- >>> GenName "my_messages" "MyMessages"
-- GenName {nameOcc = "my_messages", nameRes = "MyMessages"}
--
-- After an associated pair is generated, it can be used to test a renaming (call
-- it @f@) by checking:
--
-- prop> f (nameOcc nm) == nameRes nm
data GenName = GenName
  { nameOcc :: String
  , nameRes :: String
  }
  deriving (Eq, Show)

-- | Generate the name of a Protobuf file and the Haskell module it should be
-- resolved to.
protofile :: forall m. MonadGen m => Range Int -> m GenName
protofile len = do
  (psize, usize) <- Gen.sized \sz -> do
    x <- Gen.int (Range.linear 0 (fromIntegral sz))
    y <- Gen.int (Range.linear 0 (fromIntegral sz))
    pure (x, y)

  nm <- Gen.sized (ident . Range.linear 1 . fromIntegral)
  ps <- nameParts (Range.linear 0 psize)
  us <- underscores (Range.linear 0 usize)

  let nameOcc = nm ++ concatMap fst ps ++ us
  let nameRes = upperFirst nm ++ concatMap snd ps ++ us
  pure GenName {nameOcc, nameRes}
  where
    ident :: Range Int -> m String
    ident = liftA2 (:) Gen.alpha . alphaNum

    nameParts :: Range Int -> m [(String, String)]
    nameParts rng = Gen.list len do
      n <- Gen.sized (Gen.int . Range.linear 1 . fromIntegral)
      nm <- ident rng
      us <- underscores (Range.linear 1 (fromIntegral n))
      pure (us ++ nm, drop 1 us ++ upperFirst nm)

    upperFirst :: String -> String
    upperFirst (c : cs) = Char.toUpper c : cs
    upperFirst "" = ""

-- -----------------------------------------------------------------------------
--
-- Primitive Name Generation Combinators
--

-- | Generate a string made up of underscores.
underscores :: MonadGen m => Range Int -> m String
underscores rng = Gen.list rng (pure '_')

-- | Generate a name containing alphabetical and numeric characters
-- @'a' .. 'z'@, @'A' .. 'Z'@, and @'0' .. '1'@.
alphaNum :: MonadGen m => Range Int -> m String
alphaNum rng = Gen.list rng Gen.alphaNum
