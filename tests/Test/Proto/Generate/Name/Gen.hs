{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}

-- |
module Test.Proto.Generate.Name.Gen
  ( GenName (GenName, nameOcc, nameRes),
    protofile,
  )
where

import Hedgehog (MonadGen, Range)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Control.Applicative
import Data.Char qualified as Char
import Data.List qualified as List

-- -----------------------------------------------------------------------------

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
protofile :: MonadGen m => Range Int -> m GenName
protofile len = do
  parts <- Gen.list len $ Gen.sized \sz -> do
    let rng = Range.linear 1 (fromIntegral sz)
    liftA2 (:) Gen.alpha (alphaNum rng)

  let nameOcc = List.intercalate "_" parts
  let nameRes = concatMap upperFirst parts
  pure GenName {nameOcc, nameRes}
  where
    upperFirst :: String -> String
    upperFirst (c : cs) = Char.toUpper c : cs
    upperFirst "" = ""

-- -----------------------------------------------------------------------------
--
-- Primitive Name Generation Combinators
--

-- | Generate a name containing alphabetical and numeric characters
-- @'a' .. 'z'@, @'A' .. 'Z'@, and @'0' .. '1'@.
alphaNum :: MonadGen m => Range Int -> m String
alphaNum rng = Gen.list rng Gen.alphaNum
