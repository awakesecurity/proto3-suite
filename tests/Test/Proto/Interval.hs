
module Test.Proto.Interval (testTree) where

import Control.Applicative (liftA2)

import Data.Int (Int8)

import Proto3.Suite.DotProto.Internal (joinIntervals)

import Hedgehog (assert, discard, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup
    "Test.Proto.Interval"
    [ testIntervalMeasure
    ]

-- Verify interval merging by checking that the size of the merged interval is 
-- bounded below the sum of the sizes of each individual interval that was 
-- merged. 
testIntervalMeasure :: TestTree
testIntervalMeasure = testProperty "Measure" $ property do
  i1 <- forAll (liftA2 (,) (Gen.int8 Range.constantBounded) (Gen.int8 Range.constantBounded))
  i2 <- forAll (liftA2 (,) (Gen.int8 Range.constantBounded) (Gen.int8 Range.constantBounded))

  case joinIntervals i1 i2 of 
    Nothing -> discard
    Just i3 -> do 
      assert (fst i3 == min (fst i1) (fst i2))
      assert (snd i3 == max (snd i1) (snd i2))
      assert (magnitude i3 <= magnitude i1 + magnitude i2)
  where 
    magnitude :: (Int8, Int8) -> Integer
    magnitude i = toInteger (snd i) - toInteger (fst i)
