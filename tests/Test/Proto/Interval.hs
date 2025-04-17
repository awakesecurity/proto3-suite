{-# LANGUAGE TypeApplications #-}

module Test.Proto.Interval (testTree) where

import Control.Monad (unless)

import Data.Int (Int8)

import Proto3.Suite.DotProto.Internal (isOverlappingIntervals, joinIntervals)

import Hedgehog (Gen, forAll, property, (===))
import Hedgehog qualified
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

--------------------------------------------------------------------------------

overlappingIntervals :: (Bounded a, Integral a) => Gen ((a, a), (a, a))
overlappingIntervals = do 
  x <- Gen.integral Range.constantBounded
  y <- Gen.integral Range.constantBounded

  u <- Gen.integral (Range.constant x y) 
  v <- Gen.integral (Range.constant y maxBound)

  pure ((x, y), (u, v))

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup
    "Test.Proto.Interval"
    [ testProperty "Overlap" $ property do
        (i1@(x, y), i2@(u, v)) <- forAll (overlappingIntervals @Int8)

        Hedgehog.assert (isOverlappingIntervals i1 i2)

        -- This test is invalid for intervals (x, y) and (u, v) when any of the
        -- following conditions are true: x == y, u == v, x == v or y == u. This 
        -- is because 'Gen.int8' can produce the boundary values of the given
        -- range which can lead to situations where splitting @i1@ and @i2@ at 
        -- the subinterval covered by @i1@ and @i2@ are still "touching" along
        -- the upper boundary of @i1@ and the lower boundary of @i2@, e.g.
        --
        -- @
        -- i1  = (0, 5)
        -- i2  = (5, 10)
        -- i1' = (0, 5) 
        -- i2' = (5, 10) 
        -- @
        --
        -- In all other cases we are gauranteed to have disjoint @i1'@ and 
        -- @i2'@, thus @'not' . 'isOverlappingIntervals'@ should be implied.
        unless (x == y || u == v || x == v || y == u) do
          let i1' :: (Int8, Int8)
              i1' = (min (fst i1) (fst i2), min (snd i1) (snd i2))

          let i2' :: (Int8, Int8)
              i2' = (min (fst i1) (fst i2), min (snd i1) (snd i2))

          Hedgehog.assert (not (isOverlappingIntervals i1' i2'))
    , testIntervalMeasure
    ]

-- Verify interval merging by checking that the size of the merged interval is 
-- bounded below the sum of the sizes of each individual interval that was 
-- merged. 
testIntervalMeasure :: TestTree
testIntervalMeasure = testProperty "Measure" $ property do
  (i1, i2) <- forAll (overlappingIntervals @Int8)

  let d1 :: Integer 
      d1 = magnitude i1

  let d2 :: Integer 
      d2 = magnitude i2

  Hedgehog.annotateShow d1
  Hedgehog.annotateShow d2

  case joinIntervals i1 i2 of 
    Nothing -> Hedgehog.failure
    Just i3 -> do 
      fst i3 === min (fst i1) (fst i2)
      snd i3 === max (snd i1) (snd i2)
      Hedgehog.diff (magnitude i3) (<=) (magnitude i1 + magnitude i2)
  where 
    magnitude :: (Int8, Int8) -> Integer
    magnitude i = toInteger (snd i) - toInteger (fst i)
