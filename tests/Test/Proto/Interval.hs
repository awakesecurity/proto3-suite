{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Test.Proto.Interval (testTree) where

import Data.Foldable (for_)

import Proto3.Suite.DotProto.Internal (joinIntervals, normalizeIntervals)

import Hedgehog (Gen, forAll, property, (===))
import Hedgehog qualified
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

--------------------------------------------------------------------------------

-- | Check if the given value is some given bounds (inclusive).
isIn :: (Ord a) => a -> (a, a) -> Bool
isIn x (a, b) = a <= x && x <= b

-- | Check if the given value is any of the given bounds (inclusive).
isInAnyOf :: (Ord a) => a -> [(a, a)] -> Bool
isInAnyOf = any . isIn

-- | Generates an arbitrary interval.  May generate
-- empty intervals, though at about 10% probability.
genInterval :: (Bounded a, Integral a) => Gen (a, a)
genInterval = Gen.frequency [(4, alwaysNonempty), (1, halfEmpty)]
  where
    alwaysNonempty = do
      x <- Gen.integral Range.constantBounded
      y <- Gen.integral Range.constantBounded
      pure (min x y, max x y)

    halfEmpty = do
      x <- Gen.integral Range.constantBounded
      y <- Gen.integral Range.constantBounded
      pure (x, y)

-- | A value in the range [-8 .. 7].  We intentionally limit this type to
-- just a few values in order to increase the chance of overlapping intervals
-- in some tests, while still retaining enough distinct values to allow many
-- different relationships between intervals.  We also want to check for
-- arithmetic overflow in the code under test by throwing an exception,
-- which should cause the test to fail.
newtype Nybble = UnsafeNybble Int
  deriving newtype (Eq, Ord, Real, Show)

{-# COMPLETE Nybble #-}

pattern Nybble :: Int -> Nybble
pattern Nybble x <- UnsafeNybble x
  where
    Nybble x
      | -8 <= x && x <= 7 = UnsafeNybble x
      | otherwise = error $ "Nybble out of bounds: " ++ show x

instance Bounded Nybble
  where
    minBound = Nybble -8
    maxBound = Nybble 7

-- | Manual implementation to ensure an exception is thrown for out-of-bounds values.
instance Enum Nybble
  where
    toEnum x = Nybble x
    fromEnum (Nybble x) = x

-- | Manual implementation to ensure an exception is thrown for arithmetic overflow.
instance Num Nybble
  where
    Nybble x + Nybble y = Nybble (x + y)
    Nybble x - Nybble y = Nybble (x - y)
    Nybble x * Nybble y = Nybble (x * y)
    negate (Nybble x) = Nybble (negate x)
    abs (Nybble x) = Nybble (abs x)
    signum (Nybble x) = Nybble (signum x)
    fromInteger x = Nybble (fromInteger x)

-- | Manual implementation to ensure an exception is thrown for arithmetic overflow.
instance Integral Nybble
  where
    quotRem (Nybble n) (Nybble d) = let (q, r) = quotRem n d in (Nybble q, Nybble r)
    toInteger (Nybble x) = toInteger x

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup
    "Test.Proto.Interval"
    [ testJoinIntervals
    , testNormalizeIntervals
    ]

-- This test verifies the documented behavior of 'joinIntervals'.
testJoinIntervals :: TestTree
testJoinIntervals = testProperty "joinIntervals" $ property do
  (a, b) <- forAll $ genInterval @Nybble
  (c, d) <- forAll $ genInterval @Nybble
  case joinIntervals (a, b) (c, d) of
    Nothing -> do
      -- The code under test says that one cannot merge the two intervals into
      -- a single interval.  For that to be true, both intervals must be nonempty/valid:
      Hedgehog.assert (a <= b)
      Hedgehog.assert (c <= d)
      -- And in addition, the least interval containing both of the given intervals
      -- must /not/ be their union.  Namely, it must include some extra element:
      let extra x = not (x `isIn` (a, b) || x `isIn` (c, d))
      Hedgehog.assert (any extra [min a c .. max b d])
      -- That suffices to verify that the intervals cannot be combined, but because
      -- the code under test currently considers similar issues, we note that there
      -- must be at least one value strictly between the two given intervals;
      -- otherwise we could merge them because there would be nothing between them:
      let between x = (b < x && x < c) || (d < x && x < a)
      Hedgehog.assert (any between [minBound .. maxBound])
    Just combined -> do
      -- The code under test claims that 'combined' equals the union of the given
      -- intervals.  We check that conclusion by testing every possible element:
      Hedgehog.annotateShow combined
      for_ [minBound .. maxBound] \x -> do
        x `isIn` combined === (x `isIn` (a, b) || x `isIn` (c, d))

-- This test verifies the documented behavior of 'normalizeIntervals'.
-- (Note the correspondence between the various documented properties
-- and the specific checks included here.)
--
-- This test also checks an additional property: that 'normalizeIntervals'
-- never increases the sum of the sizes of the listed intervals, where by
-- "size" we mean the count of elements within the interval.  That is, we
-- are only eliminating redundancy in the
testNormalizeIntervals :: TestTree
testNormalizeIntervals = testProperty "normalizeIntervals" $ property do
  messy <- forAll $ Gen.list (Range.linear 0 20) $ genInterval @Nybble

  let clean :: [(Nybble, Nybble)]
      clean = normalizeIntervals messy
  Hedgehog.annotateShow clean

  -- The result must not contain any empty intervals.
  for_ clean \(a, b) ->
    Hedgehog.assert (a <= b)

  -- The union of the result must be the same as the union of the input.
  for_ [minBound .. maxBound] \x -> do
    (x, isInAnyOf x clean) === (x, isInAnyOf x messy)

  -- Check that the listed intervals do not overlap, and moreover,
  -- cannot be merged because there are no values between them.
  --
  -- Note that here we rely upon the accuracy of 'joinIntervals',
  -- which is checked in isolation by the test 'testJoinIntervals'.
  for_ (zip [1 ..] clean) \(d, i1) ->
    for_ (drop d clean) \i2 -> do
      (i1, i2, joinIntervals i1 i2) === (i1, i2, Nothing)

  -- The code under test should have ensured that for any two consecutive intervals,
  -- there is at least one value strictly inbetween those two intervals.  This check
  -- should subsume the previous one, but we run both checks just to be sure.
  --
  -- The nonempty gaps between intervals prevent further merging, making 'clean'
  -- at least locally minimal.  But a simple exhaustive search would be extremely
  -- expensive, and hence we rely upon the mathematical argument in the comments for
  -- 'normalizeIntervals' to completely rule out the possibility of shorter lists.
  for_ (zip clean (drop 1 clean)) \((_, b), (c, _)) ->
    Hedgehog.assert (b < c && succ b < c)  -- The first check is to sure that 'succ' is safe.

  -- Verify interval merging by checking that the size of the merged intervals is
  -- bounded below the sum of the sizes of each individual interval that was merged.
  let intervalSize :: (Nybble, Nybble) -> Integer
      intervalSize (a, b) = if a <= b then toInteger b + 1 - toInteger a else 0
      overallSize :: [(Nybble, Nybble)] -> Integer
      overallSize = sum . map intervalSize
  Hedgehog.annotateShow (overallSize clean, overallSize messy)
  Hedgehog.assert (overallSize clean <= overallSize messy)
