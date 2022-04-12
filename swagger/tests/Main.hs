module Main where

import qualified Test.DocTest
import           Test.Tasty
import           Test.Tasty.HUnit            (testCase)

-- -----------------------------------------------------------------------------

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ docTests
  ]

-- -----------------------------------------------------------------------------
-- Doctests

docTests :: TestTree
docTests = testCase "doctests" $ do
  putStrLn "Running all doctests..."
  Test.DocTest.doctest
    [ "-isrc"
    , "-itests"
    , "-igen"
    , "gen/TestProto.hs"  -- No tests, but this lets us import it.
    , "gen/TestProtoOneof.hs"  -- No tests, but this lets us import it.
    , "tests/TestCodeGen.hs"
    ]
