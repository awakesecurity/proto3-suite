{-# LANGUAGE CPP #-}

module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase)

import qualified Test.DocTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [docTests]

docTests :: TestTree
docTests = testCase "doctests" $ do
  putStrLn "Running all doctests..."
  Test.DocTest.doctest
    [ "-isrc"
    , "-itests"
#ifdef SWAGGER
    , "-DSWAGGER"
#endif
#ifdef DHALL
    , "-DDHALL"
#endif
    , "src/Proto3/Suite/DotProto/Internal.hs"
    , "src/Proto3/Suite/JSONPB/Class.hs"
    ]
