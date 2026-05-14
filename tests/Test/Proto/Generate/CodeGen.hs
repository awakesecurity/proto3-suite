{-# LANGUAGE OverloadedStrings #-}

module Test.Proto.Generate.CodeGen (testTree) where

import Control.Monad.Except (runExceptT)
import Proto3.Suite.DotProto.Generate (getExtraInstances)
import Proto3.Suite.Haskell.Parser (initLogger)
import Test.Tasty
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)

testTree :: TestTree
testTree = testGroup "Code generation"
  [ extraInstanceParsing
  ]

-- | 'getExtraInstances' includes both regular instance declarations and
-- standalone deriving declarations from extra instance files.
extraInstanceParsing :: TestTree
extraInstanceParsing =
  testCase "getExtraInstances includes standalone deriving declarations" $ do
    logger <- initLogger
    result <- runExceptT $ getExtraInstances logger "test-files/extra_instances_deriving.hs"
    case result of
      Left err -> assertFailure (show err)
      Right (_imports, decls) ->
        assertEqual "expected 2 declarations (1 instance + 1 standalone deriving)"
          2 (length decls)
