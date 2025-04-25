
module Test.Proto.Parse (testTree) where

import Hedgehog (property, (===))

import Proto3.Suite.DotProto.Parsing qualified as Proto3

import Test.Proto.Parse.Core (runParseTest)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup
    "Test.Proto.Parse"
    [ testProperty "empty" $ property do
        runParseTest Proto3.pEmptyStmt ";" === Right ()
    ]