module Test.Proto.Parse (testTree) where

import qualified Test.Proto.Parse.Extend
import qualified Test.Proto.Parse.Option
import Test.Tasty (TestTree, testGroup)

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup
    "Test.Proto.Parse"
    [ Test.Proto.Parse.Extend.testTree
    , Test.Proto.Parse.Option.testTree
    ]
