module TestDhall where

import           TestProtoOneof
import           Test.Tasty
import           Test.Tasty.HUnit               (testCase, (@?=))

import qualified Dhall
import qualified Dhall.Core

dhallTests :: TestTree
dhallTests = testGroup "Dhall to/from proto unit tests"
  [ roundTripDhall ]

roundTripDhall :: TestTree
roundTripDhall = testCase "round trip to Dhall and back" $ do
  
  let hsProtoMsg = Something 42 99 (Just (SomethingPickOneSomeid 66))

  let dhProtoMsg = Dhall.Core.pretty (Dhall.embed Dhall.inject hsProtoMsg)

  hsProtoMsg2 <- Dhall.input Dhall.auto dhProtoMsg

  hsProtoMsg2 @?= hsProtoMsg
