module TestDhall where

import           TestProtoOneof
import           Test.Tasty
import           Test.Tasty.HUnit               (testCase, (@?=))
import TestProtoOneof
import Proto3.Suite.DhallPB
import qualified Dhall
import qualified Dhall.Core
import qualified Dhall.Marshal.Decode
import qualified Dhall.Marshal.Encode

instance Dhall.FromDhall Something

instance Dhall.ToDhall Something

instance Dhall.FromDhall SomethingPickOne

instance Dhall.ToDhall SomethingPickOne

instance Dhall.FromDhall DummyMsg

instance Dhall.ToDhall DummyMsg

instance Dhall.FromDhall DummyEnum

instance Dhall.ToDhall DummyEnum

dhallTests :: TestTree
dhallTests = testGroup "Dhall to/from proto unit tests"
  [ roundTripDhall ]

roundTripDhall :: TestTree
roundTripDhall = testCase "round trip to Dhall and back" $ do
  
  let hsProtoMsg = Something 42 99 (Just (SomethingPickOneSomeid 66))

  let dhProtoMsg = Dhall.Core.pretty (Dhall.embed Dhall.inject hsProtoMsg)

  hsProtoMsg2 <- Dhall.input Dhall.auto dhProtoMsg

  hsProtoMsg2 @?= hsProtoMsg
