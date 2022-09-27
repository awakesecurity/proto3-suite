{-# LANGUAGE BlockArguments #-}

module Test.Proto.Parse.Core (
  testParseCase,
  testParseTrip, 
  runParseTest,
  parseTrip,
) where

import Hedgehog (Gen, PropertyT, forAll, property, (===))
import qualified Hedgehog

import Proto3.Suite.DotProto.Parsing (ProtoParser)
import qualified Proto3.Suite.DotProto.Parsing as Proto3
import Proto3.Suite.DotProto.Rendering ()

import Text.Parsec (ParseError)
import qualified Text.Parsec as Parsec
import Text.PrettyPrint (render)
import Text.PrettyPrint.HughesPJClass (Pretty, pPrint) -- orphan Pretty DotProtoIdentifier

import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty ( TestName, TestTree )

--------------------------------------------------------------------------------

runParseTest :: ProtoParser a -> String -> Either ParseError a
runParseTest p = Parsec.parse (Proto3.runProtoParser p) ""

testParseCase ::
  (Eq a, Pretty a, Show a) =>
  TestName ->
  ProtoParser a ->
  Either ParseError a ->
  Gen String ->
  TestTree
testParseCase name parse expect gen =
  testProperty name $ property do
    input <- forAll gen
    expect === runParseTest parse input

testParseTrip ::
  (Eq a, Pretty a, Show a) =>
  TestName ->
  ProtoParser a ->
  Gen a ->
  TestTree
testParseTrip name parse gen =
  testProperty name $ property do
    idt <- forAll gen
    parseTrip idt parse

parseTrip :: (Eq a, Pretty a, Show a) => a -> ProtoParser a -> PropertyT IO ()
parseTrip x p = Hedgehog.tripping x (render . pPrint) (runParseTest p)
