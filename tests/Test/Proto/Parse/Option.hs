
module Test.Proto.Parse.Option (testTree) where

import Hedgehog (Property, forAll, property, (===))
import qualified Hedgehog as Hedgehog
import qualified Hedgehog.Gen as Gen
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Test.Proto.Parse.Core (runParseTest, parseTrip)
import qualified Test.Proto.Parse.Gen as Gen

import qualified Data.Char as Char
import Data.Either (isLeft)
import Text.Parsec (ParseError)

import qualified Proto3.Suite.DotProto.Parsing as Proto3
import Proto3.Suite.DotProto.Rendering () -- orphan Pretty DotProtoIdentifier

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup
    "Test.Proto.Parse.Option"
    [ testProperty "Unqualified Option Identifier" propParseName
    , testProperty "Qualified Option Identifier" propParseQName
    , testProperty "Keyword 'Option'" propParseOptionKw
    , testsOptionKw
    ]

propParseName :: Property
propParseName = property $ do
  idt <- forAll Gen.optionName
  parseTrip idt Proto3.pOptionId

propParseQName :: Property
propParseQName = property $ do
  idt <- forAll Gen.optionQName
  parseTrip idt Proto3.pOptionId

--------------------------------------------------------------------------------

testsOptionKw :: TestTree
testsOptionKw =
  testGroup
    "Test.Proto.Parse.Option.Keyword"
    [ testProperty "Keyword 'Option'" propParseOptionKw
    , testProperty "Keyword Malformed" propParseOptionKwMalformed
    ]

propParseOptionKw :: Property
propParseOptionKw = property $ do
  str <- forAll Gen.optionKw
  case runParseTest Proto3.pOptionKw str of
    Left err -> Hedgehog.footnoteShow err >> Hedgehog.failure
    Right () -> Hedgehog.success

-- | Ensure the parser handling the keyword "option" must be followed by a
-- non-alphanumeric, otherwise the parser should fail.
propParseOptionKwMalformed :: Property
propParseOptionKwMalformed = property $ do
  chr <- forAll Gen.ascii
  let result :: Either ParseError ()
      result = runParseTest Proto3.pOptionKw ("option" ++ [chr])
   in if Char.isAlphaNum chr
        then Hedgehog.assert (isLeft result)
        else result === Right ()
