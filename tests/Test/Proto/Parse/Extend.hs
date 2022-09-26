
module Test.Proto.Parse.Extend (testTree) where

import Hedgehog (Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen

import Text.Parsec (ParseError)

import qualified Proto3.Suite.DotProto.Parsing as Proto3
import Proto3.Suite.DotProto.Rendering () 

import Test.Proto.Parse.Core (runParseTest, testParseCase)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import qualified Test.Proto.Parse.Gen as Gen
import Text.Parsec.Error (newErrorMessage, Message (UnExpect))
import Text.Parsec.Pos (newPos)

--------------------------------------------------------------------------------

testTree :: TestTree
testTree = 
  testGroup 
    "Extend"
    [ testGroup 
        "Keyword" 
        [ testParseCase "extend" Proto3.pExtendKw (Right ()) Gen.extendKw
        , testProperty "counter-example" propParseExtendCounterExample
        ]
    ]

-- | Ensure the parser handling the keyword "extend" must be followed by a 
-- non-alphanumeric, otherwise the parser should fail.
propParseExtendCounterExample :: Property
propParseExtendCounterExample = property $ do 
  chr <- forAll Gen.alphaNum 
  let result :: Either ParseError ()
      result = runParseTest Proto3.pExtendKw ("extend" ++ [chr])
   in result === Left (newErrorMessage (UnExpect (show chr)) (newPos "" 1 8))