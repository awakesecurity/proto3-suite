
module Test.Proto.Parse.Option (tests) where

import Hedgehog (Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen

import Text.Parsec (ParseError)

import qualified Proto3.Suite.DotProto.Parsing as Proto3
import Proto3.Suite.DotProto.Rendering () 

import Test.Proto.Parse.Core (runParseTest, testParseTrip, testParseCase)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import qualified Test.Proto.Parse.Gen as Gen
import Text.Parsec.Error (newErrorMessage, Message (UnExpect))
import Text.Parsec.Pos (newPos)

--------------------------------------------------------------------------------

tests :: TestTree
tests = 
  testGroup 
    "Option"
    [ testGroup 
        "Identifier" 
        [ testParseTrip "Unqualified" Proto3.pOptionId Gen.optionName
        , testParseTrip "Qualified" Proto3.pOptionId Gen.optionName
        ]
    , testGroup 
        "Keyword" 
        [ testParseCase "Option" Proto3.pOptionKw (Right ()) Gen.optionKw 
        , testProperty "Keyword Malformed" propParseOptionKwMalformed
        ]
    ]

-- | Ensure the parser handling the keyword "option" must be followed by a 
-- non-alphanumeric, otherwise the parser should fail.
propParseOptionKwMalformed :: Property
propParseOptionKwMalformed = property $ do 
  chr <- forAll Gen.alphaNum 
  let result :: Either ParseError ()
      result = runParseTest Proto3.pOptionKw ("option" ++ [chr])
   in result === Left (newErrorMessage (UnExpect (show chr)) (newPos "" 1 8))