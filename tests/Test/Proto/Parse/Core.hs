
module Test.Proto.Parse.Core
  ( runParseTest
  , parseTrip
  ) where

import Hedgehog (PropertyT)
import Hedgehog qualified as Hedgehog

import Text.Parsec (ParseError)
import Text.Parsec qualified as Parsec
import Text.PrettyPrint (render)
import Text.PrettyPrint.HughesPJClass (Pretty, pPrint)

import Proto3.Suite.DotProto.Parsing (ProtoParser)
import Proto3.Suite.DotProto.Parsing qualified as Proto3

--------------------------------------------------------------------------------

runParseTest :: ProtoParser a -> String -> Either ParseError a
runParseTest p = Parsec.parse (Proto3.runProtoParser p) ""

parseTrip :: (Eq a, Pretty a, Show a) => a -> ProtoParser a -> PropertyT IO ()
parseTrip x p = Hedgehog.tripping x (render . pPrint) (runParseTest p)
