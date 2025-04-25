
module Test.Proto.Parse.Gen
  ( optionId,
    optionName,
    optionQName,
    optionKw,
  )
where

import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Data.List.NonEmpty qualified as NonEmpty

import Proto3.Suite.DotProto.AST

optionId :: Gen DotProtoIdentifier
optionId = Gen.choice [optionName, optionQName]

optionQName :: Gen DotProtoIdentifier
optionQName = liftA2 Qualified optionName optionName

optionName :: Gen DotProtoIdentifier
optionName =
  Gen.sized $ \s -> do
    let range = Range.linear 1 (fromIntegral s)
    nms <- Gen.nonEmpty range gNameString
    pure $ if null (NonEmpty.tail nms)
      then Single (NonEmpty.head nms)
      else Dots (Path nms)
  where
    gNameString :: Gen String
    gNameString =
      Gen.sized $ \s -> do
        let range = Range.linear 1 (fromIntegral s)
        chr <- Gen.alpha
        chrs <- Gen.list range Gen.alphaNum
        pure (chr : chrs)

optionKw :: Gen String
optionKw = do
  lspace <- whitespace
  rspace <- whitespace
  pure (lspace ++ "option" ++ rspace)

whitespace :: Gen String
whitespace =
  Gen.sized $ \s -> do
    len <- Gen.int (Range.linear 0 (fromIntegral s))
    pure (replicate len ' ')
