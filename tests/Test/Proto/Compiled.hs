{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.Proto.Compiled where

import "ghc-lib-parser" GHC.Types.SrcLoc

import "ghc-lib-parser" Language.Haskell.Syntax.Extension

import NeatInterpolation as Neat

import Proto3.Suite.DotProto.Generate (StringType (..))

import Test.Proto.Quote (embedProtoDefinitions)

--------------------------------------------------------------------------------

import Control.DeepSeq qualified as Hs

import Data.Int qualified as Hs (Int32)

import Proto3.Suite.Class qualified as HsProtobuf
import Proto3.Suite.JSONPB qualified as HsJSONPB

--------------------------------------------------------------------------------

$(embedProtoDefinitions [Neat.text|
syntax = "proto3";

message Trivial {
  int32 trivialField = 1;
}

|])