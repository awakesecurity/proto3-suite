{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Test.Proto.Compiled where

import "ghc-lib-parser" GHC.Types.SrcLoc

import "ghc-lib-parser" Language.Haskell.Syntax.Extension

import NeatInterpolation as Neat

import Proto3.Suite.DotProto.Generate (StringType (..))
import Data.Text.Lazy qualified as Hs
import Test.Proto.Quote (embedProtoDefinitions)

--------------------------------------------------------------------------------

import qualified Prelude as Hs
import qualified Proto3.Suite.Class as HsProtobuf
import qualified Proto3.Suite.DotProto as HsProtobufAST
import qualified Proto3.Suite.JSONPB as HsJSONPB
import Proto3.Suite.JSONPB ( (.=), (.:) )
import qualified Proto3.Suite.Types as HsProtobuf
import qualified Proto3.Wire as HsProtobuf
import qualified Proto3.Wire.Decode as HsProtobuf
    ( Parser, RawField )
import qualified Control.Applicative as Hs
import Control.Applicative ( (<*>), (<|>), (<$>) )
import qualified Control.DeepSeq as Hs
import qualified Control.Monad as Hs
import qualified Data.ByteString as Hs
import qualified Data.Coerce as Hs
import qualified Data.Int as Hs ( Int16, Int32, Int64 )
import qualified Data.List.NonEmpty as Hs ( NonEmpty(..) )
import qualified Data.Map as Hs ( Map, mapKeysMonotonic )
import qualified Data.Proxy as Proxy
import qualified Data.String as Hs ( fromString )
import qualified Data.String as Hs ( String )
import qualified Data.Vector as Hs ( Vector )
import qualified Data.Word as Hs ( Word16, Word32, Word64 )
import qualified GHC.Enum as Hs
import qualified GHC.Generics as Hs
import qualified Google.Protobuf.Wrappers.Polymorphic as HsProtobuf
    ( Wrapped(..) )
import qualified Unsafe.Coerce as Hs

--------------------------------------------------------------------------------


$(let ?stringType =  StringType "Data.Text.Lazy" "Text"
      ?typeLevelFormat = Hs.False
  in embedProtoDefinitions [Neat.text|
syntax = "proto3";

package TestProto;

message Trivial {
  int32 trivialField = 1;
}

message MultipleFields {
  double multiFieldDouble = 1;
  float multiFieldFloat = 2;
  int32 multiFieldInt32 = 3;
  int64 multiFieldInt64 = 4;
  string multiFieldString = 5;
  bool multiFieldBool = 6;
}

message SignedInts {
  sint32 signed32 = 1;
  sint64 signed64 = 2;
}

message WithEnum {
  enum TestEnum {
    ENUM1 = 0;
    ENUM2 = 1;
    ENUM3 = 2;
  }
  TestEnum enumField = 1;
}

message NestedInts {
    int32 nestedInt1 = 1;
    int32 nestedInt2 = 2;
  }

message WithRepetition {
  repeated int32 repeatedField1 = 1;
}

message WithRepeatedSigned {
  repeated sint32 r32 = 1;
  repeated sint64 r64 = 2;
}

message WithFixed {
  fixed32 fixed1 = 1;
  sfixed32 fixed2 = 2;
  fixed64 fixed3 = 3;
  sfixed64 fixed4 = 4;
}

message WithBytes {
  bytes bytes1 = 1;
  repeated bytes bytes2 = 2;
}

message WithPacking {
  repeated int32 packing1 = 1 [packed=false];
  repeated int32 packing2 = 2 [packed=true];
}

enum E { FLD0 = 0; FLD1 = 1; }

message AllPackedTypes {
  repeated uint32 packedWord32 = 1 [packed=true];
  repeated uint64 packedWord64 = 2 [packed=true];
  repeated int32 packedInt32 = 3 [packed=true];
  repeated int64 packedInt64 = 4 [packed=true];
  repeated fixed32 packedFixed32 = 5 [packed=true];
  repeated fixed64 packedFixed64 = 6 [packed=true];
  repeated float packedFloat = 7 [packed=true];
  repeated double packedDouble = 8 [packed=true];
  repeated sfixed32 packedSFixed32 = 9 [packed=true];
  repeated sfixed64 packedSFixed64 = 10 [packed=true];
  repeated bool packedBool = 11 [packed=true];
  repeated E packedEnum = 12 [packed=true];
  repeated E unpackedEnum = 13 [packed=false];
}

message OutOfOrderFields {
  repeated uint32 field1 = 2001 [packed = true];
  string field2 = 101;
  int64 field3 = 30;
  repeated string field4 = 1002;
}

message ShadowedMessage {
  string name = 2;
  int32 value = 1;
}

message MessageShadower {
  message ShadowedMessage {
    string name = 1;
    string value = 2;  // Same as ShadowedMessage above, but with field numbers flipped, and different types for value
  }
  ShadowedMessage shadowed_message = 1;
  string name = 2;  // Tests if the field names are shadowed or not
}

message WithQualifiedName {
  ShadowedMessage qname1 = 100;
  MessageShadower.ShadowedMessage qname2 = 200;
}

message Wrapped {
  Wrapped wrapped = 1;
}

enum EnumAnnots {
  FOO = 0;
  BAR = 1 [deprecated=true];
}

message WrappedTrivial {
  Trivial trivial = 1;
}

message MapTest {
  map<string, sint32> prim = 1;
  map<int32, WrappedTrivial> trivial = 2;
  map<sint32, sint32> signed = 3; // both the key and value needs to be wrapped with 'Signed'
}

// The protobuf specification requires that the binary encoding of MapTest
// should be identical to that of this type.  We exploit this equivalence
// to work around a limitation of the Python generated code: that it seems
// to provide no way to distinguish between an unset mapped submessage
// and one that is set but has default values for all of its fields.
message MapTestEmulation {
  message Prim {
    string key = 1;
    sint32 value = 2;
  }
  repeated Prim prim = 1;

  message Trivial {
    int32 key = 1;
    WrappedTrivial value = 2;
  }
  repeated Trivial trivial = 2;

  message Signed {
    sint32 key = 1;
    sint32 value = 2;
  }
  repeated Signed signed = 3; // both the key and value needs to be wrapped with 'Signed'
}
|])