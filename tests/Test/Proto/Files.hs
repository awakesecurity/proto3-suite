{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Proto.Files 
  ( dotProtoEmptyField
  , dotProtoImport
  , dotProtoLeadingDot
  , dotProtoNegativeEnum
  , dotProtoNestedMessage
  , dotProtoOneofImport
  , dotProtoOneof
  , dotProtoOptional
  , dotProtoProtocPlugin
  , dotProtoWrappers
  , dotProtoCommon
  ) where

import Proto3.Suite.DotProto.AST (DotProto)

import NeatInterpolation qualified as Neat

import Test.Proto.Quote (dotProtoTestFile)

--------------------------------------------------------------------------------

dotProtoEmptyField :: DotProto
dotProtoEmptyField = 
  $(dotProtoTestFile (Just "test-files/test_proto_empty_field.proto") [Neat.text|
    syntax = "proto3";

    package TestProtoEmptyField;

    enum EnumWithEmptyField {
      enum_foo = 0;
      ;
      enum_bar = 1;
    }

    message MessageWithEmptyField {
      uint32 foo = 1;
      ;
      uint32 bar = 2;
    }
  |])

dotProtoImport :: DotProto
dotProtoImport = 
  $(dotProtoTestFile (Just "test-files/test_proto_import.proto") [Neat.text|
    syntax="proto3";

    package TestProtoImport;

    message WithNesting {
      message Nested {
        int32 nestedField1 = 1;
        int32 nestedField2 = 2;
      }
      Nested nestedMessage1 = 1;
      Nested nestedMessage2 = 100;
    }
  |])

dotProtoLeadingDot :: DotProto
dotProtoLeadingDot =
  $(dotProtoTestFile (Just "test-files/test_proto_leading_dot.proto") [Neat.text|
    syntax = "proto3";

    package LeadingDot.Rpc;

    import "leading_dot/data.proto";

    message Request {
      uint32 foo = 1;
      .LeadingDot.Rpc.Data.Request bar = 2;
      Buz buz = 3;

      message Buz {
        uint32 foo = 1;
        .LeadingDot.Rpc.Data.Request bar = 2;
      }
    }

    service Service {
      rpc CreateSite(Request) returns (.LeadingDot.Rpc.Data.Response);
      rpc createSite(Request) returns (.LeadingDot.Rpc.Data.Response);
    }
  |])

dotProtoNegativeEnum :: DotProto
dotProtoNegativeEnum =
  $(dotProtoTestFile (Just "test-files/test_proto_negative_enum.proto") [Neat.text|
    syntax="proto3";

    package TestProtoNegativeEnum;

    enum NegativeEnum {
      NEGATIVE_ENUM_0 = 0;

      NEGATIVE_ENUM_NEGATIVE_1 = -1;

      NEGATIVE_ENUM_1 = 1;

      NEGATIVE_ENUM_NEGATIVE_128 = -128;

      NEGATIVE_ENUM_128 = 128;
    }

    message WithNegativeEnum {
      NegativeEnum v = 123;
    }
  |])

dotProtoNestedMessage :: DotProto
dotProtoNestedMessage =
  $(dotProtoTestFile (Just "test-files/test_proto_nested_message.proto") [Neat.text|
    syntax = "proto3";

    package TestProto.NestedMessage;

    message Request {
      Foo foo = 1;
      message Foo {
        Bar bar = 1;
        message Bar {
          Buz buz = 1;
          message Buz {
            uint32 x = 1;
          }
        }
      }
    }
  |])

dotProtoOneofImport :: DotProto
dotProtoOneofImport =
  $(dotProtoTestFile (Just "test-files/test_proto_oneof_import.proto") [Neat.text|
    syntax="proto3";

    package TestProtoOneofImport;

    message AMessage {
      string x = 1;
      int32 y = 2;
    }
    message WithOneof {
      oneof pickOne {
        string a = 1;
        int32  b = 2;
        AMessage c = 3;
      }
    }
  |])

dotProtoOneof :: DotProto
dotProtoOneof = 
  $(dotProtoTestFile (Just "test-files/test_proto_oneof.proto") [Neat.text|
    syntax = "proto3";

    package TestProtoOneof;

    import "test_proto_oneof_import.proto";

    message DummyMsg {
      int32 dummy = 1;
    }

    enum DummyEnum {
      DUMMY0 = 0;
      DUMMY1 = 1;
    }

    // Also handles the case where the oneof field is syntatically the last one in
    // the message (this exercises field ordering logic in the code generator)
    message Something {
      sint64 value          = 1;
      sint32 another        = 2;
      oneof  pickOne {
        string    name      = 4;
        int32     someid    = 9;
        DummyMsg  dummyMsg1 = 10;
        DummyMsg  dummyMsg2 = 11;
        DummyEnum dummyEnum = 12;
      }
    }

    // Handles the case where the oneof field is syntatically the first one in the
    // message (this exercises field ordering logic in the code generator)
    message OneofFirst {
      oneof first {
        string choice1 = 1;
        string choice2 = 2;
      }
      int32 last = 3;
    }

    // Handles the case where the oneof field is syntatically between other fields
    // in the message (this exercises field ordering logic in the code generator)
    message OneofMiddle {
      int32 first = 1;
      oneof middle {
        string choice1 = 2;
        string choice2 = 3;
      }
      int32 last = 4;
    }

    message WithImported {
      oneof pickOne {
        DummyMsg                       dummyMsg1 = 1;
        TestProtoOneofImport.WithOneof withOneof = 2;
      }
    }
  |])

dotProtoOptional :: DotProto
dotProtoOptional =
  $(dotProtoTestFile (Just "test-files/test_proto_optional.proto") [Neat.text|
    syntax = "proto3";

    package TestProtoOptional;

    enum Enum {
      UNKNOWN = 0;
      Code55 = 55;
    }

    message Submessage {
      int32 someField = 77;
    }

    message WithOptional {
      optional double optionalDouble = 10;
      optional float optionalFloat = 20;
      optional int32 optionalInt32 = 30;
      optional int64 optionalInt64 = 40;
      optional uint32 optionalUint32 = 50;
      optional uint64 optionalUint64 = 60;
      optional sint32 optionalSint32 = 70;
      optional sint64 optionalSint64 = 80;
      optional fixed32 optionalFixed32 = 90;
      optional fixed64 optionalFixed64 = 100;
      optional sfixed32 optionalSfixed32 = 110;
      optional sfixed64 optionalSfixed64 = 120;
      optional bool optionalBool = 130;
      optional string optionalString = 140;
      optional bytes optionalBytes = 150;
      optional Enum optionalEnum = 160;
      optional Submessage optionalSubmessage = 170;
        // The "optional" keyword should be redundant here because all messages are implicitly optional.
    }
  |])

dotProtoProtocPlugin :: DotProto
dotProtoProtocPlugin =
  $(dotProtoTestFile (Just "test-files/test_proto_protoc_plugin.proto") [Neat.text|
    syntax = "proto3";

    package ProtocPlugin;

    //
    // TODO : Automatically ignore proto2
    // imports which are used only in options/extensions.
    // They are not used in generated Haskell AST anyway,
    // and absence of such imports will not break any code.
    //
    //import "protoc_plugin/elixirpb.proto";
    import "protoc_plugin/google_protobuf.proto";

    message DateTime {
      option (elixirpb.message).typespec = "DateTime.t";
      int64 microseconds = 1;
    }

    message Foo {
      uint32 bar = 1;
      Google.Protobuf.StringValue buz = 2;
    }
  |])

dotProtoWrappers :: DotProto
dotProtoWrappers =
  $(dotProtoTestFile (Just "test-files/test_proto_wrappers.proto") [Neat.text|
    syntax = "proto3";

    package TestProtoWrappers;

    import "google/protobuf/wrappers.proto";

    message TestDoubleValue {
      google.protobuf.DoubleValue wrapper = 2;
      repeated google.protobuf.DoubleValue many = 3;
      oneof pickOne {
        google.protobuf.DoubleValue one = 4;
      }
    }

    message TestFloatValue {
      google.protobuf.FloatValue wrapper = 2;
      repeated google.protobuf.FloatValue many = 3;
      oneof pickOne {
        google.protobuf.FloatValue one = 4;
      }
    }

    message TestInt64Value {
      google.protobuf.Int64Value wrapper = 2;
      repeated google.protobuf.Int64Value many = 3;
      oneof pickOne {
        google.protobuf.Int64Value one = 4;
      }
    }

    message TestUInt64Value {
      google.protobuf.UInt64Value wrapper = 2;
      repeated google.protobuf.UInt64Value many = 3;
      oneof pickOne {
        google.protobuf.UInt64Value one = 4;
      }
    }

    message TestInt32Value {
      google.protobuf.Int32Value wrapper = 2;
      repeated google.protobuf.Int32Value many = 3;
      oneof pickOne {
        google.protobuf.Int32Value one = 4;
      }
    }

    message TestUInt32Value {
      google.protobuf.UInt32Value wrapper = 2;
      repeated google.protobuf.UInt32Value many = 3;
      oneof pickOne {
        google.protobuf.UInt32Value one = 4;
      }
    }

    message TestBoolValue {
      google.protobuf.BoolValue wrapper = 2;
      repeated google.protobuf.BoolValue many = 3;
      oneof pickOne {
        google.protobuf.BoolValue one = 4;
      }
    }

    message TestStringValue {
      google.protobuf.StringValue wrapper = 2;
      repeated google.protobuf.StringValue many = 3;
      oneof pickOne {
        google.protobuf.StringValue one = 4;
      }
    }

    message TestBytesValue {
      google.protobuf.BytesValue wrapper = 2;
      repeated google.protobuf.BytesValue many = 3;
      oneof pickOne {
        google.protobuf.BytesValue one = 4;
      }
    }
  |])

dotProtoCommon :: DotProto
dotProtoCommon =
  $(dotProtoTestFile (Just "test-files/test_proto.proto") [Neat.text|
    syntax = "proto3";

    package TestProto;

    import "test_proto_import.proto";

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

    message WithNesting {
      message Nested {
        string nestedField1 = 1;
        int32 nestedField2 = 2;
        repeated int32 nestedPacked = 3 [packed=true];
        repeated int32 nestedUnpacked = 4 [packed=false];
      }
      Nested nestedMessage = 1;
    }

    message WithNestingRepeated {
      message Nested {
        string nestedField1 = 1;
        int32 nestedField2 = 2;
        repeated int32 nestedPacked = 3 [packed=true];
        repeated int32 nestedUnpacked = 4 [packed=false];
      }
      repeated Nested nestedMessages = 1;
    }

    message NestedInts {
        int32 nestedInt1 = 1;
        int32 nestedInt2 = 2;
      }

    message WithNestingRepeatedInts {
      repeated NestedInts nestedInts = 1;
    }

    message WithNestingInts {
      NestedInts nestedInts = 1;
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

    message UsingImported {
      TestProtoImport.WithNesting importedNesting = 100;
      WithNesting localNesting = 200;
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