{-# LANGUAGE QuasiQuotes #-}

module Test.Proto.TH (testTree) where

import Data.List.NonEmpty (NonEmpty (..))

import Hedgehog (Property, property, (===), withTests)

import Proto3.Suite.DotProto.AST 
  ( DotProto (..)
  , DotProtoDefinition (..)
  , DotProtoIdentifier (..)
  , DotProtoMessagePart (..)
  , DotProtoMeta (..)
  , DotProtoPackageSpec (..)
  , DotProtoField (..)
  , DotProtoPrimType (..)
  , Path (..)
  , DotProtoType (..)
  )
import Proto3.Suite.DotProto.Rendering ()

import Test.Proto.Quote (dotProto3Syntax)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup
    "Test.Proto.TH"
    [ testProperty "dotProtoQuoter" testDotProtoQuoter
    ]

testDotProtoQuoter :: Property
testDotProtoQuoter = 
  withTests 1 $ property do
    protoImports dotProtoParsed === protoImports dotProtoExpected
    protoOptions dotProtoParsed === protoOptions dotProtoExpected
    protoPackage dotProtoParsed === protoPackage dotProtoExpected
    protoDefinitions dotProtoParsed === protoDefinitions dotProtoExpected
  where 
    dotProtoExpected :: DotProto
    dotProtoExpected = 
      DotProto 
        { protoImports = [] 
        , protoOptions = []
        , protoPackage = DotProtoNoPackage
        , protoDefinitions = 
            [ DotProtoMessage 
                ""
                (Single "ShadowedMessage") 
                [ DotProtoMessageField 
                    DotProtoField
                      { dotProtoFieldNumber = 2
                      , dotProtoFieldType = Prim String
                      , dotProtoFieldName = Single "name"
                      , dotProtoFieldOptions = []
                      , dotProtoFieldComment = ""
                      }
                , DotProtoMessageField 
                    DotProtoField
                      { dotProtoFieldNumber = 1
                      , dotProtoFieldType = Prim Int32
                      , dotProtoFieldName = Single "value"
                      , dotProtoFieldOptions = []
                      , dotProtoFieldComment = ""
                      }
                ]
            , DotProtoMessage 
                ""
                (Single "MessageShadower")
                [ DotProtoMessageDefinition $ DotProtoMessage
                    ""
                    (Single "ShadowedMessage") 
                    [ DotProtoMessageField 
                        DotProtoField
                          { dotProtoFieldNumber = 1
                          , dotProtoFieldType = Prim String
                          , dotProtoFieldName = Single "name"
                          , dotProtoFieldOptions = []
                          , dotProtoFieldComment = ""
                          }
                    , DotProtoMessageField 
                        DotProtoField
                          { dotProtoFieldNumber = 2
                          , dotProtoFieldType = Prim String
                          , dotProtoFieldName = Single "value"
                          , dotProtoFieldOptions = []
                          , dotProtoFieldComment = ""
                          }
                    ]
                , DotProtoMessageField 
                    DotProtoField
                      { dotProtoFieldNumber = 1
                      , dotProtoFieldType = Prim (Named (Single "ShadowedMessage"))
                      , dotProtoFieldName = Single "shadowed_message"
                      , dotProtoFieldOptions = []
                      , dotProtoFieldComment = ""
                      }
                ]
            ]
        , protoMeta = DotProtoMeta (Path ("" :| []))
        } 

    dotProtoParsed :: DotProto
    dotProtoParsed = [dotProto3Syntax|
      syntax = "proto3";

      message ShadowedMessage {
        string name = 2;
        int32 value = 1;
      }

      message MessageShadower {
        message ShadowedMessage {
          string name = 1;
          string value = 2;
        }
        ShadowedMessage shadowed_message = 1;
      }
    |]