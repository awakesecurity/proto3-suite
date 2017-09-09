{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
-- | Generated by Haskell protocol buffer compiler. DO NOT EDIT!
module TestProtoImport where
import qualified Prelude as Hs
import qualified Proto3.Suite.DotProto as HsProtobuf
import qualified Proto3.Suite.Types as HsProtobuf
import qualified Proto3.Suite.Class as HsProtobuf
import qualified Proto3.Suite.JSONPB as HsJSONPB
import Proto3.Suite.JSONPB ((.=), (.:))
import qualified Proto3.Wire as HsProtobuf
import Control.Applicative ((<*>), (<|>))
import qualified Data.Text.Lazy as Hs (Text)
import qualified Data.ByteString as Hs
import qualified Data.String as Hs (fromString)
import qualified Data.Vector as Hs (Vector)
import qualified Data.Int as Hs (Int16, Int32, Int64)
import qualified Data.Word as Hs (Word16, Word32, Word64)
import qualified GHC.Generics as Hs
import qualified GHC.Enum as Hs
 
data WithNesting = WithNesting{withNestingNestedMessage1 ::
                               Hs.Maybe TestProtoImport.WithNesting_Nested,
                               withNestingNestedMessage2 ::
                               Hs.Maybe TestProtoImport.WithNesting_Nested}
                 deriving (Hs.Show, Hs.Eq, Hs.Ord, Hs.Generic)
 
instance HsProtobuf.Named WithNesting where
        nameOf _ = (Hs.fromString "WithNesting")
 
instance HsProtobuf.Message WithNesting where
        encodeMessage _
          WithNesting{withNestingNestedMessage1 = withNestingNestedMessage1,
                      withNestingNestedMessage2 = withNestingNestedMessage2}
          = (Hs.mconcat
               [(HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 1)
                   (HsProtobuf.Nested withNestingNestedMessage1)),
                (HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 100)
                   (HsProtobuf.Nested withNestingNestedMessage2))])
        decodeMessage _
          = (Hs.pure WithNesting) <*>
              ((Hs.pure HsProtobuf.nested) <*>
                 (HsProtobuf.at HsProtobuf.decodeMessageField
                    (HsProtobuf.FieldNumber 1)))
              <*>
              ((Hs.pure HsProtobuf.nested) <*>
                 (HsProtobuf.at HsProtobuf.decodeMessageField
                    (HsProtobuf.FieldNumber 100)))
        dotProto _
          = [(HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 1)
                (HsProtobuf.Prim (HsProtobuf.Named (HsProtobuf.Single "Nested")))
                (HsProtobuf.Single "nestedMessage1")
                []
                Hs.Nothing),
             (HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 100)
                (HsProtobuf.Prim (HsProtobuf.Named (HsProtobuf.Single "Nested")))
                (HsProtobuf.Single "nestedMessage2")
                []
                Hs.Nothing)]
 
instance HsJSONPB.ToJSONPB WithNesting where
        toEncodingPB (WithNesting f1 f100)
          = (HsJSONPB.fieldsPB
               ["nestedMessage1" .= f1, "nestedMessage2" .= f100])
 
instance HsJSONPB.FromJSONPB WithNesting where
        parseJSONPB
          = (HsJSONPB.withObject "WithNesting"
               (\ obj ->
                  (Hs.pure WithNesting) <*> obj .: "nestedMessage1" <*>
                    obj .: "nestedMessage2"))
 
data WithNesting_Nested = WithNesting_Nested{withNesting_NestedNestedField1
                                             :: Hs.Int32,
                                             withNesting_NestedNestedField2 :: Hs.Int32}
                        deriving (Hs.Show, Hs.Eq, Hs.Ord, Hs.Generic)
 
instance HsProtobuf.Named WithNesting_Nested where
        nameOf _ = (Hs.fromString "WithNesting_Nested")
 
instance HsProtobuf.Message WithNesting_Nested where
        encodeMessage _
          WithNesting_Nested{withNesting_NestedNestedField1 =
                               withNesting_NestedNestedField1,
                             withNesting_NestedNestedField2 = withNesting_NestedNestedField2}
          = (Hs.mconcat
               [(HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 1)
                   withNesting_NestedNestedField1),
                (HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 2)
                   withNesting_NestedNestedField2)])
        decodeMessage _
          = (Hs.pure WithNesting_Nested) <*>
              (HsProtobuf.at HsProtobuf.decodeMessageField
                 (HsProtobuf.FieldNumber 1))
              <*>
              (HsProtobuf.at HsProtobuf.decodeMessageField
                 (HsProtobuf.FieldNumber 2))
        dotProto _
          = [(HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 1)
                (HsProtobuf.Prim HsProtobuf.Int32)
                (HsProtobuf.Single "nestedField1")
                []
                Hs.Nothing),
             (HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 2)
                (HsProtobuf.Prim HsProtobuf.Int32)
                (HsProtobuf.Single "nestedField2")
                []
                Hs.Nothing)]
 
instance HsJSONPB.ToJSONPB WithNesting_Nested where
        toEncodingPB (WithNesting_Nested f1 f2)
          = (HsJSONPB.fieldsPB ["nestedField1" .= f1, "nestedField2" .= f2])
 
instance HsJSONPB.FromJSONPB WithNesting_Nested where
        parseJSONPB
          = (HsJSONPB.withObject "WithNesting_Nested"
               (\ obj ->
                  (Hs.pure WithNesting_Nested) <*> obj .: "nestedField1" <*>
                    obj .: "nestedField2"))