{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
-- | Generated by Haskell protocol buffer compiler. DO NOT EDIT!
module TestProtoOneof where
import qualified Prelude as Hs
import qualified Proto3.Suite.DotProto as HsProtobuf
import qualified Proto3.Suite.Types as HsProtobuf
import qualified Proto3.Suite.Class as HsProtobuf
import qualified Proto3.Suite.JSONPB as HsJSONPB
import Proto3.Suite.JSONPB ((.=), (.:))
import qualified Proto3.Wire as HsProtobuf
import Control.Applicative ((<*>), (<|>), (<$>))
import qualified Control.Monad as Hs
import qualified Data.Text.Lazy as Hs (Text)
import qualified Data.ByteString as Hs
import qualified Data.String as Hs (fromString)
import qualified Data.Vector as Hs (Vector)
import qualified Data.Int as Hs (Int16, Int32, Int64)
import qualified Data.Word as Hs (Word16, Word32, Word64)
import qualified GHC.Generics as Hs
import qualified GHC.Enum as Hs
 
data DummyMsg = DummyMsg{dummyMsgDummy :: Hs.Int32}
              deriving (Hs.Show, Hs.Eq, Hs.Ord, Hs.Generic)
 
instance HsProtobuf.Named DummyMsg where
        nameOf _ = (Hs.fromString "DummyMsg")
 
instance HsProtobuf.Message DummyMsg where
        encodeMessage _ DummyMsg{dummyMsgDummy = dummyMsgDummy}
          = (Hs.mconcat
               [(HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 1)
                   dummyMsgDummy)])
        decodeMessage _
          = (Hs.pure DummyMsg) <*>
              (HsProtobuf.at HsProtobuf.decodeMessageField
                 (HsProtobuf.FieldNumber 1))
        dotProto _
          = [(HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 1)
                (HsProtobuf.Prim HsProtobuf.Int32)
                (HsProtobuf.Single "dummy")
                []
                Hs.Nothing)]
 
instance HsJSONPB.ToJSONPB DummyMsg where
        toEncodingPB (DummyMsg f1) = (HsJSONPB.fieldsPB ["dummy" .= f1])
 
instance HsJSONPB.FromJSONPB DummyMsg where
        parseJSONPB
          = (HsJSONPB.withObject "DummyMsg"
               (\ obj -> (Hs.pure DummyMsg) <*> obj .: "dummy"))
 
data DummyEnum = DummyEnumDUMMY
               | DummyEnumDUMMY2
               deriving (Hs.Show, Hs.Bounded, Hs.Eq, Hs.Ord, Hs.Generic)
 
instance HsProtobuf.Named DummyEnum where
        nameOf _ = (Hs.fromString "DummyEnum")
 
instance Hs.Enum DummyEnum where
        toEnum 0 = DummyEnumDUMMY
        toEnum 1 = DummyEnumDUMMY2
        toEnum i = (Hs.toEnumError "DummyEnum" i (0 :: Hs.Int, 1))
        fromEnum (DummyEnumDUMMY) = 0
        fromEnum (DummyEnumDUMMY2) = 1
        succ (DummyEnumDUMMY) = DummyEnumDUMMY2
        succ _ = Hs.succError "DummyEnum"
        pred (DummyEnumDUMMY2) = DummyEnumDUMMY
        pred _ = Hs.predError "DummyEnum"
 
instance HsJSONPB.ToJSONPB DummyEnum where
        toEncodingPB x _ = HsJSONPB.namedEncoding x
 
instance HsJSONPB.FromJSONPB DummyEnum where
        parseJSONPB (HsJSONPB.String "DUMMY") = Hs.pure DummyEnumDUMMY
        parseJSONPB (HsJSONPB.String "DUMMY2") = Hs.pure DummyEnumDUMMY2
        parseJSONPB v = (HsJSONPB.typeMismatch "DummyEnum" v)
 
data Something = Something{somethingValue :: Hs.Int64,
                           somethingAnother :: Hs.Int32, somethingPickOne :: SomethingPickOne}
               deriving (Hs.Show, Hs.Eq, Hs.Ord, Hs.Generic)
 
instance HsProtobuf.Named Something where
        nameOf _ = (Hs.fromString "Something")
 
instance HsProtobuf.Message Something where
        encodeMessage _
          Something{somethingValue = somethingValue,
                    somethingAnother = somethingAnother,
                    somethingPickOne = somethingPickOne}
          = (Hs.mconcat
               [(HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 1)
                   (HsProtobuf.Signed somethingValue)),
                (HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 2)
                   (HsProtobuf.Signed somethingAnother)),
                case somethingPickOne of
                    SomethingPickOneName x
                      -> (HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 4) x)
                    SomethingPickOneSomeid x
                      -> (HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 9) x)
                    SomethingPickOneDummyMsg x
                      -> (HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 10)
                            (HsProtobuf.Nested x))
                    SomethingPickOneDummyEnum x
                      -> (HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 11) x)
                    SomethingPickOne_NOT_SET -> Hs.mempty])
        decodeMessage _
          = (Hs.pure Something) <*>
              ((Hs.pure HsProtobuf.signed) <*>
                 (HsProtobuf.at HsProtobuf.decodeMessageField
                    (HsProtobuf.FieldNumber 1)))
              <*>
              ((Hs.pure HsProtobuf.signed) <*>
                 (HsProtobuf.at HsProtobuf.decodeMessageField
                    (HsProtobuf.FieldNumber 2)))
              <*>
              (HsProtobuf.oneof
                 [((HsProtobuf.FieldNumber 4),
                   (Hs.pure SomethingPickOneName) <*> HsProtobuf.decodeMessageField),
                  ((HsProtobuf.FieldNumber 9),
                   (Hs.pure SomethingPickOneSomeid) <*>
                     HsProtobuf.decodeMessageField),
                  ((HsProtobuf.FieldNumber 10),
                   (Hs.pure SomethingPickOneDummyMsg) <*>
                     ((Hs.pure HsProtobuf.nested) <*> HsProtobuf.decodeMessageField)),
                  ((HsProtobuf.FieldNumber 11),
                   (Hs.pure SomethingPickOneDummyEnum) <*>
                     HsProtobuf.decodeMessageField)])
        dotProto _
          = [(HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 1)
                (HsProtobuf.Prim HsProtobuf.SInt64)
                (HsProtobuf.Single "value")
                []
                Hs.Nothing),
             (HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 2)
                (HsProtobuf.Prim HsProtobuf.SInt32)
                (HsProtobuf.Single "another")
                []
                Hs.Nothing)]
 
instance HsJSONPB.ToJSONPB Something where
        toEncodingPB (Something f1 f2 f4_or_f9_or_f10_or_f11)
          = (HsJSONPB.fieldsPB
               ["value" .= f1, "another" .= f2,
                case f4_or_f9_or_f10_or_f11 of
                    SomethingPickOneName f4 -> (HsJSONPB.pair "name" f4)
                    SomethingPickOneSomeid f9 -> (HsJSONPB.pair "someid" f9)
                    SomethingPickOneDummyMsg f10 -> (HsJSONPB.pair "dummyMsg" f10)
                    SomethingPickOneDummyEnum f11 -> (HsJSONPB.pair "dummyEnum" f11)
                    SomethingPickOne_NOT_SET -> Hs.mempty])
 
instance HsJSONPB.FromJSONPB Something where
        parseJSONPB
          = (HsJSONPB.withObject "Something"
               (\ obj ->
                  (Hs.pure Something) <*> obj .: "value" <*> obj .: "another" <*>
                    Hs.msum
                      [SomethingPickOneName <$> (HsJSONPB.parseField obj "name"),
                       SomethingPickOneSomeid <$> (HsJSONPB.parseField obj "someid"),
                       SomethingPickOneDummyMsg <$> (HsJSONPB.parseField obj "dummyMsg"),
                       SomethingPickOneDummyEnum <$>
                         (HsJSONPB.parseField obj "dummyEnum"),
                       Hs.pure SomethingPickOne_NOT_SET]))
 
data SomethingPickOne = SomethingPickOne_NOT_SET
                      | SomethingPickOneName Hs.Text
                      | SomethingPickOneSomeid Hs.Int32
                      | SomethingPickOneDummyMsg (Hs.Maybe TestProtoOneof.DummyMsg)
                      | SomethingPickOneDummyEnum (HsProtobuf.Enumerated
                                                     TestProtoOneof.DummyEnum)
                      deriving (Hs.Show, Hs.Eq, Hs.Ord, Hs.Generic)