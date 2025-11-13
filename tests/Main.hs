{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ImplicitParams        #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import           ArbitraryGeneratedTestTypes ()
import qualified Data.ByteString             as B
import qualified Data.ByteString.Char8       as BC
import qualified Data.ByteString.Lazy        as BL
import qualified Data.ByteString.Short       as BS
import           Data.Either                 (isRight)
import           Data.Functor.Identity       (Identity(..))
import           Data.Int                    (Int8, Int16, Int32, Int64)
import qualified Data.List.NonEmpty          as NE
import           Data.Proxy                  (Proxy(..))
import           Data.String
import qualified Data.Text.Short
import           Data.Typeable               (Typeable, showsTypeRep, typeRep)
import           Data.Word                   (Word8, Word16, Word32, Word64)
import qualified Data.Vector                 as V
import           GHC.Exts                    (fromList, Proxy#)
import           GHC.TypeLits                (Nat, SomeNat(..), Symbol, TypeError, someNatVal)
import           Proto3.Suite
import qualified Proto3.Suite.Form           as Form
import qualified Proto3.Suite.Form.Encode    as FormE
import           Proto3.Suite.Haskell.Parser (Logger, initLogger)
import qualified Proto3.Suite.JSONPB.Class   as JSONPB
import           Proto3.Wire.Decode          (ParseError)
import qualified Proto3.Wire.Decode          as Decode
import qualified Proto3.Wire.Reverse         as RB
import           Proto3.Wire.Types           as P
import Test.DocTest qualified 
import           Test.QuickCheck             (Arbitrary, Property, (.&&.), arbitrary, choose,
                                              counterexample, forAll, oneof, property)
import           Test.Tasty
import           Test.Tasty.HUnit            (Assertion, assertBool, assertEqual,
                                              testCase, (@=?), (@?=))
import           Test.Tasty.QuickCheck       (testProperty, (===))
import           TestCodeGen
import qualified TestProto                   as TP
import qualified TestProtoOneof              as TPO
import qualified TestProtoOneofImport        as TPOI

#ifdef DHALL
import           TestDhall
#endif

import qualified Test.Proto.Generate.Name
import qualified Test.Proto.Parse.Option
import qualified Test.Proto.Interval
import qualified Test.Proto.TH
import           Test.Proto.ToEncoder        (Iterator(Forward, Vector),
                                              Stripping(Keep, Strip), ToEncoder(..))

-- -----------------------------------------------------------------------------

main :: IO ()
main = do
  logger <- initLogger
  defaultMain (tests logger)

tests :: Logger -> TestTree
tests logger = testGroup "Tests"
  [ docTests 
  , qcProperties
  , encodeUnitTests
  , decodeUnitTests
  , parserUnitTests
  , dotProtoUnitTests
  , codeGenTests logger
  , Test.Proto.Generate.Name.testTree
  , Test.Proto.Parse.Option.testTree
  , Test.Proto.Interval.testTree
  , Test.Proto.TH.testTree

#ifdef DHALL
  , dhallTests
#endif
  ]

--------------------------------------------------------------------------------
-- Doctests

docTests :: TestTree
docTests = testCase "doctests" $ do
  putStrLn "Running all doctests..."
  Test.DocTest.doctest
    [ "--verbose"
    , "-package"
    , "ghc-lib-parser"
    , "-isrc"
    , "-XBlockArguments"
    , "-XPackageImports"
#ifdef SWAGGER
#ifdef SWAGGER_WRAPPER_FORMAT
    , "-isrc/swagger-wrapper-format"
    , "-DSWAGGER_WRAPPER_FORMAT"
#else
    , "-isrc/no-swagger-wrapper-format"
#endif
    , "-DSWAGGER"
#endif
#ifdef DHALL
    , "-DDHALL"
#endif
#ifdef LARGE_RECORDS
    , "-DLARGE_RECORDS"
#endif
    , "src/Proto3/Suite/DotProto/Internal.hs"
    , "src/Proto3/Suite/DotProto/Generate.hs"
    , "src/Proto3/Suite/JSONPB/Class.hs"
    , "src/Proto3/Suite/Tutorial.hs"
    ]

--------------------------------------------------------------------------------
-- QuickCheck properties

type MsgProp a = a -> Property

qcProperties :: TestTree
qcProperties = testGroup "QuickCheck properties"
  [ qcPropDecEncId
  ]

-- | Verifies that @decode . encode = id@ for various message types
qcPropDecEncId :: TestTree
qcPropDecEncId = testGroup "Property: (decode . encode = id) for various message types"
  [ testProperty "Trivial"             (prop :: MsgProp TP.Trivial)
  , testProperty "MultipleFields"      (prop :: MsgProp TP.MultipleFields)
  , testProperty "WithEnum"            (prop :: MsgProp TP.WithEnum)
  , testProperty "WithNesting"         (prop :: MsgProp TP.WithNesting)
  , testProperty "WithRepetition"      (prop :: MsgProp TP.WithRepetition)
  , testProperty "WithRepeatedSigned"  (prop :: MsgProp TP.WithRepeatedSigned)
  , testProperty "WithFixed"           (prop :: MsgProp TP.WithFixed)
  , testProperty "WithBytes"           (prop :: MsgProp TP.WithBytes)
  , testProperty "AllPackedTypes"      (prop :: MsgProp TP.AllPackedTypes)
  , testProperty "SignedInts"          (prop :: MsgProp TP.SignedInts)
  , testProperty "WithNestingRepeated" (prop :: MsgProp TP.WithNestingRepeated)
  , deeplyNest prop 1000
  ]
  where
    prop :: (Message a, Arbitrary a, Eq a, Show a) => MsgProp a
    prop msg = msg === (dec . enc) msg
      where
        dec = either (error . ("error parsing: " <>) . show) id . fromByteString
        enc = BL.toStrict . toLazyByteString

    deeplyNest :: MsgProp TP.Wrapped -> Int -> TestTree
    deeplyNest pf 0 = testProperty "Deeply nested" pf
    deeplyNest pf n = deeplyNest (pf . TP.Wrapped . Just) (n-1)


--------------------------------------------------------------------------------
-- Encoding

encodeUnitTests :: TestTree
encodeUnitTests = testGroup "Encoder unit tests"
  [ encoderMatchesGoldens
  , encoderPromotionsAndAuto
  , encodeBytesFromBuilder
  , encodeMessageReflection
  , encodeCachedSubmessage
  , testShowMessageEncoder
  , testShowMessageEncoding
  , testToJSONPBMessageEncoder
  , testToJSONPBMessageEncoding
  , testFromJSONPBMessageEncoder
  , testFromJSONPBMessageEncoding
  ]

-- TODO: We should consider generating the reference encodings
-- (test-files/make_reference_encodings.py) as a part of running the test suite
-- rather than having them in the repository.
encoderMatchesGoldens :: TestTree
encoderMatchesGoldens = testGroup "Encoder matches golden encodings"
  [ check "trivial.bin"               $ TP.Trivial 123
  , check "trivial_negative.bin"      $ TP.Trivial (-1)
  , check "multiple_fields.bin"       $ TP.MultipleFields 1.23 (-0.5) 123 1234567890 "Hello, world!" True
  , check "signedints.bin"            $ TP.SignedInts (-42) (-84)
  , check "with_nesting.bin"          $ TP.WithNesting $ Just $ TP.WithNesting_Nested "123abc" 123456 [] []
  , check "with_enum0.bin"            $ TP.WithEnum $ Enumerated $ Right $ TP.WithEnum_TestEnumENUM1
  , check "with_enum1.bin"            $ TP.WithEnum $ Enumerated $ Right $ TP.WithEnum_TestEnumENUM2
  , check "with_repetition.bin"       $ TP.WithRepetition [1..5]
  , check "with_repeated_signed.bin"  $ TP.WithRepeatedSigned
      [ 0, -1, 1, -2, 2,
        0x3FFFFFFF, -0x40000000, 0x40000000, -0x40000001,
        0x7FFFFFFF, -0x7FFFFFFF - 1 ]
      [ 0, -1, 1, -2, 2,
        0x3FFFFFFFFFFFFFFF, -0x4000000000000000, 0x4000000000000000, -0x4000000000000001,
        0x7FFFFFFFFFFFFFFF, -0x7FFFFFFFFFFFFFFF - 1 ]
  , check "with_bytes.bin"            $ TP.WithBytes (BC.pack "abc") (fromList $ map BC.pack ["abc","123"])
  , check "with_nesting_repeated.bin" $ TP.WithNestingRepeated
                                          [ TP.WithNestingRepeated_Nested "123abc" 123456 [1,2,3,4] [5,6,7,8]
                                          , TP.WithNestingRepeated_Nested "abc123" 654321 [0,9,8,7] [6,5,4,3]
                                          ]
  ]
  where
    check fp v = testCase fp $ do
      goldenEncoding <- BL.readFile (testFilesPfx <> fp)
      assertEqual (show fp ++ ": encoding from intermediate representation")
        goldenEncoding (toLazyByteString v)
      -- NOTE: We skip the 'Identity' iterator in this test because we expect it
      -- will create longer output for packed fields than the golden result when
      -- there is more than one element of the repetition--in which case our test
      -- code misuses 'Identity' by using it repeatedly, once for each element.
      -- Though suboptimal, the resulting encoding remains decodable by parsers
      -- that adhere to the protobuf specification.  We check that decodability
      -- in other tests that pair a Haskell encoder with a Python decoder.
      assertEqual (show fp ++ ": direct encoding, Forward iterator, keep wrappers")
        goldenEncoding $ FormE.messageEncoderToLazyByteString $
          let ?iterator = Forward
              ?stripping = Keep
          in toEncoder v
      assertEqual (show fp ++ ": direct encoding, Forward iterator, strip wrappers")
        goldenEncoding $ FormE.messageEncoderToLazyByteString $
          let ?iterator = Forward
              ?stripping = Strip
          in toEncoder v
      assertEqual (show fp ++ ": direct encoding, Vector iterator, keep wrappers")
        goldenEncoding $ FormE.messageEncoderToLazyByteString $
          let ?iterator = Vector
              ?stripping = Keep
          in toEncoder v
      assertEqual (show fp ++ ": direct encoding, Vector iterator, strip wrappers")
        goldenEncoding $ FormE.messageEncoderToLazyByteString $
          let ?iterator = Vector
              ?stripping = Strip
          in toEncoder v

-- Simulated protobuf message type having a single field named @myBytes@ of
-- type @bytes@ with field number @8@ and the specified 'Form.Cardinality'.
data MyWithBytes (r :: Form.Cardinality)

type instance Form.NamesOf (MyWithBytes r) = '["x"]

type instance Form.NumberOf (MyWithBytes r) name = MyWithBytes_NumberOf r name

type family MyWithBytes_NumberOf (r :: Form.Cardinality) (name :: Symbol) :: Nat
  where
    MyWithBytes_NumberOf _ "myBytes" = 8
    MyWithBytes_NumberOf r name = TypeError (Form.FieldNotFound (MyWithBytes r) name)

type instance Form.ProtoTypeOf (MyWithBytes r) name = MyWithBytes_ProtoTypeOf r name

type family MyWithBytes_ProtoTypeOf (r :: Form.Cardinality) (name :: Symbol) :: Form.ProtoType
  where
    MyWithBytes_ProtoTypeOf _ "myBytes" = 'Form.Bytes
    MyWithBytes_ProtoTypeOf r name = TypeError (Form.FieldNotFound (MyWithBytes r) name)

type instance Form.OneOfOf (MyWithBytes r) name = MyWithBytes_OneOfOf r name

type family MyWithBytes_OneOfOf (r :: Form.Cardinality) (name :: Symbol) :: Symbol
  where
    MyWithBytes_OneOfOf 'Form.Optional "myBytes" = "pickOne"
    MyWithBytes_OneOfOf 'Form.Optional "pickOne" = "pickOne"
    MyWithBytes_OneOfOf r "myBytes" = ""
    MyWithBytes_OneOfOf r name = TypeError (Form.FieldOrOneOfNotFound (MyWithBytes r) name)

type instance Form.CardinalityOf (MyWithBytes r) name = MyWithBytes_CardinalityOf r name

type family MyWithBytes_CardinalityOf (r :: Form.Cardinality) (name :: Symbol) :: Form.Cardinality
  where
    MyWithBytes_CardinalityOf r "myBytes" = r
    MyWithBytes_CardinalityOf 'Form.Optional "pickOne" = 'Form.Optional
    MyWithBytes_CardinalityOf r name = TypeError (Form.FieldOrOneOfNotFound (MyWithBytes r) name)

encodeBytesFromBuilder :: TestTree
encodeBytesFromBuilder = testCase "bytes from builder" $ do
    assertEqual "implicitly empty"
      (enc @'Form.Implicit (mempty :: RB.BuildR)) ""
    assertEqual "implicit but nonempty"
      (enc @'Form.Implicit (RB.byteString "xyz")) "B\ETXxyz"
    assertEqual "unset optional"
      (enc @'Form.Optional (Nothing :: Maybe RB.BuildR)) ""
    assertEqual "set empty optional"
      (enc @'Form.Optional (Just (mempty :: RB.BuildR))) "B\NUL"
    assertEqual "set nonempty optional"
      (enc @'Form.Optional (Just (RB.byteString "xyz"))) "B\ETXxyz"
    assertEqual "zero repetitions"
      (enc @('Form.Repeated 'Form.Unpacked)
           ([] :: [RB.BuildR]))
      ""
    assertEqual "single empty"
      (enc @('Form.Repeated 'Form.Unpacked)
           ([mempty] :: [RB.BuildR]))
      "B\NUL"
    assertEqual "double empty"
      (enc @('Form.Repeated 'Form.Unpacked)
           ([mempty, mempty] :: [RB.BuildR]))
      "B\NULB\NUL"
    assertEqual "empty nonempty"
      (enc @('Form.Repeated 'Form.Unpacked)
           ([mempty, RB.byteString "xyz"] :: [RB.BuildR]))
      "B\NULB\ETXxyz"
    assertEqual "nonempty empty"
      (enc @('Form.Repeated 'Form.Unpacked)
           (V.fromList [RB.byteString "uv", mempty]))
      "B\STXuvB\NUL"
    assertEqual "double nonempty"
      (enc @('Form.Repeated 'Form.Unpacked)
           (V.fromList [RB.byteString "uv", RB.byteString "xyz"]))
      "B\STXuvB\ETXxyz"
  where
    enc ::
      forall r a .
      ( FormE.Distinct (MyWithBytes r) (FormE.Occupy (MyWithBytes r) "myBytes" '[])
      , FormE.Field "myBytes" a (MyWithBytes r)
      ) =>
      a ->
      BL.ByteString
    enc =
      FormE.messageEncoderToLazyByteString .
      FormE.fieldsToMessage .
      FormE.field @"myBytes" @a @(MyWithBytes r)

encodeMessageReflection :: TestTree
encodeMessageReflection = testCase "messageReflection" $ do
  let msg = TP.Trivial 123
  FormE.messageEncoderToLazyByteString (FormE.messageReflection msg) @?= toLazyByteString msg

encodeCachedSubmessage :: TestTree
encodeCachedSubmessage = testGroup "Cached Submessages"
  [ testOptional
  , testRepeated
  , testOneof
  ]
  where
    testOptional :: TestTree
    testOptional = testCase "cached optional submessage" $ do
      let trivial = TP.Trivial 123
          wrappedTrivial = FormE.fieldsToMessage @TP.WrappedTrivial $
            FormE.field @"trivial" (Just (FormE.messageCache trivial))
      FormE.messageEncoderToLazyByteString wrappedTrivial @?=
        toLazyByteString (TP.WrappedTrivial (Just trivial))

    testRepeated :: TestTree
    testRepeated = testCase "cached repeated submessage" $ do
      let trivials :: V.Vector TP.MapTestEmulation_Trivial
          trivials =
            [ TP.MapTestEmulation_Trivial 15 (Just (TP.WrappedTrivial (Just (TP.Trivial 465))))
            , TP.MapTestEmulation_Trivial 25 (Just (TP.WrappedTrivial (Just (TP.Trivial 789))))
            ]
          mapTestEmulation = FormE.fieldsToMessage @TP.MapTestEmulation $
            FormE.field @"trivial" (V.map FormE.messageCache trivials)
      FormE.messageEncoderToLazyByteString mapTestEmulation @?=
        toLazyByteString (TP.MapTestEmulation mempty trivials mempty)

    testOneof :: TestTree
    testOneof = testCase "cached submessage within oneof" $ do
      let withOneof = TPOI.WithOneof (Just (TPOI.WithOneofPickOneB 123))
          withImported = FormE.fieldsToMessage @TPO.WithImported $
            FormE.field @"withOneof" (Just (FormE.messageCache withOneof))
      FormE.messageEncoderToLazyByteString withImported @?=
        toLazyByteString (TPO.WithImported (Just (TPO.WithImportedPickOneWithOneof withOneof)))

testShowMessageEncoding :: TestTree
testShowMessageEncoding = testCase "Show MessageEncoding" $ do
  let trivial = TP.Trivial 123
      badForm = "abc"
      encoding :: FormE.MessageEncoding TP.Trivial
      encoding = FormE.messageCache trivial
      bogus :: FormE.MessageEncoding TP.Trivial
      bogus = FormE.unsafeByteStringToMessageEncoding badForm
  -- Test desired behavior for valid encoding:
  show (Just encoding) @?=
    "Just (Proto3.Suite.Form.Encode.messageCache " ++ showsPrec 11 trivial ")"
  -- Test fallback behavior for invalid encoding:
  show (Just bogus) @?=
    "Just (Proto3.Suite.Form.Encode.unsafeByteStringToMessageEncoding " ++ showsPrec 11 badForm ")"

testShowMessageEncoder :: TestTree
testShowMessageEncoder = testCase "Show MessageEncoder" $ do
  let trivial = TP.Trivial 123
      badForm = "abc"
      encoder :: FormE.MessageEncoder TP.Trivial
      encoder = FormE.messageReflection trivial
      bogus :: FormE.MessageEncoder TP.Trivial
      bogus = FormE.unsafeByteStringToMessageEncoder badForm
  -- Test desired behavior for valid encoder:
  show (Just encoder) @?=
    "Just (Proto3.Suite.Form.Encode.messageReflection " ++ showsPrec 11 trivial ")"
  -- Test fallback behavior for invalid encoder:
  show (Just bogus) @?=
    "Just (Proto3.Suite.Form.Encode.unsafeByteStringToMessageEncoder " ++ showsPrec 11 badForm ")"

testToJSONPBMessageEncoding :: TestTree
testToJSONPBMessageEncoding = testProperty "ToJSONPB MessageEncoding" $ \opts ->
  let trivial = TP.Trivial 123
      badForm = "abc"
      encoding :: FormE.MessageEncoding TP.Trivial
      encoding = FormE.messageCache trivial
      bogus :: FormE.MessageEncoding TP.Trivial
      bogus = FormE.unsafeByteStringToMessageEncoding badForm
  in
    -- Test desired behavior for valid encoding:
    JSONPB.toJSONPB encoding opts === JSONPB.toJSONPB trivial opts
    .&&.
    JSONPB.toEncodingPB encoding opts === JSONPB.toEncodingPB trivial opts
    .&&.
    -- Test fallback behavior for invalid encoding:
    JSONPB.toJSONPB bogus opts === JSONPB.toJSONPB badForm opts
    .&&.
    JSONPB.toEncodingPB bogus opts === JSONPB.toEncodingPB badForm opts

testToJSONPBMessageEncoder :: TestTree
testToJSONPBMessageEncoder = testProperty "ToJSONPB MessageEncoder" $ \opts ->
  let trivial = TP.Trivial 123
      badForm = "abc"
      encoder :: FormE.MessageEncoder TP.Trivial
      encoder = FormE.messageReflection trivial
      bogus :: FormE.MessageEncoder TP.Trivial
      bogus = FormE.unsafeByteStringToMessageEncoder badForm
  in
    -- Test desired behavior for valid encoder:
    JSONPB.toJSONPB encoder opts === JSONPB.toJSONPB trivial opts
    .&&.
    JSONPB.toEncodingPB encoder opts === JSONPB.toEncodingPB trivial opts
    .&&.
    -- Test fallback behavior for invalid encoder:
    JSONPB.toJSONPB bogus opts === JSONPB.toJSONPB badForm opts
    .&&.
    JSONPB.toEncodingPB bogus opts === JSONPB.toEncodingPB badForm opts

testFromJSONPBMessageEncoding :: TestTree
testFromJSONPBMessageEncoding = testProperty "FromJSONPB MessageEncoding" $ \opts ->
  let trivial = TP.Trivial 123
      badForm = "abc"
      encoding :: FormE.MessageEncoding TP.Trivial
      encoding = FormE.messageCache trivial
      bogus :: FormE.MessageEncoding TP.Trivial
      bogus = FormE.unsafeByteStringToMessageEncoding badForm
      edec :: BL.ByteString -> Either String (FormE.MessageEncoding TP.Trivial)
      edec = JSONPB.eitherDecode
  in
    -- Test desired behavior for valid encoder:
    fmap (fromByteString . FormE.messageEncodingToByteString) (edec (JSONPB.encode opts encoding))
      === Right (Right trivial)
    .&&.
    -- Test fallback behavior for invalid encoder:
    fmap FormE.messageEncodingToByteString (edec (JSONPB.encode opts bogus)) === Right badForm

testFromJSONPBMessageEncoder :: TestTree
testFromJSONPBMessageEncoder = testProperty "FromJSONPB MessageEncoder" $ \opts ->
  let trivial = TP.Trivial 123
      badForm = "abc"
      encoder :: FormE.MessageEncoder TP.Trivial
      encoder = FormE.messageReflection trivial
      bogus :: FormE.MessageEncoder TP.Trivial
      bogus = FormE.unsafeByteStringToMessageEncoder badForm
      edec :: BL.ByteString -> Either String (FormE.MessageEncoder TP.Trivial)
      edec = JSONPB.eitherDecode
  in
    -- Test desired behavior for valid encoder:
    fmap (fromByteString . FormE.messageEncoderToByteString) (edec (JSONPB.encode opts encoder))
      === Right (Right trivial)
    .&&.
    -- Test fallback behavior for invalid encoder:
    fmap FormE.messageEncoderToByteString (edec (JSONPB.encode opts bogus)) === Right badForm

data TestMessage
       (num :: Nat)
       (repetition :: Maybe Form.Cardinality)
       (protoType :: Form.ProtoType)
  -- ^ Omitting the repetition means that field @"name"@ is in a @oneof@ named @"pickOne"@.

type instance Form.NamesOf (TestMessage num maybeRepetition protoType) = '[ "name" ]

type instance Form.NumberOf (TestMessage num maybeRepetition protoType) "name" = num

type instance Form.ProtoTypeOf (TestMessage num maybeRepetition protoType) "name" = protoType

type instance Form.OneOfOf (TestMessage num 'Nothing protoType) "name" = "pickOne"
type instance Form.OneOfOf (TestMessage num 'Nothing protoType) "pickOne" = "pickOne"
type instance Form.OneOfOf (TestMessage num ('Just _) protoType) "name" = ""

type instance Form.CardinalityOf (TestMessage num 'Nothing protoType) "name" = 'Form.Optional
type instance Form.CardinalityOf (TestMessage num 'Nothing protoType) "pickOne" = 'Form.Optional
type instance Form.CardinalityOf (TestMessage num ('Just repetition) protoType) "name" = repetition

encoderPromotionsAndAuto :: TestTree
encoderPromotionsAndAuto = testGroup "Encoder promotes types correctly and Auto-selection works"
  [ testGroup "Encoder promotes types correctly"
      [ check @'Form.Int32 @Int8 @Int32 "int32" (fromIntegral @Int8 @Int32)
      , check @'Form.Int32 @Word8 @Int32 "int32" (fromIntegral @Word8 @Int32)
      , check @'Form.Int32 @Int16 @Int32 "int32" (fromIntegral @Int16 @Int32)
      , check @'Form.Int32 @Word16 @Int32 "int32" (fromIntegral @Word16 @Int32)
      , check @'Form.Int64 @Int8 @Int64 "int64" (fromIntegral @Int8 @Int64)
      , check @'Form.Int64 @Word8 @Int64 "int64" (fromIntegral @Word8 @Int64)
      , check @'Form.Int64 @Int16 @Int64 "int64"(fromIntegral @Int16 @Int64)
      , check @'Form.Int64 @Word16 @Int64 "int64"(fromIntegral @Word16 @Int64)
      , check @'Form.Int64 @Int32 @Int64 "int64"(fromIntegral @Int32 @Int64)
      , check @'Form.Int64 @Word32 @Int64 "int64"(fromIntegral @Word32 @Int64)
      , check @'Form.SInt32 @Int8 @Int32 "sint32" (fromIntegral @Int8 @Int32)
      , check @'Form.SInt32 @Word8 @Int32 "sint32" (fromIntegral @Word8 @Int32)
      , check @'Form.SInt32 @Int16 @Int32 "sint32" (fromIntegral @Int16 @Int32)
      , check @'Form.SInt32 @Word16 @Int32 "sint32" (fromIntegral @Word16 @Int32)
      , check @'Form.SInt64 @Int8 @Int64 "sint64" (fromIntegral @Int8 @Int64)
      , check @'Form.SInt64 @Word8 @Int64 "sint64" (fromIntegral @Word8 @Int64)
      , check @'Form.SInt64 @Int16 @Int64 "sint64" (fromIntegral @Int16 @Int64)
      , check @'Form.SInt64 @Word16 @Int64 "sint64" (fromIntegral @Word16 @Int64)
      , check @'Form.SInt64 @Int32 @Int64 "sint64" (fromIntegral @Int32 @Int64)
      , check @'Form.SInt64 @Word32 @Int64 "sint64" (fromIntegral @Word32 @Int64)
      , check @'Form.UInt32 @Word8 @Word32 "uint32" (fromIntegral @Word8 @Word32)
      , check @'Form.UInt32 @Word16 @Word32 "uint32" (fromIntegral @Word16 @Word32)
      , check @'Form.UInt64 @Word8 @Word64 "uint64" (fromIntegral @Word8 @Word64)
      , check @'Form.UInt64 @Word16 @Word64 "uint64" (fromIntegral @Word16 @Word64)
      , check @'Form.UInt64 @Word32 @Word64 "uint64" (fromIntegral @Word32 @Word64)
      , check @'Form.Fixed32 @Word8 @Word32 "fixed32" (fromIntegral @Word8 @Word32)
      , check @'Form.Fixed32 @Word16 @Word32 "fixed32" (fromIntegral @Word16 @Word32)
      , check @'Form.Fixed64 @Word8 @Word64 "fixed64" (fromIntegral @Word8 @Word64)
      , check @'Form.Fixed64 @Word16 @Word64 "fixed64" (fromIntegral @Word16 @Word64)
      , check @'Form.Fixed64 @Word32 @Word64 "fixed64" (fromIntegral @Word32 @Word64)
      , check @'Form.SFixed32 @Int8 @Int32 "sfixed32" (fromIntegral @Int8 @Int32)
      , check @'Form.SFixed32 @Word8 @Int32 "sfixed32" (fromIntegral @Word8 @Int32)
      , check @'Form.SFixed32 @Int16 @Int32 "sfixed32" (fromIntegral @Int16 @Int32)
      , check @'Form.SFixed32 @Word16 @Int32 "sfixed32" (fromIntegral @Word16 @Int32)
      , check @'Form.SFixed64 @Int8 @Int64 "sfixed64" (fromIntegral @Int8 @Int64)
      , check @'Form.SFixed64 @Word8 @Int64 "sfixed64" (fromIntegral @Word8 @Int64)
      , check @'Form.SFixed64 @Int16 @Int64 "sfixed64" (fromIntegral @Int16 @Int64)
      , check @'Form.SFixed64 @Word16 @Int64 "sfixed64" (fromIntegral @Word16 @Int64)
      , check @'Form.SFixed64 @Int32 @Int64 "sfixed64" (fromIntegral @Int32 @Int64)
      , check @'Form.SFixed64 @Word32 @Int64 "sfixed64" (fromIntegral @Word32 @Int64)
      , check @'Form.Double @Float @Double "double" (realToFrac @Float @Double)
      ]
  , testGroup "Encoder Auto wrapper does not change format"
      [ check @'Form.Int32 @Int32 @(FormE.Auto Int32) "int32" FormE.Auto
      , check @'Form.Int64 @Int64 @(FormE.Auto Int64) "int64" FormE.Auto
      , check @'Form.SInt32 @Int32 @(FormE.Auto Int32) "sint32" FormE.Auto
      , check @'Form.SInt64 @Int64 @(FormE.Auto Int64) "sint64" FormE.Auto
      , check @'Form.UInt32 @Word32 @(FormE.Auto Word32) "uint32" FormE.Auto
      , check @'Form.UInt64 @Word64 @(FormE.Auto Word64) "uint64" FormE.Auto
      , check @'Form.Fixed32 @Word32 @(FormE.Auto Word32) "fixed32" FormE.Auto
      , check @'Form.Fixed64 @Word64 @(FormE.Auto Word64) "fixed64" FormE.Auto
      , check @'Form.SFixed32 @Int32 @(FormE.Auto Int32) "sfixed32" FormE.Auto
      , check @'Form.SFixed64 @Int64 @(FormE.Auto Int64) "sfixed64" FormE.Auto
      , check @'Form.Bool @Bool @(FormE.Auto Bool) "bool" FormE.Auto
      , check @'Form.Float @Float @(FormE.Auto Float) "float" FormE.Auto
      , check @'Form.Double @Double @(FormE.Auto Double) "double" FormE.Auto
      , checkNotPacked @'Form.String @Data.Text.Short.ShortText @(FormE.Auto Data.Text.Short.ShortText) "string" FormE.Auto
      , checkNotPacked @'Form.Bytes @BS.ShortByteString @(FormE.Auto BS.ShortByteString) "bytes" FormE.Auto
      ]
  ]
  where
    check ::
      forall (protoType :: Form.ProtoType) a b .
      ( Typeable a
      , Typeable b
      , Arbitrary a
      , Show a
      , FormE.FieldForm 'Form.Implicit protoType a
      , FormE.FieldForm 'Form.Implicit protoType b
      , FormE.FieldForm 'Form.Optional protoType (Maybe a)
      , FormE.FieldForm 'Form.Optional protoType (Maybe b)
      , FormE.FieldForm ('Form.Repeated 'Form.Unpacked) protoType (Identity a)
      , FormE.FieldForm ('Form.Repeated 'Form.Unpacked) protoType (Identity b)
      , FormE.FieldForm ('Form.Repeated 'Form.Unpacked) protoType [a]
      , FormE.FieldForm ('Form.Repeated 'Form.Unpacked) protoType [b]
      , FormE.FieldForm ('Form.Repeated 'Form.Unpacked) protoType (V.Vector a)
      , FormE.FieldForm ('Form.Repeated 'Form.Unpacked) protoType (V.Vector b)
      , FormE.FieldForm ('Form.Repeated 'Form.Packed) protoType (Identity a)
      , FormE.FieldForm ('Form.Repeated 'Form.Packed) protoType (Identity b)
      , FormE.FieldForm ('Form.Repeated 'Form.Packed) protoType [a]
      , FormE.FieldForm ('Form.Repeated 'Form.Packed) protoType [b]
      , FormE.FieldForm ('Form.Repeated 'Form.Packed) protoType (V.Vector a)
      , FormE.FieldForm ('Form.Repeated 'Form.Packed) protoType (V.Vector b)
      ) =>
      String ->
      (a -> b) ->
      TestTree
    check protoTypeName convert =
      testProperty (showString protoTypeName $ showString ": " $
                    showsType @a $ showString " -> " $ showsType @b "") $
        propNotPacked @protoType convert .&&.
        propPacked @protoType convert

    checkNotPacked ::
      forall (protoType :: Form.ProtoType) a b .
      ( Typeable a
      , Typeable b
      , Arbitrary a
      , Show a
      , FormE.FieldForm 'Form.Implicit protoType a
      , FormE.FieldForm 'Form.Implicit protoType b
      , FormE.FieldForm 'Form.Optional protoType (Maybe a)
      , FormE.FieldForm 'Form.Optional protoType (Maybe b)
      , FormE.FieldForm ('Form.Repeated 'Form.Unpacked) protoType (Identity a)
      , FormE.FieldForm ('Form.Repeated 'Form.Unpacked) protoType (Identity b)
      , FormE.FieldForm ('Form.Repeated 'Form.Unpacked) protoType [a]
      , FormE.FieldForm ('Form.Repeated 'Form.Unpacked) protoType [b]
      , FormE.FieldForm ('Form.Repeated 'Form.Unpacked) protoType (V.Vector a)
      , FormE.FieldForm ('Form.Repeated 'Form.Unpacked) protoType (V.Vector b)
      ) =>
      String ->
      (a -> b) ->
      TestTree
    checkNotPacked protoTypeName convert =
      testProperty (showString protoTypeName $ showString ": " $
                    showsType @a $ showString " -> " $ showsType @b "") $
        propNotPacked @protoType convert

    propNotPacked ::
      forall (protoType :: Form.ProtoType) a b .
      ( Typeable a
      , Typeable b
      , Arbitrary a
      , Show a
      , FormE.FieldForm 'Form.Implicit protoType a
      , FormE.FieldForm 'Form.Implicit protoType b
      , FormE.FieldForm 'Form.Optional protoType (Maybe a)
      , FormE.FieldForm 'Form.Optional protoType (Maybe b)
      , FormE.FieldForm ('Form.Repeated 'Form.Unpacked) protoType (Identity a)
      , FormE.FieldForm ('Form.Repeated 'Form.Unpacked) protoType (Identity b)
      , FormE.FieldForm ('Form.Repeated 'Form.Unpacked) protoType [a]
      , FormE.FieldForm ('Form.Repeated 'Form.Unpacked) protoType [b]
      , FormE.FieldForm ('Form.Repeated 'Form.Unpacked) protoType (V.Vector a)
      , FormE.FieldForm ('Form.Repeated 'Form.Unpacked) protoType (V.Vector b)
      ) =>
      (a -> b) ->
      Property
    propNotPacked convert =
      forAll (choose (1, 536870911)) $ \fieldNum ->
      case someNatVal fieldNum of
        Nothing ->
          property False  -- Should never happen.
        Just (SomeNat (_ :: Proxy num)) ->
          forAll arbitrary $ \a ->
          forAll arbitrary $ \(as :: [a])->
          let b :: b
              b = convert a
              bs :: [b]
              bs = map convert as
              asVec :: V.Vector a
              asVec = V.fromList as
              bsVec :: V.Vector b
              bsVec = V.fromList bs
          in
          counterexample "Implicit"
            (check1 @num @('Just 'Form.Implicit) @protoType a b) .&&.
          counterexample "Optional - Nothing"
            (check1 @num @('Just 'Form.Optional) @protoType (Nothing @a) (Nothing @b)) .&&.
          counterexample "Optional - Just"
            (check1 @num @('Just 'Form.Optional) @protoType (Just a) (Just b)) .&&.
          counterexample "inside oneof"
            (check1 @num @'Nothing @protoType (Just a) (Just b)) .&&.
          counterexample "Unpacked Identity"
            (check1 @num @('Just ('Form.Repeated 'Form.Unpacked)) @protoType (Identity a) (Identity b)) .&&.
          counterexample "Unpacked Forward"
            (check1 @num @('Just ('Form.Repeated 'Form.Unpacked)) @protoType as bs) .&&.
          counterexample "Unpacked Vector"
            (check1 @num @('Just ('Form.Repeated 'Form.Unpacked)) @protoType asVec bsVec)

    propPacked ::
      forall (protoType :: Form.ProtoType) a b .
      ( Typeable a
      , Typeable b
      , Arbitrary a
      , Show a
      , FormE.FieldForm ('Form.Repeated 'Form.Packed) protoType (Identity a)
      , FormE.FieldForm ('Form.Repeated 'Form.Packed) protoType (Identity b)
      , FormE.FieldForm ('Form.Repeated 'Form.Packed) protoType [a]
      , FormE.FieldForm ('Form.Repeated 'Form.Packed) protoType [b]
      , FormE.FieldForm ('Form.Repeated 'Form.Packed) protoType (V.Vector a)
      , FormE.FieldForm ('Form.Repeated 'Form.Packed) protoType (V.Vector b)
      ) =>
      (a -> b) ->
      Property
    propPacked convert =
      forAll (choose (1, 536870911)) $ \fieldNum ->
      case someNatVal fieldNum of
        Nothing ->
          property False  -- Should never happen.
        Just (SomeNat (_ :: Proxy num)) ->
          forAll arbitrary $ \a ->
          forAll arbitrary $ \(as :: [a])->
          let b :: b
              b = convert a
              bs :: [b]
              bs = map convert as
              asVec :: V.Vector a
              asVec = V.fromList as
              bsVec :: V.Vector b
              bsVec = V.fromList bs
          in
          counterexample "Packed Identity"
            (check1 @num @('Just ('Form.Repeated 'Form.Packed)) @protoType (Identity a) (Identity b)) .&&.
          counterexample "Packed Forward"
            (check1 @num @('Just ('Form.Repeated 'Form.Packed)) @protoType as bs) .&&.
          counterexample "Packed Vector"
            (check1 @num @('Just ('Form.Repeated 'Form.Packed)) @protoType asVec bsVec)

    check1 ::
      forall (num :: Nat) (maybeRepetition :: Maybe Form.Cardinality)
             (protoType :: Form.ProtoType) a b .
      ( FormE.Field "name" a (TestMessage num maybeRepetition protoType)
      , FormE.Field "name" b (TestMessage num maybeRepetition protoType)
      , FormE.Distinct (TestMessage num maybeRepetition protoType)
                       (FormE.Occupy (TestMessage num maybeRepetition protoType) "name" '[])
      ) =>
      a ->
      b ->
      Property
    check1 a b =
      FormE.messageEncoderToLazyByteString
        (FormE.fieldsToMessage (FormE.field @"name" @a @(TestMessage num maybeRepetition protoType) a))
      ===
      FormE.messageEncoderToLazyByteString
        (FormE.fieldsToMessage (FormE.field @"name" @b @(TestMessage num maybeRepetition protoType) b))

    showsType :: forall a . Typeable a => ShowS
    showsType = showsTypeRep (typeRep (Proxy :: Proxy a))

--------------------------------------------------------------------------------
-- Decoding

decodeUnitTests :: TestTree
decodeUnitTests = testGroup "Decoder unit tests"
  [ decodeFromGoldens
  ]

decodeFromGoldens :: TestTree
decodeFromGoldens = testGroup "Decode golden encodings into key/value lists"
  [ check "trivial.bin"
  , check "trivial_negative.bin"
  , check "multiple_fields.bin"
  , check "signedints.bin"
  , check "with_nesting.bin"
  , check "with_enum0.bin"
  , check "with_enum1.bin"
  , check "with_repetition.bin"
  , check "with_bytes.bin"
  , check "with_nesting_repeated.bin"
  ]
  where
    check fp = testCase fp $ do
      kvs <- Decode.decodeWire <$> B.readFile (testFilesPfx <> fp)
      assertBool ("parsing " <> fp <> " into a key-value list succeeds") (isRight kvs)

--------------------------------------------------------------------------------
-- Parser

parserUnitTests :: TestTree
parserUnitTests = testGroup "Parser unit tests"
  [ parseFromGoldens
  ]

parseFromGoldens :: TestTree
parseFromGoldens = testGroup "Parse golden encodings"
  [ check "trivial.bin"               $ TP.Trivial 123
  , check "multiple_fields.bin"       $ TP.MultipleFields 1.23 (-0.5) 123 1234567890 "Hello, world!" True
  , check "signedints.bin"            $ TP.SignedInts (-42) (-84)
  , check "with_nesting.bin"          $ TP.WithNesting $ Just $ TP.WithNesting_Nested "123abc" 123456 [] []
  , check "with_enum0.bin"            $ TP.WithEnum $ Enumerated $ Right $ TP.WithEnum_TestEnumENUM1
  , check "with_enum1.bin"            $ TP.WithEnum $ Enumerated $ Right $ TP.WithEnum_TestEnumENUM2
  , check "with_repetition.bin"       $ TP.WithRepetition [1..5]
  , check "with_repeated_signed.bin"  $ TP.WithRepeatedSigned
      [ 0, -1, 1, -2, 2,
        0x3FFFFFFF, -0x40000000, 0x40000000, -0x40000001,
        0x7FFFFFFF, -0x7FFFFFFF - 1 ]
      [ 0, -1, 1, -2, 2,
        0x3FFFFFFFFFFFFFFF, -0x4000000000000000, 0x4000000000000000, -0x4000000000000001,
        0x7FFFFFFFFFFFFFFF, -0x7FFFFFFFFFFFFFFF - 1 ]
  , check "with_fixed.bin"            $ TP.WithFixed 16 (-123) 4096 (-4096)
  , check "with_bytes.bin"            $ TP.WithBytes (BC.pack "abc") (fromList $ map BC.pack ["abc","123"])
  , check "with_packing.bin"          $ TP.WithPacking [1,2,3] [1,2,3]
  , check "all_packed_types.bin"      $ TP.AllPackedTypes
                                          [1,2,3]
                                          [1,2,3]
                                          [-1,-2,-3]
                                          [-1,-2,-3]
                                          (fromList [1..3])
                                          (fromList [1..3])
                                          [1.0,2.0]
                                          [1.0,-1.0]
                                          (fromList [1,2,3])
                                          (fromList [1,2,3])
                                          [False,True]
                                          (Enumerated . Right <$> [TP.EFLD0, TP.EFLD1])
                                          (Enumerated . Right <$> [TP.EFLD0, TP.EFLD1])
  , check "with_nesting_repeated.bin" $ TP.WithNestingRepeated
                                          [ TP.WithNestingRepeated_Nested "123abc" 123456 [1,2,3,4] [5,6,7,8]
                                          , TP.WithNestingRepeated_Nested "abc123" 654321 [0,9,8,7] [6,5,4,3]
                                          ]
  , -- Checks parsing repeated embedded messages when one is expected (i.e.,
    -- this tests correct merging; this value was encoded as a
    -- WithNestingRepeated).
    check "with_nesting_repeated.bin" $ TP.WithNesting $ Just $ TP.WithNesting_Nested "abc123" 654321 [1,2,3,4,0,9,8,7] [5,6,7,8,6,5,4,3]
  , -- Checks that embedded message merging works correctly when fields have
    -- default values; this value was encoded as a WithNestingRepeatedInts
    check "with_nesting_ints.bin"     $ TP.WithNestingInts $ Just $ TP.NestedInts 2 2
  ]
  where
    check fp = testCase fp . testParser (testFilesPfx <> fp) fromByteString

testParser :: (Show a, Eq a)
           => FilePath -> (B.ByteString -> Either ParseError a) -> a -> IO ()
testParser fp p reference = do
  bs <- B.readFile fp
  case p bs of
    Left err        -> error $ "Got error: " ++ show err
    Right ourResult -> ourResult @?= reference

testDotProtoParse :: FilePath -> DotProto -> Assertion
testDotProtoParse file ast = do
  contents <- readFile file
  let path = metaModulePath $ protoMeta ast
  case parseProto path contents of
    Left err     -> error $ show err
    Right result -> ast @=? result

testDotProtoPrint :: DotProto -> String -> Assertion
testDotProtoPrint ast expected = expected @=? toProtoFileDef ast

testDotProtoRoundtrip :: DotProto -> Assertion
testDotProtoRoundtrip ast =
  let path = metaModulePath $ protoMeta ast in
  Right ast @=? parseProto path (toProtoFileDef ast)

dotProtoUnitTests :: TestTree
dotProtoUnitTests = testGroup ".proto parsing tests"
  [ dotProtoParseTrivial
  , dotProtoPrintTrivial
  , dotProtoParseEmptyStatement
  , dotProtoRoundtripTrivial
  , dotProtoRoundtripSimpleMessage
  , dotProtoRoundtripExtend
  , qcDotProtoRoundtrip
  ]

trivialDotProto :: DotProto
trivialDotProto = DotProto [] [] DotProtoNoPackage [] (DotProtoMeta (Path $ "test-files" NE.:| ["test_trivial"]))

dotProtoParseTrivial :: TestTree
dotProtoParseTrivial = testCase
  "Parse a content-less file" $
  testDotProtoParse (testFilesPfx <> "trivial.proto") trivialDotProto

dotProtoPrintTrivial :: TestTree
dotProtoPrintTrivial = testCase
  "Print a content-less DotProto" $
  testDotProtoPrint trivialDotProto "syntax = \"proto3\";"

dotProtoParseEmptyStatement :: TestTree
dotProtoParseEmptyStatement =
  testCase "Parse empty statements" (testDotProtoParse filePath dotProtoAST)
  where
    filePath :: FilePath
    filePath = testFilesPfx <> "test_proto_empty_field.proto"

    dotProtoAST :: DotProto
    dotProtoAST =
      DotProto [] []
        (DotProtoPackageSpec (Single "TestProtoEmptyField"))
        [ DotProtoEnum
            ""
            (Single "EnumWithEmptyField")
            [ DotProtoEnumField (Single "enum_foo") 0 []
            , DotProtoEnumField (Single "enum_bar") 1 []
            ]
        , DotProtoMessage
            ""
            (Single "MessageWithEmptyField")
            [ DotProtoMessageField (DotProtoField
                { dotProtoFieldNumber = 1
                , dotProtoFieldType = Prim UInt32
                , dotProtoFieldName = Single "foo"
                , dotProtoFieldOptions = []
                , dotProtoFieldComment = ""
                })
            , DotProtoMessageField (DotProtoField
                { dotProtoFieldNumber = 2
                , dotProtoFieldType = Prim UInt32
                , dotProtoFieldName = Single "bar"
                , dotProtoFieldOptions = []
                , dotProtoFieldComment = ""
                })
            ]
        ]
        (DotProtoMeta (Path (testFilesPfx NE.:| ["test_proto_empty_field.proto"])))

dotProtoRoundtripTrivial :: TestTree
dotProtoRoundtripTrivial = testCase
  "Printing then parsing a content-less DotProto yields an empty DotProto" $
  testDotProtoRoundtrip trivialDotProto

dotProtoSimpleMessage :: DotProto
dotProtoSimpleMessage = DotProto [] [] DotProtoNoPackage
  [ DotProtoMessage "" (Single "MessageTest")
      [ DotProtoMessageField $
          DotProtoField (fieldNumber 1) (Prim Int32) (Single "testfield") [] ""
      ]
  ]
  (DotProtoMeta (Path ("test-files" NE.:| ["simple"])))

dotProtoRoundtripSimpleMessage :: TestTree
dotProtoRoundtripSimpleMessage = testCase
  "Round-trip for a single flat message" $
  testDotProtoRoundtrip dotProtoSimpleMessage

qcDotProtoRoundtrip :: TestTree
qcDotProtoRoundtrip = testProperty
  "Round-trip for a randomly-generated .proto AST" roundtrip
  where
    roundtrip :: DotProto -> Property
    roundtrip ast = let generated = toProtoFileDef ast
                    in case parseProto fakePath generated of
                      Left err     -> error $ formatParseError err generated
                      Right result -> counterexample (formatMismatch ast generated result ) (ast == result)

    formatMismatch initial generated result = "AST changed during reparsing\n\nInitial AST:\n\n"
                                           ++ show initial
                                           ++ "\n\nGenerated .proto file:\n\n"
                                           ++ generated
                                           ++ "\n\nReparsed AST:\n\n"
                                           ++ show result
                                           ++ "\n\nRegenerated .proto file:\n\n"
                                           ++ (toProtoFileDef result)
    formatParseError err generated = "Parsec error:\n\n"
                                  ++ show err
                                  ++ "\n\nWhen attempting to parse:\n\n"
                                  ++ generated
                                  ++ "\n\nInitial AST:\n\n"

dotProtoRoundtripExtend :: TestTree
dotProtoRoundtripExtend =
  let expected :: DotProto
      expected = DotProto [] [] DotProtoNoPackage [] (DotProtoMeta (Path ("test-files" NE.:| ["test_proto_extend"])))
   in testCase
        "Round-trip for a message extension"
        (testDotProtoRoundtrip expected)

--------------------------------------------------------------------------------
-- Helpers

dotProtoFor :: (Named a, Message a) => Proxy# a -> DotProto
dotProtoFor proxy = DotProto [] [] DotProtoNoPackage
  [ DotProtoMessage
      "" (Single (nameOf proxy)) (DotProtoMessageField <$> dotProto proxy)
  ]
  (DotProtoMeta (Path $ "mypath" NE.:| []))

showDotProtoFor :: (Named a, Message a) => Proxy# a -> IO ()
showDotProtoFor proxy = putStrLn . toProtoFileDef $ dotProtoFor proxy

instance Arbitrary WireType where
  arbitrary = oneof $ map return [Varint, P.Fixed32, P.Fixed64, LengthDelimited]

testFilesPfx :: IsString a => a
testFilesPfx = "test-files/"
