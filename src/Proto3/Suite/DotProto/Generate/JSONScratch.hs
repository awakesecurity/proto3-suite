-- | This module provides functions to generate Haskell code for doing JSON
-- serdes for protobuf messages, as per the proto3 canonical JSON encoding
-- described at https://developers.google.com/protocol-buffers/docs/proto3#json.

{-

.d8888.  .o88b. d8888b.  .d8b.  d888888b  .o88b. db   db       .o88b.  .d88b.  d8888b. d88888b
88'  YP d8P  Y8 88  `8D d8' `8b `~~88~~' d8P  Y8 88   88      d8P  Y8 .8P  Y8. 88  `8D 88'
`8bo.   8P      88oobY' 88ooo88    88    8P      88ooo88      8P      88    88 88   88 88ooooo
  `Y8b. 8b      88`8b   88~~~88    88    8b      88~~~88      8b      88    88 88   88 88~~~~~
db   8D Y8b  d8 88 `88. 88   88    88    Y8b  d8 88   88      Y8b  d8 `8b  d8' 88  .8D 88.
`8888Y'  `Y88P' 88   YD YP   YP    YP     `Y88P' YP   YP       `Y88P'  `Y88P'  Y8888D' Y88888P

Currently this module just contains a bunch of experimental code -- mostly
prototyping for the kind of code that we'll want to end up generating.

-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Proto3.Suite.DotProto.Generate.JSONScratch where

-- Stuff we need for experimentation. Once we have the whole import set and
-- decide how to namespace it, we will need to carry these additional imports
-- over into the code generators.
import Proto3.Suite.DotProto
import Proto3.Suite.DotProto.Generate
import qualified Data.Map as Hs (fromList)
import qualified Proto3.Suite as P3S
import qualified Proto3.Suite.DotProto.Rendering as P3S
import qualified Data.Proxy as DP
import qualified Proto3.Wire.Encode as Enc
import qualified Proto3.Wire.Decode as Dec
import qualified Control.Monad as Hs (fail)
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.Aeson.Encoding as A
import qualified Safe
import           Data.Monoid ((<>))
import qualified Data.Char as Hs (toLower)
import Data.Coerce

-- Imports from the generated code below, lifted here for convenience.
import qualified Prelude as Hs
import qualified Prelude as Prelude
import qualified Proto3.Suite.DotProto as HsProtobuf
import qualified Proto3.Suite.Types as HsProtobuf
import qualified Proto3.Suite.Class as HsProtobuf
import qualified Proto3.Wire as HsProtobuf
import Control.Applicative ((<$>), (<*>), (<|>))
import qualified Data.Text as Hs (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Hs (encodeUtf8)
import qualified Data.ByteString as Hs
import qualified Data.ByteString.Lazy as LBS
import qualified Data.String as Hs (fromString)
import qualified Data.Vector as Hs (Vector)
import qualified Data.Int as Hs (Int16, Int32, Int64)
import qualified Data.Word as Hs (Word16, Word32, Word64)
import GHC.Generics as Hs
import GHC.Enum as Hs

-- Experiments scratch

--------------------------------------------------------------------------------
-- Testing and Aeson instance helpers

-- TODO: Let's not scope all of this so explicitly, eh? Makes the code rather
-- difficult to read.

roundTrip :: (A.ToJSON a, A.FromJSON a, Hs.Eq a) => a -> Hs.Either Hs.String Hs.Bool
roundTrip x = Hs.either Hs.Left (Hs.Right Hs.. (x Hs.==)) Hs.. jsonToPB Hs.. pbToJSON Hs.$ x

pbToJSON :: A.ToJSON a => a -> LBS.ByteString
pbToJSON = A.encode

jsonToPB :: A.FromJSON a => LBS.ByteString -> Hs.Either Hs.String a
jsonToPB = A.eitherDecode

dropFldPfx :: HsProtobuf.Named a => DP.Proxy a -> Hs.String -> Hs.String
dropFldPfx p s = case dropNamed s of [] -> []; (c:cs) -> Hs.toLower c : cs
  where
    dropNamed = Prelude.drop (Prelude.length (HsProtobuf.nameOf p :: Hs.String))

pbOpts :: (HsProtobuf.Named a) => DP.Proxy a -> A.Options
pbOpts pr = A.defaultOptions{ A.fieldLabelModifier = dropFldPfx pr }

--------------------------------------------------------------------------------
-- Trivial

-- message Trivial {
--   int32 trivialField = 1;
-- }

-- Via eg. (readDotProtoWithContext "/w/proto3-suite/t.proto")
trivialDotProtoAST :: Hs.Either CompileError (DotProto, TypeContext)
trivialDotProtoAST =
  Hs.Right
    ( DotProto
        { protoImports = []
        , protoOptions = []
        , protoPackage = DotProtoPackageSpec (Single "tee")
        , protoDefinitions =
            [ DotProtoMessage
                (Single "Trivial")
                [ DotProtoMessageField
                    DotProtoField
                      { dotProtoFieldNumber = HsProtobuf.FieldNumber { HsProtobuf.getFieldNumber = 1 }
                      , dotProtoFieldType = Prim Int32
                      , dotProtoFieldName = Single "trivialField"
                      , dotProtoFieldOptions = []
                      , dotProtoFieldComment = Hs.Nothing
                      }
                ]
            ]
        }
    , Hs.fromList []
    )

-- Via eg. (renderHsModuleForDotProtoFile "/w/proto3-suite/t.proto" >>= \(Prelude.Right s) -> Prelude.putStrLn s)
data Trivial = Trivial{trivialTrivialField32 :: Hs.Int32,
                       trivialTrivialField64 :: Hs.Int64}
             deriving (Hs.Show, Hs.Eq, Hs.Ord, Hs.Generic)

instance HsProtobuf.Named Trivial where
        nameOf _ = (Hs.fromString "Trivial")

instance HsProtobuf.Message Trivial where
        encodeMessage _
          Trivial{trivialTrivialField32 = trivialTrivialField32,
                  trivialTrivialField64 = trivialTrivialField64}
          = (Hs.mconcat
               [(HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 1)
                   trivialTrivialField32),
                (HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 2)
                   trivialTrivialField64)])
        decodeMessage _
          = (Hs.pure Trivial) <*>
              (HsProtobuf.at HsProtobuf.decodeMessageField
                 (HsProtobuf.FieldNumber 1))
              <*>
              (HsProtobuf.at HsProtobuf.decodeMessageField
                 (HsProtobuf.FieldNumber 2))
        dotProto _
          = [(HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 1)
                (HsProtobuf.Prim HsProtobuf.Int32)
                (HsProtobuf.Single "trivialField32")
                []
                Hs.Nothing),
             (HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 2)
                (HsProtobuf.Prim HsProtobuf.Int64)
                (HsProtobuf.Single "trivialField64")
                []
                Hs.Nothing)]


-- Render the .proto for the Trivial message
emitTrivialProto :: Hs.IO ()
emitTrivialProto = Prelude.putStrLn Hs.$ toProtoFileDef Hs.$
  packageFromDefs "tee" [HsProtobuf.message (DP.Proxy :: DP.Proxy Trivial) :: P3S.DotProtoDefinition]

-- Use e.g. P3S.toLazyByteString (encTrivial (t :: Trivial)) to test encoding.
-- E.g. P3s.toLazyByteString (Trivial 99)
--
-- Check understanding: is the encode function above equivalent to the
-- encodeMessage instance method for Message? Yes, it is:
--
-- > P3S.toLazyByteString (Trivial 99) (note that this one takes something in the Message typeclass)
-- "\bc"
-- ===
-- > Enc.toLazyByteString (encTrivial (Trivial 99)) (note that this takes the Enc.Builder as a param; slight difference from above call!)
-- "\bc"
--
encTrivial :: Trivial -> Enc.Builder
encTrivial (Trivial fld1 fld2) = encTrivialFld1 fld1 <> encTrivialFld2 fld2

-- Describes encoding fld1 at the wire level via Proto3.Wire.Encode.int32 etc.
encTrivialFld1 :: Hs.Int32 -> Enc.Builder
encTrivialFld1 = Enc.int32 (HsProtobuf.fieldNumber 1)

-- Describes encoding fld2 at the wire level via Proto3.Wire.Encode.int64 etc.
encTrivialFld2 :: Hs.Int64 -> Enc.Builder
encTrivialFld2 = Enc.int64 (HsProtobuf.fieldNumber 2)

-- > decTrivial "\bc"
-- Right (Trivial {trivialTrivialField = 99})
-- ===
-- > P3S.fromByteString "\bc" :: Hs.Either Dec.ParseError Trivial
-- Right (Trivial {trivialTrivialField = 99})
decTrivial :: Hs.ByteString -> Hs.Either Dec.ParseError Trivial
decTrivial = Dec.parse parseTrivial

-- Describes decodeing fld1 at the wire level via Proto3.Wire.Decode.int32
-- etc. This is exactly the kind of code that is written and then mconcat'd
-- together
parseTrivial :: Dec.Parser Dec.RawMessage Trivial
parseTrivial = Trivial
  <$> do Dec.one Dec.int32 0 `Dec.at` HsProtobuf.fieldNumber 1
  <*> do Dec.one Dec.int64 0 `Dec.at` HsProtobuf.fieldNumber 2

-- Canonical JSON encoding for the Trivial message
--
-- 1) Try to use generic facilities as much as possible
--
-- 2) Note that omitNothingFields will likely be insufficient, since probably
-- any fields with value equal to their default/zero-initialized value should be
-- omitted. omitNothingFields will catch omitted nested messages, but that's
-- about it.
--
-- 3) Note that e.g. field name "trivialField" of the Trivial message is named
-- as "trivialTrivialField" in the generated Haskell record type. Thus, the
-- Aeson instances field name must drop the message name prefix via dropFldPfx
-- from the field name.
--
-- 4) We are likely going to have to construct our own `genericToEncoding` /
-- `genericParseJSON` instances to get us what we want, but I'm not sure what's
-- involved in that yet. I think we are going to have to type-select on various
-- fields in order to render them correctly, but we might be able to do that
-- with built-in Aeson instances for the various types. I think some additional
-- experiments are in order for slightly more complex field types first.

-- NB: We will probably have to construct our own "genericToEncoding" mechanism

-- Test via:
-- > (A.decode Hs.. A.encode) (Trivial 99) :: Prelude.Maybe Trivial

-- The rule for int32s etc is:
--
-- int32, fixed32, uint32: number, 1, -10, 0 JSON value will be a decimal
-- number. And then: "___Either numbers or strings are accepted___" means that
-- if you get a string value for a numeric field, it will be turned into a
-- numeric field. Fack -- I think this means that we must have to write our
-- custom FromJSON parsers sooner rather than later. Maybe we should just go
-- ahead and explore what would be involved in making our own genericToEncoding
-- and genericParseJSON helpers -- my bet is that it is fairly nontrivial.
--
-- because without something custom, what's going to happen is something like
-- this:
--
-- > A.eitherDecode "{\"trivialField\":\"99\"}" :: Hs.Either Hs.String Trivial
-- Left "Error in $: expected Int32, encountered String"
--
-- which seems like it is not supposed to be okay according to the
-- specification. Blech!
--
-- Let's go ahead and do a slightly less trivial example first and then look
-- into the generic derivation implementation etc.
--
-- One idea may be to write smaller, miniature parsers for each type which is
-- legal for protobuf types; this would allow us to accept e.g. the string
-- representations of values. E.g. Parser Int32 would accept either a number or
-- a String representation.

-- instance A.FromJSON Trivial where
--   parseJSON = A.genericParseJSON
--                 A.defaultOptions{
--                   A.fieldLabelModifier = dropFldPfx (DP.Proxy :: DP.Proxy Trivial)
--                 }

-- This FromJSON instance for Trivial fails to parse a number-in-string-form, as
-- per the spec. Ie,
--
-- > jsonToPB "{\"trivialField\":42}" :: Hs.Either Hs.String Trivial
-- Right (Trivial {trivialTrivialField = 42})
--
-- but
--
-- > jsonToPB "{\"trivialField\":\"42\"}" :: Hs.Either Hs.String Trivial
-- Left "Error in $: expected Int32, encountered String"
--
-- Here's an alternate FromJSON instance attempt which does the more liberal
-- parsing for the int32 field according to
-- https://developers.google.com/protocol-buffers/docs/proto3#json, which
-- specifies that (for int32):
--
--   JSON value will be a decimal number. Either numbers or strings are
--   accepted.
--
-- We need to write (parseJSON :: A.FromJSON a => A.Value => A.Parser a) such
-- that this kind of liberal parsing is allowed.
--
-- See
-- λ> do result <- decode "{\"name\":\"Dave\",\"age\":2}"
--       flip parseMaybe result $ \obj -> do
--         age <- obj .: "age"
--         name <- obj .: "name"
--         return (name ++ ": " ++ show (age*2))
--
-- Just "Dave: 4"
--
-- example from
-- https://hackage.haskell.org/package/aeson-0.11.2.1/docs/Data-Aeson.html under
-- "decoding a mixed-type object". Problem here is that I think we are already
-- "too late" to do this inside the existing implementation. E.g can we just
-- write the easy parser first? I.e. the one that is already being
-- generated. Exercise.
--
-- Also go look at, because that's where the underlying scientific -> int32
-- conversion is happening. instance A.FromJSON Hs.Int32 Defined in
-- ‘aeson-0.11.2.1:Data.Aeson.Types.Instances’
--
-- Another possibility is tweaking the values in the hashmap from strings to
-- numbers and then converting normally: so we canonicalize the incoming json
-- structure and then convert via the normal genericParseJSON + field label
-- modifiers.

-- instance FromJSON Person where
--     parseJSON = withObject "Person" $ \v -> Person
--         <$> v .: "name"
--         <*> v .: "age"
--
-- But this kind of approach seems much more sensible to me:
-- data KVPair = KVPair { key :: Int, value :: Either String Int }
-- instance FromJSON KVPair where
--     parseJSON (Object v)
--         =   KVPair
--         <$> v .: "Key"
--         <*> (   (Left  <$> v .: "Value")
--             <|> (Right <$> v .: "Value")
--             )

-- We should start this out as a very simple exercise: write an Aeson parser for
-- a JSON value which can both be a numeric literal or a string literal
-- interpreted as one. So e.g. it would succeeed on 42 or "42" (and produce the
-- integer value 42 in the latter case), but reject on e.g. "zz".
--
-- If we can do this in standalone/isolation then it should be pretty
-- straightforward to carry it over into the Generic-based parsing mechanism.

-- Totally handrolled, doesn't use <|> and lifting as above, which is probably
-- the better way to do it.

-- NB: these will go into a support module
newtype PBInt32 = PBInt32 Hs.Int32 deriving (Hs.Show, Hs.Generic, Hs.Eq)
instance Prelude.Monoid PBInt32 where
  mempty                          = PBInt32 0
  mappend (PBInt32 0) (PBInt32 y) = PBInt32 y
  mappend (PBInt32 x) (PBInt32 0) = PBInt32 x
  mappend (PBInt32 _) (PBInt32 x) = PBInt32 x
instance A.ToJSON PBInt32 -- default Generic instance is sufficient
instance A.FromJSON PBInt32 where
  parseJSON v@A.Number{} = PBInt32 <$> A.parseJSON v
  parseJSON (A.String t) = fromDecimalString PBInt32 t
  parseJSON v            = A.typeMismatch "PBInt32" v

-- NB: these will go into a support module
newtype PBInt64 = PBInt64 Hs.Int64 deriving (Hs.Show, Hs.Eq)
instance Prelude.Monoid PBInt64 where
  mempty = PBInt64 0
  mappend (PBInt64 0) (PBInt64 y) = PBInt64 y
  mappend (PBInt64 x) (PBInt64 0) = PBInt64 x
  mappend (PBInt64 _) (PBInt64 x) = PBInt64 x
instance A.ToJSON PBInt64 where
  toJSON (PBInt64 x)     = A.String (Text.pack (Hs.show x))
  toEncoding (PBInt64 x) = A.int64Text x
instance A.FromJSON PBInt64 where
  parseJSON v@A.Number{} = PBInt64 <$> A.parseJSON v
  parseJSON (A.String t) = fromDecimalString PBInt64 t
  parseJSON v            = A.typeMismatch "PBInt64" v

-- into a support module
fromDecimalString :: (A.FromJSON a, Hs.Monad m) => (a -> b) -> Hs.Text -> m b
fromDecimalString f =
  Hs.either Hs.fail (Hs.pure Hs.. f)
  Hs.. A.eitherDecode
  Hs.. LBS.fromStrict
  Hs.. Hs.encodeUtf8

-- -- Generic version which we don't have to do anything special with at all
-- -- because for Int32 we just emit a JSON number. For Int64, we will need a
-- -- custom instance because we will emit the value as a string. Blech.
-- instance A.ToJSON Trivial where
--   toEncoding = A.genericToEncoding
--                  A.defaultOptions{
--                    A.fieldLabelModifier = dropFldPfx (DP.Proxy :: DP.Proxy Trivial)
--                  }

-- toJSON :: a -> Value
-- Convert a Haskell value to a JSON-friendly intermediate type.

-- instance ToJSON Coord where
--   toJSON (Coord x y) = object ["x" .= x, "y" .= y]
--   toEncoding (Coord x y) = pairs ("x" .= x <> "y" .= y)

-- We will Generic-generate something like this that just uses the appropriate
-- PB<type> newtype wrapper on a per-field basis to obtain the implementation.
instance A.ToJSON Trivial where
  toEncoding (Trivial x y) = A.pairs
    (  (if PBInt32 x Hs.== Hs.mempty then Hs.mempty else "trivialField32" A..= PBInt32 x)
    <> (if PBInt64 y Hs.== Hs.mempty then Hs.mempty else "trivialField64" A..= PBInt64 y)
    )

pbFld :: (A.FromJSON a, Prelude.Monoid a) => A.Object -> Hs.Text -> A.Parser a
pbFld o fldSel = o A..:? fldSel A..!= Hs.mempty

-- We will Generic-generate something like this that just uses the appropriate
-- PB<type> newtype wrapper on a per-field basis to obtain the implementation.
--
-- A hand-crafted instance might look something like this:
--
-- instance A.FromJSON Trivial where
--   parseJSON = A.withObject "Trivial" Hs.$ \obj -> do
--     PBInt32 x <- pbFld obj "trivialField32"
--     PBInt64 y <- pbFld obj "trivialField64"
--     Hs.pure (Trivial x y)

-- Of course, this won't do the correct parsing yet because we are reusing the
-- vanilla generic parser generators, which do not account for the PB
-- modifications.

-- This would be generated code (but it re-uses genericParseJSONPB)
instance A.FromJSON Trivial where
  parseJSON = genericParseJSONPB (pbOpts (DP.Proxy :: DP.Proxy Trivial))

-- HERE: Try to implement a generic parser generator that does the PB encoding;
-- probably need to define our own GFromJSONPB typeclass to mirror the GFromJSON
-- one, since I believe it is the GFromJSON one that actually handles the
-- "standard" json -> value parsing. Perhaps we can try replicating the
-- implementation over there in a piecewise fashion and learn as we go, and then
-- figure out how to tweak it to get what we want above.

genericParseJSONPB :: (Generic a, A.GFromJSON A.Zero (Rep a))
                   => A.Options -> A.Value -> A.Parser a
genericParseJSONPB opts v = to <$> A.gParseJSON opts A.NoFromArgs v

--------------------------------------------------------------------------------
-- TwoField
--
-- message TwoField {
--   int32 fieldOne = 1;
--   bool  fieldTwo = 2;
-- }
--

data TwoField = TwoField{twoFieldFieldOne :: Hs.Int32,
                         twoFieldFieldTwo :: Hs.Bool}
              deriving (Hs.Show, Hs.Eq, Hs.Ord, Hs.Generic)

instance HsProtobuf.Named TwoField where
        nameOf _ = (Hs.fromString "TwoField")

instance HsProtobuf.Message TwoField where
        encodeMessage _
          TwoField{twoFieldFieldOne = twoFieldFieldOne,
                   twoFieldFieldTwo = twoFieldFieldTwo}
          = (Hs.mconcat
               [(HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 1)
                   twoFieldFieldOne),
                (HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 2)
                   twoFieldFieldTwo)])
        decodeMessage _
          = (Hs.pure TwoField) <*>
              (HsProtobuf.at HsProtobuf.decodeMessageField
                 (HsProtobuf.FieldNumber 1))
              <*>
              (HsProtobuf.at HsProtobuf.decodeMessageField
                 (HsProtobuf.FieldNumber 2))
        dotProto _
          = [(HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 1)
                (HsProtobuf.Prim HsProtobuf.Int32)
                (HsProtobuf.Single "fieldOne")
                []
                Hs.Nothing),
             (HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 2)
                (HsProtobuf.Prim HsProtobuf.Bool)
                (HsProtobuf.Single "fieldTwo")
                []
                Hs.Nothing)]

instance A.ToJSON TwoField where
  toEncoding =
    A.genericToEncoding A.defaultOptions{ A.fieldLabelModifier = dropFldPfx (DP.Proxy :: DP.Proxy TwoField) }

instance A.FromJSON TwoField where
  parseJSON =
    A.genericParseJSON A.defaultOptions{ A.fieldLabelModifier = dropFldPfx (DP.Proxy :: DP.Proxy TwoField) }

-- > pbToJSON (TwoField 42 False)
-- "{\"fieldOne\":42,\"fieldTwo\":false}"
--
-- > roundTrip (TwoField 42 Prelude.False)
-- Just (TwoField {twoFieldFieldOne = 42, twoFieldFieldTwo = False})

-- So this works, but again, does not accept the string value and thus does not
-- conform completely to the canonical JSON encoding form here. It might be
-- enough for preliminary bidirectional flow, though.

--------------------------------------------------------------------------------
-- MultiField
--
-- message MultiField {
--   double multiFieldDouble = 1;
--   float  multiFieldFloat  = 2;
--   int32  multiFieldInt32  = 3;
--   int64  multiFieldInt64  = 4;
--   string multiFieldString = 5;
--   bool   multiFieldBool   = 6;
-- }

data MultiField = MultiField{multiFieldMultiFieldDouble :: Hs.Double,
                             multiFieldMultiFieldFloat :: Hs.Float,
                             multiFieldMultiFieldInt32 :: Hs.Int32,
                             multiFieldMultiFieldInt64 :: Hs.Int64,
                             multiFieldMultiFieldString :: Hs.Text,
                             multiFieldMultiFieldBool :: Hs.Bool}
                deriving (Hs.Show, Hs.Eq, Hs.Ord, Hs.Generic)

instance HsProtobuf.Named MultiField where
        nameOf _ = (Hs.fromString "MultiField")

instance HsProtobuf.Message MultiField where
        encodeMessage _
          MultiField{multiFieldMultiFieldDouble = multiFieldMultiFieldDouble,
                     multiFieldMultiFieldFloat = multiFieldMultiFieldFloat,
                     multiFieldMultiFieldInt32 = multiFieldMultiFieldInt32,
                     multiFieldMultiFieldInt64 = multiFieldMultiFieldInt64,
                     multiFieldMultiFieldString = multiFieldMultiFieldString,
                     multiFieldMultiFieldBool = multiFieldMultiFieldBool}
          = (Hs.mconcat
               [(HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 1)
                   multiFieldMultiFieldDouble),
                (HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 2)
                   multiFieldMultiFieldFloat),
                (HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 3)
                   multiFieldMultiFieldInt32),
                (HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 4)
                   multiFieldMultiFieldInt64),
                (HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 5)
                   multiFieldMultiFieldString),
                (HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 6)
                   multiFieldMultiFieldBool)])
        decodeMessage _
          = (Hs.pure MultiField) <*>
              (HsProtobuf.at HsProtobuf.decodeMessageField
                 (HsProtobuf.FieldNumber 1))
              <*>
              (HsProtobuf.at HsProtobuf.decodeMessageField
                 (HsProtobuf.FieldNumber 2))
              <*>
              (HsProtobuf.at HsProtobuf.decodeMessageField
                 (HsProtobuf.FieldNumber 3))
              <*>
              (HsProtobuf.at HsProtobuf.decodeMessageField
                 (HsProtobuf.FieldNumber 4))
              <*>
              (HsProtobuf.at HsProtobuf.decodeMessageField
                 (HsProtobuf.FieldNumber 5))
              <*>
              (HsProtobuf.at HsProtobuf.decodeMessageField
                 (HsProtobuf.FieldNumber 6))
        dotProto _
          = [(HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 1)
                (HsProtobuf.Prim HsProtobuf.Double)
                (HsProtobuf.Single "multiFieldDouble")
                []
                Hs.Nothing),
             (HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 2)
                (HsProtobuf.Prim HsProtobuf.Float)
                (HsProtobuf.Single "multiFieldFloat")
                []
                Hs.Nothing),
             (HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 3)
                (HsProtobuf.Prim HsProtobuf.Int32)
                (HsProtobuf.Single "multiFieldInt32")
                []
                Hs.Nothing),
             (HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 4)
                (HsProtobuf.Prim HsProtobuf.Int64)
                (HsProtobuf.Single "multiFieldInt64")
                []
                Hs.Nothing),
             (HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 5)
                (HsProtobuf.Prim HsProtobuf.String)
                (HsProtobuf.Single "multiFieldString")
                []
                Hs.Nothing),
             (HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 6)
                (HsProtobuf.Prim HsProtobuf.Bool)
                (HsProtobuf.Single "multiFieldBool")
                []
                Hs.Nothing)]

instance A.ToJSON MultiField where
  toEncoding =
    A.genericToEncoding A.defaultOptions{ A.fieldLabelModifier = dropFldPfx (DP.Proxy :: DP.Proxy MultiField) }

instance A.FromJSON MultiField where
  parseJSON =
    A.genericParseJSON A.defaultOptions{ A.fieldLabelModifier = dropFldPfx (DP.Proxy :: DP.Proxy MultiField) }

--------------------------------------------------------------------------------

-- message WithNesting {
--   message Nested {
--     string nestedField1 = 1;
--     int32 nestedField2 = 2;
--   }
--   Nested nestedMessage = 1;
-- }

-- Corresponds to the outer message
data WithNesting = WithNesting{withNestingNestedMessage ::
                               Hs.Maybe WithNesting_Nested}
                 deriving (Hs.Show, Hs.Eq, Hs.Ord, Hs.Generic)

instance HsProtobuf.Named WithNesting where
        nameOf _ = (Hs.fromString "WithNesting")

instance HsProtobuf.Message WithNesting where
        encodeMessage _
          WithNesting{withNestingNestedMessage = withNestingNestedMessage}
          = (Hs.mconcat
               [(HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 1)
                   (HsProtobuf.Nested withNestingNestedMessage))])
        decodeMessage _
          = (Hs.pure WithNesting) <*>
              ((Hs.pure HsProtobuf.nested) <*>
                 (HsProtobuf.at HsProtobuf.decodeMessageField
                    (HsProtobuf.FieldNumber 1)))
        dotProto _
          = [(HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 1)
                (HsProtobuf.Prim (HsProtobuf.Named (HsProtobuf.Single "Nested")))
                (HsProtobuf.Single "nestedMessage")
                []
                Hs.Nothing)]

-- Corresponds to inner message
data WithNesting_Nested = WithNesting_Nested{withNesting_NestedNestedField1
                                             :: Hs.Text,
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
                (HsProtobuf.Prim HsProtobuf.String)
                (HsProtobuf.Single "nestedField1")
                []
                Hs.Nothing),
             (HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 2)
                (HsProtobuf.Prim HsProtobuf.Int32)
                (HsProtobuf.Single "nestedField2")
                []
                Hs.Nothing)]

instance A.ToJSON WithNesting where
  toEncoding =
    A.genericToEncoding A.defaultOptions{ A.fieldLabelModifier = dropFldPfx (DP.Proxy :: DP.Proxy WithNesting) }

instance A.FromJSON WithNesting where
  parseJSON =
    A.genericParseJSON A.defaultOptions{ A.fieldLabelModifier = dropFldPfx (DP.Proxy :: DP.Proxy WithNesting) }

instance A.ToJSON WithNesting_Nested where
  toEncoding =
    A.genericToEncoding A.defaultOptions{ A.fieldLabelModifier = dropFldPfx (DP.Proxy :: DP.Proxy WithNesting_Nested) }

instance A.FromJSON WithNesting_Nested where
  parseJSON =
    A.genericParseJSON A.defaultOptions{ A.fieldLabelModifier = dropFldPfx (DP.Proxy :: DP.Proxy WithNesting_Nested) }

-- Okay, so message nesting worked in a pretty sensible fashion.

--------------------------------------------------------------------------------
-- WithRepetition

data WithRepetition = WithRepetition{withRepetitionRepeatedField1
                                     :: Hs.Vector Hs.Int32}
                    deriving (Hs.Show, Hs.Eq, Hs.Ord, Hs.Generic)

instance HsProtobuf.Named WithRepetition where
        nameOf _ = (Hs.fromString "WithRepetition")

instance HsProtobuf.Message WithRepetition where
        encodeMessage _
          WithRepetition{withRepetitionRepeatedField1 =
                           withRepetitionRepeatedField1}
          = (Hs.mconcat
               [(HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 1)
                   (HsProtobuf.PackedVec withRepetitionRepeatedField1))])
        decodeMessage _
          = (Hs.pure WithRepetition) <*>
              ((Hs.pure HsProtobuf.packedvec) <*>
                 (HsProtobuf.at HsProtobuf.decodeMessageField
                    (HsProtobuf.FieldNumber 1)))
        dotProto _
          = [(HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 1)
                (HsProtobuf.Repeated HsProtobuf.Int32)
                (HsProtobuf.Single "repeatedField1")
                []
                Hs.Nothing)]

instance A.ToJSON WithRepetition where
  toEncoding =
    A.genericToEncoding A.defaultOptions{ A.fieldLabelModifier = dropFldPfx (DP.Proxy :: DP.Proxy WithRepetition) }

instance A.FromJSON WithRepetition where
  parseJSON =
    A.genericParseJSON A.defaultOptions{ A.fieldLabelModifier = dropFldPfx (DP.Proxy :: DP.Proxy WithRepetition) }

-- https://godoc.org/github.com/golang/protobuf/jsonpb

-- HERE: Test JSON encoding on the Golang grpc side via the url above and check
-- assumptions about stringly values...because maybe it's time to try a simple
-- Golang experiment for its JSON encoding; or the Python JSON encoding if the
-- library actually supports that. In particular, it would be VERY useful to
-- know if those codec implementations ACTUALLY support the "type variant"
-- version in the specification or not. Because if not, it's going to be a much
-- better PITA to implement this -- but possibly a very good experience
-- regardless.
--
-- Just go bolt something into the webservices repository, in an experimental/
-- directory or something, on a branch. Generate the .pb.go the same way, and
-- write some golang code that emits JSON from the pb-generated data
-- structures. Then go in the opposite direction and see what it does with the
-- stringly values! This also gives you a good mid-level picture of the activity
-- involved on both sides of the json/grpc fence.
--
-- THEN: If we have to go the funky-JSON route, start by exploring a
-- `genericFromPBJSON` or somesuch just for the `Trivial` type above, and see if
-- we can succeed with it via the "flexible parsing" mechanism. Yuck, though,
-- right? Blech. At least consider writing some quickcheck property tests on the
-- way up? Try to prioritize -- only have a few more days of this to sketch
-- things out...maybe it might be a little more worthwhile to look over the
-- GHC.Generics stuff and the existing genericParseJSON machinery and see if we
-- can just interpret the spec directly? That's still a little problematic, it's
-- probably a safer bet to model the other's implementation? Or be sufficiently
-- general? That's the thing that makes me think we should just interpret it so
-- that we accept the largest set of possible encodings, and emit the most
-- restrictive (eg we won't send out numbers stringly).
--
-- Another possibility may be to generate code for each type which just provided
-- a FromJSON instance for each that did the funky-JSON way of parsing values?
-- Maybe we can do that by hand for an instance or two before we move onto
-- attempting the generic way of doing it? Perhaps investigate how the generic
-- implementation for parseJSON selecs the parsers for the various underlying
-- types; must just be via their own FromJSON constraints; meaning we are likely
-- picking instances for primtypes that are preventing the parse from succeeding
-- when it should actually try to parse the string as the particular value
-- kind. At least for the simple numerical case -- get this right here before
-- moving onto other types. And seriously do go and check the golang version too
-- for sanity and comprehension. The definition
-- https://developers.google.com/protocol-buffers/docs/proto3#json here might
-- not really be being implemented. But my guess is that we do need to do
-- something like this.
--
-- Also, another thing to do in parallel-ish is to hook up the preliminary test
-- case workflow to quickcheck etc; e.g. test the first one we came up with
-- (Trivial) against the equivalent json rendering in golang. This might require
-- some additional nix tooling as well, for all of the golang support, but I
-- think this might be time well spent so that we can beat on grpc <-> json
-- marshalling against golang up front so that we can be convertible with its
-- JSON rendering. This lays some of the groundwork needed to be able to have a
-- grpc <-> rest path (at least for unary calls): a json-encoded request payload
-- -> pb encoded payload -> grpc call -> pb encoded grpc response ->
-- json-encoded response payload -> reply path.
--

{-

Do something like this too, after converting it to pb3 style? Might as well
start that way.

 message Book {
   required string title = 1;
   optional float price = 2;
   repeated Person authors = 3;
 }
 {
  "title": "Book example",
  "price": 12.7,
  "authors": [
   {
    "id": 123,
    "name": "person name",
    "email": "user@example.com"
   },
   {
    "id": 456,
    "name": "another person",
   }
  ]
 }

-}
