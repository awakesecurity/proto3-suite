{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TypeOperators          #-}

module Main (main) where

import           Data.List                        (sort, sortOn)
import           Data.RangeSet.List               (fromRangeList, toRangeList)
import           Data.Semigroup                   (Min(..), Option(..))
import           Options.Generic
import           Prelude                          hiding (FilePath)
import           Proto3.Suite.DotProto.AST
import           Proto3.Suite.DotProto.Generate
import           Proto3.Suite.DotProto.Rendering
import           Proto3.Wire.Types                (FieldNumber (..))
import           Turtle                           (FilePath)

data Args = Args
  { proto :: FilePath <?> "Path to input .proto file"
  } deriving (Generic, Show)
instance ParseRecord Args

main :: IO ()
main = do
  protoPath <- unHelpful . proto <$> getRecord "Dumps a canonical .proto file to stdout"
  readDotProtoWithContext protoPath >>= \case
    Left err      -> fail (show err)
    Right (dp, _) -> putStr (toProtoFile defRenderingOptions (canonicalize dp))

class Canonicalize a where
  canonicalize :: a -> a

class Ord r => CanonicalRank a r | a -> r where
  canonicalRank :: a -> r
  default canonicalRank :: Ord a => a -> a
  canonicalRank = id

canonicalSort :: (CanonicalRank a r, Canonicalize a) => [a] -> [a]
canonicalSort = sortOn canonicalRank . map canonicalize

instance Canonicalize DotProto where
  canonicalize DotProto{..} = DotProto
    { protoImports     = canonicalize protoImports
    , protoOptions     = canonicalize protoOptions
    , protoPackage     = canonicalize protoPackage
    , protoDefinitions = canonicalize protoDefinitions
    }

instance Canonicalize [DotProtoImport] where canonicalize = canonicalSort

instance CanonicalRank DotProtoImport DotProtoImport

instance Canonicalize DotProtoImport where canonicalize = id

instance Canonicalize [DotProtoOption] where canonicalize = canonicalSort

instance CanonicalRank DotProtoOption DotProtoOption

instance Canonicalize DotProtoOption where
  canonicalize DotProtoOption{..} = DotProtoOption
    { dotProtoOptionIdentifier = canonicalize dotProtoOptionIdentifier
    , dotProtoOptionValue      = canonicalize dotProtoOptionValue
    }

instance Canonicalize DotProtoPackageSpec where
  canonicalize = \case
    DotProtoPackageSpec name -> DotProtoPackageSpec (canonicalize name)
    DotProtoNoPackage -> DotProtoNoPackage

instance Canonicalize [DotProtoDefinition] where canonicalize = canonicalSort

instance CanonicalRank DotProtoDefinition (Int, DotProtoIdentifier) where
  canonicalRank = \case
    DotProtoEnum    name _ -> (1, name)
    DotProtoMessage name _ -> (2, name)
    DotProtoService name _ -> (3, name)

instance Canonicalize DotProtoDefinition where
  canonicalize = \case
    DotProtoMessage name parts ->
      DotProtoMessage (canonicalize name) (canonicalize parts)
    DotProtoEnum    name parts ->
      DotProtoEnum    (canonicalize name) (canonicalize parts)
    DotProtoService name parts ->
      DotProtoService (canonicalize name) (canonicalize parts)

instance Canonicalize [DotProtoMessagePart] where
  canonicalize parts = canonicalSort (resNumbers ++ resNames ++ other)
    where
      (reservations, other) = flip foldMap parts $ \case
        DotProtoMessageReserved fs -> (fs, [])
        part -> ([], [part])

      resNumbers = reserve $ filter (not . isName) reservations
      resNames = reserve $ filter isName reservations

      reserve [] = []
      reserve fs = [DotProtoMessageReserved fs]

      isName = \case
        SingleField _ -> False
        FieldRange _ _ -> False
        ReservedIdentifier _ -> True

instance CanonicalRank DotProtoMessagePart
         (Either (Either (Int, DotProtoIdentifier) ())
                 (Maybe FieldNumber)) where
  canonicalRank = \case
    DotProtoMessageField f -> Right (canonicalRank f)
    DotProtoMessageOneOf _ fs -> Right (canonicalRank fs)
    DotProtoMessageDefinition d -> Left (Left (canonicalRank d))
    DotProtoMessageReserved fs -> Left (Right ())
      -- We use '()' here because 'Canonicalize [DotProtoMessagePart]'
      -- collapses all of the 'DotProtoMessageReserved's into just one.

instance Canonicalize DotProtoMessagePart where
  canonicalize = \case
    DotProtoMessageField f ->
      DotProtoMessageField (canonicalize f)
    DotProtoMessageOneOf n fs ->
      DotProtoMessageOneOf (canonicalize n) (canonicalize fs)
    DotProtoMessageDefinition d ->
      DotProtoMessageDefinition (canonicalize d)
    DotProtoMessageReserved fs ->
      DotProtoMessageReserved (canonicalize fs)

instance CanonicalRank [DotProtoField] (Maybe FieldNumber) where
  canonicalRank =
    fmap getMin . getOption . foldMap (Option . fmap Min . canonicalRank)

instance Canonicalize [DotProtoField] where
  canonicalize = canonicalSort . filter keep
    where
      keep DotProtoEmptyField = False
      keep _ = True

instance CanonicalRank DotProtoField (Maybe FieldNumber) where
  canonicalRank = \case
    DotProtoField{..} -> Just dotProtoFieldNumber
    DotProtoEmptyField -> Nothing

instance Canonicalize DotProtoField where
  canonicalize DotProtoField{..} = DotProtoField
    { dotProtoFieldNumber = dotProtoFieldNumber
    , dotProtoFieldType = canonicalize dotProtoFieldType
    , dotProtoFieldName = canonicalize dotProtoFieldName
    , dotProtoFieldOptions = canonicalize dotProtoFieldOptions
    , dotProtoFieldComment = Nothing  -- In future we might add a command-line
                                      -- option to preserve comments.
    }
  canonicalize DotProtoEmptyField = DotProtoEmptyField

instance Canonicalize DotProtoType where canonicalize = id

instance Canonicalize [DotProtoReservedField] where
  canonicalize fields = numbers ++ names
    where
      (rangeList, nameList) = flip foldMap fields $ \case
        SingleField number -> ([(number, number)], [])
        FieldRange lo hi -> ([(lo, hi)], [])
        ReservedIdentifier name -> ([], [name])

      names = map ReservedIdentifier (unique (sort nameList))

      unique [] = []
      unique [n] = [n]
      unique (x : xs@(y : _)) = (if x == y then id else (x :)) (unique xs)

      numbers = map reserveNumbers (toRangeList (fromRangeList rangeList))

      reserveNumbers (lo, hi) | lo == hi = SingleField lo
                              | otherwise = FieldRange lo hi

instance Canonicalize [DotProtoEnumPart] where
  canonicalize = canonicalSort . filter keep
    where
      keep DotProtoEnumEmpty = False
      keep _ = True

instance CanonicalRank DotProtoEnumPart
                       (Either (Maybe DotProtoOption) DotProtoEnumValue) where
  canonicalRank = \case
    DotProtoEnumField _ value -> Right value
    DotProtoEnumOption option -> Left (Just option)
    DotProtoEnumEmpty -> Left Nothing

instance Canonicalize DotProtoEnumPart where
  canonicalize = \case
    DotProtoEnumField name value ->
      DotProtoEnumField (canonicalize name) value
    DotProtoEnumOption option ->
      DotProtoEnumOption (canonicalize option)
    DotProtoEnumEmpty ->
      DotProtoEnumEmpty

instance Canonicalize [DotProtoServicePart] where
  canonicalize = canonicalSort . filter keep
    where
      keep DotProtoServiceEmpty = False
      keep _ = True

instance CanonicalRank DotProtoServicePart
                       (Either (Maybe DotProtoOption) DotProtoIdentifier) where
  canonicalRank = \case
    DotProtoServiceRPC name _ _ _ -> Right name
    DotProtoServiceOption option -> Left (Just option)
    DotProtoServiceEmpty -> Left Nothing

instance Canonicalize DotProtoServicePart where
  canonicalize = \case
    DotProtoServiceRPC name (reqN, reqS) (rspN, rspS) options ->
      DotProtoServiceRPC (canonicalize name)
                         (canonicalize reqN, reqS)
                         (canonicalize rspN, rspS)
                         (canonicalize options)
    DotProtoServiceOption option ->
      DotProtoServiceOption (canonicalize option)
    DotProtoServiceEmpty ->
      DotProtoServiceEmpty

instance Canonicalize DotProtoValue where
  canonicalize = \case
    Identifier name -> Identifier (canonicalize name)
    StringLit str -> StringLit str
    IntLit j -> IntLit j
    FloatLit x -> FloatLit x
    BoolLit b -> BoolLit b

instance Canonicalize DotProtoIdentifier where
  canonicalize = \case
    Single part -> Single part
    Path [part] -> Single part
    Path parts -> Path parts
    Qualified x y -> Qualified (canonicalize x) (canonicalize y)
    Anonymous -> Anonymous
