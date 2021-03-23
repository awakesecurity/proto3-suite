{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}

module Main (main) where

import           Control.Monad.Except
import           Data.List                        (sort, sortOn)
import qualified Data.List.NonEmpty              as NE
import           Data.RangeSet.List               (fromRangeList, toRangeList)
import           Data.Semigroup                   (Min(..), Option(..))
import           Options.Generic
import           Prelude                          hiding (FilePath)
import           Proto3.Suite.DotProto.AST
import           Proto3.Suite.DotProto.Generate
import           Proto3.Suite.DotProto.Rendering
import           Proto3.Wire.Types                (FieldNumber (..))
import           Turtle                           (FilePath)

data Args w = Args
  { includeDir :: w ::: [FilePath] <?> "Path to search for included .proto files (can be repeated, and paths will be searched in order; the current directory is used if this option is not provided)"
  , proto      :: w ::: FilePath   <?> "Path to input .proto file"
  } deriving Generic
instance ParseRecord (Args Wrapped)
deriving instance Show (Args Unwrapped)

main :: IO ()
main = do
  Args{..} :: Args Unwrapped <- unwrapRecord "Dumps a canonicalized .proto file to stdout"
  runExceptT (readDotProtoWithContext includeDir proto) >>= \case
    Left err      -> fail (show err)
    Right (dp, _) -> putStr (toProtoFile defRenderingOptions (canonicalize dp))

data PartRank
    = PartRankOption DotProtoOption
    | PartRankDefinition (Int, DotProtoIdentifier)
    | PartRankReserved
    | PartRankField (Maybe FieldNumber) deriving (Eq, Ord)

class Canonicalize a where
  canonicalize :: a -> a

class Ord r => CanonicalRank a r | a -> r where
  canonicalRank :: a -> r
  default canonicalRank :: (Ord a, a ~ r) => a -> r
  canonicalRank = id

canonicalSort :: (CanonicalRank a r, Canonicalize a) => [a] -> [a]
canonicalSort = sortOn canonicalRank . map canonicalize

instance Canonicalize DotProto where
  canonicalize DotProto{..} = DotProto
    { protoImports     = canonicalize protoImports
    , protoOptions     = canonicalize protoOptions
    , protoPackage     = canonicalize protoPackage
    , protoDefinitions = canonicalize protoDefinitions
    , protoMeta        = protoMeta
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
    DotProtoEnum    _ name _ -> (1, name)
    DotProtoMessage _ name _ -> (2, name)
    DotProtoService _ name _ -> (3, name)

instance Canonicalize DotProtoDefinition where
  canonicalize = \case
    DotProtoMessage _ name parts ->
      DotProtoMessage "" (canonicalize name) (canonicalize parts)
    DotProtoEnum    _ name parts ->
      DotProtoEnum    "" (canonicalize name) (canonicalize parts)
    DotProtoService _ name parts ->
      DotProtoService "" (canonicalize name) (canonicalize parts)

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

instance CanonicalRank DotProtoMessagePart PartRank where
  canonicalRank = \case
    DotProtoMessageOption x -> PartRankOption x
    DotProtoMessageDefinition d -> PartRankDefinition (canonicalRank d)
    DotProtoMessageReserved _fs -> PartRankReserved
      -- We don't use '_fs' here because 'Canonicalize [DotProtoMessagePart]'
      -- collapses all of the 'DotProtoMessageReserved's into just one.
    DotProtoMessageField f -> PartRankField (canonicalRank f)
    DotProtoMessageOneOf _ fs -> PartRankField (canonicalRank fs)

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
    DotProtoMessageOption option ->
      DotProtoMessageOption (canonicalize option)

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
    , dotProtoFieldComment = ""  -- In future we might add a command-line
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
    DotProtoEnumField _ value _ -> Right value
    DotProtoEnumOption option -> Left (Just option)
    DotProtoEnumEmpty -> Left Nothing

instance Canonicalize DotProtoEnumPart where
  canonicalize = \case
    DotProtoEnumField name value opts ->
      DotProtoEnumField (canonicalize name) value (map canonicalize opts)
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
    DotProtoServiceRPCMethod method -> Right (rpcMethodName method)
    DotProtoServiceOption option -> Left (Just option)
    DotProtoServiceEmpty -> Left Nothing

instance Canonicalize DotProtoServicePart where
  canonicalize = \case
    DotProtoServiceRPCMethod guts ->
      DotProtoServiceRPCMethod (canonicalize guts)
    DotProtoServiceOption option ->
      DotProtoServiceOption (canonicalize option)
    DotProtoServiceEmpty ->
      DotProtoServiceEmpty

instance Canonicalize RPCMethod where
  canonicalize (RPCMethod name reqN reqS rspN rspS options) =
    RPCMethod (canonicalize name)
              (canonicalize reqN) reqS
              (canonicalize rspN) rspS
              (canonicalize options)

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
    Dots (Path (part NE.:| [])) -> Single part
    Dots path -> Dots path
    Qualified x y -> Qualified (canonicalize x) (canonicalize y)
    Anonymous -> Anonymous
