{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}

module Main where

import Control.Monad (guard)
import Control.Monad.Except (runExceptT)

import Data.List (sort, sortOn)
import Data.List.NonEmpty qualified as NE
import Data.Semigroup (Min(..))

import GHC.Generics (Generic)

import Options.Generic 
  ( ParseRecord
  , Unwrapped
  , Wrapped 
  , (:::)
  , type (<?>) (..)
  , unwrapRecord
  )

import Prelude hiding (FilePath)

import Proto3.Suite.DotProto.AST 
  ( DotProto (..)
  , DotProtoDefinition (..)
  , DotProtoEnumPart (..)
  , DotProtoEnumValue 
  , DotProtoField (..)
  , DotProtoIdentifier (..)
  , DotProtoImport (..)
  , DotProtoMessagePart (..)
  , DotProtoOption (..)
  , DotProtoPackageSpec (..)
  , DotProtoServicePart (..)
  , DotProtoReservedField (..)
  , DotProtoType (..)
  , DotProtoValue (..)
  , Path (..)
  , RPCMethod (..)
  )
import Proto3.Suite.DotProto.Generate (readDotProtoWithContext)
import Proto3.Suite.DotProto.Rendering (defRenderingOptions, toProtoFile)
import Proto3.Wire.Types (FieldNumber (..))

import Turtle (FilePath)

--------------------------------------------------------------------------------

data Args w = Args
  { includeDir :: w ::: [FilePath] <?> "Path to search for included .proto files (can be repeated, and paths will be searched in order; the current directory is used if this option is not provided)"
  , proto      :: w ::: FilePath   <?> "Path to input .proto file"
  } deriving Generic

instance ParseRecord (Args Wrapped)

deriving instance Show (Args Unwrapped)

-- | Check if the given values is some given bounds (inclusive).
between :: (Ord a, Enum a) => a -> (a, a) -> Bool
between x i = fst i <= x && x <= snd i

-- | @('isOverlappingIntervals' a b :: 'Bool')@ is a relation between two 
-- interval-like types @a@ and @b@ that is 'True' when the two intervals are 
-- overlapping or touching over a subinterval of @a@ and @b@.
isOverlappingIntervals :: (Ord a, Enum a) => (a, a) -> (a, a) -> Bool
isOverlappingIntervals i1@(x, y) i2@(u, v) = x `between` i2 || y `between` i2 || u `between` i1 || v `between` i1

-- | @('joinIntervals' a b :: 'Maybe' (a, a))@ will join the two given intervals 
-- @a@ and @b@ into a larger interval if 
--
-- @
-- ('isOverlappingIntervals' a b '==' 'True')
-- @
--
-- Otherwise, returns 'Nothing'.
joinIntervals :: (Ord a, Enum a) => (a, a) -> (a, a) -> Maybe (a, a)
joinIntervals a b = do 
  guard (isOverlappingIntervals a b)
  pure (min (fst a) (fst b), max (snd a) (snd b))

-- | Normalizes a list of intervals. 
--
-- 1. Filters out any "invalid" intervals from the resulting list, i.e. any 
--    intervals @(x, y) :: (a, a)@ with @x > y@. 
--
-- 2. Additionally, the resulting list will have all overlapping intervals 
--    merged into a larger interval that covers each overlapping interval.
normalizeIntervals :: (Ord a, Enum a) => [(a, a)] -> [(a, a)]
normalizeIntervals = mergeIntervals . filter \(x, y) -> x <= y

-- | Returns a sorted list that contains all intervals from the minimal set of 
-- intervals to represent the given list of intervals. 
--
-- "Merges" overlapping intervals in a list of intervals. Think disjunctive 
-- normal form.
mergeIntervals :: (Ord a, Enum a) => [(a, a)] -> [(a, a)]
mergeIntervals = foldr step [] . sort
  where 
    step :: (Ord a, Enum a) => (a, a) -> [(a, a)] -> [(a, a)]
    step x [] = [x]
    step x (y : ys) = case joinIntervals x y of 
      Nothing -> x : y : ys 
      Just xy -> xy : ys

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
    fmap getMin . foldMap (fmap Min . canonicalRank)

instance Canonicalize [DotProtoField] where
  canonicalize = canonicalSort

instance CanonicalRank DotProtoField (Maybe FieldNumber) where
  canonicalRank DotProtoField{..} = Just dotProtoFieldNumber

instance Canonicalize DotProtoField where
  canonicalize DotProtoField{..} = DotProtoField
    { dotProtoFieldNumber = dotProtoFieldNumber
    , dotProtoFieldType = canonicalize dotProtoFieldType
    , dotProtoFieldName = canonicalize dotProtoFieldName
    , dotProtoFieldOptions = canonicalize dotProtoFieldOptions
    , dotProtoFieldComment = ""  -- In future we might add a command-line
                                 -- option to preserve comments.
    }

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

      numbers = map reserveNumbers (normalizeIntervals rangeList)

      reserveNumbers (lo, hi) | lo == hi = SingleField lo
                              | otherwise = FieldRange lo hi

instance Canonicalize [DotProtoEnumPart] where
  canonicalize = canonicalSort

instance CanonicalRank DotProtoEnumPart
                       (Either (Maybe DotProtoOption) DotProtoEnumValue) where
  canonicalRank = \case
    DotProtoEnumField _ value _ -> Right value
    DotProtoEnumOption option -> Left (Just option)
    DotProtoEnumReserved _ -> Left Nothing

instance Canonicalize DotProtoEnumPart where
  canonicalize = \case
    DotProtoEnumField name value opts ->
      DotProtoEnumField (canonicalize name) value (map canonicalize opts)
    DotProtoEnumOption option ->
      DotProtoEnumOption (canonicalize option)
    DotProtoEnumReserved reservedFields ->
      DotProtoEnumReserved (canonicalize reservedFields)

instance Canonicalize [DotProtoServicePart] where
  canonicalize = canonicalSort

instance CanonicalRank DotProtoServicePart
                       (Either (Maybe DotProtoOption) DotProtoIdentifier) where
  canonicalRank = \case
    DotProtoServiceRPCMethod method -> Right (rpcMethodName method)
    DotProtoServiceOption option -> Left (Just option)

instance Canonicalize DotProtoServicePart where
  canonicalize = \case
    DotProtoServiceRPCMethod guts ->
      DotProtoServiceRPCMethod (canonicalize guts)
    DotProtoServiceOption option ->
      DotProtoServiceOption (canonicalize option)

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
