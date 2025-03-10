{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Haskell types used to express standard protobuf wrapper message types.
module Google.Protobuf.Wrappers.Polymorphic
  ( Wrapped(..)
  ) where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

-- | A Haskell type representing the standard protobuf wrapper
-- message that is associated with the given Haskell type.
--
-- Note that if Google ever adds wrappers for "sint..." or "...fixed..."
-- then this newtype could still be used, provided its type parameter
-- involves the appropriate combination of `Proto3.Suite.Types.Signed`
-- and/or `Proto3.Suite.Types.Fixed`.  Because the latter two types
-- are themselves newtypes, the corresponding coercions should work.
newtype Wrapped a = Wrapped a
  deriving (Foldable, Functor, Generic, Show, Traversable)
  deriving newtype (Bounded, Enum, Eq, FromJSON, Monoid,
                    NFData, Num, Ord, Semigroup, ToJSON)
