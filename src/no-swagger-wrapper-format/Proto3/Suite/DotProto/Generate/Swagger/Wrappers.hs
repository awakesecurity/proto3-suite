{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Proto3.Suite.DotProto.Generate.Swagger.Wrappers () where

import Data.Proxy (Proxy (..))
import Data.Swagger (ToSchema (..))
import Proto3.Suite.DotProto.Generate.Swagger.OverrideToSchema (OverrideToSchema(..))

import qualified Data.ByteString as B

instance {-# OVERLAPPING #-}
         ToSchema (Maybe (OverrideToSchema B.ByteString)) =>
         ToSchema (OverrideToSchema (Maybe B.ByteString)) where
  declareNamedSchema _ =
    declareNamedSchema (Proxy :: Proxy (Maybe (OverrideToSchema B.ByteString)))
  {-# INLINE declareNamedSchema #-}
