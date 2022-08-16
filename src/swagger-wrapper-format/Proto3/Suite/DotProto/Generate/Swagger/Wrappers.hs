{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Proto3.Suite.DotProto.Generate.Swagger.Wrappers where

#if MIN_VERSION_swagger2(2,4,0)
import Control.Lens ((?~))
#else
import Control.Lens ((.~))
#endif
import Data.Functor ((<&>))
import Data.Int (Int32, Int64)
import Data.Proxy (Proxy (..))
import Data.Swagger
  ( Definitions
  , NamedSchema (..)
  , Schema
  , ToSchema (..)
  , byteSchema
  , format
  , paramSchema
  , schema
  )
import Data.Swagger.Declare (Declare)
import Data.Word (Word32, Word64)
import Google.Protobuf.Wrappers.Polymorphic (Wrapped(..))

import qualified Data.Text as T
import qualified Proto3.Suite.Types

-- | Wrapped Type Schemas

setFormat
  :: T.Text
  -> Declare (Definitions Schema) NamedSchema
  -> Declare (Definitions Schema) NamedSchema
setFormat formatValue namedSchema =
  namedSchema
#if MIN_VERSION_swagger2(2,4,0)
    <&> schema . paramSchema . format ?~ formatValue
#else
    <&> schema . paramSchema . format .~ formatValue
#endif

declareWrapperNamedSchema
  :: forall a
   . ToSchema a
  => T.Text
  -> Proxy (Maybe (Wrapped a))
  -> Declare (Definitions Schema) NamedSchema
declareWrapperNamedSchema formatValue _ =
  setFormat formatValue (declareNamedSchema (Proxy :: Proxy a))

instance {-# OVERLAPPING #-} ToSchema (Maybe (Wrapped Double)) where
  declareNamedSchema = declareWrapperNamedSchema "DoubleValue"

instance {-# OVERLAPPING #-} ToSchema (Maybe (Wrapped Float)) where
  declareNamedSchema = declareWrapperNamedSchema "FloatValue"

instance {-# OVERLAPPING #-} ToSchema (Maybe (Wrapped Int64)) where
  declareNamedSchema = declareWrapperNamedSchema "Int64Value"

instance {-# OVERLAPPING #-} ToSchema (Maybe (Wrapped Word64)) where
  declareNamedSchema = declareWrapperNamedSchema "UInt64Value"

instance {-# OVERLAPPING #-} ToSchema (Maybe (Wrapped Int32)) where
  declareNamedSchema = declareWrapperNamedSchema "Int32Value"

instance {-# OVERLAPPING #-} ToSchema (Maybe (Wrapped Word32)) where
  declareNamedSchema = declareWrapperNamedSchema "UInt32Value"

instance {-# OVERLAPPING #-} ToSchema (Maybe (Wrapped Bool)) where
  declareNamedSchema = declareWrapperNamedSchema "BoolValue"

instance {-# OVERLAPPING #-} ToSchema (Maybe (Wrapped (Proto3.Suite.Types.String a))) where
  declareNamedSchema _ =
    setFormat "StringValue" (declareNamedSchema (Proxy :: Proxy String))

instance {-# OVERLAPPING #-} ToSchema (Maybe (Wrapped (Proto3.Suite.Types.Bytes a))) where
  declareNamedSchema _ =
    setFormat "BytesValue" (pure (NamedSchema Nothing byteSchema))
