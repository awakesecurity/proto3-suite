{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module provides helper functions to generate Swagger schemas that
-- describe JSONPB encodings for protobuf types.
module Proto3.Suite.DotProto.Generate.Swagger
  ( ppSchema
  , OverrideToSchema(..)
  , asProxy
  , insOrdFromList
  )
where

#if MIN_VERSION_swagger2(2,4,0)
import           Control.Lens                    ((&), (?~))
#else
import           Control.Lens                    ((&), (.~), (?~))
#endif
import           Data.Aeson                      (Value (String), ToJSONKey,
                                                  ToJSONKeyFunction(..))
import qualified Data.Aeson                      as Aeson
import           Data.Aeson.Encode.Pretty        (encodePretty)
import qualified Data.ByteString                 as B
import qualified Data.ByteString.Lazy            as BL
import qualified Data.ByteString.Lazy.Char8      as LC8
import           Data.Functor                    ((<&>))
import           Data.Hashable                   (Hashable)
import           Data.HashMap.Strict.InsOrd      (InsOrdHashMap)
import qualified Data.HashMap.Strict.InsOrd
import           Data.Map                        (Map)
import           Data.Swagger
import           Data.Swagger.Declare            (Declare)
import qualified Data.Text                       as T
import qualified Data.Text.Lazy                  as TL
import           Data.Proxy
import qualified Data.Vector                     as V
import           GHC.Exts                        (Proxy#, proxy#)
import           GHC.Int
import           GHC.Word
import           Proto3.Suite                    (Enumerated (..), Finite (..),
                                                  Fixed (..), Named (..), enumerate)

-- | Convenience re-export so that users of generated code don't have to add
--   an explicit dependency on @insert-ordered-containers@
insOrdFromList :: (Eq k, Hashable k) => [(k, v)] -> InsOrdHashMap k v
insOrdFromList = Data.HashMap.Strict.InsOrd.fromList

{-| This is a hack to work around the `swagger2` library forbidding `ToSchema`
    instances for `ByteString`s
-}
newtype OverrideToSchema a = OverrideToSchema { unOverride :: a }

instance {-# OVERLAPPABLE #-} ToSchema a => ToSchema (OverrideToSchema a) where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy a)

instance {-# OVERLAPPING #-} ToSchema (OverrideToSchema B.ByteString) where
  declareNamedSchema _ = return (NamedSchema Nothing byteSchema)

-- | This instance is the same as the instance for @OverrideToSchema ByteString@.
-- See: https://hackage.haskell.org/package/swagger2-2.6/docs/src/Data.Swagger.Internal.Schema.html#line-451
instance {-# OVERLAPPING #-} ToSchema (OverrideToSchema (Maybe B.ByteString)) where
  declareNamedSchema _ = return (NamedSchema Nothing byteSchema)

instance {-# OVERLAPPING #-} ToSchema (OverrideToSchema (V.Vector B.ByteString)) where
  declareNamedSchema _ = return (NamedSchema Nothing schema_)
    where
      schema_ = mempty
#if MIN_VERSION_swagger2(2,4,0)
        & type_ ?~ SwaggerArray
#else
        & type_ .~ SwaggerArray
#endif
        & items ?~ SwaggerItemsObject (Inline byteSchema)

instance {-# OVERLAPPING #-} (ToJSONKey k, ToSchema k) => ToSchema (OverrideToSchema (Map k B.ByteString)) where
  declareNamedSchema _ = case Aeson.toJSONKey :: ToJSONKeyFunction k of
      ToJSONKeyText _ _ -> do
          return (NamedSchema Nothing schema_)
      ToJSONKeyValue _ _ -> do
          declareNamedSchema (Proxy :: Proxy [(k, (OverrideToSchema B.ByteString))])
    where
      schema_ = mempty
#if MIN_VERSION_swagger2(2,4,0)
        & type_ ?~ SwaggerObject
#else
        & type_ .~ SwaggerObject
#endif
        & additionalProperties ?~ AdditionalPropertiesSchema (Inline byteSchema)

-- | Wrapped Type Schemas

declareWrapperNamedSchema
  :: forall a
   . ToSchema a
  => T.Text
  -> Proxy (OverrideToSchema a)
  -> Declare (Definitions Schema) NamedSchema
declareWrapperNamedSchema formatValue _ =
  declareNamedSchema (Proxy :: Proxy a)
#if MIN_VERSION_swagger2(2,4,0)
    <&> schema . paramSchema . format ?~ formatValue
#else
    <&> schema . paramSchema . format .~ formatValue
#endif

instance {-# OVERLAPPING #-} ToSchema (OverrideToSchema (Maybe Double)) where
  declareNamedSchema = declareWrapperNamedSchema "DoubleValue"

instance {-# OVERLAPPING #-} ToSchema (OverrideToSchema (Maybe Float)) where
  declareNamedSchema = declareWrapperNamedSchema "FloatValue"

instance {-# OVERLAPPING #-} ToSchema (OverrideToSchema (Maybe Int64)) where
  declareNamedSchema = declareWrapperNamedSchema "Int64Value"

instance {-# OVERLAPPING #-} ToSchema (OverrideToSchema (Maybe Word64)) where
  declareNamedSchema = declareWrapperNamedSchema "UInt64Value"

instance {-# OVERLAPPING #-} ToSchema (OverrideToSchema (Maybe Int32)) where
  declareNamedSchema = declareWrapperNamedSchema "Int32Value"

instance {-# OVERLAPPING #-} ToSchema (OverrideToSchema (Maybe Word32)) where
  declareNamedSchema = declareWrapperNamedSchema "UInt32Value"

instance {-# OVERLAPPING #-} ToSchema (OverrideToSchema (Maybe Bool)) where
  declareNamedSchema = declareWrapperNamedSchema "BoolValue"

instance {-# OVERLAPPING #-} ToSchema (OverrideToSchema (Maybe T.Text)) where
  declareNamedSchema = declareWrapperNamedSchema "StringValue"

instance {-# OVERLAPPING #-} ToSchema (OverrideToSchema (Maybe TL.Text)) where
  declareNamedSchema = declareWrapperNamedSchema "StringValue"

-- TODO: Figure out what to do about conflicting `Maybe ByteString` instance
-- above

-- instance {-# OVERLAPPING #-} ToSchema (OverrideToSchema (Maybe B.ByteString)) where
--   declareNamedSchema = declareWrapperNamedSchema "BytesValue"

-- instance {-# OVERLAPPING #-} ToSchema (OverrideToSchema (Maybe BL.ByteString)) where
--   declareNamedSchema = declareWrapperNamedSchema "BytesValue"

{-| This is a convenience function that uses type inference to select the
    correct instance of `ToSchema` to use for fields of a message
-}
asProxy :: (Proxy (OverrideToSchema a) -> b) -> Proxy a
asProxy _ = Proxy

-- | Pretty-prints a schema. Useful when playing around with schemas in the
-- REPL.
ppSchema :: ToSchema a => Proxy a -> IO ()
ppSchema = LC8.putStrLn . encodePretty . toSchema

-- | JSONPB schemas for protobuf enumerations
instance (Finite e, Named e) => ToSchema (Enumerated e) where
  declareNamedSchema _ = do
    let enumName        = nameOf (proxy# :: Proxy# e)
    let dropPrefix      = T.drop (T.length enumName)
    let enumMemberNames = dropPrefix . fst <$> enumerate (proxy# :: Proxy# e)
    return $ NamedSchema (Just enumName)
           $ mempty
#if MIN_VERSION_swagger2(2,4,0)
             & type_ ?~ SwaggerString
#else
             & type_ .~ SwaggerString
#endif
             & enum_ ?~ fmap String enumMemberNames

instance ToSchema (Fixed Int32) where
  declareNamedSchema _ = declareNamedSchema (Proxy @Int32)

instance ToSchema (Fixed Int64) where
  declareNamedSchema _ = declareNamedSchema (Proxy @Int64)

instance ToSchema (Fixed Word32) where
  declareNamedSchema _ = declareNamedSchema (Proxy @Word32)

instance ToSchema (Fixed Word64) where
  declareNamedSchema _ = declareNamedSchema (Proxy @Word64)
