{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash           #-}
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
import           Data.Aeson                      (Value (String))
import           Data.Aeson.Encode.Pretty        (encodePretty)
import           Data.ByteString                 (ByteString)
import qualified Data.ByteString.Lazy.Char8      as LC8
import           Data.Hashable                   (Hashable)
import           Data.HashMap.Strict.InsOrd      (InsOrdHashMap)
import qualified Data.HashMap.Strict.InsOrd
import           Data.Swagger
import qualified Data.Text                       as T
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
newtype OverrideToSchema a = OverrideToSchema a

instance {-# OVERLAPPABLE #-} ToSchema a => ToSchema (OverrideToSchema a) where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy a)

instance {-# OVERLAPPING #-} ToSchema (OverrideToSchema ByteString) where
  declareNamedSchema _ = return (NamedSchema Nothing byteSchema)

instance {-# OVERLAPPING #-} ToSchema (OverrideToSchema (V.Vector ByteString)) where
  declareNamedSchema _ = return (NamedSchema Nothing schema_)
    where
      schema_ = mempty
#if MIN_VERSION_swagger2(2,4,0)
        & type_ ?~ SwaggerArray
#else
        & type_ .~ SwaggerArray
#endif
        & items ?~ SwaggerItemsObject (Inline byteSchema)

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
