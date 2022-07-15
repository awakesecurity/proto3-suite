{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module provides helper functions to generate Swagger schemas that
-- describe JSONPB encodings for protobuf types.
module Proto3.Suite.DotProto.Generate.Swagger
  ( ppSchema
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
import qualified Data.ByteString.Lazy.Char8      as LC8
import           Data.Hashable                   (Hashable)
import           Data.HashMap.Strict.InsOrd      (InsOrdHashMap)
import qualified Data.HashMap.Strict.InsOrd
import           Data.Swagger
import qualified Data.Text                       as T
import           Data.Proxy
import qualified Data.Vector                     as V
import           GHC.Exts                        (Proxy#, proxy#)
import           Google.Protobuf.Wrappers.Polymorphic (Wrapped(..))
import           Proto3.Suite                    (Enumerated (..), Finite (..),
                                                  Fixed (..), Named (..),
                                                  Nested (..), NestedVec (..),
                                                  PackedVec (..), Signed (..),
                                                  UnpackedVec (..), enumerate)
import qualified Proto3.Suite.Types
import           Proto3.Suite.DotProto.Generate.Swagger.Wrappers ()

-- | Convenience re-export so that users of generated code don't have to add
--   an explicit dependency on @insert-ordered-containers@
insOrdFromList :: (Eq k, Hashable k) => [(k, v)] -> InsOrdHashMap k v
insOrdFromList = Data.HashMap.Strict.InsOrd.fromList

-- Distinctions between varint and fixed-width formats do not matter to JSONPB.
deriving via a instance ToSchema a => ToSchema (Fixed a)

-- Zig-zag encoding issues do not matter to JSONPB.
deriving via a instance ToSchema a => ToSchema (Signed a)

-- Packed/unpacked distinctions do not matter to JSONPB.
deriving via (V.Vector a) instance ToSchema a => ToSchema (NestedVec a)
deriving via (V.Vector a) instance ToSchema a => ToSchema (PackedVec a)
deriving via (V.Vector a) instance ToSchema a => ToSchema (UnpackedVec a)

-- Unless and until the overlapping instances for @Maybe (Wrapped _)@
-- are selected, the schema is unaffected by 'Wrapped'.
deriving via a instance ToSchema a => ToSchema (Wrapped a)

instance ToSchema (Proto3.Suite.Types.String a) where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy String)

instance ToSchema (Proto3.Suite.Types.Bytes a) where
  declareNamedSchema _ = pure (NamedSchema Nothing byteSchema)

deriving via (Maybe a) instance ToSchema a => ToSchema (Nested a)

{-| This is a convenience function that uses type inference to select the
    correct instance of `ToSchema` to use for fields of a message
-}
asProxy :: (Proxy a -> b) -> Proxy a
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
