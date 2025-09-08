{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

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
deriving newtype instance ToSchema a => ToSchema (Fixed a)

-- Zig-zag encoding issues do not matter to JSONPB.
deriving newtype instance ToSchema a => ToSchema (Signed a)

-- `Proto3.Suite.Types.ForceEmit` affects the binary format, not the JSONPB schema, but
-- that might change if we find a way to express the meaning of omission in such a schema.
deriving newtype instance ToSchema a => ToSchema (Proto3.Suite.Types.ForceEmit a)

-- Packed/unpacked distinctions do not matter to JSONPB.
deriving via (V.Vector a) instance ToSchema a => ToSchema (NestedVec a)
deriving via (V.Vector a) instance ToSchema a => ToSchema (PackedVec a)
deriving via (V.Vector a) instance ToSchema a => ToSchema (UnpackedVec a)

-- Unless and until the overlapping instances for @Maybe (Wrapped _)@
-- are selected, the schema is unaffected by 'Wrapped'.
deriving newtype instance ToSchema a => ToSchema (Wrapped a)

instance ToSchema (Proto3.Suite.Types.String a) where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy String)

instance ToSchema (Proto3.Suite.Types.Bytes a) where
  declareNamedSchema _ = pure (NamedSchema Nothing byteSchema)

-- Note that the context is @ToSchema (Maybe a)@, NOT @ToSchema a@.
-- This design keeps this instance from bypassing overlapping
-- instances such as @ToSchema (Maybe (Wrapped Bool))@ that
-- are included by cabal flag @-fswagger-wrapper-format@.
-- We use MonoLocalBinds to avoid the resultant compiler warning.
deriving via (Maybe a) instance ToSchema (Maybe a) => ToSchema (Nested a)

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
