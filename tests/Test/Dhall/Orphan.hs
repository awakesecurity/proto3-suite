{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Dhall.Orphan () where

#if defined(DHALL)
#if !MIN_VERSION_dhall(1,41,2)

import qualified Data.Text.Short
import           Dhall
import           Dhall.Core

instance FromDhall Data.Text.Short.ShortText where
    autoWith _ = fmap Data.Text.Short.fromText strictText

instance ToDhall Data.Text.Short.ShortText where
    injectWith _ = Encoder {..}
      where
        embed text =
            TextLit (Chunks [] (Data.Text.Short.toText text))

        declared = Text

#endif
#endif
