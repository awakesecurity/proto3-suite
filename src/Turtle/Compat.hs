{-# LANGUAGE CPP #-}

-- | Compatibility module to avoid compiler warnings about deprecation
-- of obsolete Turtle functionality related to FilePath.  Once we
-- no longer support pre-1.6 Turtle we can eliminate this module.
module Turtle.Compat
  ( absolute
  , collapse
  , encodeString
  , fromText
  , toText
  ) where

import qualified Data.Text
import qualified System.FilePath
import qualified Turtle

absolute :: Turtle.FilePath -> Bool
collapse :: Turtle.FilePath -> Turtle.FilePath
encodeString :: Turtle.FilePath -> String
fromText :: Turtle.Text -> Turtle.FilePath
toText :: Turtle.FilePath -> Either Turtle.Text Turtle.Text

#if MIN_VERSION_turtle(1,6,0)

absolute = System.FilePath.isAbsolute
collapse = System.FilePath.normalise
encodeString = id
fromText = Data.Text.unpack
toText = Right . Data.Text.pack

#else

absolute = Turtle.absolute
collapse = Turtle.collapse
encodeString = Turtle.encodeString
fromText = Turtle.fromText
toText = Turtle.toText

#endif
