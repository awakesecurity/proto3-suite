-- | This module provides misc internal helpers and utilities

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE ViewPatterns      #-}

module Proto3.Suite.DotProto.Internal where

import qualified Control.Foldl             as FL
import qualified Data.Text                 as T
import           Filesystem.Path.CurrentOS (absolute, (</>))
import qualified NeatInterpolation         as Neat
import           Prelude                   hiding (FilePath)
import           Proto3.Suite.DotProto
import           Text.Parsec               (ParseError)
import           Turtle                    (ExitCode (..), FilePath, MonadIO,
                                            Text)
import qualified Turtle
import           Turtle.Format             ((%))
import qualified Turtle.Format             as F

dieLines :: MonadIO m => Text -> m a
dieLines (Turtle.textToLines -> msg) = do
  mapM_ Turtle.err msg
  Turtle.exit (ExitFailure 1)

-- | @importProto searchPaths toplevel inc@ attempts to import @inc@ after
-- locating it somewhere in the @searchPaths@; @toplevel@ is simply the path of
-- toplevel .proto being processed so we can report it in an error message.
importProto :: MonadIO m => [FilePath] -> FilePath -> FilePath -> m (Either ParseError DotProto)
importProto searchPaths toplevelFP protoFP = findProto searchPaths protoFP >>= \case
  Just fp -> parseProtoFile fp
  Nothing -> dieLines [Neat.text|
    Error: while processing the transitive includes for "${topLevelFPText}", failed
    to find the imported file "${protoFPText}", after looking in the following
    locations (controlled via the --includeDir switch(es)):

    $searchPathsText
    |]
    where
      searchPathsText = T.unlines (Turtle.format ("  "%F.fp) . (</> protoFP) <$> searchPaths)
      topLevelFPText  = Turtle.format F.fp toplevelFP
      protoFPText     = Turtle.format F.fp protoFP

-- | Locates the first (if any) filename that exists on the given search paths
findProto :: MonadIO m => [FilePath] -> FilePath -> m (Maybe FilePath)
findProto searchPaths protoFP
  | absolute protoFP = dieLines [Neat.text|
      Error: Absolute paths to .proto files are not permitted; rather, .proto
      filenames must be relative to the current directory or some search path
      specified via --includeDir.

      This is because we use the relative-to-search-path name
      decide the structure of the Haskell module tree that we emit during code
      generation.
      |]
  | otherwise = flip Turtle.fold FL.head $ do
      fp <- (</> protoFP) <$> Turtle.select searchPaths
      True <- Turtle.testfile fp
      pure fp
