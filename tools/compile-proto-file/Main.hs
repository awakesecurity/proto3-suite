{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeOperators       #-}

import           Data.Monoid                    ((<>))
import           Filesystem.Path.CurrentOS      (encodeString)
import           Options.Generic
import           Prelude                        hiding (FilePath)
import           Proto3.Suite.DotProto.Generate
import           Turtle                         (FilePath)

data Args w = Args
  { proto      :: w ::: FilePath   <?> "Path to input .proto file"
  , includeDir :: w ::: [FilePath] <?> "Path to search for included .proto files (can be repeated, and paths will be searched in order)"
  } deriving Generic
instance ParseRecord (Args Wrapped)
deriving instance Show (Args Unwrapped)

main :: IO ()
main = do
  Args{..} :: Args Unwrapped <- unwrapRecord "Compiles a .proto file to one or more Haskell modules"
  readDotProtoWithContext includeDir proto >>= \case
    Left err        -> fail (show err)
    Right (dp, ctx) -> case renderHsModuleForDotProto dp ctx of
      Left err  -> fail ("Error compiling " <> encodeString proto <> ": " <> show err)
      Right src -> putStrLn src
