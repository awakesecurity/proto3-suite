{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeOperators       #-}

import           Options.Applicative
import           Prelude                        hiding (FilePath)
import           Proto3.Suite.DotProto.Generate
import           Proto3.Suite.Haskell.Parser    (initLogger)

parseArgs :: ParserInfo CompileArgs
parseArgs = info (helper <*> parser) (fullDesc <> progDesc "Compiles a .proto file to a Haskell module")
  where
    parser = CompileArgs <$> includes <*> extraInstances <*> proto <*> out <*> stringType <*> recordStyle

    includes = many $ strOption $
      long "includeDir"
        <> metavar "DIR"
        <> help "Path to search for included .proto files (can be repeated, and paths will be searched in order; the current directory is used if this option is not provided)"

    extraInstances = many $ strOption $
      long "extraInstanceFile"
        <> metavar "FILE"
        <> help "Additional file to provide instances that would otherwise be generated. Can be used multiple times. Types for which instance overrides are given must be fully qualified."

    proto = strOption $
      long "proto"
        <> metavar "FILE"
        <> help "Path to input .proto file"

    out = strOption $
      long "out"
        <> metavar "DIR"
        <> help "Output directory path where generated Haskell modules will be written (directory is created if it does not exist; note that files in the output directory may be overwritten!)"

    stringType = option (eitherReader parseStringType)
      $ long "string-type"
      <> metavar "Data.Text.Lazy.Text"
      <> help "Haskell representation of strings"
      <> value (StringType "Data.Text.Lazy" "Text")

    recordStyle = flag RegularRecords LargeRecords
      $ long "largeRecords"
      <> help "Use large-records library to optimize the core code size of generated records"

main :: IO ()
main = do
  logger <- initLogger
  compileArgs <- execParser parseArgs
  compileDotProtoFileOrDie logger compileArgs
