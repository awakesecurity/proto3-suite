{-# LANGUAGE RecordWildCards #-}

module Test.Proto.Quote (
  dotProtoTestFile,
) where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO

import GHC.Hs qualified as GHC
import GHC.Hs.Type qualified as GHC
import GHC.Utils.Logger (initLogger)

import "template-haskell" Language.Haskell.TH as TH
import "template-haskell" Language.Haskell.TH.Syntax as TH

import Proto3.Suite.DotProto.AST (Path (..))
import Proto3.Suite.DotProto.Parsing (parseProtoWithFile)
import Proto3.Suite.DotProto.Generate (
  CompileArgs (..),
  StringType (..),
  compileDotProtoFile,
  hsModuleForDotProto,
  getExtraInstances,
  readDotProtoWithContext,
 )

import System.Directory qualified as Directory

--------------------------------------------------------------------------------

embedProtoDefinitions ::
  -- | TODO: docs
  CompileArgs -> 
  -- | TODO: docs
  Text -> 
  -- | TODO: docs
  Q [Dec]
embedProtoDefinitions CompileArgs{..} src = do 
  (dotProto, importTypeContext) <- readDotProtoWithContext includeDir inputProto

  logger <- TH.runIO initLogger

  extraInstances <- traverse (getExtraInstances logger) extraInstanceFiles
   
  GHC.HsModule _ _ _ _ decls <- hsModuleForDotProto (mconcat extraInstances) dotProto importTypeContext

  _

dotProtoTestFile :: 
  -- | If specified, the 'FilePath' to write the given protobuf source to. 
  -- Otherwise, 'dotProtoTestFile' will not write the source to a file.
  Maybe FilePath -> 
  -- | The protobuf source code to lift into a 'DotProto' AST.
  Text -> 
  -- | Returns the given protobuf source code in the 'DotProto' AST. 
  Q Exp 
dotProtoTestFile optFilepath src = do
  case optFilepath of 
    Nothing -> pure ()
    Just filepath -> do 
        TH.runIO (Text.IO.writeFile filepath src)
        compileTestFile filepath 

  TH.Module pkgName modName <- TH.thisModule

  let pkg :: Path 
      pkg = Path (pure (show pkgName))

  case parseProtoWithFile pkg (show modName) (Text.unpack src) of 
    Left err -> fail (show err)
    Right dotProto -> TH.lift dotProto

compileTestFile :: FilePath -> Q ()
compileTestFile filepath = do
  result <- TH.runIO do 
    logger <- initLogger
    compileDotProtoFile logger compileArgs

  case result of
    Left exn -> fail (show exn)
    Right () -> pure ()
  where
    compileArgs :: CompileArgs
    compileArgs = 
      CompileArgs
        { includeDir = [ "./test_files/" ]
        , extraInstanceFiles = []
        , inputProto = filepath
        , outputDir = "./gen/"
        , stringType = StringType "Data.String" "String"
        , typeLevelFormat = False
        }