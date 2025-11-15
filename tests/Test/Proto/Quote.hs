
module Test.Proto.Quote (
  dotProtoTestFile,
) where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO

import "template-haskell" Language.Haskell.TH as TH
import "template-haskell" Language.Haskell.TH.Syntax as TH

import Proto3.Suite.DotProto.AST (Path (..))
import Proto3.Suite.DotProto.Parsing (parseProtoWithFile)

import System.Directory qualified as Directory

--------------------------------------------------------------------------------

dotProtoTestFile :: Maybe FilePath -> Text -> Q Exp 
dotProtoTestFile optFilepath src = do
  case optFilepath of 
    Nothing -> pure ()
    Just filepath -> TH.runIO do
      dir <- Directory.getCurrentDirectory
      putStrLn ("getCurrentDirectory: " ++ dir)
      Text.IO.writeFile filepath src

  TH.Module pkgName modName <- TH.thisModule

  let pkg :: Path 
      pkg = Path (pure (show pkgName))

  case parseProtoWithFile pkg (show modName) (Text.unpack src) of 
    Left err -> fail (show err)
    Right dotProto -> TH.lift dotProto
