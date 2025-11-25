{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Proto.Quote (
  embedProtoDefinitions,
  dotProtoTestFile,
) where

import Control.Monad.Except (ExceptT, runExceptT)

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Data.Maybe (fromMaybe)

import GHC.Hs qualified as GHC
import GHC.Hs.Type qualified as GHC
import GHC.Utils.Logger (initLogger)
import GHC.Types.SrcLoc qualified as GHC (unLoc)

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
import Proto3.Suite.Haskell.Syntax (hsDeclToDec)
--------------------------------------------------------------------------------

runExceptQ :: Show e => ExceptT e Q a -> Q a
runExceptQ action = 
  runExceptT action >>= \case
    Left exn -> fail (show exn)
    Right rx -> pure rx

embedProtoDefinitions ::
  -- ( (?stringType :: StringType)
  -- , (?typeLevelFormat :: Bool)
  -- ) =>
  -- | TODO: docs
  Text -> 
  -- | TODO: docs
  Q [Dec]
embedProtoDefinitions src = do
  logger <- TH.runIO initLogger
  
  TH.Module pkgName modName <- TH.thisModule

  let pkg :: Path 
      pkg = Path (pure (show pkgName))

  let CompileArgs {..} = makeTestCompileArgs "quote.proto"

  runExceptQ do 
    extras <- traverse (getExtraInstances logger) extraInstanceFiles

    let extraImports :: [GHC.LImportDecl GHC.GhcPs]
        extraImports = foldr ((++) . fst) [] extras 

    let extraDecls :: [GHC.LHsDecl GHC.GhcPs]
        extraDecls = foldr ((++) . snd) [] extras

    case parseProtoWithFile pkg (show modName) (Text.unpack src) of 
      Left err -> fail (show err)
      Right dotProto -> do 
        -- (dotProto, importTypeContext) <- readDotProtoWithContext _ _

        GHC.HsModule _ _ _ _ moduleDecls <- hsModuleForDotProto (extraImports, extraDecls) dotProto mempty

        pure (map (hsDeclToDec . GHC.unLoc) moduleDecls)
  where 
    ?stringType = StringType "Data.String" "String"
    ?typeLevelFormat = False

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
    compileDotProtoFile logger (makeTestCompileArgs filepath)

  case result of
    Left exn -> fail (show exn)
    Right () -> pure ()

makeTestCompileArgs :: FilePath -> CompileArgs
makeTestCompileArgs filepath = 
  CompileArgs
    { includeDir = 
        [ "./."
        , "./test-files" 
        ]
    , extraInstanceFiles = []
    , inputProto = filepath
    , outputDir = "./gen/"
    , stringType = StringType "Data.String" "String"
    , typeLevelFormat = False
    }