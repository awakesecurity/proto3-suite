{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Proto.Quote (
  embedProtoDefinitions,
  dotProtoTestFile,
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except (ExceptT, runExceptT)

import Data.Foldable (traverse_)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO

import "ghc-lib-parser" GHC.Hs qualified as GHC
import "ghc-lib-parser" GHC.Hs.Type qualified as GHC ()
import "ghc-lib-parser" GHC.Utils.Logger (initLogger)
import "ghc-lib-parser" GHC.Types.SrcLoc qualified as GHC (unLoc)

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
 )

import Proto3.Suite.Haskell.Syntax (hsDeclToDec)

--------------------------------------------------------------------------------

runExceptQ :: Show e => ExceptT e Q a -> Q a
runExceptQ action = 
  runExceptT action >>= \case
    Left exn -> fail (show exn)
    Right rx -> pure rx

embedProtoDefinitions ::
  ( (?stringType :: StringType)
  , (?typeLevelFormat :: Bool)
  ) =>
  -- | TODO: docs
  Text -> 
  -- | TODO: docs
  Q [Dec]
embedProtoDefinitions src = do
  logger <- TH.runIO initLogger
  
  TH.Module _ modName <- TH.thisModule

  let pkg :: Path 
      pkg = Path (pure "")

  let CompileArgs {..} = makeTestCompileArgs "quote.proto"

  -- TH.newDeclarationGroup
  runExceptQ do 
    extras <- traverse (getExtraInstances logger) extraInstanceFiles

    let extraImports :: [GHC.LImportDecl GHC.GhcPs]
        !extraImports = foldr ((++) . fst) [] extras 

    let extraDecls :: [GHC.LHsDecl GHC.GhcPs]
        !extraDecls = foldr ((++) . snd) [] extras

    case parseProtoWithFile pkg (show modName) (Text.unpack src) of 
      Left err -> fail (show err)
      Right dotProto -> do 
        GHC.HsModule _ _ _ _ moduleDecls <- hsModuleForDotProto (extraImports, extraDecls) dotProto mempty

        let decls :: [Dec]
            decls = foldr ((++) . hsDeclToDec . GHC.unLoc) [] moduleDecls
        
        liftIO $ traverse_ (putStrLn . show . TH.ppr) decls
        pure decls

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
        , "./include/"
        ]
    , extraInstanceFiles = []
    , inputProto = filepath
    , outputDir = "./gen/"
    , stringType = StringType "Data.String" "String"
    , typeLevelFormat = False
    }