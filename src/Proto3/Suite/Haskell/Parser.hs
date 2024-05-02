module Proto3.Suite.Haskell.Parser
  ( parseModule
  ) where

import qualified GHC.Data.EnumSet as EnumSet
import GHC.Data.StringBuffer (StringBuffer)
import GHC.Driver.Errors (printMessages)
import GHC.Driver.Errors.Types (PsMessage)
import GHC.Driver.Session (languageExtensions)
import GHC.Hs (HsModule)
import qualified GHC.Parser
import GHC.Parser.Lexer
         (P(..), PState(errors, warnings), ParseResult(..), initParserState, mkParserOpts)
import GHC.Types.Error (partitionMessages, unionMessages)
import GHC.Types.SrcLoc (Located, RealSrcLoc)
import GHC.Utils.Error (DiagOpts(..), Messages)
import GHC.Utils.Logger (Logger)
import GHC.Utils.Outputable (defaultSDocContext)

parseModule :: Logger -> RealSrcLoc -> StringBuffer -> IO (Maybe (Located HsModule))
parseModule logger location buffer = do
    case unP GHC.Parser.parseModule initialState of
      POk finalState m -> do
        printWarningsAndErrors logger diagOpts (warnings finalState) (errors finalState)
        pure (Just m)
      PFailed finalState -> do
        printWarningsAndErrors logger diagOpts (warnings finalState) (errors finalState)
        pure Nothing
  where
    diagOpts = DiagOpts
      { diag_warning_flags = mempty
      , diag_fatal_warning_flags = mempty
      , diag_warn_is_error = False
      , diag_reverse_errors = False
      , diag_max_errors = Nothing
      , diag_ppr_ctx = defaultSDocContext
      }
    extensions = EnumSet.fromList (languageExtensions Nothing)
    parserOpts = mkParserOpts extensions diagOpts [] False True True True
    initialState = initParserState parserOpts buffer location

printWarningsAndErrors :: Logger -> DiagOpts -> Messages PsMessage -> Messages PsMessage -> IO ()
printWarningsAndErrors logger diagOpts ws es = do
  let (warningsAsWarnings, warningsAsErrors) = partitionMessages ws
  printMessages logger diagOpts warningsAsWarnings
  printMessages logger diagOpts (unionMessages es warningsAsErrors)
