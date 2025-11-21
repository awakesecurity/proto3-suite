{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Proto3.Suite.Haskell.Parser
  ( Logger
  , initLogger
  , parseModule
  , renderSDoc
  ) where

import qualified GHC.Data.EnumSet as EnumSet
import GHC.Data.StringBuffer (StringBuffer)
import GHC.Driver.Session (languageExtensions)
import qualified GHC.Parser
import GHC.Parser.Lexer (P(..), PState, ParseResult(..))
import GHC.Types.SrcLoc (Located, RealSrcLoc)
import GHC.Utils.Outputable (SDoc)

#if MIN_VERSION_ghc_lib_parser(9,8,0)
import qualified GHC.Driver.Errors (printMessages)
import GHC.Parser.Errors.Types (PsMessage)
import GHC.Parser.Lexer (getPsMessages, initParserState, mkParserOpts)
import GHC.Types.Error (Messages, NoDiagnosticOpts(..), partitionMessages, unionMessages)
import GHC.Utils.Error (DiagOpts, emptyDiagOpts)
import GHC.Utils.Logger (Logger, initLogger)
import GHC.Utils.Outputable (defaultSDocContext, renderWithContext)
#elif MIN_VERSION_ghc_lib_parser(9,6,0)
import qualified GHC.Driver.Errors (printMessages)
import GHC.Parser.Errors.Types (PsMessage)
import GHC.Parser.Lexer (getPsMessages, initParserState, mkParserOpts)
import GHC.Types.Error (Messages, NoDiagnosticOpts(..), partitionMessages, unionMessages)
import GHC.Utils.Error (DiagOpts(..))
import GHC.Utils.Logger (Logger, initLogger)
import GHC.Utils.Outputable (defaultSDocContext, renderWithContext)
#elif MIN_VERSION_ghc_lib_parser(9,4,0)
import qualified GHC.Driver.Errors (printMessages)
import GHC.Parser.Errors.Types (PsMessage)
import GHC.Parser.Lexer (getPsMessages, initParserState, mkParserOpts)
import GHC.Types.Error (Messages, partitionMessages, unionMessages)
import GHC.Utils.Error (DiagOpts(..))
import GHC.Utils.Logger (Logger, initLogger)
import GHC.Utils.Outputable (defaultSDocContext, renderWithContext)
#else
import Control.Arrow ((***))
import Data.Foldable (traverse_)
import GHC.ByteOrder (targetByteOrder)
import GHC.Data.Bag (Bag)
import GHC.Driver.Session
         (DynFlags(..), FileSettings(..), GhcNameVersion(..),
          LlvmConfig(..), Settings(..), defaultDynFlags)
import GHC.Parser.Errors.Ppr (pprError, pprWarning)
import GHC.Parser.Lexer (getMessages, initParserState, mkParserOpts)
import GHC.Platform (ArchOS(..))
import GHC.Settings (Platform(..), PlatformMisc(..), ToolSettings(..))
import GHC.Types.Error (DecoratedSDoc, MsgEnvelope(..), renderDiagnostic)
import GHC.Utils.Error (formatBulleted, sortMsgBag)
import GHC.Utils.Fingerprint (fingerprint0)
import GHC.Utils.Logger (Logger, initLogger, putLogMsg)
import GHC.Utils.Outputable (defaultSDocContext, mkErrStyle, renderWithContext, withPprStyle)
#endif
import Proto3.Suite.Haskell.Syntax (HsModule)

-- | Parses the module with the specified location and content,
-- returning 'Nothing' on parse failure.  Errors and warnings
-- are sent to the given 'Logger', except on GHC 9.0, which
-- would require a 'DynFlags' in order to report messages.
-- Unfortunately, creating a 'DynFlags' would require a directory
-- containing a GHC installation; we do not wish to require that.
parseModule ::
  Logger ->
  RealSrcLoc ->
  StringBuffer ->
  IO (Maybe (Located HsModule))
parseModule logger location input = do
    case unP GHC.Parser.parseModule initialState of
      POk _finalState m -> do
        printWarningsAndErrors logger diagOpts _finalState
        pure (Just m)
      PFailed _finalState -> do
        printWarningsAndErrors logger diagOpts _finalState
        pure Nothing
  where
    exts = EnumSet.fromList (languageExtensions Nothing)
#if MIN_VERSION_ghc_lib_parser(9,4,0)
    diagOpts =
#if MIN_VERSION_ghc_lib_parser(9,8,0)
      emptyDiagOpts
#else
      DiagOpts
        { diag_warning_flags = mempty
        , diag_fatal_warning_flags = mempty
        , diag_warn_is_error = False
        , diag_reverse_errors = False
        , diag_max_errors = Nothing
        , diag_ppr_ctx = defaultSDocContext
        }
#endif
    parserOpts = mkParserOpts exts diagOpts [] False True True True
    initialState = initParserState parserOpts input location
#else
    diagOpts = DiagOpts
    parserOpts = mkParserOpts EnumSet.empty exts False True True True
    initialState = initParserState parserOpts input location
#endif

printWarningsAndErrors :: Logger -> DiagOpts -> PState -> IO ()
printWarningsAndErrors logger diagOpts state = do
#if MIN_VERSION_ghc_lib_parser(9,4,0)
  let (ws, es) = getPsMessages state
  let (warnings, unionMessages es -> errors) = partitionMessages ws
#else
  let (warnings, errors) = (fmap pprWarning *** fmap pprError) (getMessages state)
#endif
  printMessages logger diagOpts warnings
  printMessages logger diagOpts errors

#if MIN_VERSION_ghc_lib_parser(9,6,0)

printMessages :: Logger -> DiagOpts -> Messages PsMessage -> IO ()
printMessages logger = GHC.Driver.Errors.printMessages logger NoDiagnosticOpts

#elif MIN_VERSION_ghc_lib_parser(9,4,0)

printMessages :: Logger -> DiagOpts -> Messages PsMessage -> IO ()
printMessages = GHC.Driver.Errors.printMessages

#else

printMessages :: Logger -> DiagOpts -> Bag (MsgEnvelope DecoratedSDoc) -> IO ()
printMessages logger _ = traverse_ report . sortMsgBag Nothing
  where
    report MsgEnvelope
             { errMsgContext = errCtxt
             , errMsgDiagnostic = diagnostic
             , errMsgReason = reason
             , errMsgSeverity = severity
             , errMsgSpan = sp } =
        putLogMsg logger renderingDynFlags reason severity sp $
          withPprStyle (mkErrStyle errCtxt) $
            formatBulleted defaultSDocContext (renderDiagnostic diagnostic)

#endif

renderSDoc :: SDoc -> String
renderSDoc = renderWithContext defaultSDocContext

#if !MIN_VERSION_ghc_lib_parser(9,4,0)

data DiagOpts = DiagOpts

-- | 'DynFlags' suitable only for rendering
-- Haskell source and diagnostic messages.
--
-- NOTE: For use with GHC 9.0 only.  These flags are rather questionable
-- all of the tools direction information consists of placeholders so
-- that compile-proto-file does not require an actual GHC installation.
renderingDynFlags :: DynFlags
renderingDynFlags = defaultDynFlags placeholderSettings placeholderLlvmConfig
  where
    archUnknown = read "ArchUnknown"
    osUnknown = read "OSUnknown"
    placeholderSettings = Settings
      { sGhcNameVersion = GhcNameVersion "compile-proto-file" "v?"
      , sFileSettings = FileSettings
        { fileSettings_ghcUsagePath = mempty
        , fileSettings_ghciUsagePath = mempty
        , fileSettings_toolDir = Nothing
        , fileSettings_topDir = mempty
        , fileSettings_tmpDir = mempty
        , fileSettings_globalPackageDatabase = mempty
        }
      , sTargetPlatform = Platform
        { platformArchOS = ArchOS archUnknown osUnknown
        , platformWordSize = read "8"
        , platformByteOrder = targetByteOrder
        , platformUnregisterised = True
        , platformHasGnuNonexecStack = False
        , platformHasIdentDirective = False
        , platformHasSubsectionsViaSymbols = False
        , platformIsCrossCompiling = True
        , platformLeadingUnderscore = True
        , platformTablesNextToCode = False
        , platform_constants = Nothing
        }
      , sToolSettings = ToolSettings
        { toolSettings_ldSupportsCompactUnwind = False
        , toolSettings_ldSupportsBuildId = False
        , toolSettings_ldSupportsFilelist = False
        , toolSettings_ldIsGnuLd = False
        , toolSettings_ccSupportsNoPie = False
        , toolSettings_pgm_L = mempty
        , toolSettings_pgm_P = mempty
        , toolSettings_pgm_F = mempty
        , toolSettings_pgm_c = mempty
        , toolSettings_pgm_a = mempty
        , toolSettings_pgm_l = mempty
        , toolSettings_pgm_lm = mempty
        , toolSettings_pgm_dll = mempty
        , toolSettings_pgm_T = mempty
        , toolSettings_pgm_windres = mempty
        , toolSettings_pgm_libtool = mempty
        , toolSettings_pgm_ar = mempty
        , toolSettings_pgm_otool = mempty
        , toolSettings_pgm_install_name_tool = mempty
        , toolSettings_pgm_ranlib = mempty
        , toolSettings_pgm_lo = mempty
        , toolSettings_pgm_lc = mempty
        , toolSettings_pgm_lcc = mempty
        , toolSettings_pgm_i = mempty
        , toolSettings_opt_L = mempty
        , toolSettings_opt_P = mempty
        , toolSettings_opt_P_fingerprint = fingerprint0
        , toolSettings_opt_F = mempty
        , toolSettings_opt_c = mempty
        , toolSettings_opt_cxx = mempty
        , toolSettings_opt_a = mempty
        , toolSettings_opt_l = mempty
        , toolSettings_opt_lm = mempty
        , toolSettings_opt_windres = mempty
        , toolSettings_opt_lo = mempty
        , toolSettings_opt_lc = mempty
        , toolSettings_opt_lcc = mempty
        , toolSettings_opt_i = mempty
        , toolSettings_extraGccViaCFlags = mempty
        }
      , sPlatformMisc = PlatformMisc
        { platformMisc_targetPlatformString = mempty
        , platformMisc_ghcWithInterpreter = False
        , platformMisc_ghcWithSMP = False
        , platformMisc_ghcRTSWays = mempty
        , platformMisc_libFFI = False
        , platformMisc_ghcRtsWithLibdw = False
        , platformMisc_llvmTarget = mempty
        }
      , sRawSettings = mempty
      }

    placeholderLlvmConfig = LlvmConfig
      { llvmTargets = mempty
      , llvmPasses = mempty
      }

#endif
