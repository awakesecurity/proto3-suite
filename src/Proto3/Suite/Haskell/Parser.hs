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
import qualified GHC.Hs
import qualified GHC.Parser
import GHC.Parser.Lexer (P(..), PState, ParseResult(..))
import GHC.Types.SrcLoc (Located, RealSrcLoc)
import GHC.Utils.Outputable (SDoc)

#if MIN_VERSION_ghc(9,6,0)
import qualified GHC.Driver.Errors (printMessages)
import GHC.Parser.Errors.Types (PsMessage)
import GHC.Parser.Lexer (getPsMessages, initParserState, mkParserOpts)
import GHC.Types.Error (Messages, NoDiagnosticOpts(..), partitionMessages, unionMessages)
import GHC.Utils.Error (DiagOpts(..))
import GHC.Utils.Logger (Logger, initLogger)
import GHC.Utils.Outputable (defaultSDocContext, renderWithContext)
#elif MIN_VERSION_ghc(9,4,0)
import qualified GHC.Driver.Errors (printMessages)
import GHC.Parser.Errors.Types (PsMessage)
import GHC.Parser.Lexer (getPsMessages, initParserState, mkParserOpts)
import GHC.Types.Error (Messages, partitionMessages, unionMessages)
import GHC.Utils.Error (DiagOpts(..))
import GHC.Utils.Logger (Logger, initLogger)
import GHC.Utils.Outputable (defaultSDocContext, renderWithContext)
#elif MIN_VERSION_ghc(9,2,0)
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
#else
import Data.Foldable (traverse_)
import GHC.ByteOrder (targetByteOrder)
import GHC.Data.Bag (Bag)
import GHC.Driver.Session
         (DynFlags(..), FileSettings(..), GhcNameVersion(..),
          LlvmConfig(..), Settings(..), defaultDynFlags)
import GHC.Parser.Lexer (getMessages, mkParserFlags', mkPStatePure)
import GHC.Settings
         (Platform(..), PlatformConstants(..), PlatformMini(..), PlatformMisc(..), ToolSettings(..))
import GHC.Unit.Types (UnitId(..))
import GHC.Utils.Error (ErrMsg, pprErrMsgBagWithLoc)
import GHC.Utils.Fingerprint (fingerprint0)
import GHC.Utils.Outputable (SDocContext, defaultDumpStyle, initSDocContext, renderWithStyle)
#endif

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
  IO (Maybe (Located (GHC.Hs.HsModule
#if MIN_VERSION_ghc(9,6,0)
                                      GHC.Hs.GhcPs
#endif
                     )))
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
#if MIN_VERSION_ghc(9,4,0)
    diagOpts = DiagOpts
      { diag_warning_flags = mempty
      , diag_fatal_warning_flags = mempty
      , diag_warn_is_error = False
      , diag_reverse_errors = False
      , diag_max_errors = Nothing
      , diag_ppr_ctx = defaultSDocContext
      }
    parserOpts = mkParserOpts exts diagOpts [] False True True True
    initialState = initParserState parserOpts input location
#elif MIN_VERSION_ghc(9,2,0)
    diagOpts = DiagOpts
    parserOpts = mkParserOpts EnumSet.empty exts False True True True
    initialState = initParserState parserOpts input location
#else
    diagOpts = DiagOpts
    unitId = UnitId "compile-proto-file-extra-instance-0.0:extrainstance+AAAAAA"
    parserFlags = mkParserFlags' EnumSet.empty exts unitId False True True True
    initialState = mkPStatePure parserFlags input location
#endif

printWarningsAndErrors :: Logger -> DiagOpts -> PState -> IO ()
printWarningsAndErrors logger diagOpts state = do
#if MIN_VERSION_ghc(9,4,0)
  let (ws, es) = getPsMessages state
  let (warnings, unionMessages es -> errors) = partitionMessages ws
#elif MIN_VERSION_ghc(9,2,0)
  let (warnings, errors) = (fmap pprWarning *** fmap pprError) (getMessages state)
#else
  let (warnings, errors) = getMessages state renderingDynFlags
#endif
  printMessages logger diagOpts warnings
  printMessages logger diagOpts errors

#if MIN_VERSION_ghc(9,6,0)

printMessages :: Logger -> DiagOpts -> Messages PsMessage -> IO ()
printMessages logger = GHC.Driver.Errors.printMessages logger NoDiagnosticOpts

#elif MIN_VERSION_ghc(9,4,0)

printMessages :: Logger -> DiagOpts -> Messages PsMessage -> IO ()
printMessages = GHC.Driver.Errors.printMessages

#elif MIN_VERSION_ghc(9,2,0)

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

#else

printMessages :: Logger -> DiagOpts -> Bag ErrMsg -> IO ()
printMessages _ _ = traverse_ (putStrLn . renderSDoc) . pprErrMsgBagWithLoc

#endif

renderSDoc :: SDoc -> String
renderSDoc =
#if MIN_VERSION_ghc(9,2,0)
  renderWithContext
#else
  renderWithStyle
#endif
    defaultSDocContext

#if !MIN_VERSION_ghc(9,2,0)

data Logger = Logger

initLogger :: IO Logger
initLogger = pure Logger

-- | To be used only for rendering Haskell source, warnings, and errors.
-- Makes use of questionable value of 'DynFlags' so that we do not have
-- to require a GHC installation director to run compile-proto-file.
defaultSDocContext :: SDocContext
defaultSDocContext = initSDocContext renderingDynFlags defaultDumpStyle

#endif

#if !MIN_VERSION_ghc(9,4,0)

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
        {
#if MIN_VERSION_ghc(9,2,0)
          platformArchOS = ArchOS
#else
          platformMini = PlatformMini
#endif
            archUnknown osUnknown
        , platformWordSize = read "8"
        , platformByteOrder = targetByteOrder
        , platformUnregisterised = True
        , platformHasGnuNonexecStack = False
        , platformHasIdentDirective = False
        , platformHasSubsectionsViaSymbols = False
        , platformIsCrossCompiling = True
        , platformLeadingUnderscore = True
        , platformTablesNextToCode = False
#if MIN_VERSION_ghc(9,2,0)
        , platform_constants = Nothing
#endif
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
#if !MIN_VERSION_ghc(9,2,0)
        , platformMisc_ghcThreaded = False
        , platformMisc_ghcDebugged = False
#endif
        , platformMisc_ghcRtsWithLibdw = False
        , platformMisc_llvmTarget = mempty
        }
#if !MIN_VERSION_ghc(9,2,0)
      , sPlatformConstants = PlatformConstants
        { pc_CONTROL_GROUP_CONST_291 = 0
        , pc_STD_HDR_SIZE = 0
        , pc_PROF_HDR_SIZE = 0
        , pc_BLOCK_SIZE = 0
        , pc_BLOCKS_PER_MBLOCK = 0
        , pc_TICKY_BIN_COUNT = 0
        , pc_OFFSET_StgRegTable_rR1 = 0
        , pc_OFFSET_StgRegTable_rR2 = 0
        , pc_OFFSET_StgRegTable_rR3 = 0
        , pc_OFFSET_StgRegTable_rR4 = 0
        , pc_OFFSET_StgRegTable_rR5 = 0
        , pc_OFFSET_StgRegTable_rR6 = 0
        , pc_OFFSET_StgRegTable_rR7 = 0
        , pc_OFFSET_StgRegTable_rR8 = 0
        , pc_OFFSET_StgRegTable_rR9 = 0
        , pc_OFFSET_StgRegTable_rR10 = 0
        , pc_OFFSET_StgRegTable_rF1 = 0
        , pc_OFFSET_StgRegTable_rF2 = 0
        , pc_OFFSET_StgRegTable_rF3 = 0
        , pc_OFFSET_StgRegTable_rF4 = 0
        , pc_OFFSET_StgRegTable_rF5 = 0
        , pc_OFFSET_StgRegTable_rF6 = 0
        , pc_OFFSET_StgRegTable_rD1 = 0
        , pc_OFFSET_StgRegTable_rD2 = 0
        , pc_OFFSET_StgRegTable_rD3 = 0
        , pc_OFFSET_StgRegTable_rD4 = 0
        , pc_OFFSET_StgRegTable_rD5 = 0
        , pc_OFFSET_StgRegTable_rD6 = 0
        , pc_OFFSET_StgRegTable_rXMM1 = 0
        , pc_OFFSET_StgRegTable_rXMM2 = 0
        , pc_OFFSET_StgRegTable_rXMM3 = 0
        , pc_OFFSET_StgRegTable_rXMM4 = 0
        , pc_OFFSET_StgRegTable_rXMM5 = 0
        , pc_OFFSET_StgRegTable_rXMM6 = 0
        , pc_OFFSET_StgRegTable_rYMM1 = 0
        , pc_OFFSET_StgRegTable_rYMM2 = 0
        , pc_OFFSET_StgRegTable_rYMM3 = 0
        , pc_OFFSET_StgRegTable_rYMM4 = 0
        , pc_OFFSET_StgRegTable_rYMM5 = 0
        , pc_OFFSET_StgRegTable_rYMM6 = 0
        , pc_OFFSET_StgRegTable_rZMM1 = 0
        , pc_OFFSET_StgRegTable_rZMM2 = 0
        , pc_OFFSET_StgRegTable_rZMM3 = 0
        , pc_OFFSET_StgRegTable_rZMM4 = 0
        , pc_OFFSET_StgRegTable_rZMM5 = 0
        , pc_OFFSET_StgRegTable_rZMM6 = 0
        , pc_OFFSET_StgRegTable_rL1 = 0
        , pc_OFFSET_StgRegTable_rSp = 0
        , pc_OFFSET_StgRegTable_rSpLim = 0
        , pc_OFFSET_StgRegTable_rHp = 0
        , pc_OFFSET_StgRegTable_rHpLim = 0
        , pc_OFFSET_StgRegTable_rCCCS = 0
        , pc_OFFSET_StgRegTable_rCurrentTSO = 0
        , pc_OFFSET_StgRegTable_rCurrentNursery = 0
        , pc_OFFSET_StgRegTable_rHpAlloc = 0
        , pc_OFFSET_stgEagerBlackholeInfo = 0
        , pc_OFFSET_stgGCEnter1 = 0
        , pc_OFFSET_stgGCFun = 0
        , pc_OFFSET_Capability_r = 0
        , pc_OFFSET_bdescr_start = 0
        , pc_OFFSET_bdescr_free = 0
        , pc_OFFSET_bdescr_blocks = 0
        , pc_OFFSET_bdescr_flags = 0
        , pc_SIZEOF_CostCentreStack = 0
        , pc_OFFSET_CostCentreStack_mem_alloc = 0
        , pc_REP_CostCentreStack_mem_alloc = 0
        , pc_OFFSET_CostCentreStack_scc_count = 0
        , pc_REP_CostCentreStack_scc_count = 0
        , pc_OFFSET_StgHeader_ccs = 0
        , pc_OFFSET_StgHeader_ldvw = 0
        , pc_SIZEOF_StgSMPThunkHeader = 0
        , pc_OFFSET_StgEntCounter_allocs = 0
        , pc_REP_StgEntCounter_allocs = 0
        , pc_OFFSET_StgEntCounter_allocd = 0
        , pc_REP_StgEntCounter_allocd = 0
        , pc_OFFSET_StgEntCounter_registeredp = 0
        , pc_OFFSET_StgEntCounter_link = 0
        , pc_OFFSET_StgEntCounter_entry_count = 0
        , pc_SIZEOF_StgUpdateFrame_NoHdr = 0
        , pc_SIZEOF_StgMutArrPtrs_NoHdr = 0
        , pc_OFFSET_StgMutArrPtrs_ptrs = 0
        , pc_OFFSET_StgMutArrPtrs_size = 0
        , pc_SIZEOF_StgSmallMutArrPtrs_NoHdr = 0
        , pc_OFFSET_StgSmallMutArrPtrs_ptrs = 0
        , pc_SIZEOF_StgArrBytes_NoHdr = 0
        , pc_OFFSET_StgArrBytes_bytes = 0
        , pc_OFFSET_StgTSO_alloc_limit = 0
        , pc_OFFSET_StgTSO_cccs = 0
        , pc_OFFSET_StgTSO_stackobj = 0
        , pc_OFFSET_StgStack_sp = 0
        , pc_OFFSET_StgStack_stack = 0
        , pc_OFFSET_StgUpdateFrame_updatee = 0
        , pc_OFFSET_StgFunInfoExtraFwd_arity = 0
        , pc_REP_StgFunInfoExtraFwd_arity = 0
        , pc_SIZEOF_StgFunInfoExtraRev = 0
        , pc_OFFSET_StgFunInfoExtraRev_arity = 0
        , pc_REP_StgFunInfoExtraRev_arity = 0
        , pc_MAX_SPEC_SELECTEE_SIZE = 0
        , pc_MAX_SPEC_AP_SIZE = 0
        , pc_MIN_PAYLOAD_SIZE = 0
        , pc_MIN_INTLIKE = 0
        , pc_MAX_INTLIKE = 0
        , pc_MIN_CHARLIKE = 0
        , pc_MAX_CHARLIKE = 0
        , pc_MUT_ARR_PTRS_CARD_BITS = 0
        , pc_MAX_Vanilla_REG = 0
        , pc_MAX_Float_REG = 0
        , pc_MAX_Double_REG = 0
        , pc_MAX_Long_REG = 0
        , pc_MAX_XMM_REG = 0
        , pc_MAX_Real_Vanilla_REG = 0
        , pc_MAX_Real_Float_REG = 0
        , pc_MAX_Real_Double_REG = 0
        , pc_MAX_Real_XMM_REG = 0
        , pc_MAX_Real_Long_REG = 0
        , pc_RESERVED_C_STACK_BYTES = 0
        , pc_RESERVED_STACK_WORDS = 0
        , pc_AP_STACK_SPLIM = 0
        , pc_WORD_SIZE = 0
        , pc_CINT_SIZE = 0
        , pc_CLONG_SIZE = 0
        , pc_CLONG_LONG_SIZE = 0
        , pc_BITMAP_BITS_SHIFT = 0
        , pc_TAG_BITS = 0
        , pc_DYNAMIC_BY_DEFAULT = False
        , pc_LDV_SHIFT = 0
        , pc_ILDV_CREATE_MASK = 0
        , pc_ILDV_STATE_CREATE = 0
        , pc_ILDV_STATE_USE = 0
        }
#endif
      , sRawSettings = mempty
      }

    placeholderLlvmConfig = LlvmConfig
      { llvmTargets = mempty
      , llvmPasses = mempty
      }

#endif
