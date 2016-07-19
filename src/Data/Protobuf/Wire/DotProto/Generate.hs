-- | This module provides functions to generate Haskell declarations for proto buf messages

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Protobuf.Wire.DotProto.Generate
  ( CompileResult, CompileError(..), TypeContext

  , hsModuleForDotProto
  , renderHsModuleForDotProto
  , readDotProtoWithContext

  -- * Exposed for unit-testing
  , typeLikeName, fieldLikeName
  ) where

import           Control.Applicative
import           Control.Monad.Except
import           Data.Protobuf.Wire.DotProto
import           Data.Char
import           Data.List (intercalate, find, nub, sortBy)
import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Monoid
import           Data.Ord (comparing)
import           Proto3.Wire.Types  (FieldNumber (..))
import           Language.Haskell.Syntax
import           Language.Haskell.Pretty
import           System.FilePath
import           Text.Parsec (ParseError)

-- * Public interface

data CompileError
  = NoPackageDeclaration
  | NoSuchType      DotProtoIdentifier
  | CompileParseError ParseError
  | CircularImport  FilePath
  | InvalidTypeName String
  | Unimplemented   String
  | InternalError   String
    deriving (Show, Eq)

-- | Result of a compilation. 'Left err' on error, where 'err' is a
--   'String' describing the error. Otherwise, the result of the
--   compilation.
type CompileResult = Either CompileError

-- | Compile a 'DotProto' AST into a 'String' representing the Haskell
--   source of a module implementing types and instances for the .proto
--   messages and enums.
renderHsModuleForDotProto :: DotProto -> TypeContext -> CompileResult String
renderHsModuleForDotProto dp importCtxt =
    fmap (("{-# LANGUAGE DeriveGeneric #-}\n" ++) . prettyPrint) $
    hsModuleForDotProto dp importCtxt

-- | Compile a Haskell module AST given a 'DotProto' package AST.
hsModuleForDotProto :: DotProto -> TypeContext -> CompileResult HsModule
hsModuleForDotProto dp@(DotProto { protoPackage = DotProtoPackageSpec pkgIdent
                                 , protoDefinitions })
                    importCtxt =
    module_ <$> pkgIdentModName pkgIdent
            <*> pure Nothing
            <*> (mappend defaultImports <$> ctxtImports importCtxt)
            <*> (do typeContext <- dotProtoTypeContext dp
                    mconcat <$> mapM (dotProtoDefinitionD (typeContext <> importCtxt)) protoDefinitions)
hsModuleForDotProto _ _ =
    Left NoPackageDeclaration

-- | Parses the file at the given path and produces an AST along with
-- a 'TypeContext' representing all types from imported '.proto' files
readDotProtoWithContext :: FilePath -> IO (CompileResult (DotProto, TypeContext))
readDotProtoWithContext dotProtoPath = runExceptT go
  where
    dotProtoPathSanitized = normalise dotProtoPath

    go = do dpRes <- parseProto <$> liftIO (readFile dotProtoPath)
            case dpRes of
              Right dp -> (dp,) . mconcat <$> mapM (readImportTypeContext (S.singleton dotProtoPathSanitized)) (protoImports dp)
              Left err -> throwError (CompileParseError err)

    wrapError :: (err' -> err) -> Either err' a -> ExceptT err IO a
    wrapError f = either (throwError . f) pure

    readImportTypeContext alreadyRead (DotProtoImport _ path)
      | path `S.member` alreadyRead = throwError (CircularImport path)
      | otherwise =
          do import_ <- wrapError CompileParseError =<< (parseProto <$> liftIO (readFile path))
             case protoPackage import_ of
               DotProtoPackageSpec importPkg ->
                 do importTypeContext <- wrapError id (dotProtoTypeContext import_)
                    let importTypeContext' = fmap (\tyInfo -> tyInfo { dotProtoTypeInfoPackage = DotProtoPackageSpec importPkg }) importTypeContext
                        qualifiedTypeContext = M.fromList <$>
                            mapM (\(nm, tyInfo) -> (,tyInfo) <$> concatDotProtoIdentifier importPkg nm)
                                (M.assocs importTypeContext')

                    importTypeContext'' <- wrapError id ((importTypeContext' <>) <$> qualifiedTypeContext)
                    (importTypeContext'' <>) . mconcat <$> sequence
                        [ readImportTypeContext (S.insert path alreadyRead) importImport
                        | importImport@(DotProtoImport DotProtoImportPublic _) <- protoImports import_]
               _ -> throwError NoPackageDeclaration

-- * Type-tracking data structures

-- | Whether a definition is an enumeration or a message
data DotProtoKind = DotProtoKindEnum
                  | DotProtoKindMessage
                  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Information about messages and enumerations
data DotProtoTypeInfo = DotProtoTypeInfo
  { dotProtoTypeInfoPackage  :: DotProtoPackageSpec
     -- ^ The package this type is defined in
  , dotProtoTypeInfoParent   :: DotProtoIdentifier
    -- ^ The message this type is nested under, or 'Anonymous' if it's top-level
  , dotProtoTypeChildContext :: TypeContext
    -- ^ The context that should be used for declarations within the
    --   scope of this type
  , dotProtoTypeInfoKind     :: DotProtoKind
    -- ^ Whether this type is an enumeration or message
  } deriving Show

-- | A mapping from .proto type identifiers to their type information
type TypeContext = M.Map DotProtoIdentifier DotProtoTypeInfo

-- ** Generating type contexts from ASTs

dotProtoTypeContext :: DotProto -> CompileResult TypeContext
dotProtoTypeContext DotProto { protoDefinitions } =
  mconcat <$> mapM definitionTypeContext protoDefinitions

definitionTypeContext :: DotProtoDefinition -> CompileResult TypeContext
definitionTypeContext (DotProtoMessage msgIdent parts) =
    do childTyContext <-
          mapM updateDotProtoTypeInfoParent =<<
          (mconcat <$> sequenceA
               [ definitionTypeContext def
               | DotProtoMessageDefinition def <- parts ])

       qualifiedChildTyContext <- M.fromList <$>
          mapM (\(nm, tyInfo) -> (,tyInfo) <$>
                                 concatDotProtoIdentifier msgIdent nm)
               (M.assocs childTyContext)

       pure (M.singleton msgIdent
                 (DotProtoTypeInfo DotProtoNoPackage Anonymous
                      childTyContext DotProtoKindMessage) <>
               qualifiedChildTyContext)
  where updateDotProtoTypeInfoParent tyInfo =
            do dotProtoTypeInfoParent <-
                     concatDotProtoIdentifier msgIdent (dotProtoTypeInfoParent tyInfo)
               pure tyInfo { dotProtoTypeInfoParent }
definitionTypeContext (DotProtoEnum enumIdent _) =
  pure (M.singleton enumIdent
            (DotProtoTypeInfo DotProtoNoPackage Anonymous mempty DotProtoKindEnum))
definitionTypeContext _ = pure mempty

concatDotProtoIdentifier :: DotProtoIdentifier -> DotProtoIdentifier
                         -> CompileResult DotProtoIdentifier
concatDotProtoIdentifier Qualified {} _ =
    internalError "concatDotProtoIdentifier: Qualified"
concatDotProtoIdentifier _ Qualified {} =
    internalError "concatDotProtoIdentifier Qualified"
concatDotProtoIdentifier Anonymous Anonymous = pure Anonymous
concatDotProtoIdentifier Anonymous b = pure b
concatDotProtoIdentifier a Anonymous = pure a
concatDotProtoIdentifier (Single a) b = concatDotProtoIdentifier (Path [a]) b
concatDotProtoIdentifier a (Single b) = concatDotProtoIdentifier a (Path [b])
concatDotProtoIdentifier (Path a) (Path b) = pure (Path (a ++ b))

-- | Given a type context, generates the import statements necessary
--   to import all the required types.
ctxtImports :: TypeContext -> CompileResult [HsImportDecl]
ctxtImports tyCtxt =
  do imports <- nub <$> sequence [ pkgIdentModName pkgIdent
                                 | DotProtoTypeInfo {
                                     dotProtoTypeInfoPackage =
                                         DotProtoPackageSpec pkgIdent
                                     } <- M.elems tyCtxt ]

     pure [ importDecl_ modName True Nothing Nothing | modName <- imports ]


-- * Functions to convert 'DotProtoType' into Haskell types

-- | Produce the Haskell type for the given 'DotProtoType' in the
--   given 'TypeContext'
hsTypeFromDotProto :: TypeContext -> DotProtoType -> CompileResult HsType
hsTypeFromDotProto ctxt (Prim (Named msgName))
    | Just DotProtoKindMessage <-
          dotProtoTypeInfoKind <$> M.lookup msgName ctxt =
        HsTyApp (primType_ "Maybe") <$>
            hsTypeFromDotProtoPrim ctxt (Named msgName)
hsTypeFromDotProto ctxt (Prim pType) =
    hsTypeFromDotProtoPrim ctxt pType
hsTypeFromDotProto ctxt (Optional (Named nm)) =
    hsTypeFromDotProto ctxt (Prim (Named nm))
hsTypeFromDotProto ctxt (Optional pType) =
    HsTyApp (primType_ "Maybe") <$> hsTypeFromDotProtoPrim ctxt pType
hsTypeFromDotProto ctxt (Repeated pType) =
    HsTyApp (primType_ "Vector") <$> hsTypeFromDotProtoPrim ctxt pType
hsTypeFromDotProto ctxt (NestedRepeated pType) =
    HsTyApp (primType_ "Vector") <$> hsTypeFromDotProtoPrim ctxt pType
hsTypeFromDotProto _    (Map _ _) =
    internalError "No support for protobuf mappings"

hsTypeFromDotProtoPrim :: TypeContext -> DotProtoPrimType -> CompileResult HsType
hsTypeFromDotProtoPrim _    Int32           = pure $ primType_ "Int32"
hsTypeFromDotProtoPrim _    Int64           = pure $ primType_ "Int64"
hsTypeFromDotProtoPrim _    SInt32          = pure $ primType_ "Int32"
hsTypeFromDotProtoPrim _    SInt64          = pure $ primType_ "Int64"
hsTypeFromDotProtoPrim _    UInt32          = pure $ primType_ "Word32"
hsTypeFromDotProtoPrim _    UInt64          = pure $ primType_ "Word64"
hsTypeFromDotProtoPrim _    Fixed32         = pure $ HsTyApp (primType_ "Fixed")
                                                             (primType_ "Word32")
hsTypeFromDotProtoPrim _    Fixed64         = pure $ HsTyApp (primType_ "Fixed")
                                                             (primType_ "Word64")
hsTypeFromDotProtoPrim _    SFixed32        = pure $ HsTyApp (primType_ "Fixed")
                                                             (primType_ "Int32")
hsTypeFromDotProtoPrim _    SFixed64        = pure $ HsTyApp (primType_ "Fixed")
                                                             (primType_ "Int64")
hsTypeFromDotProtoPrim _    String          = pure $ primType_ "Text"
hsTypeFromDotProtoPrim _    Bytes           = pure $ primType_ "ByteString"
hsTypeFromDotProtoPrim _    Bool            = pure $ primType_ "Bool"
hsTypeFromDotProtoPrim _    Float           = pure $ primType_ "Float"
hsTypeFromDotProtoPrim _    Double          = pure $ primType_ "Double"
hsTypeFromDotProtoPrim ctxt (Named msgName) =
    case M.lookup msgName ctxt of
      Just ty@(DotProtoTypeInfo { dotProtoTypeInfoKind = DotProtoKindEnum }) ->
          HsTyApp (primType_ "Enumerated") <$> msgTypeFromDpTypeInfo ty msgName
      Just ty -> msgTypeFromDpTypeInfo ty msgName
      Nothing -> noSuchTypeError msgName

-- | Generate the Haskell type name for a 'DotProtoTypeInfo' for a
--   message / enumeration being compiled
msgTypeFromDpTypeInfo :: DotProtoTypeInfo -> DotProtoIdentifier
                      -> CompileResult HsType
msgTypeFromDpTypeInfo DotProtoTypeInfo { dotProtoTypeInfoParent = parent
                                       , dotProtoTypeInfoPackage = DotProtoNoPackage }
                      ident =
    type_ <$> (nestedTypeName parent =<< dpIdentUnqualName ident)
msgTypeFromDpTypeInfo DotProtoTypeInfo { dotProtoTypeInfoParent = parent
                                       , dotProtoTypeInfoPackage = DotProtoPackageSpec pkg }
                      ident =
    HsTyCon <$> (Qual <$> pkgIdentModName pkg <*>
                     (HsIdent <$> (nestedTypeName parent =<<
                                       dpIdentUnqualName ident)))

-- | Given a 'DotProtoIdentifier' for the parent type and the unqualified name of this type, generate the corresponding Haskell name
nestedTypeName :: DotProtoIdentifier -> String -> CompileResult String
nestedTypeName Anonymous       nm = typeLikeName nm
nestedTypeName (Single parent) nm =
    intercalate "_" <$> sequenceA [ typeLikeName parent
                                  , typeLikeName nm ]
nestedTypeName (Path parents)  nm =
    (<> ("_" <> nm)) <$> (intercalate "_" <$> mapM typeLikeName parents)
nestedTypeName (Qualified {})  _  = internalError "nestedTypeName: Qualified"

dotProtoName :: String -> HsQName
dotProtoName name = Qual (Module "DotProto") (HsIdent name)

camelCased :: String -> String
camelCased s = do (prev, cur) <- zip (Nothing:map Just s) (map Just s ++ [Nothing])
                  case (prev, cur) of
                    (Just '_', Just x) | isAlpha x -> pure (toUpper x)
                    (Just '_', Nothing) -> pure '_'
                    (Just '_', Just '_') -> pure '_'
                    (_, Just '_') -> empty
                    (_, Just x) -> pure x
                    (_, _) -> empty

typeLikeName :: String -> CompileResult String
typeLikeName ident@(firstChar:remainingChars)
  | isUpper firstChar = pure (camelCased ident)
  | isLower firstChar = pure (camelCased (toUpper firstChar:remainingChars))
  | firstChar == '_'  = pure (camelCased ('X':ident))
typeLikeName ident = invalidTypeNameError ident

fieldLikeName :: String -> String
fieldLikeName ident@(firstChar:_)
  | isUpper firstChar = let (prefix, suffix) = span isUpper ident
                        in map toLower prefix ++ suffix
fieldLikeName ident = ident

prefixedConName :: String -> String -> CompileResult String
prefixedConName msgName conName =
  (msgName ++) <$> typeLikeName conName

prefixedFieldName :: String -> String -> CompileResult String
prefixedFieldName msgName fieldName =
  (fieldLikeName msgName ++) <$> typeLikeName fieldName

dpIdentUnqualName :: DotProtoIdentifier -> CompileResult String
dpIdentUnqualName (Single name)      = pure name
dpIdentUnqualName (Path   name)      = pure (last name)
dpIdentUnqualName (Qualified _ next) = dpIdentUnqualName next
dpIdentUnqualName Anonymous          = internalError "dpIdentUnqualName: Anonymous"

pkgIdentModName :: DotProtoIdentifier -> CompileResult Module
pkgIdentModName (Single s)   = Module <$> typeLikeName s
pkgIdentModName (Path paths) = Module <$> (intercalate "." <$>
                                               mapM typeLikeName paths)
pkgIdentModName _            = internalError "pkgIdentModName: Malformed package name"

-- * Generate instances for a 'DotProto' package

dotProtoDefinitionD :: TypeContext -> DotProtoDefinition -> CompileResult [HsDecl]
dotProtoDefinitionD ctxt (DotProtoMessage messageName dotProtoMessage) =
  dotProtoMessageD ctxt Anonymous messageName dotProtoMessage
dotProtoDefinitionD _ (DotProtoEnum messageName dotProtoEnum) =
  dotProtoEnumD Anonymous messageName dotProtoEnum
dotProtoDefinitionD _ (DotProtoService {}) = pure []
dotProtoDefinitionD _ DotProtoNullDef      = pure []

-- | Generate 'Named' instance for a type in this package
namedInstD :: String -> HsDecl
namedInstD messageName =
  instDecl_ (dotProtoName "Named")
      [ type_ messageName ]
      [ HsFunBind [nameOfDecl] ]
  where
    nameOfDecl = match_ (HsIdent "nameOf") [HsPWildCard]
                        (HsUnGuardedRhs (apply fromStringE
                                               [ HsLit (HsString messageName) ]))
                        []

-- ** Generate types and instances for .proto messages

-- | Generate data types, 'Message', 'Named', 'Enum', and 'Bounded'
--   instances as appropriate for the given 'DotProtoMessagePart's
dotProtoMessageD :: TypeContext -> DotProtoIdentifier -> DotProtoIdentifier
                 -> [DotProtoMessagePart] -> CompileResult [HsDecl]
dotProtoMessageD ctxt parentIdent messageIdent message =
    do messageName <- nestedTypeName parentIdent =<<
                      dpIdentUnqualName messageIdent

       let ctxt' = maybe mempty dotProtoTypeChildContext (M.lookup messageIdent ctxt) <>
                   ctxt

           messagePartFieldD (DotProtoMessageField (DotProtoField _ ty fieldName _)) =
               do fullName <- prefixedFieldName messageName =<<
                              dpIdentUnqualName fieldName
                  fullTy <- hsTypeFromDotProto ctxt' ty
                  pure [ ([HsIdent fullName], HsUnBangedTy fullTy ) ]
           messagePartFieldD (DotProtoMessageOneOf {}) =
             unimplementedError "oneof"
           messagePartFieldD _ = pure []

           nestedDecls :: DotProtoDefinition -> CompileResult [HsDecl]
           nestedDecls (DotProtoMessage subMsgName subMessageDef) =
               do parentIdent' <- concatDotProtoIdentifier parentIdent messageIdent
                  dotProtoMessageD ctxt' parentIdent' subMsgName subMessageDef
           nestedDecls (DotProtoEnum subEnumName subEnumDef) =
               do parentIdent' <- concatDotProtoIdentifier parentIdent messageIdent
                  dotProtoEnumD parentIdent' subEnumName subEnumDef
           nestedDecls _ = pure []

       conDecl <- recDecl_ (HsIdent messageName) . mconcat <$>
                  mapM messagePartFieldD message

       nestedDecls_ <- mconcat <$>
           sequence [ nestedDecls def | DotProtoMessageDefinition def <- message]

       messageInst <- messageInstD ctxt' parentIdent messageIdent message

       pure ([ dataDecl_ messageName [ conDecl ] defaultMessageDeriving
             , namedInstD messageName, messageInst ] <> nestedDecls_)


messageInstD :: TypeContext -> DotProtoIdentifier -> DotProtoIdentifier
             -> [DotProtoMessagePart] -> CompileResult HsDecl
messageInstD ctxt parentIdent msgIdent messageParts =
  do msgName <- nestedTypeName parentIdent =<<
                dpIdentUnqualName msgIdent

     qualifiedFields <-
       sequence [ dpIdentUnqualName fieldIdent >>=
                  prefixedFieldName msgName >>=
                  pure . (fieldNum, dpType, , options)
                | DotProtoMessageField (DotProtoField fieldNum dpType fieldIdent options)
                  <- messageParts ]

     encodeMessagePartEs <- sequence
         [ wrapE ctxt dpType options (HsVar (unqual_ fieldName)) >>= \fieldE ->
             pure (apply encodeMessageFieldE [ fieldNumberE fieldNum, fieldE ])
         | (fieldNum, dpType, fieldName, options) <- qualifiedFields ]

     decodeMessagePartEs <- sequence
         [ unwrapE ctxt dpType options $
           apply atE [ decodeMessageFieldE
                     , fieldNumberE fieldNum ]
         | (fieldNum, dpType, _, options) <- qualifiedFields ]

     dotProtoE <- HsList <$> sequence
         [ dpTypeE dpType >>= \typeE ->
             pure (apply dotProtoFieldC [ fieldNumberE fieldNum, typeE
                                        , dpIdentE fieldIdent
                                        , HsList (map optionE options) ])
         | DotProtoMessageField (DotProtoField fieldNum dpType fieldIdent options)
             <- messageParts ]

     let encodeMessageDecl = match_ (HsIdent "encodeMessage")
                                    [HsPWildCard, HsPRec (unqual_ msgName) punnedFieldsP]
                                    (HsUnGuardedRhs encodeMessageE) []
         decodeMessageDecl = match_ (HsIdent "decodeMessage") [ HsPWildCard ]
                                    (HsUnGuardedRhs decodeMessageE) []
         dotProtoDecl = match_ (HsIdent "dotProto") [HsPWildCard]
                               (HsUnGuardedRhs dotProtoE) []

         punnedFieldsP =
             [ HsPFieldPat (unqual_ fieldName) (HsPVar (HsIdent fieldName))
             | (_, _, fieldName, _) <- qualifiedFields ]

         encodeMessageE = apply mconcatE [ HsList encodeMessagePartEs ]
         decodeMessageE = foldl (\f -> HsInfixApp f apOp)
                                (apply pureE [ HsVar (unqual_ msgName) ])
                                decodeMessagePartEs

     pure (instDecl_ (dotProtoName "Message")
                     [ type_ msgName ]
                     (map (HsFunBind . (: []))
                      [ encodeMessageDecl
                      , decodeMessageDecl
                      , dotProtoDecl ]))

-- *** Helpers to wrap/unwrap types for protobuf (de-)serialization

wrapE, unwrapE :: TypeContext -> DotProtoType -> [DotProtoOption]
               -> HsExp -> CompileResult HsExp
wrapE ctxt (Prim ty)     _ = pure . wrapPrimE ctxt ty
wrapE ctxt (Optional ty) _ = pure . wrapPrimE ctxt ty
wrapE ctxt (Repeated (Named tyName)) _
    | Just DotProtoKindMessage <- dotProtoTypeInfoKind <$> M.lookup tyName ctxt
        = pure . wrapWithFuncE "NestedVec"
wrapE _ (Repeated ty)        opts
    | isUnpacked opts = pure . wrapWithFuncE "UnpackedVec" . wrapPrimVecE ty
    | isPacked opts || isPackableType ty = pure . wrapWithFuncE "PackedVec" .
                                           wrapPrimVecE ty
    | otherwise = pure . wrapWithFuncE "UnpackedVec" . wrapPrimVecE ty
wrapE _ _ _ = const (internalError "wrapE: unimplemented")

unwrapE ctxt (Prim ty)     _ = pure . unwrapPrimE ctxt  ty
unwrapE ctxt (Optional ty) _ = pure . unwrapPrimE ctxt ty
unwrapE ctxt (Repeated (Named tyName)) _
    | Just DotProtoKindMessage <- dotProtoTypeInfoKind <$> M.lookup tyName ctxt
        = pure . unwrapWithFuncE "nestedvec"
unwrapE _ (Repeated ty) opts
    | isUnpacked opts = pure . unwrapPrimVecE ty . unwrapWithFuncE "unpackedvec"
    | isPacked opts || isPackableType ty =  pure . unwrapPrimVecE ty .
                                            unwrapWithFuncE "packedvec"
    | otherwise = pure . unwrapPrimVecE ty . unwrapWithFuncE "unpackedvec"
unwrapE _ _ _ = const (internalError "unwrapE: unimplemented")

wrapPrimVecE, unwrapPrimVecE :: DotProtoPrimType -> HsExp -> HsExp
wrapPrimVecE SFixed32 = apply fmapE . (HsVar (dotProtoName "Signed"):) . (:[])
wrapPrimVecE SFixed64 = apply fmapE . (HsVar (dotProtoName "Signed"):) . (:[])
wrapPrimVecE _ = id

unwrapPrimVecE SFixed32 = HsParen .
    HsInfixApp (apply pureE [ apply fmapE [ HsVar (dotProtoName "signed") ] ]) apOp
unwrapPrimVecE SFixed64 = HsParen .
    HsInfixApp (apply pureE [ apply fmapE [ HsVar (dotProtoName "signed") ] ]) apOp
unwrapPrimVecE _ = id

wrapPrimE, unwrapPrimE :: TypeContext -> DotProtoPrimType -> HsExp -> HsExp
wrapPrimE ctxt (Named tyName)
    | Just DotProtoKindMessage <- dotProtoTypeInfoKind <$> M.lookup tyName ctxt
        = wrapWithFuncE "Nested"
wrapPrimE _ SFixed32 = wrapWithFuncE "Signed"
wrapPrimE _ SFixed64 = wrapWithFuncE "Signed"
wrapPrimE _ _ = id

unwrapPrimE ctxt (Named tyName)
    | Just DotProtoKindMessage <- dotProtoTypeInfoKind <$> M.lookup tyName ctxt
        = unwrapWithFuncE "nested"
unwrapPrimE _ SFixed32 = unwrapWithFuncE "signed"
unwrapPrimE _ SFixed64 = unwrapWithFuncE "signed"
unwrapPrimE _ _ = id

wrapWithFuncE, unwrapWithFuncE :: String -> HsExp -> HsExp
wrapWithFuncE wrappingFunc = HsParen . HsApp (HsVar (dotProtoName wrappingFunc))
unwrapWithFuncE unwrappingFunc = HsParen . HsInfixApp funcE apOp
  where funcE = HsParen (HsApp pureE (HsVar (dotProtoName unwrappingFunc)))

isPacked, isUnpacked :: [DotProtoOption] -> Bool
isPacked opts =
    case find (\(DotProtoOption name _) -> name == Single "packed") opts of
        Just (DotProtoOption _ (BoolLit x)) -> x
        _ -> False
isUnpacked opts =
    case find (\(DotProtoOption name _) -> name == Single "packed") opts of
        Just (DotProtoOption _ (BoolLit x)) -> not x
        _ -> False

internalError, invalidTypeNameError, unimplementedError
    :: String -> CompileResult a
internalError = Left . InternalError
invalidTypeNameError = Left . InvalidTypeName
unimplementedError = Left . Unimplemented

noSuchTypeError :: DotProtoIdentifier -> CompileResult a
noSuchTypeError = Left . NoSuchType

-- ** Generate types and instances for .proto enums

dotProtoEnumD :: DotProtoIdentifier -> DotProtoIdentifier -> [DotProtoEnumPart]
              -> CompileResult [HsDecl]
dotProtoEnumD parentIdent enumIdent enumParts =
  do enumName <- nestedTypeName parentIdent =<<
                 dpIdentUnqualName enumIdent

     enumCons <- sortBy (comparing fst) <$>
                 sequence [ (i,) <$> (prefixedConName enumName =<<
                                      dpIdentUnqualName conIdent)
                          | DotProtoEnumField conIdent i <- enumParts ]

     let enumNameE = HsLit (HsString enumName)
         enumConNames = map snd enumCons

         -- TODO assert that there is more than one enumeration constructor
         minEnumVal = fst (head enumCons)
         maxEnumVal = fst (last enumCons)
         boundsE = HsTuple [intE minEnumVal, intE maxEnumVal]

         toEnumD = toEnumDPatterns <> [ toEnumFailure ]
         fromEnumD =
             [ match_ (HsIdent "fromEnum") [ HsPApp (unqual_ conName) [] ]
                      (HsUnGuardedRhs (intE conIdx)) []
             | (conIdx, conName) <- enumCons ]
         succD = zipWith succDPattern enumConNames (tail enumConNames) <> [ succFailure]
         predD = zipWith predDPattern (tail enumConNames) enumConNames <> [ predFailure ]

         toEnumDPatterns =
             [ match_ (HsIdent "toEnum")
                      [ HsPLit (HsInt (fromIntegral conIdx)) ]
                      (HsUnGuardedRhs (HsVar (unqual_ conName))) []
             | (conIdx, conName) <- enumCons ]

         succDPattern thisCon nextCon =
             match_ (HsIdent "succ") [ HsPApp (unqual_ thisCon) [] ]
                    (HsUnGuardedRhs (HsVar (unqual_ nextCon))) []
         predDPattern thisCon prevCon =
             match_ (HsIdent "pred") [ HsPApp (unqual_ thisCon) [] ]
                    (HsUnGuardedRhs (HsVar (unqual_ prevCon))) []

         toEnumFailure   = match_ (HsIdent "toEnum") [ HsPVar (HsIdent "i") ]
                                  (HsUnGuardedRhs (apply toEnumErrorE
                                                          [enumNameE
                                                          , HsVar (unqual_ "i")
                                                          , boundsE])) []
         succFailure     = match_ (HsIdent "succ") [ HsPWildCard ]
                                  (HsUnGuardedRhs (HsApp succErrorE enumNameE)) []
         predFailure     = match_ (HsIdent "pred") [ HsPWildCard ]
                                  (HsUnGuardedRhs (HsApp predErrorE enumNameE)) []

     pure [ dataDecl_ enumName [ conDecl_ (HsIdent con) []
                               | (_, con) <- enumCons] defaultEnumDeriving
          , namedInstD enumName
          , instDecl_ (dotProtoName "Enum") [ type_ enumName ]
                      [ HsFunBind toEnumD, HsFunBind fromEnumD
                      , HsFunBind succD, HsFunBind predD ] ]

-- * Common Haskell expressions, constructors, and operators

dotProtoFieldC, primC, optionalC, repeatedC, nestedRepeatedC, namedC,
  fieldNumberC, singleC, pathC, nestedC, anonymousC, dotProtoOptionC,
  identifierC, stringLitC, intLitC, floatLitC, boolLitC, trueC, falseC,
  mconcatE, encodeMessageFieldE, fromStringE, decodeMessageFieldE, pureE, atE,
  succErrorE, predErrorE, toEnumErrorE, fmapE :: HsExp
dotProtoFieldC        = HsVar (dotProtoName "DotProtoField")
primC                 = HsVar (dotProtoName "Prim")
optionalC             = HsVar (dotProtoName "Optional")
repeatedC             = HsVar (dotProtoName "Repeated")
nestedRepeatedC       = HsVar (dotProtoName "NestedRepeated")
namedC                = HsVar (dotProtoName "Named")
fieldNumberC          = HsVar (dotProtoName "FieldNumber")
singleC               = HsVar (dotProtoName "Single")
pathC                 = HsVar (dotProtoName "Path")
nestedC               = HsVar (dotProtoName "Nested")
anonymousC            = HsVar (dotProtoName "Anonymous")
dotProtoOptionC       = HsVar (dotProtoName "DotProtoOption")
identifierC           = HsVar (dotProtoName "Identifier")
stringLitC            = HsVar (dotProtoName "StringLit")
intLitC               = HsVar (dotProtoName "IntLit")
floatLitC             = HsVar (dotProtoName "FloatLit")
boolLitC              = HsVar (dotProtoName "BoolLit")
trueC                 = HsVar (dotProtoName "True")
falseC                = HsVar (dotProtoName "False")

mconcatE              = HsVar (dotProtoName "mconcat")
encodeMessageFieldE   = HsVar (dotProtoName "encodeMessageField")
decodeMessageFieldE   = HsVar (dotProtoName "decodeMessageField")
fromStringE           = HsVar (dotProtoName "fromString")
pureE                 = HsVar (dotProtoName "pure")
atE                   = HsVar (dotProtoName "at")
succErrorE            = HsVar (dotProtoName "succError")
predErrorE            = HsVar (dotProtoName "predError")
toEnumErrorE          = HsVar (dotProtoName "toEnumError")
fmapE                 = HsVar (dotProtoName "fmap")

apOp :: HsQOp
apOp  = HsQVarOp (UnQual (HsSymbol "<*>"))

intE :: Integral a => a -> HsExp
intE = HsLit . HsInt . fromIntegral

-- ** Expressions for protobuf-wire types

fieldNumberE :: FieldNumber -> HsExp
fieldNumberE = HsParen . HsApp fieldNumberC . intE . getFieldNumber

dpIdentE :: DotProtoIdentifier -> HsExp
dpIdentE (Single n)      = apply singleC [ HsLit (HsString n) ]
dpIdentE (Path ns)       = apply pathC   [ HsList (map (HsLit . HsString) ns) ]
dpIdentE (Qualified a b) = apply nestedC [ dpIdentE a, dpIdentE b ]
dpIdentE Anonymous       = anonymousC

dpValueE :: DotProtoValue -> HsExp
dpValueE (Identifier nm) = apply identifierC [ dpIdentE nm ]
dpValueE (StringLit s)   = apply stringLitC  [ HsLit (HsString s) ]
dpValueE (IntLit i)      = apply intLitC     [ HsLit (HsInt (fromIntegral i)) ]
dpValueE (FloatLit f)    = apply floatLitC   [ HsLit (HsFrac (toRational f)) ]
dpValueE (BoolLit True)  = apply boolLitC    [ trueC ]
dpValueE (BoolLit False) = apply boolLitC    [ falseC ]

optionE :: DotProtoOption -> HsExp
optionE (DotProtoOption name value) = apply dotProtoOptionC [ dpIdentE name
                                                            , dpValueE value ]

dpTypeE :: DotProtoType -> CompileResult HsExp
dpTypeE (Prim p)           = pure (apply primC           [ dpPrimTypeE p ])
dpTypeE (Optional p)       = pure (apply optionalC       [ dpPrimTypeE p ])
dpTypeE (Repeated p)       = pure (apply repeatedC       [ dpPrimTypeE p ])
dpTypeE (NestedRepeated p) = pure (apply nestedRepeatedC [ dpPrimTypeE p ])
dpTypeE (Map _ _)          = internalError "dpTypeE: Map"

dpPrimTypeE :: DotProtoPrimType -> HsExp
dpPrimTypeE (Named named) = apply namedC [ dpIdentE named ]
dpPrimTypeE ty            =
    HsVar . dotProtoName $
    case ty of
        Int32    -> "Int32"
        Int64    -> "Int64"
        SInt32   -> "SInt32"
        SInt64   -> "SInt64"
        UInt32   -> "UInt32"
        UInt64   -> "UInt64"
        Fixed32  -> "Fixed32"
        Fixed64  -> "Fixed64"
        SFixed32 -> "SFixed32"
        SFixed64 -> "SFixed64"
        String   -> "String"
        Bytes    -> "Bytes"
        Bool     -> "Bool"
        Float    -> "Float"
        Double   -> "Double"

        -- 'error' okay because this is literally impossible
        Named _  -> error "dpPrimTypeE: impossible"

defaultImports :: [HsImportDecl]
defaultImports =
  [ importDecl_ preludeM                  True  (Just dotProto) Nothing
  , importDecl_ dataProtobufWireDotProtoM True  (Just dotProto) Nothing
  , importDecl_ dataProtobufWireTypesM    True  (Just dotProto) Nothing
  , importDecl_ proto3WireM               True  (Just dotProto) Nothing
  , importDecl_ dataProtobufWireClassM    True  (Just dotProto) Nothing
  , importDecl_ controlApplicativeM       False Nothing
                (Just (False, [ HsIAbs (HsSymbol "<*>")
                              , HsIAbs (HsSymbol "<|>") ]))
  , importDecl_ dataTextM                 True
                (Just dotProto) (Just (False, [ importSym "Text" ]))
  , importDecl_ dataByteStringM           True  (Just dotProto) Nothing
  , importDecl_ dataStringM               True  (Just dotProto)
                (Just (False, [ importSym "fromString" ]))
  , importDecl_ dataVectorM               True  (Just dotProto)
                (Just (False, [ importSym "Vector" ]))
  , importDecl_ dataIntM                  True  (Just dotProto)
                (Just (False, [ importSym "Int16", importSym "Int32"
                              , importSym "Int64" ]))
  , importDecl_ dataWordM                 True  (Just dotProto)
                (Just (False, [ importSym "Word16", importSym "Word32"
                              , importSym "Word64" ]))
  , importDecl_ ghcGenericsM              False (Just dotProto) Nothing
  , importDecl_ ghcEnumM                  False (Just dotProto) Nothing
  ]
  where preludeM                  = Module "Prelude"
        dataProtobufWireDotProtoM = Module "Data.Protobuf.Wire.DotProto"
        dataProtobufWireClassM    = Module "Data.Protobuf.Wire.Class"
        dataProtobufWireTypesM    = Module "Data.Protobuf.Wire.Types"
        proto3WireM               = Module "Proto3.Wire"
        controlApplicativeM       = Module "Control.Applicative"
        dataTextM                 = Module "Data.Text"
        dataByteStringM           = Module "Data.ByteString"
        dataStringM               = Module "Data.String"
        dataIntM                  = Module "Data.Int"
        dataVectorM               = Module "Data.Vector"
        dataWordM                 = Module "Data.Word"
        ghcGenericsM              = Module "GHC.Generics"
        ghcEnumM                  = Module "GHC.Enum"
        dotProto                  = Module "DotProto"

        importSym = HsIAbs . HsIdent

defaultMessageDeriving :: [HsQName]
defaultMessageDeriving = map dotProtoName [ "Show"
                                          , "Eq",   "Ord"
                                          , "Generic" ]

defaultEnumDeriving :: [HsQName]
defaultEnumDeriving = map dotProtoName [ "Show", "Bounded"
                                       , "Eq",   "Ord"
                                       , "Generic" ]

-- * Wrappers around haskell-src-exts constructors

apply :: HsExp -> [HsExp] -> HsExp
apply f = HsParen . foldl HsApp f

module_ :: Module -> Maybe [HsExportSpec] -> [HsImportDecl] -> [HsDecl]
        -> HsModule
module_ = HsModule l

importDecl_ :: Module -> Bool -> Maybe Module
            -> Maybe (Bool, [HsImportSpec]) -> HsImportDecl
importDecl_ = HsImportDecl l

dataDecl_ :: String -> [HsConDecl] -> [HsQName] -> HsDecl
dataDecl_ messageName = HsDataDecl l [] (HsIdent messageName) []

recDecl_ :: HsName -> [([HsName], HsBangType)] -> HsConDecl
recDecl_ = HsRecDecl l

conDecl_ :: HsName -> [HsBangType] -> HsConDecl
conDecl_ = HsConDecl l

instDecl_ :: HsQName -> [HsType] -> [HsDecl] -> HsDecl
instDecl_ = HsInstDecl l []

match_ :: HsName -> [HsPat] -> HsRhs -> [HsDecl] -> HsMatch
match_ = HsMatch l

unqual_ :: String -> HsQName
unqual_ = UnQual . HsIdent

primType_ :: String -> HsType
primType_ = HsTyCon . dotProtoName

type_ :: String -> HsType
type_ = HsTyCon . unqual_

-- | For some reason, haskell-src-exts needs this 'SrcLoc' parameter
--   for some data constructors. Its value does not affect
--   pretty-printed output
l :: SrcLoc
l = SrcLoc "<generated>" 0 0
