{-| Utilities to manipulate Haskell AST -}
module Proto3.Suite.DotProto.Generate.Syntax where

import Language.Haskell.Syntax

haskellName, jsonpbName, grpcName, lrName, protobufName, protobufASTName, proxyName :: String -> HsQName
haskellName  name = Qual (Module "Hs")         (HsIdent name)
jsonpbName   name = Qual (Module "HsJSONPB")   (HsIdent name)
grpcName     name = Qual (Module "HsGRPC")     (HsIdent name)
lrName       name = Qual (Module "LR")         (HsIdent name)
protobufName name = Qual (Module "HsProtobuf") (HsIdent name)
protobufASTName name = Qual (Module "HsProtobufAST") (HsIdent name)
proxyName    name = Qual (Module "Proxy")      (HsIdent name)

haskellNS :: Module
haskellNS = Module "Hs"

--------------------------------------------------------------------------------
--
-- * Wrappers around haskell-src-exts constructors
--

apply :: HsExp -> [HsExp] -> HsExp
apply f = paren . foldl HsApp f

maybeModify :: HsExp -> Maybe HsExp -> HsExp
maybeModify x Nothing = x
maybeModify x (Just f) = paren (HsApp f (paren x))

paren :: HsExp -> HsExp
paren e@(HsParen _) = e
paren e = HsParen e

applicativeApply :: HsExp -> [HsExp] -> HsExp
applicativeApply f = foldl snoc nil
  where
    nil = HsApp pureE f

    snoc g x = HsInfixApp g apOp x

tyApp :: HsType -> [HsType] -> HsType
tyApp = foldl HsTyApp

module_ :: Module -> Maybe [HsExportSpec] -> [HsImportDecl] -> [HsDecl] -> HsModule
module_ = HsModule defaultSrcLoc

importDecl_ :: Module -> Bool -> Maybe Module -> Maybe (Bool, [HsImportSpec]) -> HsImportDecl
importDecl_ = HsImportDecl defaultSrcLoc

dataDecl_ :: String -> [HsConDecl] -> [HsQName] -> HsDecl
dataDecl_ messageName [constructor@(HsRecDecl _ _ [_])] =
  HsNewTypeDecl defaultSrcLoc [] (HsIdent messageName) [] constructor
dataDecl_ messageName constructors =
  HsDataDecl defaultSrcLoc [] (HsIdent messageName) [] constructors

recDecl_ :: HsName -> [([HsName], HsBangType)] -> HsConDecl
recDecl_ = HsRecDecl defaultSrcLoc

conDecl_ :: HsName -> [HsBangType] -> HsConDecl
conDecl_ = HsConDecl defaultSrcLoc

instDecl_ :: HsQName -> [HsType] -> [HsDecl] -> HsDecl
instDecl_ = HsInstDecl defaultSrcLoc []

match_ :: HsName -> [HsPat] -> HsRhs -> [HsDecl] -> HsMatch
match_ = HsMatch defaultSrcLoc

unqual_ :: String -> HsQName
unqual_ = UnQual . HsIdent

uvar_ :: String -> HsExp
uvar_ = HsVar . unqual_

protobufType_, primType_, protobufStringType_, protobufBytesType_ :: String -> HsType
protobufType_ = HsTyCon . protobufName
primType_ = HsTyCon . haskellName
protobufStringType_ = HsTyApp (protobufType_ "String") . HsTyCon . haskellName
protobufBytesType_ = HsTyApp (protobufType_ "Bytes") . HsTyCon . haskellName

protobufFixedType_, protobufSignedType_, protobufWrappedType_ :: HsType -> HsType
protobufFixedType_ = HsTyApp (protobufType_ "Fixed")
protobufSignedType_ = HsTyApp (protobufType_ "Signed")
protobufWrappedType_ = HsTyApp (HsTyCon (protobufName "Wrapped"))

type_ :: String -> HsType
type_ = HsTyCon . unqual_

patVar :: String -> HsPat
patVar =  HsPVar . HsIdent

alt_ :: HsPat -> HsGuardedAlts -> [HsDecl] -> HsAlt
alt_ = HsAlt defaultSrcLoc

str_ :: String -> HsExp
str_ = HsLit . HsString

-- | For some reason, haskell-src-exts needs this 'SrcLoc' parameter
--   for some data constructors. Its value does not affect
--   pretty-printed output
defaultSrcLoc :: SrcLoc
defaultSrcLoc = SrcLoc "<generated>" 0 0

--------------------------------------------------------------------------------
--
-- * Common Haskell expressions, constructors, and operators
--

dotProtoFieldC, primC, repeatedC, nestedRepeatedC, namedC, mapC,
  fieldNumberC, singleC, dotsC, pathC, qualifiedC, anonymousC, dotProtoOptionC,
  identifierC, stringLitC, intLitC, floatLitC, boolLitC, trueC, falseC,
  nothingC, justC, forceEmitC, mconcatE, encodeMessageFieldE,
  fromStringE, decodeMessageFieldE, pureE, returnE, memptyE, msumE, atE, oneofE,
  fmapE :: HsExp

dotProtoFieldC       = HsVar (protobufASTName "DotProtoField")
primC                = HsVar (protobufASTName "Prim")
repeatedC            = HsVar (protobufASTName "Repeated")
nestedRepeatedC      = HsVar (protobufASTName "NestedRepeated")
namedC               = HsVar (protobufASTName "Named")
mapC                 = HsVar (protobufASTName "Map")
fieldNumberC         = HsVar (protobufName "FieldNumber")
singleC              = HsVar (protobufASTName "Single")
pathC                = HsVar (protobufASTName "Path")
dotsC                = HsVar (protobufASTName "Dots")
qualifiedC           = HsVar (protobufASTName "Qualified")
anonymousC           = HsVar (protobufASTName "Anonymous")
dotProtoOptionC      = HsVar (protobufASTName "DotProtoOption")
identifierC          = HsVar (protobufASTName "Identifier")
stringLitC           = HsVar (protobufASTName "StringLit")
intLitC              = HsVar (protobufASTName "IntLit")
floatLitC            = HsVar (protobufASTName "FloatLit")
boolLitC             = HsVar (protobufASTName "BoolLit")
forceEmitC           = HsVar (protobufName "ForceEmit")
encodeMessageFieldE  = HsVar (protobufName "encodeMessageField")
decodeMessageFieldE  = HsVar (protobufName "decodeMessageField")
atE                  = HsVar (protobufName "at")
oneofE               = HsVar (protobufName "oneof")

trueC                = HsVar (haskellName "True")
falseC               = HsVar (haskellName "False")
nothingC             = HsVar (haskellName "Nothing")
justC                = HsVar (haskellName "Just")
mconcatE             = HsVar (haskellName "mconcat")
fromStringE          = HsVar (haskellName "fromString")
pureE                = HsVar (haskellName "pure")
returnE              = HsVar (haskellName "return")
memptyE              = HsVar (haskellName "mempty")
msumE                = HsVar (haskellName "msum")
fmapE                = HsVar (haskellName "fmap")

apOp :: HsQOp
apOp  = HsQVarOp (UnQual (HsSymbol "<*>"))

fmapOp :: HsQOp
fmapOp  = HsQVarOp (UnQual (HsSymbol "<$>"))

composeOp :: HsQOp
composeOp = HsQVarOp (Qual haskellNS (HsSymbol "."))

bindOp :: HsQOp
bindOp = HsQVarOp (Qual haskellNS (HsSymbol ">>="))

altOp :: HsQOp
altOp = HsQVarOp (UnQual (HsSymbol "<|>"))

toJSONPBOp :: HsQOp
toJSONPBOp = HsQVarOp (UnQual (HsSymbol ".="))

parseJSONPBOp :: HsQOp
parseJSONPBOp = HsQVarOp (UnQual (HsSymbol ".:"))

neConsOp :: HsQOp
neConsOp = HsQVarOp (Qual haskellNS (HsSymbol ":|"))

intE :: Integral a => a -> HsExp
intE x = (if x < 0 then HsParen else id) . HsLit . HsInt . fromIntegral $ x

intP :: Integral a => a -> HsPat
intP x = (if x < 0 then HsPParen else id) . HsPLit . HsInt . fromIntegral $ x
