-- | This module contains a near-direct translation of the proto3 grammar
--   It uses String for easier compatibility with DotProto.Generator, which needs it for not very good reasons

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Proto3.Suite.DotProto.Parsing
  ( ProtoParser
  , runProtoParser
  , parseProto
  , parseProtoWithFile
  , parseProtoFile

    -- * Option Parsers
  , pOptionStmt
  , pFieldOptions
  , pFieldOptionStmt
  , pOptionId
  , pOptionKw

    -- * Extension Parsers
  , pExtendStmt
  , pExtendKw

    -- * Empty Statement
  , pEmptyStmt
  ) where

import Prelude hiding (fail)
import Control.Applicative hiding (empty)
import Control.Monad hiding (fail)
#if MIN_VERSION_base(4,13,0)
import Control.Monad (fail)
#endif
#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail
#endif
import qualified Data.Char as Char
import Data.Maybe (catMaybes)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Functor
import qualified Data.Text as T
import Proto3.Suite.DotProto.AST
import Proto3.Wire.Types (FieldNumber(..))
import Text.Parsec (parse, ParseError)
import Text.Parsec.String (Parser)
import Text.Parser.Char hiding (digit, hexDigit)
import Text.Parser.Combinators
import Text.Parser.LookAhead
import Text.Parser.Token
import qualified Text.Parser.Token.Style as TokenStyle
import qualified Turtle hiding (encodeString, fromText)
import qualified Turtle.Compat as Turtle (encodeString, fromText)

----------------------------------------
-- interfaces

-- | @parseProto mp inp@ attempts to parse @inp@ as a 'DotProto'. @mp@ is the
-- module path to be injected into the AST as part of 'DotProtoMeta' metadata on
-- a successful parse.
parseProto :: Path -> String -> Either ParseError DotProto
parseProto modulePath = parseProtoWithFile modulePath ""

-- | Parse a protobuf source string with an associated module path and file 
-- path. 
parseProtoWithFile ::
  -- | The module path to set in 'DotProtoMeta'.
  Path ->
  -- | The 'FilePath' the input protobuf string was read from.
  FilePath ->
  -- | The input protobuf 'String' to parse.
  String ->
  -- | Returns either a 'ParseError' or a 'DotProto'.
  Either ParseError DotProto
parseProtoWithFile modulePath filePath = parse (runProtoParser (topLevel modulePath)) filePath

-- | @parseProtoFile mp fp@ reads and parses the .proto file found at @fp@. @mp@
-- is used downstream during code generation when we need to generate names
-- which are a function of the source .proto file's filename and its path
-- relative to some @--includeDir@.
parseProtoFile :: Turtle.MonadIO m
               => Path -> Turtle.FilePath -> m (Either ParseError DotProto)
parseProtoFile modulePath (Turtle.encodeString -> fp) =
  parseProtoWithFile modulePath fp <$> Turtle.liftIO (readFile fp)

----------------------------------------
-- convenience

-- | Wrapper around @Text.Parsec.String.Parser@, overriding whitespace lexing.
newtype ProtoParser a = ProtoParser { runProtoParser :: Parser a }
  deriving ( Functor, Applicative, Alternative, Monad, MonadFail, MonadPlus
           , Parsing, CharParsing, LookAheadParsing)

instance TokenParsing ProtoParser where
  someSpace = TokenStyle.buildSomeSpaceParser
                (ProtoParser someSpace)
                TokenStyle.javaCommentStyle
  -- use the default implementation for other methods:
  -- nesting, semi, highlight, token

empty :: ProtoParser ()
empty = textSymbol ";" >> return ()

fieldNumber :: ProtoParser FieldNumber
fieldNumber = FieldNumber . fromInteger <$> integer

----------------------------------------
-- identifiers

identifierName :: ProtoParser String
identifierName = do h <- letter
                    t <- many (alphaNum <|> char '_')
                    return $ h:t

-- Parses a full identifier, without consuming trailing space.
_identifier :: ProtoParser DotProtoIdentifier
_identifier = identifierName `sepBy1` string "." >>= \case
                []  -> fail "impossible"
                [i] -> pure (Single i)
                (i:is) -> pure (Dots (Path (i NE.:| is)))

singleIdentifier :: ProtoParser DotProtoIdentifier
singleIdentifier = Single <$> token identifierName

-- Parses a full identifier, consuming trailing space.
identifier :: ProtoParser DotProtoIdentifier
identifier = token _identifier

-- Parses a full identifier, consuming trailing space.
-- The leading dot denotes that the identifier path starts in global scope.
globalIdentifier :: ProtoParser DotProtoIdentifier
globalIdentifier = token $ string "." >> _identifier

----------------------------------------
-- values

strLit :: ProtoParser String
strLit = doubleQuotedLiteral <|> singleQuotedLiteral
  where
    doubleQuotedLiteral =
        between (char '"') (char '"') (many character)
      where
        character =
            escape <|> satisfy (\c -> c `notElem` [ '\0', '\n', '\\', '"' ])

    singleQuotedLiteral =
        between (char '\'') (char '\'') (many character)
      where
        character =
            escape <|> satisfy (\c -> c `notElem` [ '\0', '\n', '\\', '\'' ])

    escape = do
        char '\\'
        hexEscape <|> octEscape <|> charEscape

    hexEscape = do
        (char 'x' <|> char 'X')
        digit0 <- hexDigit
        digit1 <- hexDigit
        let number = 16 * digit0 + digit1
        return (Char.chr number)

    octEscape = do
        digit0 <- octalDigit
        digit1 <- octalDigit
        digit2 <- octalDigit
        let number = 64 * digit0 + 8 * digit1 + digit2
        return (Char.chr number)

    charEscape =
            '\a' <$ char 'a'
        <|> '\b' <$ char 'b'
        <|> '\f' <$ char 'f'
        <|> '\n' <$ char 'n'
        <|> '\r' <$ char 'r'
        <|> '\t' <$ char 't'
        <|> '\v' <$ char 'v'
        <|> '\\' <$ char '\\'
        <|> '\'' <$ char '\''
        <|> '"'  <$ char '"'

digit :: ProtoParser Int
digit = do
    c <- satisfy (\c -> '0' <= c && c <= '9')
    return (Char.ord c - Char.ord '0')

hexDigit :: ProtoParser Int
hexDigit = digit <|> lowercase <|> uppercase
  where
    lowercase = do
        c <- satisfy (\c -> 'a' <= c && c <= 'f')
        return (Char.ord c - Char.ord 'a')

    uppercase = do
        c <- satisfy (\c -> 'A' <= c && c <= 'F')
        return (Char.ord c - Char.ord 'A')

octalDigit :: ProtoParser Int
octalDigit = do
    c <- satisfy (\c -> '0' <= c && c <= '7')
    return (Char.ord c - Char.ord '0')

bool :: ProtoParser Bool
bool = token $ lit "true" True <|> lit "false" False
  where
    -- used to distinguish "true_" (Identifier) from "true" (BoolLit)
    lit s c = string s >> notFollowedBy (alphaNum <|> char '_') >> pure c

-- the `parsers` package actually does not expose a parser for signed fractional values
floatLit :: ProtoParser Double
floatLit = do sign <- char '-' $> negate <|> char '+' $> id <|> pure id
              sign <$> double

value :: ProtoParser DotProtoValue
value = try (BoolLit              <$> bool)
    <|> try (StringLit            <$> strLit)
    <|> try (FloatLit             <$> floatLit)
    <|> try (IntLit . fromInteger <$> integer)
    <|> try (Identifier           <$> identifier)

----------------------------------------
-- types

primType :: ProtoParser DotProtoPrimType
primType = try (symbol "double"   $> Double)
       <|> try (symbol "float"    $> Float)
       <|> try (symbol "int32"    $> Int32)
       <|> try (symbol "int64"    $> Int64)
       <|> try (symbol "sint32"   $> SInt32)
       <|> try (symbol "sint64"   $> SInt64)
       <|> try (symbol "uint32"   $> UInt32)
       <|> try (symbol "uint64"   $> UInt64)
       <|> try (symbol "fixed32"  $> Fixed32)
       <|> try (symbol "fixed64"  $> Fixed64)
       <|> try (symbol "sfixed32" $> SFixed32)
       <|> try (symbol "sfixed64" $> SFixed64)
       <|> try (symbol "string"   $> String)
       <|> try (symbol "bytes"    $> Bytes)
       <|> try (symbol "bool"     $> Bool)
       <|> Named <$> (identifier <|> globalIdentifier)

--------------------------------------------------------------------------------
-- top-level parser and version annotation

syntaxSpec :: ProtoParser ()
syntaxSpec = void $ do
  symbol "syntax"
  symbol "="
  symbol "'proto3'" <|> symbol "\"proto3\""
  semi

data DotProtoStatement
  = DPSOption     DotProtoOption
  | DPSPackage    DotProtoPackageSpec
  | DPSImport     DotProtoImport
  | DPSDefinition DotProtoDefinition
  | DPSEmpty
  deriving Show

sortStatements :: Path -> [DotProtoStatement] -> DotProto
sortStatements modulePath statements
  = DotProto { protoOptions     =       [ x | DPSOption     x <- statements]
             , protoImports     =       [ x | DPSImport     x <- statements]
             , protoPackage     = adapt [ x | DPSPackage    x <- statements]
             , protoDefinitions =       [ x | DPSDefinition x <- statements]
             , protoMeta        = DotProtoMeta modulePath
             }
  where
    adapt (x:_) = x
    adapt _     = DotProtoNoPackage

topLevel :: Path -> ProtoParser DotProto
topLevel modulePath = do
  whiteSpace
  syntaxSpec
  dotProto <- sortStatements modulePath <$> many topStatement
  eof
  return dotProto

--------------------------------------------------------------------------------
-- top-level statements

topStatement :: ProtoParser DotProtoStatement
topStatement =
  choice
    [ DPSImport <$> import_
    , DPSPackage <$> package
    , DPSOption <$> pOptionStmt
    , DPSEmpty <$ try pExtendStmt
    , DPSDefinition <$> definition
    , DPSEmpty <$ empty
    ]

import_ :: ProtoParser DotProtoImport
import_ = do symbol "import"
             qualifier <- option DotProtoImportDefault $
                                 symbol "weak" $> DotProtoImportWeak
                             <|> symbol "public" $> DotProtoImportPublic
             target <- Turtle.fromText . T.pack <$> strLit
             semi
             return $ DotProtoImport qualifier target

package :: ProtoParser DotProtoPackageSpec
package = do symbol "package"
             p <- identifier
             semi
             return $ DotProtoPackageSpec p

definition :: ProtoParser DotProtoDefinition
definition =
  choice
    [ try message
    , try pEnumDefn
    , service
    ]

--------------------------------------------------------------------------------
-- options

-- | Parses a protobuf option that could appear in a service, RPC, message,
-- enumeration, or at the top-level.
--
-- @since 0.5.2
pOptionStmt :: ProtoParser DotProtoOption
pOptionStmt = token (between pOptionKw (token semi) pFieldOptionStmt)

-- | Parses zero or more message field options enclosed in square braces.
--
-- @since 0.5.2
pFieldOptions :: ProtoParser [DotProtoOption]
pFieldOptions = pOptions <|> pure []
  where
    pOptions :: ProtoParser [DotProtoOption]
    pOptions = between lbracket rbracket (commaSep1 (token pFieldOptionStmt))

    lbracket :: ProtoParser ()
    lbracket = token (char '[' $> ()) <?> "left bracket"

    rbracket :: ProtoParser ()
    rbracket = token (char ']' $> ()) <?> "right bracket"

-- | Parses a protobuf option in the context of a message field's options.
--
-- @since 0.5.2
pFieldOptionStmt :: ProtoParser DotProtoOption
pFieldOptionStmt = token $ do
  idt <- pOptionId
  token (char '=')
  val <- value
  pure (DotProtoOption idt val)

-- | Parses a (qualified) identifier for a protobuf option.
--
-- @since 0.5.2
pOptionId :: ProtoParser DotProtoIdentifier
pOptionId =
  choice
    [ try pOptionQName
    , try (parens pOptionName)
    , pOptionName
    ]
  where
    pOptionName :: ProtoParser DotProtoIdentifier
    pOptionName = token $ do
      nm <- identifierName
      nms <- many (char '.' *> identifierName)
      if null nms
        then pure (Single nm)
        else pure (Dots (Path (nm :| nms)))

    pOptionQName :: ProtoParser DotProtoIdentifier
    pOptionQName = token $ do
      idt <- parens pOptionName
      nms <- char '.' *> pOptionName
      pure (Qualified idt nms)

-- | Parses a single keyword token "option".
--
-- @since 0.5.2
pOptionKw :: ProtoParser ()
pOptionKw = do
  spaces
  token (string "option" >> notFollowedBy alphaNum)
    <?> "keyword 'option'"

--------------------------------------------------------------------------------
-- service statements

servicePart :: ProtoParser (Maybe DotProtoServicePart)
servicePart =
  try (fmap (Just . DotProtoServiceRPCMethod) rpc)
    <|> try (fmap (Just . DotProtoServiceOption) pOptionStmt)
    <|> Nothing <$ pEmptyStmt

rpcOptions :: ProtoParser [DotProtoOption]
rpcOptions = braces $ many pOptionStmt

rpcClause :: ProtoParser (DotProtoIdentifier, Streaming)
rpcClause = do
  let sid ctx = (,ctx) <$> (identifier <|> globalIdentifier)
  -- NB: Distinguish "stream stream.foo" from "stream.foo"
  try (symbol "stream" *> sid Streaming) <|> sid NonStreaming

rpc :: ProtoParser RPCMethod
rpc = do symbol "rpc"
         rpcMethodName <- singleIdentifier
         (rpcMethodRequestType, rpcMethodRequestStreaming) <- parens rpcClause
         symbol "returns"
         (rpcMethodResponseType, rpcMethodResponseStreaming) <- parens rpcClause
         rpcMethodOptions <- rpcOptions <|> (semi $> [])
         return RPCMethod{..}

service :: ProtoParser DotProtoDefinition
service = do
  symbol "service"
  name <- singleIdentifier
  statements <- braces do
    results <- many servicePart
    pure (catMaybes results)
  return $ DotProtoService mempty name statements

--------------------------------------------------------------------------------
-- message definitions

message :: ProtoParser DotProtoDefinition
message = do
  symbol "message"
  name <- singleIdentifier
  body <- braces do
    results <- many messagePart
    pure (catMaybes results)
  pure (DotProtoMessage mempty name body)

messageOneOf :: ProtoParser DotProtoMessagePart
messageOneOf = do symbol "oneof"
                  name <- singleIdentifier
                  body <- braces (many oneofField)
                  return $ DotProtoMessageOneOf name body

messagePart :: ProtoParser (Maybe DotProtoMessagePart)
messagePart =
  try (Just . DotProtoMessageDefinition <$> pEnumDefn)
    <|> try (Just . DotProtoMessageReserved   <$> pReserved)
    <|> try (Just . DotProtoMessageDefinition <$> message)
    <|> try (fmap Just messageOneOf)
    <|> try (Just . DotProtoMessageField      <$> messageField)
    <|> try (Just . DotProtoMessageOption     <$> pOptionStmt)
    <|> (Nothing <$ pEmptyStmt)

messageType :: ProtoParser DotProtoType
messageType = try mapType <|> try optType <|> try repType <|> (Prim <$> primType)
  where
    mapType = do symbol "map"
                 angles $ Map <$> (primType <* comma)
                              <*> primType

    optType = do symbol "optional"
                 Optional <$> primType

    repType = do symbol "repeated"
                 Repeated <$> primType

messageField :: ProtoParser DotProtoField
messageField = messageFieldFor messageType

oneofField :: ProtoParser DotProtoField
oneofField = messageFieldFor (Prim <$> primType)

messageFieldFor :: ProtoParser DotProtoType -> ProtoParser DotProtoField
messageFieldFor parseType = do mtype <- parseType
                               mname <- identifier
                               symbol "="
                               mnumber <- fieldNumber
                               moptions <- pFieldOptions
                               semi
                               return $ DotProtoField mnumber mtype mname moptions mempty

--------------------------------------------------------------------------------
-- enumerations

enumField :: ProtoParser DotProtoEnumPart
enumField = do fname <- identifier
               symbol "="
               fpos <- fromInteger <$> integer
               opts <- pFieldOptions
               semi
               return $ DotProtoEnumField fname fpos opts


enumStatement :: ProtoParser (Maybe DotProtoEnumPart)
enumStatement =
  try (fmap (Just . DotProtoEnumReserved) pReserved)
    <|> try (fmap (Just . DotProtoEnumOption) pOptionStmt)
    <|> try (fmap Just enumField)
    <|> (Nothing <$ pEmptyStmt)

pEnumDefn :: ProtoParser DotProtoDefinition
pEnumDefn = do
  symbol "enum"
  ename <- singleIdentifier
  ebody <- braces do
    results <- many enumStatement
    pure (catMaybes results)
  pure (DotProtoEnum mempty ename ebody)

--------------------------------------------------------------------------------
-- field reservations

range :: ProtoParser DotProtoReservedField
range = do lookAhead (integer >> symbol "to") -- [note] parsec commits to this parser too early without this lookahead
           s <- fromInteger <$> integer
           symbol "to"
           e <- fromInteger <$> integer
           return $ FieldRange s e

ranges :: ProtoParser [DotProtoReservedField]
ranges = commaSep1 (try range <|> (SingleField . fromInteger <$> integer))

pReserved :: ProtoParser [DotProtoReservedField]
pReserved = do
  symbol "reserved"
  v <- ranges <|> commaSep1 (ReservedIdentifier <$> strFieldName)
  semi
  return v

strFieldName :: ProtoParser String
strFieldName =
        between (char '"') (char '"') identifierName
    <|> between (char '\'') (char '\'') identifierName

-- Message Extensions ----------------------------------------------------------

pExtendStmt :: ProtoParser (DotProtoIdentifier, [DotProtoMessagePart])
pExtendStmt = do
  pExtendKw
  idt <- identifier
  fxs <- braces do
    results <- many messagePart
    pure (catMaybes results)
  pure (idt, fxs)

-- | Parses a single keyword token "extend".
--
-- @since 0.5.2
pExtendKw :: ProtoParser ()
pExtendKw = do
  spaces
  token (string "extend" >> notFollowedBy alphaNum)
    <?> "keyword 'extend'"

-- Parse - Empty Statements ----------------------------------------------------

-- | Parses a single empty statement (i.e. a semicolon).
--
-- See: https://protobuf.dev/reference/protobuf/proto3-spec/#emptystatement
pEmptyStmt :: ProtoParser ()
pEmptyStmt = token (() <$ char ';') <?> "empty statement"
