-- | This module contains a near-direct translation of the proto3 grammar
--   It uses String for easier compatibility with DotProto.Generator, which needs it for not very good reasons

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
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Functor
import qualified Data.Text as T
import Proto3.Suite.DotProto.AST
import Proto3.Wire.Types (FieldNumber(..))
import Text.Parsec (parse, ParseError)
import Text.Parsec.String (Parser)
import Text.Parser.Char
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

parseProtoWithFile :: Path -> String -> String -> Either ParseError DotProto
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

-- [issue] these string parsers are weak to \" and \000 octal codes
stringLit :: ProtoParser String
stringLit = stringLiteral <|> stringLiteral'

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
    <|> try (StringLit            <$> stringLit)
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
  sortStatements modulePath <$> many topStatement

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
             target <- Turtle.fromText . T.pack <$> stringLit
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
    , try enum
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

servicePart :: ProtoParser DotProtoServicePart
servicePart = DotProtoServiceRPCMethod <$> rpc
          <|> DotProtoServiceOption <$> pOptionStmt
          <|> DotProtoServiceEmpty <$ empty

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
service = do symbol "service"
             name <- singleIdentifier
             statements <- braces (many servicePart)
             return $ DotProtoService mempty name statements

--------------------------------------------------------------------------------
-- message definitions

message :: ProtoParser DotProtoDefinition
message = do symbol "message"
             name <- singleIdentifier
             body <- braces (many messagePart)
             return $ DotProtoMessage mempty name body

messageOneOf :: ProtoParser DotProtoMessagePart
messageOneOf = do symbol "oneof"
                  name <- singleIdentifier
                  body <- braces $ many (messageField <|> empty $> DotProtoEmptyField)
                  return $ DotProtoMessageOneOf name body

messagePart :: ProtoParser DotProtoMessagePart
messagePart = try (DotProtoMessageDefinition <$> enum)
          <|> try (DotProtoMessageReserved   <$> reservedField)
          <|> try (DotProtoMessageDefinition <$> message)
          <|> try messageOneOf
          <|> try (DotProtoMessageField      <$> messageField)
          <|> try (DotProtoMessageOption     <$> pOptionStmt)

messageType :: ProtoParser DotProtoType
messageType = try mapType <|> try repType <|> (Prim <$> primType)
  where
    mapType = do symbol "map"
                 angles $ Map <$> (primType <* comma)
                              <*> primType

    repType = do symbol "repeated"
                 Repeated <$> primType

messageField :: ProtoParser DotProtoField
messageField = do mtype <- messageType
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


enumStatement :: ProtoParser DotProtoEnumPart
enumStatement = try (DotProtoEnumOption <$> pOptionStmt)
            <|> enumField
            <|> empty $> DotProtoEnumEmpty

enum :: ProtoParser DotProtoDefinition
enum = do symbol "enum"
          ename <- singleIdentifier
          ebody <- braces (many enumStatement)
          return $ DotProtoEnum mempty ename ebody

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

reservedField :: ProtoParser [DotProtoReservedField]
reservedField = do symbol "reserved"
                   v <- ranges <|> commaSep1 (ReservedIdentifier <$> stringLit)
                   semi
                   return v

-- Message Extensions ----------------------------------------------------------

pExtendStmt :: ProtoParser (DotProtoIdentifier, [DotProtoMessagePart])
pExtendStmt = do 
  pExtendKw
  idt <- singleIdentifier
  fxs <- braces (many messagePart)
  pure (idt, fxs)

-- | Parses a single keyword token "extend".
--
-- @since 0.5.2
pExtendKw :: ProtoParser ()
pExtendKw = do
  spaces 
  token (string "extend" >> notFollowedBy alphaNum) 
    <?> "keyword 'extend'"
