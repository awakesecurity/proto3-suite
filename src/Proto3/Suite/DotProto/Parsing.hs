-- | This module contains a near-direct translation of the proto3 grammar
--   It uses String for easier compatibility with DotProto.Generator, which needs it for not very good reasons

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Proto3.Suite.DotProto.Parsing
  ( parseProto
  , parseProtoFile
  ) where

import Control.Applicative hiding (empty)
import Control.Monad
import Data.Functor
import qualified Data.Text as T
import qualified Filesystem.Path.CurrentOS as FP
import Proto3.Suite.DotProto.AST
import Proto3.Wire.Types (FieldNumber(..))
import Text.Parsec (parse, ParseError)
import Text.Parsec.String (Parser)
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Parser.LookAhead
import Text.Parser.Token
import qualified Text.Parser.Token.Style as TokenStyle
import qualified Turtle

----------------------------------------
-- interfaces

-- | @parseProto mp inp@ attempts to parse @inp@ as a 'DotProto'. @mp@ is the
-- module path to be injected into the AST as part of 'DotProtoMeta' metadata on
-- a successful parse.
parseProto :: Path -> String -> Either ParseError DotProto
parseProto modulePath = parse (runProtoParser (topLevel modulePath)) ""

-- | @parseProtoFile mp fp@ reads and parses the .proto file found at @fp@. @mp@
-- is used downstream during code generation when we need to generate names
-- which are a function of the source .proto file's filename and its path
-- relative to some @--includeDir@.
parseProtoFile :: Turtle.MonadIO m
               => Path -> Turtle.FilePath -> m (Either ParseError DotProto)
parseProtoFile modulePath =
  fmap (parseProto modulePath) . Turtle.liftIO . readFile . FP.encodeString

----------------------------------------
-- convenience

-- | Wrapper around @Text.Parsec.String.Parser@, overriding whitespace lexing.
newtype ProtoParser a = ProtoParser { runProtoParser :: Parser a }
  deriving ( Functor, Applicative, Alternative, Monad, MonadPlus
           , Parsing, CharParsing, LookAheadParsing)

instance TokenParsing ProtoParser where
  someSpace   = TokenStyle.buildSomeSpaceParser (ProtoParser someSpace)
                                                TokenStyle.javaCommentStyle
  nesting     = ProtoParser . nesting . runProtoParser
  semi        = ProtoParser semi
  highlight h = ProtoParser . highlight h . runProtoParser
  token       = ProtoParser . token . runProtoParser

empty :: ProtoParser ()
empty = whiteSpace >> textSymbol ";" >> return ()

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
_identifier = do is <- identifierName `sepBy1` string "."
                 return $ case is of
                   [i] -> Single i
                   _   -> Dots (Path is)

singleIdentifier :: ProtoParser DotProtoIdentifier
singleIdentifier = Single <$> token identifierName

-- Parses a full identifier, consuming trailing space.
identifier :: ProtoParser DotProtoIdentifier
identifier = token _identifier

-- [note] message and enum types are defined by the proto3 spec to have an optional leading period (messageType and enumType in the spec)
--        what this indicates is, as far as i can tell, not documented, and i haven't found this syntax used in practice
--        it's ommitted but can be fairly easily added if there is in fact a use for it

-- [update] the leading dot denotes that the identifier path starts in global scope
--          i still haven't seen a use case for this but i can add it upon request

-- Parses a nested identifier, consuming trailing space.
nestedIdentifier :: ProtoParser DotProtoIdentifier
nestedIdentifier = token $ do
  h <- parens _identifier
  string "."
  t <- _identifier
  return $ Qualified h t

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
       <|> Named <$> identifier

--------------------------------------------------------------------------------
-- top-level parser and version annotation

syntaxSpec :: ProtoParser ()
syntaxSpec = void $ do
  symbol "syntax"
  symbol "="
  symbol "'proto3'" <|> symbol "\"proto3\""
  symbol ";"

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
topLevel modulePath = do whiteSpace
                         syntaxSpec
                         sortStatements modulePath <$> many topStatement

--------------------------------------------------------------------------------
-- top-level statements

topStatement :: ProtoParser DotProtoStatement
topStatement = DPSImport     <$> import_
           <|> DPSPackage    <$> package
           <|> DPSOption     <$> topOption
           <|> DPSDefinition <$> definition
           <|> DPSEmpty      <$  empty

import_ :: ProtoParser DotProtoImport
import_ = do symbol "import"
             qualifier <- option DotProtoImportDefault $
                                 symbol "weak" $> DotProtoImportWeak
                             <|> symbol "public" $> DotProtoImportPublic
             target <- FP.fromText . T.pack <$> stringLit
             symbol ";"
             return $ DotProtoImport qualifier target

package :: ProtoParser DotProtoPackageSpec
package = do symbol "package"
             p <- identifier
             symbol ";"
             return $ DotProtoPackageSpec p

definition :: ProtoParser DotProtoDefinition
definition = message
         <|> enum
         <|> service

--------------------------------------------------------------------------------
-- options

inlineOption :: ProtoParser DotProtoOption
inlineOption = DotProtoOption <$> (optionName <* symbol "=") <*> value
  where
    optionName = try nestedIdentifier <|> identifier

optionAnnotation :: ProtoParser [DotProtoOption]
optionAnnotation = brackets (commaSep1 inlineOption) <|> pure []

topOption :: ProtoParser DotProtoOption
topOption = symbol "option" *> inlineOption <* symbol ";"

--------------------------------------------------------------------------------
-- service statements

servicePart :: ProtoParser DotProtoServicePart
servicePart = rpc
          <|> (DotProtoServiceOption <$> topOption)
          <|> empty $> DotProtoServiceEmpty

rpcOptions :: ProtoParser [DotProtoOption]
rpcOptions = braces $ many topOption

rpcClause :: ProtoParser (DotProtoIdentifier, Streaming)
rpcClause = do
  let sid ctx = (,ctx) <$> identifier
  -- NB: Distinguish "stream stream.foo" from "stream.foo"
  try (symbol "stream" *> sid Streaming) <|> sid NonStreaming

rpc :: ProtoParser DotProtoServicePart
rpc = do symbol "rpc"
         name <- singleIdentifier
         subjecttype <- parens rpcClause
         symbol "returns"
         returntype <- parens rpcClause
         options <- rpcOptions <|> (symbol ";" $> [])
         return $ DotProtoServiceRPC name subjecttype returntype options

service :: ProtoParser DotProtoDefinition
service = do symbol "service"
             name <- singleIdentifier
             statements <- braces (many servicePart)
             return $ DotProtoService name statements

--------------------------------------------------------------------------------
-- message definitions

message :: ProtoParser DotProtoDefinition
message = do symbol "message"
             name <- singleIdentifier
             body <- braces (many messagePart)
             return $ DotProtoMessage name body

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
          <|>     (DotProtoMessageField      <$> messageField)

messageType :: ProtoParser DotProtoType
messageType = try mapType <|> dotProtoType
  where
    mapType = do symbol "map"
                 symbol "<"
                 ktype <- primType
                 symbol ","
                 vtype <- primType
                 symbol ">"
                 return (Map ktype vtype)

    dotProtoType = do ctor <- try (symbol "repeated" $> Repeated) <|> pure Prim
                      mtype <- primType
                      return (ctor mtype)

messageField :: ProtoParser DotProtoField
messageField = do mtype <- messageType
                  mname <- identifier
                  symbol "="
                  mnumber <- fieldNumber
                  moptions <- optionAnnotation
                  symbol ";"
                  return $ DotProtoField mnumber mtype mname moptions Nothing

--------------------------------------------------------------------------------
-- enumerations

enumField :: ProtoParser DotProtoEnumPart
enumField = do fname <- identifier
               symbol "="
               fpos <- fromInteger <$> integer
               opts <- optionAnnotation
               symbol ";"
               return $ DotProtoEnumField fname fpos opts


enumStatement :: ProtoParser DotProtoEnumPart
enumStatement = try (DotProtoEnumOption <$> topOption)
            <|> enumField
            <|> empty $> DotProtoEnumEmpty

enum :: ProtoParser DotProtoDefinition
enum = do symbol "enum"
          ename <- singleIdentifier
          ebody <- braces (many enumStatement)
          return $ DotProtoEnum ename ebody

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
                   symbol ";"
                   return v
