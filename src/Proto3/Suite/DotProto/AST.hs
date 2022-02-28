{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

-- |
--
-- @since 1.0.0
module Proto3.Suite.DotProto.AST
  ( -- * Names
    PackageName (PackageName),
    getPackageName,

    -- * Imports
    DotProtoImport (DotProtoImport),
    dotProtoImportQualifier,
    dotProtoImportPath,

    -- * Import Qualifiers
    DotProtoImportQualifier
      ( DotProtoImportPublic,
        DotProtoImportWeak,
        DotProtoImportDefault
      ),

    -- * Package Specs
    DotProtoPackageSpec (DotProtoPackageSpec, DotProtoNoPackage),

    -- * Proto Metadata
    DotProtoMeta (DotProtoMeta),
    metaModulePath,

    -- * Protocol Buffers
    DotProto (DotProto),
    protoImports,
    protoOptions,
    protoPackage,
    protoDefinitions,
    protoMeta,

    -- * Re-exports
    module Proto3.Suite.DotProto.AST.Enumerate,
    module Proto3.Suite.DotProto.AST.Field,
    module Proto3.Suite.DotProto.AST.Identifier,
    module Proto3.Suite.DotProto.AST.Message,
    module Proto3.Suite.DotProto.AST.Option,
    module Proto3.Suite.DotProto.AST.Type,
    module Proto3.Suite.DotProto.AST.Value,
  )
where

import Data.String (IsString)
import qualified Turtle

import Proto3.Suite.DotProto.AST.Enumerate
import Proto3.Suite.DotProto.AST.Field
import Proto3.Suite.DotProto.AST.Identifier
import Proto3.Suite.DotProto.AST.Message
import Proto3.Suite.DotProto.AST.Option
import Proto3.Suite.DotProto.AST.Type
import Proto3.Suite.DotProto.AST.Value

--------------------------------------------------------------------------------

-- | Proto3 package names.
--
-- @since 1.0.0
newtype PackageName = PackageName
  {getPackageName :: String}
  deriving stock (Eq, Ord)
  deriving (IsString, Show) via String

--------------------------------------------------------------------------------

-- | TODO: docs
--
-- Top-level import declaration
--
-- @since 1.0.0
data DotProtoImport = DotProtoImport
  { dotProtoImportQualifier :: DotProtoImportQualifier
  , dotProtoImportPath :: Turtle.FilePath
  }
  deriving stock (Eq, Ord, Show)

--------------------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data DotProtoImportQualifier
  = DotProtoImportPublic
  | DotProtoImportWeak
  | DotProtoImportDefault
  deriving stock (Eq, Ord, Show)

--------------------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data DotProtoPackageSpec
  = DotProtoPackageSpec DotProtoIdentifier
  | DotProtoNoPackage
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------

-- | Tracks misc metadata about the AST
-- ^ The "module path" associated with the .proto file from which this AST
-- was parsed. The "module path" is derived from the `--includeDir`-relative
-- .proto filename passed to 'parseProtoFile'. See
-- 'Proto3.Suite.DotProto.Internal.toModulePath' for details on how module
-- path values are constructed. See
-- 'Proto3.Suite.DotProto.Generate.modulePathModName' to see how it is used
-- during code generation.
--
-- @since 1.0.0
newtype DotProtoMeta = DotProtoMeta
  {metaModulePath :: Path}
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------

-- | TODO: docs
-- This data structure represents a .proto file
--   The actual source order of protobuf statements isn't meaningful so
--   statements are sorted by type during parsing.
--   A .proto file with more than one package declaration is considered invalid.
data DotProto = DotProto
  { protoImports :: [DotProtoImport]
  , protoOptions :: [DotProtoOption]
  , protoPackage :: DotProtoPackageSpec
  , protoDefinitions :: [DotProtoDefinition]
  , protoMeta :: DotProtoMeta
  }
  deriving stock (Eq, Show)
