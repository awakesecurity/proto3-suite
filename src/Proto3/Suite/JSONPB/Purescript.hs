module Proto3.Suite.JSONPB.Purescript where


-- Purescript types

newtype PsTypeIdent = PsTypeIdent String
  deriving (Show)

newtype PsIdent = PsIdent String
  deriving (Show)

data PsCtor = PsCtor String [PsTypeIdent]
  deriving (Show)
-- data PsContainerType
--   = PsArray PsType

data PsPrimType = PInt | PString | PDouble
  deriving (Show)

-- a typedef of a record.
data PsRecordType =
  PsRecordType PsTypeIdent
               (M.Map PsIdentifier PsType)
  deriving (Show)

data PsSumType
  = PsSumType PsTypeIdent [PsCtor]
  deriving (Show)

data PsType
  = PsPrim PsPrimType
  | PsRec PsRecordType
  | PsArray PsType
  | PsSum PsSumType
  | PsDefined PsTypeIdent
  deriving (Show)

data PsModule = PsModule [PsType]
  deriving (Show)


--------------------------------------------------------------------------------

pursModuleForDotProto
  :: MonadError CompileError m
  => DotProto
  -> TypeContext
  -> m PsModule
pursModuleForDotProto proto typeCtxt = do
  <-
