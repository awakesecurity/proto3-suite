module Proto3.Suite.DotProto (module M) where

import Proto3.Suite.DotProto.AST       as M
import Proto3.Suite.DotProto.Parsing   as M
import Proto3.Suite.DotProto.Rendering as M
-- exported for testing
import Proto3.Suite.DotProto.Internal  as M (fieldLikeName, prefixedEnumFieldName, typeLikeName)
