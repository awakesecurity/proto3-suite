-- | This module contains the purescript code generator.
module Proto3.Suite.DotProto.Generate.Purescript where
import           Control.Applicative
import           Control.Arrow                  ((&&&))
import           Control.Monad.Except
import           Control.Lens                   (ix, over)
import           Data.Bifunctor                 (first)
import           Data.Char
import           Data.Coerce
import           Data.Either                    (partitionEithers)
import           Data.List                      (find, intercalate, nub, sortBy,
                                                 stripPrefix)
import qualified Data.Map                       as M
import           Data.Maybe                     (catMaybes, fromMaybe)
import           Data.Monoid
import           Data.Ord                       (comparing)
import qualified Data.Set                       as S
import           Data.String                    (fromString)
import qualified Data.Text                      as T
import           Filesystem.Path.CurrentOS      ((</>), (<.>))
import qualified Filesystem.Path.CurrentOS      as FP
import           Language.Haskell.Pretty
import           Language.Haskell.Syntax
import           Language.Haskell.Parser        (ParseResult(..), parseModule)
import qualified NeatInterpolation              as Neat
import           Prelude                        hiding (FilePath)
import           Proto3.Suite.DotProto
import           Proto3.Suite.DotProto.Rendering (Pretty(..))
import           Proto3.Suite.DotProto.Internal
import           Proto3.Suite.DotProto.Generate.Common
import           Proto3.Wire.Types              (FieldNumber (..))
import           System.IO                      (writeFile, readFile)
import           Text.Parsec                    (ParseError)


import Text.PrettyPrint as PP


-- * PureScript abstract syntax.
-- Here we mainly recycle the haskell abstract syntax provided by
-- haskell-src. The main difference to account for are Purescript's records,
-- which we do by providing a hacked up prettyprinter.


---------------------------------------------------------------------

dotProtoToPurescript
  :: MonadError CompileError m
  => DotProto -> TypeContext -> m PP.Doc
dotProtoToPurescript DotProto{} tc = do
  undefined



dpptToPSType :: MonadError DotProtoPrimType ->
