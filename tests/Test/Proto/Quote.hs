
module Test.Proto.Quote (
  dotProto3Syntax,
) where

import "template-haskell" Language.Haskell.TH as TH
import "template-haskell" Language.Haskell.TH.Syntax as TH
import "template-haskell" Language.Haskell.TH.Quote (QuasiQuoter (..))

import Proto3.Suite.DotProto.AST (Path (..))
import Proto3.Suite.DotProto.Parsing (parseProtoWithFile)

--------------------------------------------------------------------------------

dotProto3Syntax :: QuasiQuoter
dotProto3Syntax = 
  QuasiQuoter 
    { quoteExp = quoteDotProto 
    , quotePat = \_ -> fail "proto3 quasiquoter does not support patterns"
    , quoteType = \_ -> fail "proto3 quasiquoter does not support types"
    , quoteDec = \_ -> fail "proto3 quasiquoter does not support declarations"
    }

quoteDotProto :: String -> Q Exp
quoteDotProto input = do
  TH.Module pkgName modName <- TH.thisModule

  let pkg :: Path 
      pkg = Path (pure (show pkgName))

  case parseProtoWithFile pkg (show modName) input of 
    Left err -> fail (show err)
    Right expr -> TH.lift expr