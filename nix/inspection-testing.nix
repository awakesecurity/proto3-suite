{ mkDerivation, base, containers, ghc, mtl, stdenv
, template-haskell, transformers
}:
mkDerivation {
  pname = "inspection-testing";
  version = "0.4.2.4";
  sha256 = "0396b32e3fa6585a93db8001d6ebc43727a792ae9ad52392d5070e6a8a44df86";
  libraryHaskellDepends = [
    base containers ghc mtl template-haskell transformers
  ];
  testHaskellDepends = [ base ];
  homepage = "https://github.com/nomeata/inspection-testing";
  description = "GHC plugin to do inspection testing";
  license = stdenv.lib.licenses.mit;
}
