{ mkDerivation, array, base, happy, pretty, stdenv, syb }:
mkDerivation {
  pname = "haskell-src";
  version = "1.0.3.1";
  sha256 = "869cc710004c2161470d8a788dab96d2cff054fa106c301be6689109f57e5132";
  libraryHaskellDepends = [ array base pretty syb ];
  libraryToolDepends = [ happy ];
  description = "Support for manipulating Haskell source code";
  license = stdenv.lib.licenses.bsd3;
}
