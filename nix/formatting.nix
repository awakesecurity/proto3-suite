{ mkDerivation, array, base, bytestring, clock, ghc-prim, hspec
, integer-gmp, old-locale, scientific, stdenv, text, time
, transformers
}:
mkDerivation {
  pname = "formatting";
  version = "6.3.0";
  sha256 = "16xngayk1jd92bj2qaf7fmrgzdskdnc7rsgpk1ij06xd8cdgahf1";
  libraryHaskellDepends = [
    array base bytestring clock ghc-prim integer-gmp old-locale
    scientific text time transformers
  ];
  testHaskellDepends = [ base hspec ];
  description = "Combinator-based type-safe formatting (like printf() or FORMAT)";
  license = stdenv.lib.licenses.bsd3;
}
