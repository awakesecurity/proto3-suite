{ mkDerivation, base, data-diverse, hspec, stdenv, transformers }:
mkDerivation {
  pname = "parameterized";
  version = "0.5.0.0";
  sha256 = "d76bc473c671f3c448cc0683ec44f4495cb21938ab27a8e2c35a94683a373346";
  libraryHaskellDepends = [ base data-diverse transformers ];
  testHaskellDepends = [ base data-diverse hspec transformers ];
  homepage = "https://github.com/louispan/parameterized#readme";
  description = "Parameterized/indexed monoids and monads using only a single parameter type variable";
  license = stdenv.lib.licenses.bsd3;
}
