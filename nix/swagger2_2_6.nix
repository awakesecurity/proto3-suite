{ mkDerivation, aeson, aeson-pretty, base, base-compat-batteries
, bytestring, Cabal, cabal-doctest, containers, cookie, doctest
, generics-sop, Glob, hashable, hspec, hspec-discover, http-media
, HUnit, insert-ordered-containers, lens, mtl, network, optics-core
, optics-th, QuickCheck, quickcheck-instances, scientific, stdenv
, template-haskell, text, time, transformers, transformers-compat
, unordered-containers, utf8-string, uuid-types, vector
}:
mkDerivation {
  pname = "swagger2";
  version = "2.6";
  sha256 = "682afe3b43d6b7c394cab330bb48692b8045dff8db3e8913bbfabee0fa8c706e";
  revision = "2";
  editedCabalFile = "1gdq1kiccn6qv05fnkb2dzsnsds2v3gri29gd8l1x9vx74mpbh0j";
  setupHaskellDepends = [ base Cabal cabal-doctest ];
  libraryHaskellDepends = [
    aeson aeson-pretty base base-compat-batteries bytestring containers
    cookie generics-sop hashable http-media insert-ordered-containers
    lens mtl network optics-core optics-th QuickCheck scientific
    template-haskell text time transformers transformers-compat
    unordered-containers uuid-types vector
  ];
  testHaskellDepends = [
    aeson base base-compat-batteries bytestring containers doctest Glob
    hashable hspec HUnit insert-ordered-containers lens mtl QuickCheck
    quickcheck-instances template-haskell text time
    unordered-containers utf8-string vector
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/GetShopTV/swagger2";
  description = "Swagger 2.0 data model";
  license = stdenv.lib.licenses.bsd3;
}
