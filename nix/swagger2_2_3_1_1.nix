{ mkDerivation, aeson, base, base-compat-batteries, bytestring
, Cabal, cabal-doctest, containers, cookie, doctest, generics-sop
, Glob, hashable, hspec, hspec-discover, http-media, HUnit
, insert-ordered-containers, lens, mtl, network, QuickCheck
, quickcheck-instances, scientific, stdenv, template-haskell, text
, time, transformers, transformers-compat, unordered-containers
, utf8-string, uuid-types, vector
}:
mkDerivation {
  pname = "swagger2";
  version = "2.3.1.1";
  sha256 = "477793bbbe0017d2fcbb762e0b16fe6b7dd9ada5ae69c4826a605a8625a4daa5";
  revision = "1";
  editedCabalFile = "1g6jiadrvglrbf0857nzfbnjxmb3lwqamgs47j3qv9k6kfwilzyk";
  setupHaskellDepends = [ base Cabal cabal-doctest ];
  libraryHaskellDepends = [
    aeson base base-compat-batteries bytestring containers cookie
    generics-sop hashable http-media insert-ordered-containers lens mtl
    network QuickCheck scientific template-haskell text time
    transformers transformers-compat unordered-containers uuid-types
    vector
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
