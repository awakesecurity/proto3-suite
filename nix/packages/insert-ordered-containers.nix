{ mkDerivation, aeson, base, base-compat, deepseq, hashable
, indexed-traversable, lens, lib, optics-core, optics-extra
, QuickCheck, semigroupoids, tasty, tasty-quickcheck, text
, transformers, unordered-containers
}:
mkDerivation {
  pname = "insert-ordered-containers";
  version = "0.2.5.3";
  sha256 = "f04f6e59795d8e362d15422a62d7c7c48312c2d97d2bc4372002a8a9b9a2436c";
  revision = "1";
  editedCabalFile = "12fkswr70fw2av11yy45v189r6cb8fcg0l1r7mayvwha3gls0j3n";
  libraryHaskellDepends = [
    aeson base deepseq hashable indexed-traversable lens optics-core
    optics-extra semigroupoids text transformers unordered-containers
  ];
  testHaskellDepends = [
    aeson base base-compat hashable lens QuickCheck semigroupoids tasty
    tasty-quickcheck text unordered-containers
  ];
  homepage = "https://github.com/phadej/insert-ordered-containers#readme";
  description = "Associative containers retaining insertion order for traversals";
  license = lib.licenses.bsd3;
}
