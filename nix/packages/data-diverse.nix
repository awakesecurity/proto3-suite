{ mkDerivation, base, containers, criterion, deepseq, ghc-prim
, hashable, hspec, lib, tagged
}:
mkDerivation {
  pname = "data-diverse";
  version = "4.7.1.0";
  sha256 = "98722c2a85d8a6c2a8fec04820459f32111edb7b20058303d88af3b5ea4f4d80";
  libraryHaskellDepends = [
    base containers deepseq ghc-prim hashable tagged
  ];
  testHaskellDepends = [ base hspec tagged ];
  benchmarkHaskellDepends = [ base criterion ];
  homepage = "https://github.com/louispan/data-diverse#readme";
  description = "Extensible records and polymorphic variants";
  license = lib.licenses.bsd3;
}
