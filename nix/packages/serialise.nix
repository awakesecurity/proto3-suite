{ mkDerivation, aeson, array, base, binary, bytestring, cborg
, cereal, cereal-vector, containers, criterion, deepseq, directory
, fail, filepath, ghc-prim, half, hashable, lib, pretty, primitive
, QuickCheck, quickcheck-instances, semigroups, store, strict, tar
, tasty, tasty-hunit, tasty-quickcheck, text, these, time
, unordered-containers, vector, zlib
}:
mkDerivation {
  pname = "serialise";
  version = "0.2.6.1";
  sha256 = "63949799ffd10675ef70ea701c1eb63e618629b3b2f7b25f07c5a966e24e77f4";
  revision = "2";
  editedCabalFile = "1y21m10vx4k75x0im5448pl5mhl772hz1hfk47ryb3whz1l6d9vf";
  libraryHaskellDepends = [
    array base bytestring cborg containers ghc-prim half hashable
    primitive strict text these time unordered-containers vector
  ];
  testHaskellDepends = [
    base bytestring cborg containers directory filepath primitive
    QuickCheck quickcheck-instances tasty tasty-hunit tasty-quickcheck
    text time unordered-containers vector
  ];
  benchmarkHaskellDepends = [
    aeson array base binary bytestring cborg cereal cereal-vector
    containers criterion deepseq directory fail filepath ghc-prim half
    pretty semigroups store tar text time vector zlib
  ];
  homepage = "https://github.com/well-typed/cborg";
  description = "A binary serialisation library for Haskell values";
  license = lib.licenses.bsd3;
}
