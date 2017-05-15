{ mkDerivation, array, base, ghc-prim, integer-gmp, QuickCheck
, smallcheck, stdenv, tasty, tasty-hunit, tasty-quickcheck
, tasty-smallcheck
}:
mkDerivation {
  pname = "integer-logarithms";
  version = "1";
  sha256 = "0g0hrx32l33hzvjc9qzwgq1gv1pp0l9z84wr2mhfgw3cxalvfd4s";
  revision = "2";
  editedCabalFile = "ee7f145ff4250ef4babd7e0b679b1a26c79da0897da2453cc12281a78f992a04";
  libraryHaskellDepends = [ array base ghc-prim integer-gmp ];
  testHaskellDepends = [
    base QuickCheck smallcheck tasty tasty-hunit tasty-quickcheck
    tasty-smallcheck
  ];
  homepage = "https://github.com/Bodigrim/integer-logarithms";
  description = "Integer logarithms";
  license = stdenv.lib.licenses.mit;
}
