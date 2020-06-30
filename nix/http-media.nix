{ mkDerivation, base, bytestring, case-insensitive, containers
, QuickCheck, stdenv, test-framework, test-framework-quickcheck2
, utf8-string
}:
mkDerivation {
  pname = "http-media";
  version = "0.8.0.0";
  sha256 = "398279d1dff5b60cd8b8c650caceca248ea1184d694bedf5df5426963b2b9c53";
  revision = "3";
  editedCabalFile = "1ndz5x6njl35h73il5c6qpaqd4ynvg21n6k7sb8aq09gfbg544d8";
  libraryHaskellDepends = [
    base bytestring case-insensitive containers utf8-string
  ];
  testHaskellDepends = [
    base bytestring case-insensitive containers QuickCheck
    test-framework test-framework-quickcheck2 utf8-string
  ];
  homepage = "https://github.com/zmthy/http-media";
  description = "Processing HTTP Content-Type and Accept headers";
  license = stdenv.lib.licenses.mit;
}
