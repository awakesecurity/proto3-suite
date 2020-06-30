{ mkDerivation, array, base, bytestring, deepseq, httpd-shed, HUnit
, mtl, network, network-uri, parsec, pureMD5, split, stdenv
, test-framework, test-framework-hunit, time
}:
mkDerivation {
  pname = "HTTP";
  version = "4000.3.14";
  sha256 = "a602d7f30e917164c6a634f8cb1f5df4849048858db01380a0875e16e5aa687b";
  revision = "1";
  editedCabalFile = "1inz9grpl9605bbymy6n5y4as54mlykbsiw8wpm5gl6qvxgrf69w";
  libraryHaskellDepends = [
    array base bytestring mtl network network-uri parsec time
  ];
  testHaskellDepends = [
    base bytestring deepseq httpd-shed HUnit mtl network network-uri
    pureMD5 split test-framework test-framework-hunit
  ];
  homepage = "https://github.com/haskell/HTTP";
  description = "A library for client-side HTTP";
  license = stdenv.lib.licenses.bsd3;
}
