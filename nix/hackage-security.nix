{ mkDerivation, aeson, base, base16-bytestring, base64-bytestring
, bytestring, Cabal, containers, cryptohash-sha256, directory
, ed25519, filepath, ghc-prim, lukko, mtl, network, network-uri
, parsec, pretty, QuickCheck, stdenv, tar, tasty, tasty-hunit
, tasty-quickcheck, template-haskell, temporary, time, transformers
, unordered-containers, vector, zlib
}:
mkDerivation {
  pname = "hackage-security";
  version = "0.6.0.1";
  sha256 = "9162b473af5a21c1ff32a50b972b9acf51f4c901604a22cf08a2dccac2f82f17";
  libraryHaskellDepends = [
    base base16-bytestring base64-bytestring bytestring Cabal
    containers cryptohash-sha256 directory ed25519 filepath ghc-prim
    lukko mtl network network-uri parsec pretty tar template-haskell
    time transformers zlib
  ];
  testHaskellDepends = [
    aeson base bytestring Cabal containers network-uri QuickCheck tar
    tasty tasty-hunit tasty-quickcheck temporary time
    unordered-containers vector zlib
  ];
  homepage = "https://github.com/haskell/hackage-security";
  description = "Hackage security library";
  license = stdenv.lib.licenses.bsd3;
}
