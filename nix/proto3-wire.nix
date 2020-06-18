{ mkDerivation, base, bytestring, cereal, containers, deepseq
, doctest, fetchgit, ghc-prim, hashable, parameterized, primitive
, QuickCheck, safe, stdenv, tasty, tasty-hunit, tasty-quickcheck
, text, transformers, unordered-containers, vector
}:
mkDerivation {
  pname = "proto3-wire";
  version = "1.2.0";
  src = fetchgit {
    url = "https://github.com/awakesecurity/proto3-wire.git";
    sha256 = "1d2ir9ds4vawrn6lkxqgyw9zg8h2l4d6m8ihhy6znjllh12fmjyp";
    rev = "5df56fe1ad26a18b1dfbb2a5b8d35b4c1ad63f53";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base bytestring cereal containers deepseq ghc-prim hashable
    parameterized primitive QuickCheck safe text transformers
    unordered-containers vector
  ];
  testHaskellDepends = [
    base bytestring cereal doctest QuickCheck tasty tasty-hunit
    tasty-quickcheck text transformers vector
  ];
  description = "A low-level implementation of the Protocol Buffers (version 3) wire format";
  license = stdenv.lib.licenses.asl20;
}
