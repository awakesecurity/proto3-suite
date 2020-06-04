{ mkDerivation, base, bytestring, cereal, containers, deepseq
, doctest, fetchgit, ghc-prim, hashable, primitive, QuickCheck
, safe, stdenv, tasty, tasty-hunit, tasty-quickcheck, text
, transformers, unordered-containers, vector
}:
mkDerivation {
  pname = "proto3-wire";
  version = "1.2.0";
  src = fetchgit {
    url = "https://github.com/awakesecurity/proto3-wire";
    sha256 = "0rhf20yhqg141riyix5s3vpypfn9xz010snhn62lhswyhpb6g5zp";
    rev = "cdf19f50929657c37e866f6937e7cc8fe8624983";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base bytestring cereal containers deepseq ghc-prim hashable
    primitive QuickCheck safe text transformers unordered-containers
    vector
  ];
  testHaskellDepends = [
    base bytestring cereal doctest QuickCheck tasty tasty-hunit
    tasty-quickcheck text transformers vector
  ];
  description = "A low-level implementation of the Protocol Buffers (version 3) wire format";
  license = stdenv.lib.licenses.asl20;
}
