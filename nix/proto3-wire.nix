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
    sha256 = "0af7k2fdhhyc3qf05caphqzl8786fsypf11j797bj6disxbbbhwk";
    rev = "a161f3ae2e13328a5469dd8674b7de96a7119db2";
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
