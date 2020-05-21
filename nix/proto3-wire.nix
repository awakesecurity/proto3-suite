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
    sha256 = "0vac5a82ip20pclb80kh2a39z285a866ki6d06hh7fam300k6q3i";
    rev = "99a8a2c70a80e62c4a09d41689ec3aa5efe2f67a";
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
