{ mkDerivation, base, bytestring, cereal, containers, deepseq
, doctest, ghc-prim, hashable, parameterized, primitive, QuickCheck
, safe, stdenv, tasty, tasty-hunit, tasty-quickcheck, text
, transformers, unordered-containers, vector
}:
mkDerivation {
  pname = "proto3-wire";
  version = "1.2.1";
  sha256 = "9924b8a17af75594ab1c785307ba80648d6bfff2716e83a2ff05c7229337e044";
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
