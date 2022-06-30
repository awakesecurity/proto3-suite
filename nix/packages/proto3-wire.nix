{ mkDerivation, base, bytestring, cereal, containers, criterion
, deepseq, doctest, ghc-prim, hashable, parameterized, primitive
, QuickCheck, random, safe, stdenv, tasty, tasty-hunit
, tasty-quickcheck, text, transformers, unordered-containers
, vector, word-compat
}:
mkDerivation {
  pname = "proto3-wire";
  version = "1.4.0";
  sha256 = "be140771b2beff80929d7e1dd8d0af8dfdcb73bd12e3869208750f9e88d8d2af";
  libraryHaskellDepends = [
    base bytestring cereal containers deepseq ghc-prim hashable
    parameterized primitive QuickCheck safe text transformers
    unordered-containers vector word-compat
  ];
  testHaskellDepends = [
    base bytestring cereal doctest QuickCheck tasty tasty-hunit
    tasty-quickcheck text transformers vector
  ];
  benchmarkHaskellDepends = [ base bytestring criterion random ];
  description = "A low-level implementation of the Protocol Buffers (version 3) wire format";
  license = stdenv.lib.licenses.asl20;
}
