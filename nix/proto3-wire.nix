{ mkDerivation, base, bytestring, cereal, containers, deepseq
, doctest, fetchgit, hashable, QuickCheck, safe, stdenv, tasty
, tasty-hunit, tasty-quickcheck, text, unordered-containers
}:
mkDerivation {
  pname = "proto3-wire";
  version = "1.0.0";
  src = fetchgit {
    url = "https://github.com/awakesecurity/proto3-wire";
    sha256 = "12c2z7infbj9l270s3pdmjcrh2i0f8iyiby5paxnqhz24yg8vmd4";
    rev = "4cca9f06613cf8d6f4841fd4077295ee5873e97c";
  };
  libraryHaskellDepends = [
    base bytestring cereal containers deepseq hashable QuickCheck safe
    text unordered-containers
  ];
  testHaskellDepends = [
    base bytestring cereal doctest QuickCheck tasty tasty-hunit
    tasty-quickcheck text
  ];
  description = "A low-level implementation of the Protocol Buffers (version 3) wire format";
  license = stdenv.lib.licenses.asl20;
}
