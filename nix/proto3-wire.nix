{ mkDerivation, base, bytestring, cereal, containers, deepseq
, fetchgit, QuickCheck, safe, stdenv, tasty, tasty-hunit
, tasty-quickcheck, text
}:
mkDerivation {
  pname = "proto3-wire";
  version = "1.0.0";
  src = fetchgit {
    url = "https://github.com/awakenetworks/proto3-wire.git";
    sha256 = "128fcdwra3j1mcxnra4h8q79r671img5x2xph9pc8lf1wxr2hyi3";
    rev = "8585640e531673229bbe9e4748457cd62f5982e0";
  };
  libraryHaskellDepends = [
    base bytestring cereal containers deepseq QuickCheck safe text
  ];
  testHaskellDepends = [
    base bytestring cereal QuickCheck tasty tasty-hunit
    tasty-quickcheck text
  ];
  description = "A low-level implementation of the Protocol Buffers (version 3) wire format";
  license = stdenv.lib.licenses.asl20;
}
