{ mkDerivation, aeson, base, bytestring, cereal, containers
, deepseq, filepath, haskell-src, mtl, parsec, parsers, pretty
, proto3-wire, QuickCheck, safe, semigroups, stdenv, tasty
, tasty-hunit, tasty-quickcheck, text, transformers, turtle, vector
}:
mkDerivation {
  pname = "proto3-suite";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring cereal containers deepseq filepath
    haskell-src mtl parsec parsers pretty proto3-wire QuickCheck safe
    semigroups text transformers vector
  ];
  testHaskellDepends = [
    aeson base bytestring cereal proto3-wire QuickCheck semigroups
    tasty tasty-hunit tasty-quickcheck text transformers turtle
  ];
  description = "A low level library for writing out data in the Protocol Buffers wire format";
  license = stdenv.lib.licenses.asl20;
}
