{ mkDerivation, base, bytestring, cereal, containers, deepseq, mtl
, parsec, parsers, pipes, pretty, proto3-wire, QuickCheck, safe
, semigroups, stdenv, tasty, tasty-hunit, tasty-quickcheck, text
, vector
}:
mkDerivation {
  pname = "protobuf-wire";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring cereal containers deepseq mtl parsec parsers pipes
    pretty proto3-wire QuickCheck safe semigroups text vector
  ];
  testHaskellDepends = [
    base bytestring cereal proto3-wire QuickCheck semigroups tasty
    tasty-hunit tasty-quickcheck text
  ];
  description = "A low level library for writing out data in the Protocol Buffers wire format";
  license = stdenv.lib.licenses.unfree;
}
