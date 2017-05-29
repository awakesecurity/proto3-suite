{ mkDerivation, base, bytestring, cereal, containers, deepseq
, filepath, haskell-src, mtl, optparse-generic, parsec, parsers
, pretty, proto3-wire, QuickCheck, safe, semigroups, stdenv
, system-filepath, tasty, tasty-hunit, tasty-quickcheck, text
, transformers, turtle, vector
}:
mkDerivation {
  pname = "proto3-suite";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring cereal containers deepseq filepath haskell-src mtl
    parsec parsers pretty proto3-wire QuickCheck safe semigroups text
    transformers vector
  ];
  executableHaskellDepends = [
    base optparse-generic system-filepath turtle
  ];
  testHaskellDepends = [
    base bytestring cereal proto3-wire QuickCheck semigroups tasty
    tasty-hunit tasty-quickcheck text transformers turtle
  ];
  description = "A low level library for writing out data in the Protocol Buffers wire format";
  license = stdenv.lib.licenses.asl20;
}
