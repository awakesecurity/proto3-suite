{ proto3-suite-no-tests
, bash
, ghc
, protobuf3_0
, python
, python_protobuf3_0
, writeText
}:

attrs@
{ mkDerivation, base, bytestring, cereal, containers, deepseq
, filepath, haskell-src, mtl, parsec, parsers, pipes, pretty
, proto3-wire, QuickCheck, safe, semigroups, stdenv, tasty
, tasty-hunit, tasty-quickcheck, text, transformers, turtle, vector
}:

let
  mkDerivation' = oldAttrs: mkDerivation (oldAttrs // {
    patches = [ tests/tests.patch ];

    postPatch = ''
      substituteInPlace tests/encode.sh --replace @ghc@ ${ghc} --replace @bash@ ${bash}
      substituteInPlace tests/decode.sh --replace @ghc@ ${ghc} --replace @bash@ ${bash}
    '';

    testHaskellDepends = oldAttrs.testHaskellDepends ++ [
      ghc proto3-suite-no-tests protobuf3_0 python python_protobuf3_0
    ];

    doCheck = true;
  });

in import ./default.nix (attrs // { mkDerivation = mkDerivation'; })
