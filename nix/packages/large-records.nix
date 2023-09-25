{ mkDerivation, base, containers, generic-deriving, ghc
, large-generics, lib, mtl, newtype, primitive
, record-dot-preprocessor, record-hasfield, syb, tasty, tasty-hunit
, template-haskell, transformers
}:
mkDerivation {
  pname = "large-records";
  version = "0.4";
  sha256 = "de51d5f473ca9e57b818221e9bf776e6f68ca90e5cbca1171dc14ce690a7093c";
  libraryHaskellDepends = [
    base containers ghc large-generics mtl primitive
    record-dot-preprocessor record-hasfield syb template-haskell
    transformers
  ];
  testHaskellDepends = [
    base generic-deriving large-generics mtl newtype
    record-dot-preprocessor record-hasfield tasty tasty-hunit
    template-haskell transformers
  ];
  description = "Efficient compilation for large records, linear in the size of the record";
  license = lib.licenses.bsd3;
}
