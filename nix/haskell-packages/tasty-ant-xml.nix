{ mkDerivation, base, containers, directory, filepath
, generic-deriving, ghc-prim, lib, mtl, stm, tagged, tasty
, transformers, xml
}:
mkDerivation {
  pname = "tasty-ant-xml";
  version = "1.1.7";
  sha256 = "6f2312de739436528c74b1351e2b72edbd4be8d32da7c2a98e73d858b10c7905";
  libraryHaskellDepends = [
    base containers directory filepath generic-deriving ghc-prim mtl
    stm tagged tasty transformers xml
  ];
  homepage = "http://github.com/ocharles/tasty-ant-xml";
  description = "Render tasty output to XML for Jenkins";
  license = lib.licenses.bsd3;
}
