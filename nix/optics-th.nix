{ mkDerivation, base, containers, mtl, optics-core, stdenv, tagged
, template-haskell, th-abstraction, transformers
}:
mkDerivation {
  pname = "optics-th";
  version = "0.3";
  sha256 = "6580c3089c7d575a343a5325a033fae91ebc7d91e51e37b9962f589962e413cd";
  libraryHaskellDepends = [
    base containers mtl optics-core template-haskell th-abstraction
    transformers
  ];
  testHaskellDepends = [ base optics-core tagged ];
  description = "Optics construction using TemplateHaskell";
  license = stdenv.lib.licenses.bsd3;
}
