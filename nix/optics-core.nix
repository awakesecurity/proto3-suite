{ mkDerivation, array, base, containers, indexed-profunctors
, stdenv, transformers
}:
mkDerivation {
  pname = "optics-core";
  version = "0.3";
  sha256 = "165ddaffd7bd0c14a4ee01cfc5d9cfcd91552fa85044810e2033b1735f5e5d3e";
  libraryHaskellDepends = [
    array base containers indexed-profunctors transformers
  ];
  description = "Optics as an abstract interface: core definitions";
  license = stdenv.lib.licenses.bsd3;
}
