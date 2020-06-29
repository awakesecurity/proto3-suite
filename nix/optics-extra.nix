{ mkDerivation, array, base, bytestring, containers, hashable
, indexed-profunctors, mtl, optics-core, stdenv, text, transformers
, unordered-containers, vector
}:
mkDerivation {
  pname = "optics-extra";
  version = "0.3";
  sha256 = "a4d6155814111a5c6ce89640915ba5987296fec3cecc766ece3d4112abfd7697";
  libraryHaskellDepends = [
    array base bytestring containers hashable indexed-profunctors mtl
    optics-core text transformers unordered-containers vector
  ];
  description = "Extra utilities and instances for optics-core";
  license = stdenv.lib.licenses.bsd3;
}
