{ mkDerivation, array, base, bytestring, containers, hashable
, indexed-profunctors, indexed-traversable-instances, lib, mtl
, optics-core, text, transformers, unordered-containers, vector
}:
mkDerivation {
  pname = "optics-extra";
  version = "0.4.2.1";
  sha256 = "7e23a7a325e3448354614d3d958279c9ac2fdd0831ceee2808830e7a962fca41";
  revision = "2";
  editedCabalFile = "0w7am7pvwg4mx0nrksxgr2rvnk99nhqfpj80ig21m9xbpxsh22x4";
  libraryHaskellDepends = [
    array base bytestring containers hashable indexed-profunctors
    indexed-traversable-instances mtl optics-core text transformers
    unordered-containers vector
  ];
  description = "Extra utilities and instances for optics-core";
  license = lib.licenses.bsd3;
}
