{ mkDerivation, aeson, base, deepseq, generic-deriving
, generics-sop, lib, microlens, mtl, primitive, QuickCheck
, sop-core, tasty, tasty-hunit, tasty-quickcheck
}:
mkDerivation {
  pname = "large-generics";
  version = "0.2.1";
  sha256 = "3f50991fa9504d5269a4db89548e25c5d969511951ba5adbd60e00125490632b";
  revision = "1";
  editedCabalFile = "0j2dmwf0ccggs25npzrrvz6zpachdbywn2crdlyl1dim3m6psrrh";
  libraryHaskellDepends = [
    aeson base deepseq generics-sop primitive sop-core
  ];
  testHaskellDepends = [
    aeson base generic-deriving generics-sop microlens mtl QuickCheck
    sop-core tasty tasty-hunit tasty-quickcheck
  ];
  description = "Generic programming API for large-records and large-anon";
  license = lib.licenses.bsd3;
}
