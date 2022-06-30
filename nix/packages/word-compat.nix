{ mkDerivation, base, ghc-prim, lib
}:
mkDerivation {
  pname = "word-compat";
  version = "0.0.2";
  sha256 = "164zpk3cqw9i9mblz0cdshgk1382pcnbakr7pxamz4r0gyhs3r9n";
  libraryHaskellDepends = [
    base ghc-prim
  ];
  testHaskellDepends = [
    base
  ];
  description = "This package offers a workaround for the breaking change in Word/Int. Import GHC.Word.Compat in place of GHC.Word to take effect.
";
  license = lib.licenses.bsd3;
}
