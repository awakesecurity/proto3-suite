{ mkDerivation, async, base, bytestring, filepath, singleton-bool
, stdenv, tasty, tasty-expected-failure, tasty-hunit, temporary
}:
mkDerivation {
  pname = "lukko";
  version = "0.1.1.2";
  sha256 = "8a79d113dc0ccef16c24d83379cc457485943027e777529c46362fecc06607d2";
  libraryHaskellDepends = [ base ];
  testHaskellDepends = [
    async base bytestring filepath singleton-bool tasty
    tasty-expected-failure tasty-hunit temporary
  ];
  description = "File locking";
  license = "GPL-2.0-or-later AND BSD-3-Clause";
}
