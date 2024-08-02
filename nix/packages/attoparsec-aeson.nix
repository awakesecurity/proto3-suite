{ mkDerivation, aeson, base, lib }:
mkDerivation {
  pname = "attoparsec-aeson";
  version = "2.1.0.0";
  sha256 = "b34afb1c330428d9ff7e9e99655ece826ed7a1928dd5880c8127e73a12e906a9";
  libraryHaskellDepends = [ aeson base ];
  doHaddock = false;
  homepage = "https://github.com/haskell/aeson";
  description = "Parsing of aeson's Value with attoparsec";
  license = lib.licenses.bsd3;
}
