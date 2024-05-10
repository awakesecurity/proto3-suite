{ mkDerivation, base, lib, parsec }:
mkDerivation {
  pname = "parsec-class";
  version = "1.0.1.0";
  sha256 = "068686c03627ffca77128a762de295c4a43095b9e8dbe3829efc91fed00c418c";
  libraryHaskellDepends = [ base parsec ];
  homepage = "https://github.com/peti/parsec-class";
  description = "Class of types that can be constructed from their text representation";
  license = lib.licenses.mit;
}
