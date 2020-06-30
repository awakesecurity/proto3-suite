{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "refact";
  version = "0.3.0.2";
  sha256 = "0ad029727797c8ca5d179c7abf1bfc135d86a7d72cf93785ee12ad243aeb1f6c";
  libraryHaskellDepends = [ base ];
  description = "Specify refactorings to perform with apply-refact";
  license = stdenv.lib.licenses.bsd3;
}
