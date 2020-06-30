{ mkDerivation, base, deepseq, stdenv }:
mkDerivation {
  pname = "sop-core";
  version = "0.5.0.1";
  sha256 = "dac367f1608c9bd6c5dd1697e2a30e1b760617023b96e1df7d44c6c017999db0";
  libraryHaskellDepends = [ base deepseq ];
  description = "True Sums of Products";
  license = stdenv.lib.licenses.bsd3;
}
