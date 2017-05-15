{ mkDerivation, base, QuickCheck, stdenv }:
mkDerivation {
  pname = "quickcheck-unicode";
  version = "1.0.0.1";
  sha256 = "1a8nl6x7l9b22yx61wm0bh2n1xzb1hd5i5zgg1w4fpaivjnrrhi4";
  libraryHaskellDepends = [ base QuickCheck ];
  homepage = "https://github.com/bos/quickcheck-unicode";
  description = "Generator and shrink functions for testing Unicode-related software";
  license = stdenv.lib.licenses.bsd3;
}
