{ mkDerivation, base, deepseq, network, stdenv }:
mkDerivation {
  pname = "network-bsd";
  version = "2.8.1.0";
  sha256 = "d94961ca15c42c798d19cde540ec12b25cc43435fb95e682399d6c1a02022d4e";
  revision = "3";
  editedCabalFile = "1hc3jdbmpq2wxh82xfx452v2m2l97jbdaqqbmj5nz4lljxa2696r";
  libraryHaskellDepends = [ base deepseq network ];
  homepage = "https://github.com/haskell/network-bsd";
  description = "POSIX network database (<netdb.h>) API";
  license = stdenv.lib.licenses.bsd3;
}
