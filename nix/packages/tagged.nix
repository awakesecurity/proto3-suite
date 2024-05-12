{ mkDerivation, base, deepseq, lib, template-haskell, transformers
}:
mkDerivation {
  pname = "tagged";
  version = "0.8.8";
  sha256 = "a083fa7835516203c168433a1c8dfc0290a94b05fedab566ad0640fc9137a6a7";
  revision = "1";
  editedCabalFile = "0chbxdppgpsrjqzf28z53x9wqwz0ncfimhfc6rr9knixvvxxx4wi";
  libraryHaskellDepends = [
    base deepseq template-haskell transformers
  ];
  homepage = "http://github.com/ekmett/tagged";
  description = "Haskell 98 phantom types to avoid unsafely passing dummy arguments";
  license = lib.licenses.bsd3;
}
