{ mkDerivation, base, comonad, containers, distributive, exceptions
, indexed-traversable, lib, mtl, profunctors, semigroupoids
, template-haskell, th-abstraction, transformers, transformers-base
}:
mkDerivation {
  pname = "free";
  version = "5.2";
  sha256 = "72867f7c89173263765736e8d395e94291f1aaea626ecb1d673d72ce90b94f89";
  revision = "4";
  editedCabalFile = "0vic3p2viip8gjww8fx19ax6ry7y34h7xclvhzkvmbspjh9d219x";
  libraryHaskellDepends = [
    base comonad containers distributive exceptions indexed-traversable
    mtl profunctors semigroupoids template-haskell th-abstraction
    transformers transformers-base
  ];
  homepage = "http://github.com/ekmett/free/";
  description = "Monads for free";
  license = lib.licenses.bsd3;
}
