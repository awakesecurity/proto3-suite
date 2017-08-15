{ mkDerivation, base, base-prelude, HTF, parsec, stdenv
, template-haskell, text
}:
mkDerivation {
  pname = "neat-interpolation";
  version = "0.3.2.1";
  sha256 = "0550dy0vwh81byi9bxhdzqx5y9lnvkwj5rbks5rbj2fylhyf8c2m";
  libraryHaskellDepends = [
    base base-prelude parsec template-haskell text
  ];
  testHaskellDepends = [ base-prelude HTF ];
  homepage = "https://github.com/nikita-volkov/neat-interpolation";
  description = "A quasiquoter for neat and simple multiline text interpolation";
  license = stdenv.lib.licenses.mit;
}
