{ mkDerivation, base, pcre-light, QuickCheck, stdenv, tagged, tasty
, tasty-hunit
}:
mkDerivation {
  pname = "tasty-quickcheck";
  version = "0.8.4";
  sha256 = "15rjxib5jmjq0hzj47x15kgp3awc73va4cy1pmpf7k3hvfv4qprn";
  libraryHaskellDepends = [ base QuickCheck tagged tasty ];
  testHaskellDepends = [ base pcre-light tasty tasty-hunit ];
  homepage = "http://documentup.com/feuerbach/tasty";
  description = "QuickCheck support for the Tasty test framework";
  license = stdenv.lib.licenses.mit;
}
