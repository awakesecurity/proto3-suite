{ mkDerivation, base, containers, deepseq, hashable, stdenv, tasty
, tasty-quickcheck
}:
mkDerivation {
  pname = "range-set-list";
  version = "0.1.3.1";
  sha256 = "12e8d9cb99a2847da32934ed7f44a5acedaa59d8fa19eff0f46aa77921460c55";
  revision = "1";
  editedCabalFile = "0ma1gxmk2in2fj4rxhwshy2zq690ylw1zz0c9cnyin8mxkp96inc";
  libraryHaskellDepends = [ base containers deepseq hashable ];
  testHaskellDepends = [
    base containers deepseq hashable tasty tasty-quickcheck
  ];
  homepage = "https://github.com/phadej/range-set-list#readme";
  description = "Memory efficient sets with ranges of elements";
  license = stdenv.lib.licenses.mit;
}
