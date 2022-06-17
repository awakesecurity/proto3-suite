{ mkDerivation, ansi-wl-pprint, async, base, bytestring, clock
, containers, directory, doctest, exceptions, filepath, foldl
, hostname, lib, managed, optional-args, optparse-applicative
, process, stm, streaming-commons, tasty, tasty-bench, tasty-hunit
, temporary, text, time, transformers, unix, unix-compat
}:
mkDerivation {
  pname = "turtle";
  version = "1.6.1";
  sha256 = "2795445c93a4b646dd02b68ebf4006f8ec3588c85ccfe9d47810597e638e3b9c";
  libraryHaskellDepends = [
    ansi-wl-pprint async base bytestring clock containers directory
    exceptions filepath foldl hostname managed optional-args
    optparse-applicative process stm streaming-commons temporary text
    time transformers unix unix-compat
  ];
  testHaskellDepends = [
    base doctest filepath tasty tasty-hunit temporary
  ];
  benchmarkHaskellDepends = [ base tasty-bench text ];
  description = "Shell programming, Haskell-style";
  license = lib.licenses.bsd3;
}
