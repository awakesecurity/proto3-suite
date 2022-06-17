{ mkDerivation, ansi-wl-pprint, async, base, bytestring, clock
, containers, directory, doctest, exceptions, filepath, foldl
, hostname, lib, managed, optional-args, optparse-applicative
, process, stm, streaming-commons, tasty, tasty-bench, tasty-hunit
, temporary, text, time, transformers, unix, unix-compat
}:
mkDerivation {
  pname = "turtle";
  version = "1.6.0";
  sha256 = "8551a72f62ece4e53a51209b184a555dc5f8af5155da09698e35ea3f58a265bc";
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
