{ mkDerivation, aeson, aeson-pretty, ansi-terminal, base
, bytestring, case-insensitive, cborg, cborg-json, containers
, contravariant, criterion, cryptonite, deepseq, Diff, directory
, doctest, dotgen, exceptions, filepath, haskeline, http-client
, http-client-tls, http-types, lens-family-core, megaparsec, memory
, mockery, mtl, optparse-applicative, parsers, prettyprinter
, prettyprinter-ansi-terminal, QuickCheck, quickcheck-instances
, repline, scientific, serialise, stdenv, tasty, tasty-hunit
, tasty-quickcheck, template-haskell, text, transformers
, unordered-containers, uri-encode, vector
}:
mkDerivation {
  pname = "dhall";
  version = "1.20.1";
  sha256 = "e077e8f4945484db4e35e8ae422e9eabac1b155972602f0404d818e3e185bcdc";
  revision = "2";
  editedCabalFile = "0629z8lc97rapfcqcgvxwp9x4x3xqpzrly8m0nsn0dds7400jxrk";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-pretty ansi-terminal base bytestring case-insensitive
    cborg cborg-json containers contravariant cryptonite Diff directory
    dotgen exceptions filepath haskeline http-client http-client-tls
    http-types lens-family-core megaparsec memory mtl
    optparse-applicative parsers prettyprinter
    prettyprinter-ansi-terminal repline scientific serialise
    template-haskell text transformers unordered-containers uri-encode
    vector
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base containers deepseq directory doctest filepath mockery
    prettyprinter QuickCheck quickcheck-instances serialise tasty
    tasty-hunit tasty-quickcheck text transformers vector
  ];
  benchmarkHaskellDepends = [
    base bytestring containers criterion directory serialise text
  ];
  description = "A configuration language guaranteed to terminate";
  license = stdenv.lib.licenses.bsd3;
}
