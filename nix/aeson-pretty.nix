{ mkDerivation, aeson, attoparsec, base, base-compat, bytestring
, cmdargs, scientific, stdenv, text, unordered-containers, vector
}:
mkDerivation {
  pname = "aeson-pretty";
  version = "0.8.2";
  sha256 = "1c5r1w1hcv297pmj9yjpz9al22k3mh61gimi37wddga02212kd3c";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base base-compat bytestring scientific text
    unordered-containers vector
  ];
  executableHaskellDepends = [
    aeson attoparsec base bytestring cmdargs
  ];
  homepage = "http://github.com/informatikr/aeson-pretty";
  description = "JSON pretty-printing library and command-line tool";
  license = stdenv.lib.licenses.bsd3;
}
