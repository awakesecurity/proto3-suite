{ mkDerivation, base, bytestring, filepath, lib, Only
, optparse-applicative, text, time, transformers
, transformers-compat, void
}:
mkDerivation {
  pname = "optparse-generic";
  version = "1.5.2";
  sha256 = "c7b451f32d34124aab838fadaab4cf2e271e558ddc13a702458b6c790a2e8a35";
  revision = "1";
  editedCabalFile = "190nlp7dh878232ia2nsl75q6bzr62szl1vcyinz528lmdbnbpdc";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring filepath Only optparse-applicative text time
    transformers transformers-compat void
  ];
  executableHaskellDepends = [ base ];
  description = "Auto-generate a command-line parser for your datatype";
  license = lib.licenses.bsd3;
}
