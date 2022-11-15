{ mkDerivation, base, extra, filepath, ghc, lib, record-hasfield
, uniplate
}:
mkDerivation {
  pname = "record-dot-preprocessor";
  version = "0.2.14";
  sha256 = "9788c5b21a6f981141924a2a9c148738b08c2b64dc0430b124e182312f48181e";
  revision = "1";
  editedCabalFile = "03sp3wkvl1x68pcjdrkxf4ys73x4ka4sz7x75icy4xd285zrzqb0";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base extra ghc uniplate ];
  executableHaskellDepends = [ base extra ];
  testHaskellDepends = [ base extra filepath record-hasfield ];
  homepage = "https://github.com/ndmitchell/record-dot-preprocessor#readme";
  description = "Preprocessor to allow record.field syntax";
  license = lib.licenses.bsd3;
}
