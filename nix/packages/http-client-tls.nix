{ mkDerivation, base, bytestring, case-insensitive, containers
, crypton, crypton-connection, data-default-class, exceptions
, gauge, hspec, http-client, http-types, lib, memory, network
, network-uri, text, tls, transformers
}:
mkDerivation {
  pname = "http-client-tls";
  version = "0.3.6.3";
  sha256 = "38dcfc3d772eb6898b4a8856d6159824d13f65eb291733619f625a802dad9095";
  libraryHaskellDepends = [
    base bytestring case-insensitive containers crypton
    crypton-connection data-default-class exceptions http-client
    http-types memory network network-uri text tls transformers
  ];
  testHaskellDepends = [
    base crypton-connection hspec http-client http-types
  ];
  benchmarkHaskellDepends = [ base gauge http-client ];
  doCheck = false;
  homepage = "https://github.com/snoyberg/http-client";
  description = "http-client backend using the connection package and tls library";
  license = lib.licenses.mit;
}
