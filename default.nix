{ compiler ? "ghc8104"
, enableDhall ? false
, enableSwagger ? true
, swaggerWrapperFormat ? false
, enableLargeRecords ? false
}:

let
  pkgs = import ./nix/pkgs.nix {
    inherit compiler enableDhall enableSwagger swaggerWrapperFormat enableLargeRecords;
  };

in {
  proto3-suite-boot = pkgs.haskellPackages.proto3-suite-boot;

  proto3-suite = pkgs.haskellPackages.proto3-suite;
}
