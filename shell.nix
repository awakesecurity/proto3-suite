# Entering the shell for `proto3-suite` requires rebuilding `proto3-suite`
# when basically _any_ file changes, which makes development painful.
#
# If you don't need to run the test suite, you can forgo test dependencies
# and skip `compile-proto-file` codegen by running:
#
#   $ nix-shell --arg fast true
#
{ fast ? false
, compiler ? "ghc8104"
, enableDhall ? false
, enableSwagger ? true
, swaggerWrapperFormat ? false
, enableLargeRecords ? false
}:

let
  pkgs = import ./nix/pkgs.nix {
    inherit compiler enableDhall enableSwagger swaggerWrapperFormat enableLargeRecords;
  };

  proto3-suite =
    if fast then
      pkgs.haskellPackages.proto3-suite-boot
    else
      pkgs.haskellPackages.proto3-suite;

in proto3-suite.env.overrideAttrs (old: {
  buildInputs = (old.buildInputs or []) ++ [
    pkgs.cabal-install
    pkgs.python36Packages.virtualenv
    pkgs.ghcid
  ];
})
