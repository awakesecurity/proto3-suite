{ compiler ? "ghc884"
, enableDhall ? false
, enableSwagger ? true
}:

let
  pkgs = import ./nix/pkgs.nix {
    inherit compiler enableDhall enableSwagger;
  };

in {
  proto3-suite = pkgs.haskell.lib.buildStrictly pkgs.haskellPackages.proto3-suite;
}
