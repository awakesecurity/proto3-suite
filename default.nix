{ compiler ? "ghc8104"
}:

let
  pkgs = import ./nix/pkgs.nix {
    inherit compiler;
  };

in {
  proto3-compile = pkgs.haskellPackages.proto3-compile;

  proto3-base = pkgs.haskellPackages.proto3-base;

  proto3-dhall = pkgs.haskellPackages.proto3-dhall;

  proto3-swagger = pkgs.haskellPackages.proto3-swagger;
}
