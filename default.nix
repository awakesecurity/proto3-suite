{ compiler ? "ghc884"
, enableDhall ? false
, enableSwagger ? true
}:

let
  pkgs = import ./nix/pkgs.nix { inherit compiler enableDhall enableSwagger; };

in
{
  inherit (pkgs.haskellPackages)
    proto3-suite
    proto3-suite-compile
    proto3-suite-compile-boot
    ;
}
