{ compiler ? "ghc884"
, enableDhall ? false
, enableSwagger ? true
}:

let
  pkgs = import ./nix/pkgs.nix { inherit compiler enableDhall enableSwagger; };

in
{
  inherit (pkgs.haskell.packages."${compiler}")
    proto3-suite-boot
    proto3-suite
    ;
}
