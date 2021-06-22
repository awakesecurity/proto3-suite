{ compiler ? "ghc884"
, enableDhall ? false
}:

let
  pkgs = import ./nix/nixpkgs.nix {
    config = { allowBroken = true; };

    overlays = [
      (import ./nix/overlays/haskell.nix { inherit compiler enableDhall; })
    ];
  };

in
{
  inherit (pkgs.haskell.packages."${compiler}")
    proto3-suite-boot
    proto3-suite
    ;
}
