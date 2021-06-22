{ compiler ? "ghc884", enableDhall ? false }:

let
  config = { allowBroken = true; };

  overlays = [
    (import ./nix/overlays/haskell.nix { inherit compiler enableDhall; })
  ];

  pkgs = import ./nix/nixpkgs.nix { inherit config overlays; };

  linuxPkgs =
    import ./nix/nixpkgs.nix { inherit config overlays; system = "x86_64-linux" ; };

  darwinPkgs =
    import ./nix/nixpkgs.nix { inherit config overlays; system = "x86_64-darwin"; };

in
  { inherit (pkgs.haskell.packages."${compiler}")
      proto3-suite-boot proto3-suite;
  }
