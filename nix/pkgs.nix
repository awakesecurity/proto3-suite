{ compiler ? "ghc884"
, enableDhall ? false
, enableSwagger ? true
}:

let
  haskellOverlay = import ./overlays/haskell.nix {
    inherit
      compiler
      enableDhall
      enableSwagger
      ;
  };

in
import ./nixpkgs.nix {
  config = { };

  overlays = [
    haskellOverlay
  ];
}
