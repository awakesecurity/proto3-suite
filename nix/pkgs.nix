{ compiler, enableDhall, enableSwagger }:

import ./nixpkgs.nix {
  overlays = [
    (import ./overlays/haskell-packages.nix {
      inherit compiler enableDhall enableSwagger;
    })
  ];
}
