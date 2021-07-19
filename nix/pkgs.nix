{ compiler, enableDhall, enableSwagger }:

import ./nixpkgs.nix {
  overlays = [
    (import ./haskell-packages.nix {
      inherit compiler enableDhall enableSwagger;
    })
  ];
}
