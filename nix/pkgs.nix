{ compiler
, enableDhall
, enableSwagger
, swaggerWrapperFormat
}:

import ./nixpkgs.nix {
  overlays = [
    (import ./overlays/haskell-packages.nix {
      inherit compiler enableDhall enableSwagger swaggerWrapperFormat;
    })
  ];
}
