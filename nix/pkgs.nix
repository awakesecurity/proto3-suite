{ compiler
, enableDhall
, enableSwagger
, swaggerWrapperFormat
, enableLargeRecords
}:

import ./nixpkgs.nix {
  overlays = [
    (import ./overlays/haskell-packages.nix {
      inherit compiler enableDhall enableSwagger swaggerWrapperFormat enableLargeRecords;
    })
  ];
}
