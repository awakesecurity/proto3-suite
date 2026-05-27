{ nixpkgs
, system
, compiler
, enableDhall
, enableSwagger
, swaggerWrapperFormat
, gitignoreSource
, proto3-wire-src
}:

import nixpkgs {
  inherit system;
  config = { };
  overlays = [
    (import ./overlays/haskell-packages.nix {
      inherit
        compiler
        enableDhall
        enableSwagger
        swaggerWrapperFormat
        gitignoreSource
        proto3-wire-src;
    })
  ];
}
