{ compiler ? "ghc910"
, enableDhall ? false
, enableSwagger ? true
, swaggerWrapperFormat ? false
}:

let
  lock = builtins.fromJSON (builtins.readFile ./flake.lock);

  flake-compat-input = lock.nodes.flake-compat.locked;

  flake-compat = fetchTarball {
    url = "https://github.com/${flake-compat-input.owner}/${flake-compat-input.repo}/archive/${flake-compat-input.rev}.tar.gz";
    sha256 = flake-compat-input.narHash;
  };

  flake = (import flake-compat { src = ./.; }).defaultNix;

  result = flake.lib.mkProto3Suite {
    system = builtins.currentSystem;

    inherit
      compiler
      enableDhall
      enableSwagger
      swaggerWrapperFormat;
  };

in {
  inherit (result)
    proto3-suite
    proto3-suite-boot;
}
