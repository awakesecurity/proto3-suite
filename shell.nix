{ compiler ? "ghc884"
, enableDhall ? false
, enableSwagger ? true
}:

let
  pkgs = import ./nix/pkgs.nix {
    inherit compiler enableDhall enableSwagger;
  };

in
pkgs.haskellPackages.shellFor {
  packages = p: [
    p.proto3-suite
    p.proto3-suite-compile-base
  ];

  buildInputs = [
    pkgs.cabal-install
    pkgs.ghcid
  ] ++ pkgs.haskellPackages.proto3-suite-compile-base.env.buildInputs;
}
