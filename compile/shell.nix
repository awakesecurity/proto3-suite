{ compiler ? "ghc8104"
}:

let
  pkgs = import ../nix/pkgs.nix {
    inherit compiler;
  };

in pkgs.haskellPackages.proto3-compile.env.overrideAttrs (old: {
  buildInputs = (old.buildInputs or []) ++ [
    pkgs.cabal-install
  ];
})
