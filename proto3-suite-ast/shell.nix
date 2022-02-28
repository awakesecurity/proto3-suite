{ fast ? false
, compiler ? "ghc8104"
, enableDhall ? false
, enableSwagger ? false
, swaggerWrapperFormat ? false
}:

let
  pkgs = import ../nix/pkgs.nix {
    inherit compiler enableDhall enableSwagger swaggerWrapperFormat;
  };
in
  pkgs.haskellPackages.proto3-suite-ast.env
