{
  description = "proto3-suite: Haskell protobuf tooling";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    flake-utils.url = "github:numtide/flake-utils";

    proto3-wire-src = {
      url = "github:awakesecurity/proto3-wire/796950b8b5f4bca67cb4d632639e55a64d10c001";
      flake = false;
    };

    gitignore-nix = {
      url = "github:hercules-ci/gitignore.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, proto3-wire-src, gitignore-nix, flake-compat }:
    let
      defaultCompiler = "ghc910";

      defaultEnableDhall = false;
      defaultEnableSwagger = true;
      defaultSwaggerWrapperFormat = false;

      mkPkgs =
        { system
        , compiler ? defaultCompiler
        , enableDhall ? defaultEnableDhall
        , enableSwagger ? defaultEnableSwagger
        , swaggerWrapperFormat ? defaultSwaggerWrapperFormat
        }:
        import ./nix/pkgs.nix {
          nixpkgs = nixpkgs;
          inherit system compiler enableDhall enableSwagger swaggerWrapperFormat;
          gitignoreSource = gitignore-nix.lib.gitignoreSource;
          inherit proto3-wire-src;
        };
    in
    {
      overlays.default = final: prev:
        (import ./nix/overlays/haskell-packages.nix {
          compiler = defaultCompiler;
          enableDhall = defaultEnableDhall;
          enableSwagger = defaultEnableSwagger;
          swaggerWrapperFormat = defaultSwaggerWrapperFormat;
          gitignoreSource = gitignore-nix.lib.gitignoreSource;
          inherit proto3-wire-src;
        }) final prev;

      lib.mkProto3Suite =
        { system
        , compiler ? defaultCompiler
        , enableDhall ? defaultEnableDhall
        , enableSwagger ? defaultEnableSwagger
        , swaggerWrapperFormat ? defaultSwaggerWrapperFormat
        }:
        let
          pkgs = mkPkgs {
            inherit system compiler enableDhall enableSwagger swaggerWrapperFormat;
          };
        in {
          inherit pkgs;
          proto3-suite = pkgs.haskellPackages.proto3-suite;
          proto3-suite-boot = pkgs.haskellPackages.proto3-suite-boot;
        };
    }
    //
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = mkPkgs { inherit system; };

        mkDevShell = proto3-suite:
          proto3-suite.env.overrideAttrs (old: {
            buildInputs = (old.buildInputs or []) ++ [
              pkgs.protobuf
              pkgs.python3Packages.virtualenv
            ];
          });
      in {
        packages = {
          default = pkgs.haskellPackages.proto3-suite;
          proto3-suite = pkgs.haskellPackages.proto3-suite;
          proto3-suite-boot = pkgs.haskellPackages.proto3-suite-boot;
        };

        devShells = {
          default = mkDevShell pkgs.haskellPackages.proto3-suite;
          fast = mkDevShell pkgs.haskellPackages.proto3-suite-boot;
        };
      }
    );
}
