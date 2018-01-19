# To develop with this repository, open a Nix shell with:
#
#     $ nix-shell -A proto3-suite.env release.nix
#
# ... then run `cabal` commands as you would normally do:
#
#     [nix-shell]$ cabal configure --with-gcc=clang --enable-tests
#     [nix-shell]$ cabal build
#     [nix-shell]$ cabal test

let
  config = {
    packageOverrides = pkgs:
      { haskellPackages = pkgs.haskellPackages.override {
          overrides = haskellPackagesNew: haskellPackagesOld: rec {

            aeson =
              pkgs.haskell.lib.dontCheck (haskellPackagesNew.callPackage ./nix/aeson.nix { });

            aeson-pretty =
              haskellPackagesNew.callPackage ./nix/aeson-pretty.nix { };

            cabal-doctest =
              haskellPackagesNew.callPackage ./nix/cabal-doctest.nix { };

            insert-ordered-containers =
              haskellPackagesNew.callPackage ./nix/insert-ordered-containers.nix { };

            neat-interpolation =
              haskellPackagesNew.callPackage ./nix/neat-interpolation.nix { };

            optparse-applicative =
              haskellPackagesNew.callPackage ./nix/optparse-applicative.nix { } ;

            optparse-generic =
              haskellPackagesNew.callPackage ./nix/optparse-generic.nix { } ;

            proto3-suite-no-tests =
              pkgs.haskell.lib.dontCheck
                (haskellPackagesNew.callPackage ./default.nix { });

            proto3-suite =
              pkgs.haskell.lib.overrideCabal
                (haskellPackagesNew.callPackage ./default.nix { })
                (oldAttrs: {
                    patches = [ tests/tests.patch ];

                    postPatch = ''
                      substituteInPlace tests/encode.sh --replace @ghc@ ${pkgs.ghc} --replace @bash@ ${pkgs.bash}
                      substituteInPlace tests/decode.sh --replace @ghc@ ${pkgs.ghc} --replace @bash@ ${pkgs.bash}
                    '';

                    testHaskellDepends = oldAttrs.testHaskellDepends ++ [
                      pkgs.ghc
                      proto3-suite-no-tests
                      pkgs.protobuf3_1
                      pkgs.python
                    ];
                  }
                );

            proto3-wire =
              haskellPackagesNew.callPackage ./nix/proto3-wire.nix { };

            scientific =
              pkgs.haskell.lib.dontCheck haskellPackagesOld.scientific;

            swagger2 =
              pkgs.haskell.lib.dontCheck
                (pkgs.haskell.lib.dontHaddock
                  (haskellPackagesNew.callPackage ./nix/swagger2.nix { }));

            turtle =
              haskellPackagesNew.callPackage ./nix/turtle.nix { } ;

          };
        };
      };

    allowUnfree = true;
  };

  bootstrap = import <nixpkgs> { };

  nixpkgs =
    let
      json = builtins.fromJSON (builtins.readFile ./nixpkgs.json);
    in
      bootstrap.fetchFromGitHub {
        owner = "NixOS";

        repo = "nixpkgs";

        inherit (json) rev sha256;
      };

   linuxPkgs = import nixpkgs { inherit config; system = "x86_64-linux" ; };
  darwinPkgs = import nixpkgs { inherit config; system = "x86_64-darwin"; };
        pkgs = import nixpkgs { inherit config; };

in
  { proto3-suite-linux    =     linuxPkgs.haskellPackages.proto3-suite;
    proto3-suite-darwin   =    darwinPkgs.haskellPackages.proto3-suite;
    proto3-suite          =          pkgs.haskellPackages.proto3-suite;
    proto3-suite-no-tests = pkgs.haskellPackages.proto3-suite-no-tests;
  }
