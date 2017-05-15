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
      let
        python_protobuf3_0 =
          (pkgs.pythonPackages.protobufBuild pkgs.protobuf3_0).override {
            doCheck = false;
          };
      in
        { haskellPackages = pkgs.haskellPackages.override {
            overrides = haskellPackagesNew: haskellPackagesOld: rec {

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
                        pkgs.protobuf3_0
                        pkgs.python
                        python_protobuf3_0
                      ];
                    }
                  );

              # The following overrides to use QuickCheck 2.9 are needed because we want
              # to use a newer aeson than the one in our pinned `nixpkgs`, and so must
              # bump QuickCheck to 2.9 in a few places due to some of its transitive
              # dependencies. However, we must do this *without* using QuickCheck-2.9
              # everywhere, as we'll get some nasty conflicts from packages which depend
              # on QuickCheck < 2.9. Yay, nix!

              aeson = haskellPackagesNew.callPackage ./nix/aeson.nix { };

              integer-logarithms = haskellPackagesNew.callPackage ./nix/integer-logarithms.nix {
                QuickCheck = haskellPackagesNew.callPackage ./nix/QuickCheck-2.9.nix { };
              };

              proto3-wire = haskellPackagesNew.callPackage ./nix/proto3-wire.nix {
                QuickCheck = haskellPackagesNew.callPackage ./nix/QuickCheck-2.9.nix { };
              };

              quickcheck-unicode = haskellPackagesNew.callPackage ./nix/quickcheck-unicode.nix {
                QuickCheck = haskellPackagesNew.callPackage ./nix/QuickCheck-2.9.nix { };
              };

              scientific = pkgs.haskell.lib.dontCheck haskellPackagesOld.scientific;

              tasty-quickcheck = haskellPackagesNew.callPackage ./nix/tasty-quickcheck.nix {
                QuickCheck = haskellPackagesNew.callPackage ./nix/QuickCheck-2.9.nix { };
              };

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
  { proto3-suite-linux  =  linuxPkgs.haskellPackages.proto3-suite;
    proto3-suite-darwin = darwinPkgs.haskellPackages.proto3-suite;
    proto3-suite        =       pkgs.haskellPackages.proto3-suite;
  }
