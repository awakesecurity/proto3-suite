# To develop iteratively within this repository, open a Nix shell via:
#
#     $ nix-shell -A proto3-suite.env release.nix
#
# ... and then use `cabal` to build and test:
#
#     [nix-shell]$ cabal configure --enable-tests
#     [nix-shell]$ cabal build
#     [nix-shell]$ cabal test

let
  nixpkgs = import ./nixpkgs/17_09.nix;

  config = { allowUnfree = true; };

  overlays = [
    (newPkgs: oldPkgs: {
      haskellPackages = oldPkgs.haskellPackages.override {
        overrides = newPkgs.lib.fold newPkgs.lib.composeExtensions (_: _: {}) [
        
        (newHaskellPkgs: oldHaskellPkgs: 

         let

           mk-proto3-suite = proto3-suite-boot:
             newPkgs.haskell.lib.overrideCabal
                (newHaskellPkgs.callPackage ./default.nix { })
                (oldArgs:
                  let
                    python = newPkgs.python.withPackages (pkgs: [ pkgs.protobuf3_0 ]);

                    test-files = ./test-files;

                    cg-artifacts = newPkgs.runCommand "proto3-suite-test-cg-artifacts" {} ''
                      mkdir -p $out/protos

                      cp -r ${test-files}/. $out/protos/.

                      cd $out

                      build () {
                        echo "[proto3-suite-test-cg-artifacts] Compiling proto-file/$1"
                        ${proto3-suite-boot}/bin/compile-proto-file \
                          --out $out \
                          --includeDir "$2" \
                          --proto "$1"
                      }

                      for proto in $(find ${test-files} -name 'test_*.proto'); do
                        build ''${proto#${test-files}/} ${test-files}
                      done

                      echo "[proto3-suite-test-cg-artifacts] Protobuf CG complete"
                    '';

                  in rec {

                     postPatch = (oldArgs.preCheck or "") + ''
                       echo "Copying CG  artifacts from ${cg-artifacts} into ./gen/"
                       mkdir -p gen
                       ${newPkgs.rsync}/bin/rsync \
                         --recursive \
                         --checksum \
                         ${cg-artifacts}/ gen
                       chmod -R u+w gen
                     '';

                     shellHook = (oldArgs.shellHook or "") +
                       (let
                          ghc = newHaskellPkgs.ghcWithPackages (pkgs:
                            oldArgs.testHaskellDepends ++ [ proto3-suite-boot ]
                          );
                        in ''
                          ${postPatch}

                          export PATH=${newHaskellPkgs.cabal-install}/bin:${ghc}/bin:${python}/bin''${PATH:+:}$PATH
                        '');

                     # We need to add dhall as a dependency because
                     # cabal2nix won't add it in the generated Nix
                     # code because we guard the dhall dependency with
                     # a cabal configure flag
                     buildDepends = (oldArgs.libraryHaskellDepends or []) ++ [
                       newHaskellPkgs.dhall
                     ];

                     testHaskellDepends = (oldArgs.testHaskellDepends or []) ++ [
                       newPkgs.ghc
                       proto3-suite-boot
                       python
                       newHaskellPkgs.dhall
                     ];
                   }
              );

         in rec {

          # The test suite for proto3-suite requires:
          #
          #   - a GHC with `proto3-suite` installed, since our code generation
          #     tests compile and run generated code; since this custom GHC is
          #     also used inside the nix-shell environment for iterative
          #     development, we ensure that it is available on $PATH and that
          #     all test suite deps are available to it
          #
          #   - a Python interpreter with a protobuf package installed, which we
          #     use as a reference implementation; we also put expose this on
          #     the `nix-shell` $PATH
          #
          # Finally, we make `cabal` available in the `nix-shell`, intentionally
          # occluding any globally-installed versions of the tool.

          proto3-suite = mk-proto3-suite proto3-suite-boot;

          proto3-suite-dhall =
            newPkgs.haskell.lib.overrideCabal
              (mk-proto3-suite proto3-suite-dhall-boot)
              (oldArgs: {
                configureFlags = (oldArgs.configureFlags or []) ++ [ "-fdhall" ];
              });

          # A proto3-suite sans tests, for bootstrapping
          proto3-suite-boot =
            newPkgs.haskell.lib.overrideCabal
              (newHaskellPkgs.callPackage ./default.nix { })
              (oldArgs: {
                 configureFlags = (oldArgs.configureFlags or []) ++ [ "--disable-optimization" ];
                 doCheck        = false;
                 doHaddock      = false;

                 # We need to add dhall as a dependency because
                 # cabal2nix won't add it in the generated Nix code
                 # because we guard the dhall dependency with a cabal
                 # configure flag
                 buildDepends = (oldArgs.libraryHaskellDepends or []) ++ [
                   newHaskellPkgs.dhall
                 ];

                 testHaskellDepends = (oldArgs.testHaskellDepends or []) ++ [
                   newHaskellPkgs.dhall
                 ];
               });

          proto3-suite-dhall-boot =
            newPkgs.haskell.lib.overrideCabal
              (newHaskellPkgs.callPackage ./default.nix { })
              (oldArgs: {
                 configureFlags = (oldArgs.configureFlags or []) ++ [ "--disable-optimization" "-fdhall" ];
                 doCheck        = false;
                 doHaddock      = false;

                 # We need to add dhall as a dependency because
                 # cabal2nix won't add it in the generated Nix code
                 # because we guard the dhall dependency with a cabal
                 # configure flag
                 buildDepends = (oldArgs.libraryHaskellDepends or []) ++ [
                   newHaskellPkgs.dhall
                 ];

                 testHaskellDepends = (oldArgs.testHaskellDepends or []) ++ [
                   newHaskellPkgs.dhall
                 ];
               });

          proto3-wire =
            newHaskellPkgs.callPackage ./nix/proto3-wire.nix { };

          dhall =
            newHaskellPkgs.callPackage ./nix/dhall.nix { };

          formatting =
            newHaskellPkgs.callPackage ./nix/formatting.nix { };

          prettyprinter =
            newHaskellPkgs.callPackage ./nix/prettyprinter.nix { };

          megaparsec =
            newHaskellPkgs.callPackage ./nix/megaparsec.nix { };

          swagger2 =
            newPkgs.haskell.lib.dontCheck
              (newPkgs.haskell.lib.dontHaddock
                (newHaskellPkgs.callPackage ./nix/swagger2.nix { }));
        })
      ];
    };
  }) ];
in

let
   linuxPkgs = import nixpkgs { inherit config overlays; system = "x86_64-linux" ; };
  darwinPkgs = import nixpkgs { inherit config overlays; system = "x86_64-darwin"; };
        pkgs = import nixpkgs { inherit config overlays; };
in
  { proto3-suite-linux  =  linuxPkgs.haskellPackages.proto3-suite;
    proto3-suite-darwin = darwinPkgs.haskellPackages.proto3-suite;
    proto3-suite        =       pkgs.haskellPackages.proto3-suite;
    proto3-suite-boot   =       pkgs.haskellPackages.proto3-suite-boot;

    proto3-suite-dhall      =       pkgs.haskellPackages.proto3-suite-dhall;
    proto3-suite-dhall-boot =       pkgs.haskellPackages.proto3-suite-dhall-boot;
  }
