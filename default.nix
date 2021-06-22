{ compiler ? "ghc884", enableDhall ? false }:

let
  config = { allowBroken = true; };

  overlay =
    pkgsNew: pkgsOld: {
      haskell = pkgsOld.haskell // {
        packages = pkgsOld.haskell.packages // {
          "${compiler}" = pkgsOld.haskell.packages."${compiler}".override (old: {
            overrides =
              let
                directoryOverrides =
                  pkgsNew.haskell.lib.packagesFromDirectory {
                    directory = ./nix;
                  };

                manualOverrides = haskellPackagesNew: haskellPackagesOld: {
                  parameterized =
                    pkgsNew.haskell.lib.dontCheck
                      haskellPackagesOld.parameterized;

                  proto3-suite-base =
                    let
                      cabal2nixFlags = if enableDhall then "-fdhall" else "";
                    in
                      haskellPackagesNew.callCabal2nixWithOptions
                        "proto3-suite"
                        ./.
                        cabal2nixFlags
                        { };

                  proto3-suite-boot =
                    pkgsNew.haskell.lib.overrideCabal
                      haskellPackagesNew.proto3-suite-base
                      (oldArgs: {
                         configureFlags = (oldArgs.configureFlags or [])
                           ++ [ "--disable-optimization" ]
                           ++ (if enableDhall then [ "-fdhall" ] else []);
                         doCheck        = false;
                         doHaddock      = false;
                       }
                      );

                  proto3-suite =
                    pkgsNew.haskell.lib.overrideCabal
                      haskellPackagesNew.proto3-suite-base
                      (oldArgs:
                        let
                          inherit (pkgsNew) protobuf;

                          python = pkgsNew.python.withPackages
                            (pkgs: [ pkgs.protobuf]);

                          ghc =
                            haskellPackagesNew.ghcWithPackages
                            (pkgs: (oldArgs.testHaskellDepends or []) ++ [
                              haskellPackagesNew.proto3-suite-boot
                            ]);

                          test-files = ./test-files;

                          cg-artifacts = pkgsNew.runCommand "proto3-suite-test-cg-artifacts" {} ''
                            mkdir -p $out/protos

                            cp -r ${test-files}/. $out/protos/.

                            cd $out

                            build () {
                              echo "[proto3-suite-test-cg-artifacts] Compiling proto-file/$1"
                              ${haskellPackagesNew.proto3-suite-boot}/bin/compile-proto-file \
                                --out $out \
                                --includeDir "$2" \
                                --proto "$1"
                            }

                            for proto in $(find ${test-files} -name 'test_*.proto'); do
                              build ''${proto#${test-files}/} ${test-files}
                            done

                            echo "[proto3-suite-test-cg-artifacts] Protobuf CG complete"
                          '';

                          copyGeneratedCode = ''
                            echo "Copying CG  artifacts from ${cg-artifacts} into ./gen/"
                            mkdir -p gen
                            ${pkgsNew.rsync}/bin/rsync \
                              --recursive \
                              --checksum \
                              ${cg-artifacts}/ gen
                            chmod -R u+w gen
                          '';

                        in rec {
                           configureFlags = (oldArgs.configureFlags or [])
                             ++ (if enableDhall then [ "-fdhall" ] else []);

                           postPatch = (oldArgs.postPatch or "") + copyGeneratedCode;

                           testHaskellDepends =
                             (oldArgs.testHaskellDepends or []) ++ [
                               pkgsNew.ghc
                               pkgsNew.protobuf3_1
                               haskellPackagesNew.proto3-suite-boot
                               python
                               protobuf
                             ];

                           shellHook = (oldArgs.shellHook or "") + ''
                             ${copyGeneratedCode}

                             export PATH=${haskellPackagesNew.cabal-install}/bin:${ghc}/bin:${python}/bin:${protobuf}/bin''${PATH:+:}$PATH
                           '';
                         }
                      );
                };

              in
                pkgsNew.lib.foldr pkgsNew.lib.composeExtensions (old.overrides or (_: _: { }))
                  [ directoryOverrides
                    manualOverrides
                  ];
          });
        };
      };
    };

  overlays = [ overlay ];

  pkgs = import ./nix/nixpkgs.nix { inherit config overlays; };

  linuxPkgs =
    import ./nix/nixpkgs.nix { inherit config overlays; system = "x86_64-linux" ; };

  darwinPkgs =
    import ./nix/nixpkgs.nix { inherit config overlays; system = "x86_64-darwin"; };

in
  { inherit (pkgs.haskell.packages."${compiler}")
      proto3-suite-boot proto3-suite;
  }
