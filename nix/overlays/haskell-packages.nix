{ compiler, enableDhall, enableSwagger }:

pkgsNew: pkgsOld:

let
  gitignoreSource =
    let
      source = pkgsNew.fetchFromGitHub {
        owner = "hercules-ci";
        repo = "gitignore.nix";
        rev = "211907489e9f198594c0eb0ca9256a1949c9d412";
        sha256 = "06j7wpvj54khw0z10fjyi31kpafkr6hi1k0di13k1xp8kywvfyx8";
      };
    in (import source { inherit (pkgsNew) lib; }).gitignoreSource;

in {
  haskellPackages = pkgsOld.haskell.packages."${compiler}".override (old: {
    overrides =
      pkgsOld.lib.composeExtensions
        (old.overrides or (_: _: {}))
        (haskellPackagesNew: haskellPackagesOld:
          let
            proto3-suite-flags = (if enableDhall then ["-fdhall"] else []) ++ (if enableSwagger then [] else ["-f-swagger"]);

          in {
            range-set-list =
              pkgsOld.haskell.lib.overrideCabal
                (haskellPackagesOld.callHackage "range-set-list" "0.1.3.1" { })
                (oldArgs: {
                  broken = false;
                  jailbreak = true;
                });

            haskeline = haskellPackagesOld.haskeline_0_8_1_2;

            proto3-wire =
              let
                source = pkgsNew.fetchFromGitHub {
                  owner = "awakesecurity";
                  repo = "proto3-wire";
                  rev = "e5e0158ceaaa50d258bb86dbf2e6c42d5e16c3c5";
                  sha256 = "14r2qm6x4bcaywbi3cypriz4hr8i2v3j4qm61lal6x21p0z9i9ak";
                };
              in haskellPackagesNew.callCabal2nix "proto3-wire" source { };

            proto3-suite-base =
              (haskellPackagesNew.callCabal2nixWithOptions
                "proto3-suite"
                (gitignoreSource ../../.)
                (pkgsNew.lib.concatStringsSep " " proto3-suite-flags)
                { }
              ).overrideAttrs (oldAttrs: {
                pname = "proto3-suite-base";

                buildInputs =
                  (oldAttrs.buildInputs or [])
                  ++ (if enableDhall then [haskellPackagesNew.dhall] else [])
                  ++ (if enableSwagger then [haskellPackagesNew.swagger] else []);
              });

            proto3-suite-boot =
              pkgsNew.haskell.lib.overrideCabal
                haskellPackagesNew.proto3-suite-base
                (oldArgs: {
                  pname = "proto3-suite-boot";

                  configureFlags = (old.configureFlags or []) ++ ["--disable-optimization"];

                  doCheck = false;

                  doHaddock = false;

                  enableLibraryProfiling = false;

                  enableExecutableProfiling = false;
                });

            proto3-suite =
              let
                python = pkgsNew.python.withPackages (pkgs: [
                  pkgs.protobuf
                ]);

                test-files = (gitignoreSource ../../test-files);

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
                  echo "Copying CG artifacts from ${cg-artifacts} into ./gen/"
                  mkdir -p gen
                  ${pkgsNew.rsync}/bin/rsync \
                    --recursive \
                    --checksum \
                    ${cg-artifacts}/ gen
                  chmod -R u+w gen
                '';

              in pkgsNew.haskell.lib.overrideCabal haskellPackagesNew.proto3-suite-base (oldArgs:
                let
                  ghc =
                    haskellPackagesNew.ghcWithPackages
                      (pkgs: (oldArgs.testHaskellDepends or [ ]) ++ [
                        haskellPackagesNew.proto3-suite-boot
                      ]);

                in {
                  testHaskellDepends =
                    (oldArgs.testHaskellDepends or []) ++ [
                      pkgsNew.ghc
                      pkgsNew.protobuf3_1
                      pkgsNew.protobuf
                      python
                    ];

                  postPatch = (oldArgs.postPatch or "") + copyGeneratedCode;

                  shellHook = (oldArgs.shellHook or "") + ''
                    ${copyGeneratedCode}

                    export PATH=${haskellPackagesNew.cabal-install}/bin:${ghc}/bin:${python}/bin:${pkgsNew.protobuf}/bin''${PATH:+:}$PATH
                  '';
                });
          });
  });
}
