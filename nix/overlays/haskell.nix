{ compiler
, enableDhall
, enableSwagger
}:

pkgsNew: pkgsOld:

let
  compile-test-protos_Template =
    { compile-proto-file
    , test-files
    , out
    }:

    pkgsNew.writeShellScriptBin "compile-test-protos" ''
      #!/usr/bin/env bash

      set -euo pipefail

      test_files="${test-files}"

      compile() {
        echo "Compiling '$1'"
        ${compile-proto-file} \
          --proto "$1" \
          --includeDir "$2" \
          --out "${out}"
      }

      mkdir -p "${out}"

      for proto in $(find $test_files -name 'test_*.proto'); do
        compile ''${proto#$test_files/} $test_files
      done
    '';

  compile-test-protos_nixShell = compile-test-protos_Template {
    compile-proto-file = "cabal run proto3-suite-compile:exe:compile-proto-file --";
    test-files = "$(git rev-parse --show-toplevel)/proto3-suite-compile/test-files";
    out = "$(git rev-parse --show-toplevel)/proto3-suite-compile/gen";
  };

  compile-test-protos_nixBuild = compile-test-protos_Template {
    compile-proto-file = "${pkgsNew.haskellPackages.proto3-suite-compile-boot}/bin/compile-proto-file";
    test-files = ../../proto3-suite-compile/test-files;
    out = "$out";
  };

in
{
  haskellPackages = pkgsOld.haskell.packages.${compiler}.override (old: {
    overrides =
      let
        manualOverrides = haskellPackagesNew: haskellPackagesOld: {
          parameterized =
            pkgsNew.haskell.lib.overrideCabal
              haskellPackagesOld.parameterized
              (old: {
                doCheck = false;
                broken = false;
              });

          proto3-wire =
            let
              source = pkgsNew.fetchFromGitHub {
                owner = "awakesecurity";
                repo = "proto3-wire";
                rev = "9b1c178f8a23a5f03237cb77cce403bc386da523";
                sha256 = "0yf4008qrikxmnlcir7nvb7jx23fykjymjiinshb5j3s6kffqqzq";
              };
            in
            haskellPackagesNew.callCabal2nix "proto3-wire" source { };

          proto3-suite =
            let
              cabal2nixFlags = pkgsNew.lib.concatStringsSep " " [
                (if enableDhall then "-fdhall" else "")
                (if enableSwagger then "" else "-f-swagger")
              ];
            in
            haskellPackagesNew.callCabal2nixWithOptions
              "proto3-suite"
              ../../proto3-suite
              cabal2nixFlags
              { };

          proto3-suite-compile-base =
            let
              cabal2nixFlags = pkgsNew.lib.concatStringsSep " " [
                (if enableDhall then "-fdhall" else "")
                (if enableSwagger then "" else "-f-swagger")
              ];
            in
            (haskellPackagesNew.callCabal2nixWithOptions
              "proto3-suite-compile"
              ../../proto3-suite-compile
              cabal2nixFlags
              { }
            ).overrideAttrs (old: {
              pname = "proto3-suite-compile-base";

              configureFlags = (old.configureFlags or [ ])
                ++ (if enableDhall then [ "-fdhall" ] else [ ])
                ++ (if enableSwagger then [ "" ] else [ "-f-swagger" ]);

              passthru = old.passthru // {
                env = old.passthru.env.overrideAttrs (oldEnv: {
                  buildInputs = (oldEnv.buildInputs or [ ]) ++ [
                    compile-test-protos_nixShell
                  ];
                });
              };
            });

          proto3-suite-compile-boot =
            pkgsNew.haskell.lib.overrideCabal
              haskellPackagesNew.proto3-suite-compile-base
              (oldArgs: {
                pname = "proto3-suite-compile-boot";

                configureFlags = (oldArgs.configureFlags or [ ])
                  ++ [ "--disable-optimization" ];

                doCheck = false;

                doHaddock = false;
              });

          proto3-suite-compile =
            pkgsNew.haskell.lib.overrideCabal
              haskellPackagesNew.proto3-suite-compile-base
              (oldArgs:
                let
                  inherit (pkgsNew) protobuf;

                  python =
                    pkgsNew.python.withPackages (pkgs: [ pkgs.protobuf ]);

                  ghc =
                    haskellPackagesNew.ghcWithPackages
                      (pkgs: (oldArgs.testHaskellDepends or [ ]) ++ [
                        haskellPackagesNew.proto3-suite-compile-boot
                      ]);

                  test-files = ../../proto3-suite-compile/test-files;

                in
                {
                  pname = "proto3-suite-compile";

                  postPatch = (oldArgs.postPatch or "") + ''
                    ${compile-test-protos_nixBuild}/bin/test-files-codegen
                  '';

                  testHaskellDepends =
                    (oldArgs.testHaskellDepends or [ ]) ++ [
                      pkgsNew.ghc
                      pkgsNew.protobuf3_1
                      haskellPackagesNew.proto3-suite-compile-boot
                      python
                      protobuf
                    ];

                  shellHook = (oldArgs.shellHook or "") + ''
                    export PATH=${haskellPackagesNew.cabal-install}/bin:${ghc}/bin:${python}/bin:${protobuf}/bin''${PATH:+:}$PATH
                  '';
                }
              );
        };

      in
      pkgsNew.lib.foldr pkgsNew.lib.composeExtensions (old.overrides or (_: _: { }))
        [
          manualOverrides
        ];
  });
}
