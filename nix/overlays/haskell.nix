{ compiler
, enableDhall
, enableSwagger
}:

pkgsNew: pkgsOld:

{
  haskell = pkgsOld.haskell // {
    packages = pkgsOld.haskell.packages // {
      "${compiler}" = pkgsOld.haskell.packages."${compiler}".override (old: {
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
                  { }).overrideAttrs (old: { pname = "proto3-suite-compile-base"; });

              proto3-suite-compile-boot =
                pkgsNew.haskell.lib.overrideCabal
                  haskellPackagesNew.proto3-suite-compile-base
                  (oldArgs: {
                    pname = "proto3-suite-compile-boot";
                    configureFlags = (oldArgs.configureFlags or [ ])
                      ++ [ "--disable-optimization" ]
                      ++ (if enableDhall then [ "-fdhall" ] else [ ])
                      ++ (if enableSwagger then [ "" ] else [ "-f-swagger" ]);
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

                      cg-artifacts = pkgsNew.runCommand "proto3-suite-compile-test-cg-artifacts" { } ''
                        mkdir -p $out/protos

                        cp -r ${test-files}/. $out/protos/.

                        cd $out

                        build () {
                          echo "[proto3-suite-compile-test-cg-artifacts] Compiling proto-file/$1"
                          ${haskellPackagesNew.proto3-suite-compile-boot}/bin/compile-proto-file \
                            --out $out \
                            --includeDir "$2" \
                            --proto "$1"
                        }

                        for proto in $(find ${test-files} -name 'test_*.proto'); do
                          build ''${proto#${test-files}/} ${test-files}
                        done

                        echo "[proto3-suite-compile-test-cg-artifacts] Protobuf CG complete"
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

                    in
                    {
                      pname = "proto3-suite-compile";

                      configureFlags = (oldArgs.configureFlags or [ ])
                        ++ (if enableDhall then [ "-fdhall" ] else [ ])
                        ++ (if enableSwagger then [ "" ] else [ "-f-swagger" ]);

                      postPatch = (oldArgs.postPatch or "") + copyGeneratedCode;

                      testHaskellDepends =
                        (oldArgs.testHaskellDepends or [ ]) ++ [
                          pkgsNew.ghc
                          pkgsNew.protobuf3_1
                          haskellPackagesNew.proto3-suite-compile-boot
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
            [
              manualOverrides
            ];
      });
    };
  };
}
