{ compiler
, enableDhall
, enableSwagger
, swaggerWrapperFormat
}:

pkgsNew: pkgsOld:

let
  gitignoreSource =
    let
      source = pkgsNew.fetchFromGitHub {
        owner = "hercules-ci";
        repo = "gitignore.nix";
        rev = "a20de23b925fd8264fd7fad6454652e142fd7f73";
        sha256 = "sha256-8DFJjXG8zqoONA1vXtgeKXy68KdJL5UaXR8NtVMUbx8=";
      };
    in (import source { inherit (pkgsNew) lib; }).gitignoreSource;

in {
  haskellPackages = pkgsOld.haskell.packages."${compiler}".override (old: {
    overrides =
      pkgsNew.lib.fold pkgsNew.lib.composeExtensions
        (old.overrides or (_: _: { }))
        [ (pkgsNew.haskell.lib.packagesFromDirectory { directory = ../packages; })

          (haskellPackagesNew: haskellPackagesOld: {

          proto3-wire =
            let
              source = pkgsNew.fetchFromGitHub {
                owner = "awakesecurity";
                repo = "proto3-wire";
                rev = "d4376fb6f1c1ac03ee8ec5c5793700ca6508ea70"; # 1.4.5
                sha256 = "G+MDqooUJvwHLITl2yyDAO31PruPOa9dXh7KIY7vaFk=";
              };
            in
              pkgsNew.haskell.lib.doJailbreak
                (haskellPackagesNew.callCabal2nix "proto3-wire" source { });

          proto3-suite-base =
            let
              cabal2nixFlags = pkgsNew.lib.concatStringsSep " " [
                (if enableDhall then "-fdhall" else "")
                (if enableSwagger then "" else "-f-swagger")
                (if swaggerWrapperFormat then "-fswagger-wrapper-format" else "")
              ];
            in
            (haskellPackagesNew.callCabal2nixWithOptions
              "proto3-suite"
              (gitignoreSource ../../.)
              cabal2nixFlags
              { }
            ).overrideAttrs (oldAttrs: {
              pname = "proto3-suite-base";

              configureFlags = (old.configureFlags or [ ])
                ++ (if enableDhall then [ "-fdhall" ] else [ ])
                ++ (if enableSwagger then [ "" ] else [ "-f-swagger" ])
                ++ (if swaggerWrapperFormat then [ "-fswagger-wrapper-format" ] else [ "" ]);
            });

          proto3-suite-boot =
            pkgsNew.haskell.lib.overrideCabal
              haskellPackagesNew.proto3-suite-base
              (oldArgs: {
                pname = "proto3-suite-boot";

                configureFlags = (oldArgs.configureFlags or [ ])
                  ++ [ "--disable-optimization" ];

                doCheck = false;

                doHaddock = false;

                enableLibraryProfiling = false;

                enableExecutableProfiling = false;
              });

          proto3-suite =
            pkgsNew.haskell.lib.overrideCabal
              haskellPackagesNew.proto3-suite-base
              (oldArgs:
                let
                  inherit (pkgsNew) protobuf;

                  python =
                    pkgsNew.python3.withPackages (pkgs: [ pkgs.protobuf ]);

                  test-files = (gitignoreSource ../../test-files);

                  compile-proto-flags = {
                    typeLevelFormat = true;
                  };

                  cg-artifacts = pkgsNew.runCommand "proto3-suite-test-cg-artifacts" { } ''
                    mkdir -p $out/protos

                    cp -r ${test-files}/. $out/protos/.

                    cd $out

                    build () {
                      echo "[proto3-suite-test-cg-artifacts] Compiling proto-file/$1"
                      ${haskellPackagesNew.proto3-suite-boot}/bin/compile-proto-file \
                        ${pkgsNew.lib.cli.toGNUCommandLineShell {} compile-proto-flags} \
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

                  patchTestScripts = ''
                    echo "Patching test scripts"
                    patchShebangs tests/encode.sh
                    patchShebangs tests/decode.sh
                  '';
                in
                {
                  pname = "proto3-suite";

                  postPatch = (oldArgs.postPatch or "") + copyGeneratedCode + patchTestScripts;

                  testHaskellDepends =
                    (oldArgs.testHaskellDepends or [ ]) ++ [
                      haskellPackagesNew.proto3-suite-boot
                      python
                      protobuf
                    ];

                  shellHook = (oldArgs.shellHook or "") + ''
                    ${copyGeneratedCode}

                    export PATH=${haskellPackagesNew.cabal-install}/bin:${python}/bin:${protobuf}/bin''${PATH:+:}$PATH
                  '';
                }
              );
          })
        ];
  });
}
