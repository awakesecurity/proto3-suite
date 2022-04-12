{ compiler
}:

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

  cgArtifacts = { pkgsNew, haskellPackagesNew, part, integrations }:
    let
      test-files = gitignoreSource (../.. + "/${part}/test-files");
      pkgName = "proto3-" + part;
    in
      pkgsNew.runCommand (pkgName + "-test-cg-artifacts") { } ''
        mkdir -p $out/protos

        cp -r ${test-files}/. $out/protos/.

        cd $out

        build () {
          echo "[${pkgName}-test-cg-artifacts] Compiling proto-file/$1"
          ${haskellPackagesNew.proto3-compile}/bin/compile-proto-file \
            --out $out \
            --includeDir "$2" \
            --proto "$1" \
            ${integrations}
        }

        for proto in $(find ${test-files} -name 'test_*.proto'); do
          build ''${proto#${test-files}/} ${test-files}
        done

        echo "[${pkgName}-test-cg-artifacts] Protobuf CG complete"
      '';

  copyGeneratedCode = { pkgsNew }: cgArt: ''
    echo "Copying CG artifacts from ${cgArt} into ./gen/"
    mkdir -p gen
    ${pkgsNew.rsync}/bin/rsync --recursive --checksum ${cgArt}/ gen
    chmod -R u+w gen
  '';

in {
  haskellPackages = pkgsOld.haskell.packages."${compiler}".override (old: {
    overrides =
      pkgsNew.lib.composeExtensions
        (old.overrides or (_: _: { }))
        (haskellPackagesNew: haskellPackagesOld: {
          range-set-list =
            pkgsNew.haskell.lib.overrideCabal
              haskellPackagesOld.range-set-list
              (old: {
                broken = false;
                jailbreak = true;
              });

          proto3-wire =
            let
              source = pkgsNew.fetchFromGitHub {
                owner = "awakesecurity";
                repo = "proto3-wire";
                rev = "e5e0158ceaaa50d258bb86dbf2e6c42d5e16c3c5";
                sha256 = "14r2qm6x4bcaywbi3cypriz4hr8i2v3j4qm61lal6x21p0z9i9ak";
              };
            in haskellPackagesNew.callCabal2nix "proto3-wire" source { };

          proto3-compile =
            haskellPackagesNew.callCabal2nix
              "proto3-compile"
              (gitignoreSource ../../compile)
              { };

          proto3-base =
            pkgsNew.haskell.lib.overrideCabal
              ( haskellPackagesNew.callCabal2nix
                  "proto3-base"
                  (gitignoreSource ../../base)
                  { }
              )
              (oldArgs:
                let
                  inherit (pkgsNew) protobuf;

                  python =
                    pkgsNew.python.withPackages (pkgs: [ pkgs.protobuf ]);

                  cg-artifacts = cgArtifacts {
                    inherit pkgsNew haskellPackagesNew;
                    part = "base";
                    integrations = "";
                  };

                in
                {
                  postPatch = (oldArgs.postPatch or "") +
                    copyGeneratedCode { inherit pkgsNew; } cg-artifacts;

                  testHaskellDepends =
                    (oldArgs.testHaskellDepends or [ ]) ++ [
                      haskellPackagesNew.proto3-compile
                      python
                      protobuf
                    ];

                  shellHook = (oldArgs.shellHook or "") + ''
                    ${copyGeneratedCode { inherit pkgsNew; } cg-artifacts}

                    export PATH=${haskellPackagesNew.cabal-install}/bin:${python}/bin:${protobuf}/bin''${PATH:+:}$PATH
                  '';
                }
              );

          proto3-dhall =
            pkgsNew.haskell.lib.overrideCabal
              ( haskellPackagesNew.callCabal2nix
                  "proto3-dhall"
                  (gitignoreSource ../../dhall)
                  { }
              )
              (oldArgs:
                let
                  inherit (pkgsNew) protobuf;

                  cg-artifacts = cgArtifacts {
                    inherit pkgsNew haskellPackagesNew;
                    part = "dhall";
                    integrations = "--enableDhall";
                  };

                in
                {
                  postPatch = (oldArgs.postPatch or "") +
                    copyGeneratedCode { inherit pkgsNew; } cg-artifacts;

                  shellHook = (oldArgs.shellHook or "") + ''
                    ${copyGeneratedCode { inherit pkgsNew; } cg-artifacts}

                    export PATH=${haskellPackagesNew.cabal-install}/bin''${PATH:+:}$PATH
                  '';
                }
              );

          proto3-swagger =
            pkgsNew.haskell.lib.overrideCabal
              ( haskellPackagesNew.callCabal2nix
                  "proto3-swagger"
                  (gitignoreSource ../../swagger)
                  { }
              )
              (oldArgs:
                let
                  inherit (pkgsNew) protobuf;

                  cg-artifacts = cgArtifacts {
                    inherit pkgsNew haskellPackagesNew;
                    part = "swagger";
                    integrations = "--enableSwagger";
                  };

                in
                {
                  postPatch = (oldArgs.postPatch or "") +
                    copyGeneratedCode { inherit pkgsNew; } cg-artifacts;

                  shellHook = (oldArgs.shellHook or "") + ''
                    ${copyGeneratedCode { inherit pkgsNew; } cg-artifacts}

                    export PATH=${haskellPackagesNew.cabal-install}/bin''${PATH:+:}$PATH
                  '';
                }
              );
        });
  });
}
