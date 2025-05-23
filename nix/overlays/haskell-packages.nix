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

          # With nixpkgs-23.11 and ghc981, adjunctions wants hspec for testing,
          # which causes problems.
          adjunctions =
            pkgsNew.haskell.lib.dontCheck haskellPackagesOld.adjunctions;

          # With nixpkgs-23.11 and ghc981, aeson-2.1.2.1 thinks that th-abstraction is out of bounds.
          #
          # And we disable tests because explicitly specifying aeson-2.1.2.1
          # seems to trigger a test failure, at least on GHC 9.4.8 and 9.8.1;
          # perhaps somewhere in nixpkgs the test is suppressed and
          # overriding the Nix definition re-enables testing?
          #
          #       encodeDouble:                                FAIL
          #         *** Failed! Falsified (after 15 tests and 2 shrinks):
          #         1.0
          #         0.0
          #         "\"+inf\"" /= "null"
          #         Use --quickcheck-replay=305830 to reproduce.
          #         Use -p '/encodeDouble/' to rerun this test only.
          #
          aeson =
            pkgsNew.haskell.lib.doJailbreak
              (pkgsNew.haskell.lib.dontCheck haskellPackagesOld.aeson);

          # With nixpkgs-23.11 and ghc981, atomic-write wants hspec for testing,
          # which causes problems.
          #
          # With nixpkgs-24.11 and our overrides, atomic-write thinks that
          # filepath is out of bounds.
          atomic-write =
            pkgsNew.haskell.lib.doJailbreak
              (pkgsNew.haskell.lib.dontCheck haskellPackagesOld.atomic-write);

          # With nixpkgs-23.11 and ghc981, base-compat-batteries wants hspec for testing,
          # which causes problems.
          base-compat-batteries =
            pkgsNew.haskell.lib.dontCheck haskellPackagesOld.base-compat-batteries;

          # With nixpkgs-23.11 and ghc981, base-orphans wants hspec for testing,
          # which causes problems.
          base-orphans =
            pkgsNew.haskell.lib.dontCheck haskellPackagesOld.base-orphans;

          # With nixpkgs-23.11 and ghc981, bifunctors wants hspec for testing,
          # which causes problems.
          bifunctors =
            pkgsNew.haskell.lib.dontCheck haskellPackagesOld.bifunctors;

          # With nixpkgs-23.11 and ghc981, conduit wants hspec for testing,
          # which causes problems.
          conduit =
            pkgsNew.haskell.lib.dontCheck haskellPackagesOld.conduit;

          # With nixpkgs-23.11 and ghc981, constraints wants hspec for testing,
          # which causes problems.
          constraints =
            pkgsNew.haskell.lib.dontCheck haskellPackagesOld.constraints;

          # With nixpkgs-23.11 and ghc981, data-diverse wants hspec for testing,
          # which causes problems.
          data-diverse =
            pkgsNew.haskell.lib.dontCheck haskellPackagesOld.data-diverse;

          # With nixpkgs-23.11 and ghc981, distribution-nixpkgs wants hspec for testing,
          # which causes problems.
          distribution-nixpkgs =
            pkgsNew.haskell.lib.dontCheck haskellPackagesOld.distribution-nixpkgs;

          # With nixpkgs-23.11 and ghc981, distributive wants hspec for testing,
          # which causes problems.
          distributive =
            pkgsNew.haskell.lib.dontCheck haskellPackagesOld.distributive;

          # With ghc981, doctest-0.22.2 complains about the version of the base
          # package and depends on hspec for testing, which causes problems.
          doctest =
            pkgsNew.haskell.lib.dontCheck
              (pkgsNew.haskell.lib.doJailbreak haskellPackagesOld.doctest);

          # With nixpkgs-23.11 and ghc981, generic-deriving wants hspec for testing,
          # which causes problems.  Also, it generic-deriving thinks that
          # th-abstraction is out of bounds.
          generic-deriving =
            pkgsNew.haskell.lib.dontCheck
              (pkgsNew.haskell.lib.doJailbreak haskellPackagesOld.generic-deriving);

          # With nixpkgs-23.11 and ghc981, half thinks that deepseq is out of bounds.
          half =
            pkgsNew.haskell.lib.doJailbreak haskellPackagesOld.half;

          # With nixpkgs-23.11 and ghc981, hourglass does not support the version
          # of the time package that is provided, but that matters only to tests.
          hourglass =
            pkgsNew.haskell.lib.dontCheck haskellPackagesOld.hourglass;

          # With nixpkgs-23.11 and ghc981, hpack-0.36.0 wants hspec for testing,
          # which causes problems.
          hpack =
            pkgsNew.haskell.lib.dontCheck haskellPackagesOld.hpack;

          # With nixpkgs-23.11 and ghc981, http-types wants hspec for testing,
          # which causes problems.
          http-types =
            pkgsNew.haskell.lib.dontCheck haskellPackagesOld.http-types;

          # With nixpkgs-23.11 and ghc981, infer-license wants hspec for testing,
          # which causes problems.
          infer-license =
            pkgsNew.haskell.lib.dontCheck haskellPackagesOld.infer-license;

          # With nixpkgs-23.11 and our overrides, insert-ordered-containers thinks that lens is out of bounds.
          insert-ordered-containers =
            pkgsNew.haskell.lib.doJailbreak haskellPackagesOld.insert-ordered-containers;

          # With nixpkgs-23.11 and ghc981, invariant indirectly depends on hspec for testing,
          # which causes problems. Also, it generic-deriving thinks that
          # th-abstraction is out of bounds.
          invariant =
            pkgsNew.haskell.lib.dontCheck
              (pkgsNew.haskell.lib.doJailbreak haskellPackagesOld.invariant);

          # With nixpkgs-23.11 and ghc981, iproute wants hspec for testing,
          # which causes problems.
          iproute =
            pkgsNew.haskell.lib.dontCheck haskellPackagesOld.iproute;

          # With nixpkgs-23.11 and ghc962, generics-sop-0.5.1.4 thinks that th-abstraction is out of bounds.
          generics-sop =
            pkgsNew.haskell.lib.doJailbreak haskellPackagesOld.generics-sop;

          # With nixpkgs-24.11 and our overrides, lens thinks that template-haskell is out of bounds.
          lens =
            pkgsNew.haskell.lib.doJailbreak haskellPackagesOld.lens;

          # With nixpkgs-23.11 and ghc981 (or perhaps our customized dependencies),
          # the tests in lifted-base fail.
          lifted-base =
            pkgsNew.haskell.lib.dontCheck haskellPackagesOld.lifted-base;

          # With nixpkgs-23.11 and ghc981, monad-par wants test-framework for testing, which
          # wants language-haskell-extract, which does not support modern template-haskell.
          monad-par =
            pkgsNew.haskell.lib.dontCheck haskellPackagesOld.monad-par;

          # With nixpkgs-23.11 and ghc981, mono-traversable wants hspec for testing,
          # which causes problems.
          mono-traversable =
            pkgsNew.haskell.lib.dontCheck haskellPackagesOld.mono-traversable;

          # With nixpkgs-23.11 and our overrides, neat-interpolation that rebase is out of bounds.
          neat-interpolation =
            pkgsNew.haskell.lib.doJailbreak haskellPackagesOld.neat-interpolation;

          # With GHC 9.0/9.2, "network-uri" with testing enabled would find that
          # "th-compat" wants an older version of "directory" than does "process".
          network-uri =
            pkgsNew.haskell.lib.dontCheck haskellPackagesOld.network-uri;

          # With nixpkgs-24.11 and our overrides, repline thinks that containers is out of bounds.
          repline =
            pkgsNew.haskell.lib.doJailbreak haskellPackagesOld.repline;

          # With nixpkgs-24.11 and our overrides, serialise thinks that base is out of bounds.
          serialise =
            pkgsNew.haskell.lib.doJailbreak haskellPackagesOld.serialise;

          # With nixpkgs-23.11 and ghc981, safe-exceptions wants hspec for testing,
          # which causes problems.
          safe-exceptions =
            pkgsNew.haskell.lib.dontCheck haskellPackagesOld.safe-exceptions;

          # With nixpkgs-23.11 and ghc981, streaming-commons wants hspec for testing,
          # which causes problems.
          streaming-commons =
            pkgsNew.haskell.lib.dontCheck haskellPackagesOld.streaming-commons;

          # With nixpkgs-23.11 and our dependency overrides, swagger2 wants hspec for testing,
          # which causes problems. Also, we jailbreak to allow a newer version of lens.
          swagger2 =
            pkgsNew.haskell.lib.dontCheck
              (pkgsNew.haskell.lib.doJailbreak haskellPackagesOld.swagger2);

          # With nixpkgs-24.11 and our overrides, optics-extras thinks that containers is out of bounds.
          optics-extra =
            pkgsNew.haskell.lib.doJailbreak haskellPackagesOld.optics-extra;

          # With nixpkgs-24.11 and our overrides, optics-th thinks that containers and template-haskell are out of bounds.
          optics-th =
            pkgsNew.haskell.lib.doJailbreak haskellPackagesOld.optics-th;

          # With nixpkgs-23.11 and ghc981, reflection indirectly depends on hspec for testing,
          # which causes problems.
          reflection =
            pkgsNew.haskell.lib.dontCheck haskellPackagesOld.reflection;

          # With nixpkgs-23.11 and ghc981, resourceat wants hspec for testing,
          # which causes problems.
          resourceat =
            pkgsNew.haskell.lib.dontCheck haskellPackagesOld.resourceat;

          # With nixpkgs-23.11 and ghc981, resourcet wants hspec for testing,
          # which causes problems.
          resourcet =
            pkgsNew.haskell.lib.dontCheck haskellPackagesOld.resourcet;

          # With nixpkgs-23.11 and ghc981, tasty-discover wants hspec for testing,
          # which causes problems.
          tasty-discover =
            pkgsNew.haskell.lib.dontCheck haskellPackagesOld.tasty-discover;

          # Suppress:
          #   warning: non-portable path to file '"dist/build/Test/autogen/cabal_macros.h"'; specified path differs in case from file name on disk [-Wnonportable-include-path]
          tasty-golden =
            pkgsNew.haskell.lib.appendConfigureFlags haskellPackagesOld.tasty-golden
              [ "--ghc-option=-Wno-nonportable-include-path" ];

          # With nixpkgs-23.11 and ghc981, text-metrics wants hspec for testing,
          # which causes problems.
          text-metrics =
            pkgsNew.haskell.lib.dontCheck haskellPackagesOld.text-metrics;

          # With nixpkgs-23.11 and ghc981, th-compat wants hspec for testing,
          # which causes problems.
          th-compat =
            pkgsNew.haskell.lib.dontCheck haskellPackagesOld.th-compat;

          # With nixpkgs-23.11 and our overrides, th-lift thinks that th-abstraction is out of bounds.
          th-lift =
            pkgsNew.haskell.lib.doJailbreak haskellPackagesOld.th-lift;

          # With nixpkgs-24.11 and our overrides, turtle thinks that filepath is out of bounds.
          turtle =
            pkgsNew.haskell.lib.doJailbreak haskellPackagesOld.turtle;

          # With nixpkgs-23.11 and ghc981, unix-compat wants hspec for testing,
          # which causes problems.
          unix-compat =
            pkgsNew.haskell.lib.dontCheck haskellPackagesOld.unix-compat;

          # With nixpkgs-23.11 and ghc981, hpack-0.36.0 wants hspec for testing,
          # which causes problems.
          unix-time =
            pkgsNew.haskell.lib.dontCheck haskellPackagesOld.unix-time;

          # With nixpkgs-23.11 and ghc981, yaml wants hspec for testing,
          # which causes problems.
          yaml =
            pkgsNew.haskell.lib.dontCheck haskellPackagesOld.yaml;

          # With nixpkgs-23.11 and ghc962, proto3-wire thinks
          # that doctest and transformers are out of bounds.
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
