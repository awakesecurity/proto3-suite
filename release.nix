# To develop iteratively within this repository, open a Nix shell via:
#
#     $ nix-shell
#
# ... and then use `cabal` to build and test:
#
#     [nix-shell]$ cabal configure --enable-tests
#     [nix-shell]$ cabal test
#
# Note that nix-shell will actually build most of the code in order to
# allow testing of code generation.  You might wish to temporarily edit
# shell.nix to specify "proto3-suite-boot" instead of "proto3-suite" in
# order to get things to compile, then switch back to enable testing.

{ compiler ? "ghc865", enableDhall ? false }:

let
  fetchNixpkgs = import ./nix/fetchNixpkgs.nix;

  nixpkgsRelease = "20.03";
  unpatchedNixpkgs = fetchNixpkgs {
    rev    = "fdfd5ab05444c38a006cb107d7d1ee8cb0b15719";
    sha256 = "17hsjpjahl0hff3z2khrcwxygjyyrav2pia3qqlli0sgywfrgf95";
  };

  config = { };

  upgrade = packageList: haskellPackagesNew: haskellPackagesOld:
    let op = name: {
          inherit name;
          value = haskellPackagesNew.callPackage (./nix + "/${name}.nix") { };
        };
    in
      builtins.listToAttrs (builtins.map op packageList);

  dontCheck = haskell: packageList: haskellPackagesNew: haskellPackagesOld:
    let op = name: {
          inherit name;
          value = haskell.lib.dontCheck haskellPackagesOld.${name};
        };
    in
      builtins.listToAttrs (builtins.map op packageList);

  jailbreak = haskell: packageList: haskellPackagesNew: haskellPackagesOld:
    let op = name: {
          inherit name;
          value = haskell.lib.doJailbreak haskellPackagesOld.${name};
        };
    in
      builtins.listToAttrs (builtins.map op packageList);

  patch = haskell: packageList: haskellPackagesNew: haskellPackagesOld:
    let op = name: {
          inherit name;
          value = haskell.lib.appendPatch haskellPackagesOld.${name}
                                          (./nix + "/${name}.patch");
        };
    in
      builtins.listToAttrs (builtins.map op packageList);

  composeExtensionList = lib: lib.foldr lib.composeExtensions (_: _: {});

  overlays = [
    (pkgsNew: pkgsOld: rec {
      haskell = pkgsOld.haskell // {
        packages = pkgsOld.haskell.packages // {
          "${compiler}" = pkgsOld.haskell.packages."${compiler}".override {
            overrides =
              let
                ghcVer = pkgsOld.haskell.packages.${compiler}.ghc.version;
                geVer = v: xs:
                  if builtins.compareVersions ghcVer v >= 0 then xs else [];
                rangeVer = lowInclusive: highExclusive: xs:
                  if builtins.compareVersions ghcVer lowInclusive >= 0 &&
                     builtins.compareVersions ghcVer highExclusive < 0
                    then xs
                    else [];

                upgradeOverrides = upgrade
                  ( [ "generic-random"
                      "parameterized"
                      "proto3-wire"
                    ] ++ rangeVer "8.8" "8.10" [
                      "network-bsd"
                    ] ++ geVer "8.8" [
                      "dhall"
                      "haskell-src"
                      "insert-ordered-containers"
                      "prettyprinter"
                      "swagger2"
                    ] ++ geVer "8.10" [
                      "assoc"
                      "cabal-install"
                      "cborg"
                      "cborg-json"
                      "ChasingBottoms"
                      "ChasingBottoms"
                      "comonad"
                      "contravariant-extras"
                      "cryptohash-sha256"
                      "dec"
                      "Diff"
                      "distributive"
                      "doctest"
                      "ed25519"
                      "generics-sop"
                      "hackage-security"
                      "HTTP"
                      "http-media"
                      "inspection-testing"
                      "language-haskell-extract"
                      "lens"
                      "lukko"
                      "memory"
                      "optics-core"
                      "optics-extra"
                      "optics-th"
                      "polyparse"
                      "quickcheck-instances"
                      "range-set-list"
                      "refact"
                      "repline"
                      "resolv"
                      "semigroupoids"
                      "serialise"
                      "singleton-bool"
                      "sop-core"
                      "these"
                      "tls"
                      "turtle"
                      "vector-th-unbox"
                    ]
                  );

                patchOverrides = patch haskell
                  ([ "parameterized" ] ++ geVer "8.10.1" [ "cabal-install" ]);

                dontCheckOverrides = dontCheck haskell
                  [ "cryptohash-sha256"
                    "dhall"
                    "doctest"
                    "ed25519"
                    "http-media"
                    "network-uri"
                  ];

                jailbreakOverrides = jailbreak haskell
                  [ "cabal-install"
                    "haskell-src"
                    "language-haskell-extract"
                    "refact"
                    "system-fileio"
                  ];

                manualOverrides = haskellPackagesNew: haskellPackagesOld: rec {
                  proto3-suite-base =
                    let cabal2nixFlags = if enableDhall then "-fdhall" else "";
                    in haskellPackagesNew.callCabal2nixWithOptions
                         "proto3-suite" ./. cabal2nixFlags { };

                  proto3-suite-boot =
                    pkgsNew.haskell.lib.overrideCabal
                      proto3-suite-base
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
                      proto3-suite-base
                      (oldArgs:
                        let
                          protobuf = pkgsNew.protobuf;

                          python = pkgsNew.python.withPackages
                            (pkgs: [ pkgs.protobuf]);

                          ghc =
                            haskellPackagesNew.ghcWithPackages
                              ( pkgs: oldArgs.testHaskellDepends ++
                                      [ proto3-suite-boot ] );

                          test-files = ./test-files;

                          cg-artifacts = pkgsNew.runCommand "proto3-suite-test-cg-artifacts" {} ''
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
                               proto3-suite-boot
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
                composeExtensionList pkgsNew.lib
                  [ upgradeOverrides
                    patchOverrides
                    dontCheckOverrides
                    jailbreakOverrides
                    manualOverrides
                  ];
          };
        };
      };
    })
  ];

  unpatchedPkgs = import unpatchedNixpkgs { inherit config overlays; };

  # https://github.com/NixOS/nixpkgs/pull/85446
  nixpkgs = unpatchedPkgs.stdenvNoCC.mkDerivation {
    name = "nixpkgs-${nixpkgsRelease}-patched";

    src = unpatchedNixpkgs;

    # Backport fix <https://github.com/NixOS/nixpkgs/pull/85446> to 20.03:
    patches = [ ./nix/with-packages-wrapper.patch ];

    phases = [ "unpackPhase" "patchPhase" "installPhase" ];

    installPhase = ''
      mkdir -p $out
      cp -R ./ $out/
    '';

    preferLocalBuild = true;
  };

  linuxPkgs =
    import nixpkgs { inherit config overlays; system = "x86_64-linux" ; };

  darwinPkgs =
    import nixpkgs { inherit config overlays; system = "x86_64-darwin"; };

  pkgs =
    import nixpkgs { inherit config overlays; };

in
  { proto3-suite-linux =
      linuxPkgs.haskell.packages."${compiler}".proto3-suite;

    proto3-suite-darwin =
      darwinPkgs.haskell.packages."${compiler}".proto3-suite;

    inherit (pkgs.haskell.packages."${compiler}")
      proto3-suite-boot proto3-suite;
  }
