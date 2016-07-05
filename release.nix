# To build this repository with `nix` you run:
#
#     $ nix-build release.nix
#
# If you update the `.cabal` file (such as changing dependencies or adding new
# library/executable/test/benchmark sections), then update the `default.nix`
# expression by running:
#
#     $ cabal2nix . > default.nix
#
# If you want to update the `proto3-wire` dependency to the latest git revision,
# then run:
#
#     $ nix-prefetch-git https://github.com/awakenetworks/proto3-wire.git
#
# ... and modify the `rev` and `sha256` fields of the corresponding `fetchgit`
# expression below using the output of the `nix-prefetch-git` command.
#
# If you want to test a local `proto3-wire` repository, then replace the
# `fetchgit { ... }` expression with the relative path to the source repository
# such as:
#
#     let proto3-wire-src = ../proto3-wire;
#     in
#     ...
let config =
    {   packageOverrides = pkgs:
        {   haskellPackages = pkgs.haskellPackages.override
            {   overrides = self: _:
                {   proto3-wire   =
                        let proto3-wire-src =
                            pkgs.fetchgit
                            {   url    = "https://github.com/awakenetworks/proto3-wire.git";
                                rev    = "937a13799467dcfa4ea64811ae6536e5751d11d5";
                                sha256 = "174s8z8vsxh17fk72qv600n3vhdbsf1sg1pmi05lpjy6hmx2rkfa";
                            };
                        in
                        self.callPackage proto3-wire-src { };
                    protobuf-wire =
                        self.callPackage ./default.nix { };
                };
            };
        };

        allowUnfree = true;
    };
in
{ pkgs ? import <nixpkgs> { inherit config; } }:
pkgs.haskellPackages.protobuf-wire
