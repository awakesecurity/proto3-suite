args:

let
  nixpkgs = builtins.fetchTarball {
    # from: https://hydra.nixos.org/job/nixos/release-24.11/nixpkgs.tarball
    # build: https://hydra.nixos.org/build/284195557
    # commit: cbd8ec4de4469333c82ff40d057350c30e9f7d36
    url = "https://hydra.nixos.org/build/284195557/download/2/nixpkgs-24.11pre712431.cbd8ec4de446.tar.xz";
    sha256 = "0ljq084fq784fgvm7n9081dmnjhksz20vwzca2zics0kkkzjxh5k";
  };
in import nixpkgs ({ config = { }; } // args)
