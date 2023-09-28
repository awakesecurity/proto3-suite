args:

let
  nixpkgs = builtins.fetchTarball {
    # release: nixpkgs-23.05pre491123.261abe8a44a7
    # commit: 261abe8a44a7e8392598d038d2e01f7b33cf26d0
    url = "https://hydra.nixos.org/build/236149912/download/2/nixpkgs-23.05pre491123.261abe8a44a7.tar.xz";
    sha256 = "0yhf6zbnkj3a7wfas5clli5qk4xl0cw5zq5w4fzvd724za5nb04f";
  };
in import nixpkgs ({ config = { }; } // args)
