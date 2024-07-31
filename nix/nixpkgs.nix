args:

let
  nixpkgs = builtins.fetchTarball {
    # from: https://hydra.nixos.org/job/nixos/release-24.05/nixpkgs.tarball
    # build: https://hydra.nixos.org/build/262713240
    # commit: cc54fb41d13736e92229c21627ea4f22199fee6b
    url = "https://hydra.nixos.org/build/262713240/download/2/nixpkgs-24.05pre631579.cc54fb41d137.tar.xz";
    sha256 = "01jgwy8ixfnl7dig5qhih34lrx2g9xpj64yr49n0lyvpwj44s475";
  };
in import nixpkgs ({ config = { }; } // args)
