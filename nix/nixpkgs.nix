args:

let
  nixpkgs = builtins.fetchTarball {
    # from: https://hydra.nixos.org/job/nixos/trunk-combined/nixpkgs.tarball
    # build: https://hydra.nixos.org/build/314100404
    # commit: 117cc7f94e8072499b0a7aa4c52084fa4e11cc9b
    url =
      "https://hydra.nixos.org/build/314100404/download/2/nixpkgs-25.11pre899548.117cc7f94e80.tar.xz";
    sha256 = "1zjs02gl3kjlyx65m9jz5pm3f08s8pm1wbsx3h99cifvqbl6gb2a";
  };
in import nixpkgs ({ config = { }; } // args)
