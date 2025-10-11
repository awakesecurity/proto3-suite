args:

let
  nixpkgs = builtins.fetchTarball {
    # from: https://releases.nixos.org/nixos/unstable/nixos-25.11pre868392.e9f00bd89398
    # build: https://hydra.nixos.org/build/308438307
    # commit: e9f00bd893984bc8ce46c895c3bf7cac95331127
    url = "https://hydra.nixos.org/build/308438307/download/2/nixpkgs-25.11pre868392.e9f00bd89398.tar.xz";
    sha256 = "1n08xvypbyygjzrrkyna06sjbxg987jvmvdib5qmzp8avx6dlprb";
  };
in import nixpkgs ({ config = { }; } // args)
