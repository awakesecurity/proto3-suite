args:

let
  nixpkgs = builtins.fetchTarball {
    # build: https://hydra.nixos.org/build/258096332
    # commit: 0638fe2715d998fa81d173aad264eb671ce2ebc1
    url = "https://hydra.nixos.org/build/258096332/download/2/nixpkgs-23.11pre558121.0638fe2715d9.tar.xz";
    sha256 = "1z3s1hqg3b72g608pf9sv474d4y9s00p86nsvfw5i9xgwhjncjjb";
  };
in import nixpkgs ({ config = { }; } // args)
