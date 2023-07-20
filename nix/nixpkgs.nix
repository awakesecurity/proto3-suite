args:

let
  # nixos-22.05 as on 2023-07-17
  rev = "380be19fbd2d9079f677978361792cb25e8a3635";
  sha256 = "154x9swf494mqwi4z8nbq2f0sp8pwp4fvx51lqzindjfbb9yxxv5";

  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;
  };
in import nixpkgs ({ config = { }; } // args)
