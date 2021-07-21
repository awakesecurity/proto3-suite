args:

let
  # nixpkgs release 21.05
  rev = "7e9b0dff974c89e070da1ad85713ff3c20b0ca97";
  sha256 = "1ckzhh24mgz6jd1xhfgx0i9mijk6xjqxwsshnvq789xsavrmsc36";

  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;
  };
in import nixpkgs ({ config = { }; } // args)
