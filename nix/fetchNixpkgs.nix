{ rev                             # The Git revision of nixpkgs to fetch
, outputSha256 ? null             # The SHA256 fixed-output hash
, system ? builtins.currentSystem # This is overridable if necessary
}:

# In Nix 1.12, we can just give a `sha256` to `builtins.fetchTarball`.
builtins.fetchTarball {
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
  sha256 = outputSha256;
}
