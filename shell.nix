let fetchNixpkgs = import ./nix/fetchNixpkgs.nix;
    nixpkgs = fetchNixpkgs {
      rev = "fdfd5ab05444c38a006cb107d7d1ee8cb0b15719";
      sha256 = "17hsjpjahl0hff3z2khrcwxygjyyrav2pia3qqlli0sgywfrgf95";
    };
    pkgs = import nixpkgs {};
in
  ((import ./release.nix {}).proto3-suite.env).overrideAttrs (super: rec {
    buildInputs = super.buildInputs ++ [pkgs.wget pkgs.cacert];
  })
