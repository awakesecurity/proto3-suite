From nixos/nix:2.3

Add . /opt
Workdir /opt

Run nix-env --install --attr proto3-suite -f release.nix

Entrypoint ["compile-proto-file"]
