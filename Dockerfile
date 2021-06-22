FROM nixos/nix:2.3

ADD . /opt
WORKDIR /opt

RUN nix-env --file default.nix --install --attr proto3-suite

ENTRYPOINT ["compile-proto-file"]
