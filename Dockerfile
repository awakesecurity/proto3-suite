FROM nixos/nix:2.3

ADD . /opt
WORKDIR /opt

RUN nix-env --file default.nix --install --attr proto3-compile
RUN nix-env --file default.nix --install --attr proto3-base
RUN nix-env --file default.nix --install --attr proto3-dhall
RUN nix-env --file default.nix --install --attr proto3-swagger

ENTRYPOINT ["compile-proto-file"]
