# Entering the shell for `proto3-suite` requires rebuilding `proto3-suite`
# when basically _any_ file changes, which makes development painful.
#
# If you don't need to run the test suite, you can forgo test dependencies
# and skip `compile-proto-file` codegen by running:
#
#   $ nix-shell --arg fast true
#
{ fast ? false
, compiler ? "ghc884"
, enableDhall ? false
, enableSwagger ? true
}:

with (import ./default.nix { inherit compiler enableDhall enableSwagger; });

if fast then
  proto3-suite-boot.env
else
  proto3-suite.env
