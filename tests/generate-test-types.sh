PROTO3_SUITE_NO_TESTS=$(nix-build --no-out-link ../release.nix -A proto3-suite-no-tests)
"${PROTO3_SUITE_NO_TESTS}"/bin/compile-proto-file --includeDir ../test-files --proto test_proto.proto        > TestProto.hs
"${PROTO3_SUITE_NO_TESTS}"/bin/compile-proto-file --includeDir ../test-files --proto test_proto_import.proto > TestProtoImport.hs
