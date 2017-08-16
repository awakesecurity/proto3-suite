PROTO3_SUITE_NO_TESTS=$(nix-build --no-out-link ../release.nix -A proto3-suite-no-tests)
"${PROTO3_SUITE_NO_TESTS}"/bin/compile-proto-file --includeDir ../test-files --proto test.proto        > GeneratedTestTypes.hs
"${PROTO3_SUITE_NO_TESTS}"/bin/compile-proto-file --includeDir ../test-files --proto test_import.proto > GeneratedImportedTestTypes.hs
