PROTO3_SUITE_NO_TESTS=$(nix-build --no-out-link ../release.nix -A proto3-suite-no-tests)
(cd .. && "${PROTO3_SUITE_NO_TESTS}"/bin/compile-proto-file --proto test-files/test.proto > tests/GeneratedTestTypes.hs)
(cd .. && "${PROTO3_SUITE_NO_TESTS}"/bin/compile-proto-file --proto test-files/test_import.proto > tests/GeneratedImportedTestTypes.hs)
