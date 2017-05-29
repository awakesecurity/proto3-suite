# Code generation for tests

Running:

```bash
$ bash generate-test-types.sh
```

from inside this directly will result in `GeneratedTestTypes.hs` and
`GeneratedImportedTestTypes.hs` being regenerated from
`../test-files/test.proto` and `../test-files/test_import.proto`, respectively.

We'll eventually `nix`-ify the building of codegen artifacts, so this is a bit
of a stopgap.
