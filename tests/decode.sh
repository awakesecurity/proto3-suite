#!/bin/bash -eu

hsTmpDir=$1

ghc                                         \
    --make                                  \
    -odir $hsTmpDir                         \
    -hidir $hsTmpDir                        \
    -o $hsTmpDir/simpleDecodeDotProto       \
    $hsTmpDir/GeneratedTestTypes.hs         \
    $hsTmpDir/GeneratedImportedTestTypes.hs \
    tests/SimpleDecodeDotProto.hs           \
    >/dev/null
