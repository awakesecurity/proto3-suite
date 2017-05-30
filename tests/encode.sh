#!/bin/bash -eu

hsTmpDir=$1

ghc                                         \
    --make                                  \
    -odir $hsTmpDir                         \
    -hidir $hsTmpDir                        \
    -o $hsTmpDir/simpleEncodeDotProto       \
    $hsTmpDir/GeneratedTestTypes.hs         \
    $hsTmpDir/GeneratedImportedTestTypes.hs \
    tests/SimpleEncodeDotProto.hs           \
    >/dev/null
