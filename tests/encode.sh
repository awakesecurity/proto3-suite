#!/bin/bash -eu

hsTmpDir=$1

ghc                                         \
    --make                                  \
    -odir $hsTmpDir                         \
    -hidir $hsTmpDir                        \
    -o $hsTmpDir/simpleEncodeDotProto       \
    $hsTmpDir/TestProto.hs                  \
    $hsTmpDir/TestProtoImport.hs            \
    tests/SimpleEncodeDotProto.hs           \
    >/dev/null
