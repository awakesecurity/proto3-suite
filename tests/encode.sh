#!/bin/bash -eu

hsTmpDir=$1

stack ghc --                          \
    --make                            \
    -odir $hsTmpDir                   \
    -hidir $hsTmpDir                  \
    -o $hsTmpDir/simpleEncodeDotProto \
    $hsTmpDir/Test.hs                 \
    $hsTmpDir/TestImport.hs           \
    tests/SimpleEncodeDotProto.hs     \
    >/dev/null
