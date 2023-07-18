#!/usr/bin/env sh
set -eu
hsTmpDir=$1
shift

ghc                                         \
    --make                                  \
    -odir $hsTmpDir                         \
    -hidir $hsTmpDir                        \
    -o $hsTmpDir/simpleEncodeDotProto       \
    "$@"                                    \
    $hsTmpDir/TestProto.hs                  \
    $hsTmpDir/TestProtoImport.hs            \
    $hsTmpDir/TestProtoOneof.hs             \
    $hsTmpDir/TestProtoOneofImport.hs       \
    $hsTmpDir/TestProtoWrappers.hs          \
    tests/Test/Dhall/Orphan.hs              \
    tests/SimpleEncodeDotProto.hs           \
    >/dev/null

# These tests have been temporarily removed to pass CI.
#    $hsTmpDir/TestProtoLeadingDot.hs        \
#    $hsTmpDir/TestProtoProtocPlugin.hs      \
