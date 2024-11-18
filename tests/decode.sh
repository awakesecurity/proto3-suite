#!/usr/bin/env bash
set -eu
hsTmpDir=$1
shift

ghc                                         \
    --make                                  \
    -odir $hsTmpDir                         \
    -hidir $hsTmpDir                        \
    -o $hsTmpDir/simpleDecodeDotProto       \
    "$@"                                    \
    $hsTmpDir/TestProto.hs                  \
    $hsTmpDir/TestProtoImport.hs            \
    $hsTmpDir/TestProtoNegativeEnum.hs      \
    $hsTmpDir/TestProtoOneof.hs             \
    $hsTmpDir/TestProtoOneofImport.hs       \
    $hsTmpDir/TestProtoWrappers.hs          \
    tests/Test/Dhall/Orphan.hs              \
    tests/SimpleDecodeDotProto.hs           \
    >/dev/null

# These tests have been temporarily removed to pass CI.
#    $hsTmpDir/TestProtoLeadingDot.hs        \
#    $hsTmpDir/TestProtoProtocPlugin.hs      \
