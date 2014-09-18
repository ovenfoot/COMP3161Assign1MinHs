#!/bin/sh
cabal build
#./run_tests.sh --no-color
#for file in tests/task1/3_variables/*.mhs; do
#   ./dist/build/minhs-1/minhs-1 $file
#done
#./dist/build/minhs-1/minhs-1 tests/task1/5_let/1_functions/005.mhs
#./dist/build/minhs-1/minhs-1 smallrecursion.mhs
./dist/build/minhs-1/minhs-1 narytest.mhs