#!/bin/sh
cabal build
./run_tests.sh --no-color
for file in tests/task1/3_variables/*.mhs; do
   ./dist/build/minhs-1/minhs-1 $file
done


