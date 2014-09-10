#!/bin/sh
cabal build
#./run_tests.sh --no-color
for file in tests/task1/0_basics/lists/*.mhs; do
   ./dist/build/minhs-1/minhs-1 $file
done



