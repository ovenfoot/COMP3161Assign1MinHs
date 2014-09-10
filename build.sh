#!/bin/sh
ghc -o dist/build/minhs-1/minhs-1 Main.hs MinHS/*.hs
./run_tests.sh --no-color

