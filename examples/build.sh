#!/bin/bash

ghc -threaded -i../src -O2 -fforce-recomp -o example1 --make Example1.hs -llbfgsb 
ghc -threaded -i../src -O2 -fforce-recomp -o example2 --make Example2.hs -llbfgsb
ghc -threaded -i../src -O2 -fforce-recomp -o example3 --make Example3.hs -llbfgsb

