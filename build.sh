#!/bin/sh
ghc -O2 --make main.hs components.hs signals.hs -hidir intermediate/ -odir intermediate/ && mv main synth.out
