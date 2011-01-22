#!/bin/sh
ghc --make main.hs components.hs signals.hs -hidir intermediate/ -odir intermediate/ && mv main synth.out
