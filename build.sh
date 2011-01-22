#!/bin/sh
ghc --make "$1".hs -hidir intermediate/ -odir intermediate/ && mv "$1" "$1".out
