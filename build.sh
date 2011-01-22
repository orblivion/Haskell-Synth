#!/bin/sh
ghc --make "$1".hs && mv "$1" "$1".out
