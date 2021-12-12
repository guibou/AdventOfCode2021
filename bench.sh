#!/usr/bin/env bash
cabal run alltests
python bench.py > bench.txt
gnuplot bench.gnuplot
