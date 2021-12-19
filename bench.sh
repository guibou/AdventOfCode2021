#!/usr/bin/env bash
cabal build alltests
python bench.py > bench.txt
gnuplot bench.gnuplot

echo "Total Time:"
cat bench.txt | awk "{s += \$3} END {print s}"
