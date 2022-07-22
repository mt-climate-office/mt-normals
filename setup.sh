#!/bin/bash
# Build directory structure and download gridMET data. 

variables=("erc"  "etr"  "pet"  "pr"  "rmax"  "rmin"  "sph"  "srad"  "th"  "tmmn"  "tmmx"  "vpd"  "vs")
mkdir -p data/raw
for v in ${variables[@]}
do 
    mkdir data/raw/"$v"
    for year in {1991..2020}
    do 
        wget -nc -c -nd -P data/raw/"$v" http://www.northwestknowledge.net/metdata/data/"$v"_"$year".nc
    done
done

mkdir -p data/processed/montana
