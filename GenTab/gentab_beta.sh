#!/bin/bash
# Gentab
# It is a human readable table generator using awk and perl.
# The output is an xls file.
# www.github.com/ennaniux/R_surveys

# TITLE="TITLE from Beta"
# export TITLE

for file in ./*.R
do
Rscript $file
done


for file in ./*.csv
do
./gentab_gamma.sh $file;
./gentab.sh $file
done





